module GHC.Plugin.BSLitLinter.Internal where

import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Char              as Char
import qualified Generics.SYB           as SYB
import           Data.Maybe             (isJust)
import           Data.Typeable

import qualified GhcPlugins
import qualified Bag
import qualified ErrUtils
import qualified Finder
import qualified IfaceEnv
import qualified TcRnTypes   as TcM
import qualified TcRnMonad   as TcM

import qualified HsExtension as HsSyn
import qualified HsBinds     as HsSyn
import qualified HsExpr      as HsSyn
import qualified HsLit       as HsSyn


lintLHsBinds :: HsSyn.LHsBinds HsSyn.GhcTc -> TcM.TcM ()
lintLHsBinds lbinds = forM_ (SYB.listify checkExpr lbinds) go
  where
    go (GhcPlugins.L loc expr) = case expr of
      HsSyn.HsWrap _ _ e  -> go $ GhcPlugins.L loc e
      HsSyn.HsOverLit _ l -> lintHsOverLit loc l
      _                   -> pure ()

    checkExpr x = isJust (cast x :: Maybe (HsSyn.LHsExpr HsSyn.GhcTc))

lintHsOverLit :: GhcPlugins.SrcSpan -> HsSyn.HsOverLit HsSyn.GhcTc -> TcM.TcM ()
lintHsOverLit loc lit = checkHsOverLit lit >>= \case
    Right{} -> pure ()
    Left l  -> do
      dynFlags <- GhcPlugins.getDynFlags
      liftIO $ GhcPlugins.printOrThrowWarnings dynFlags $ warns dynFlags l
  where
    warns dynFlags l =
      let errDoc = ErrUtils.errDoc
            [ GhcPlugins.text $ "Literal \"" ++ l ++ "\" contains illegal characters for ByteString" ]
            [ ]
            [ GhcPlugins.text "Avoid to use non-8bit characters or may use Text instead."
            ]
          msg = ErrUtils.formatErrDoc dynFlags errDoc
          warnMsg = ErrUtils.mkPlainWarnMsg dynFlags loc msg
      in Bag.unitBag warnMsg

checkHsOverLit :: HsSyn.HsOverLit HsSyn.GhcTc -> TcM.TcM (Either String ())
checkHsOverLit lit = do
  ml <- getByteStringLiteral lit
  pure $ case ml of
    Just l
      | not $ isValidByteStringLiteral l -> Left l
    _ -> pure ()

isByteStringTyCon :: GhcPlugins.TyCon -> TcM.TcM Bool
isByteStringTyCon tyCon = do
    hscEnv <- TcM.getTopEnv
    orMFB
      [ strictByteStringTyConNameM hscEnv
      , lazyByteStringTyConNameM hscEnv
      ]
  where
    tn = GhcPlugins.tyConName tyCon

    strictByteStringTyConNameM hscEnv = do
      fr <- liftIO $ Finder.findImportedModule hscEnv strictByteStringModule bytestringPackage
      case fr of
        Finder.Found _ md -> do
          bsTn <- IfaceEnv.lookupOrig md $ GhcPlugins.mkTcOcc "ByteString"
          pure $ bsTn == tn
        _ -> pure False

    lazyByteStringTyConNameM hscEnv = do
      fr <- liftIO $ Finder.findImportedModule hscEnv lazyByteStringModule bytestringPackage
      case fr of
        Finder.Found _ md -> do
          bsTn <- IfaceEnv.lookupOrig md $ GhcPlugins.mkTcOcc "ByteString"
          pure $ bsTn == tn
        _ -> pure False

    bytestringPackage = Just $ GhcPlugins.fsLit "bytestring"

    strictByteStringModule = GhcPlugins.mkModuleName "Data.ByteString.Internal"
    lazyByteStringModule = GhcPlugins.mkModuleName "Data.ByteString.Lazy.Internal"

    orMFB ms = foldr (\m mb -> m >>= \x -> if x then pure x else mb) (pure False) ms
    {-# INLINE orMFB #-}

getByteStringLiteral :: HsSyn.HsOverLit HsSyn.GhcTc -> TcM.TcM (Maybe String)
getByteStringLiteral lit = case getIsStringLiteral lit of
  Nothing -> pure Nothing
  Just (l, tc) -> do
    b <- isByteStringTyCon tc
    pure $ if b then Just l else Nothing

getIsStringLiteral :: HsSyn.HsOverLit HsSyn.GhcTc -> Maybe (String, GhcPlugins.TyCon)
getIsStringLiteral (HsSyn.OverLit {
    HsSyn.ol_val = HsSyn.HsIsString _ l,
    HsSyn.ol_ext = HsSyn.OverLitTc _ ty
  }) = do
    tc <- GhcPlugins.tyConAppTyCon_maybe ty
    pure (GhcPlugins.unpackFS l, tc)
getIsStringLiteral _ = Nothing

isValidByteStringLiteral :: String -> Bool
isValidByteStringLiteral lit = all isWord8Char lit

isWord8Char :: Char -> Bool
isWord8Char c = Char.ord c <= 256
