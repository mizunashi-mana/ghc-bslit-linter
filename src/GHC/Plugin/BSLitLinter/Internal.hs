module GHC.Plugin.BSLitLinter.Internal where

import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Char              as Char
import           Data.Functor
import           Data.Foldable
import           Data.Typeable
import qualified Generics.SYB           as SYB

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
lintLHsBinds lbinds = do
    ctx <- getLinterCtx
    sequence_ $ listify (go ctx) lbinds
  where
    go ctx (GhcPlugins.L loc expr) = case expr of
      HsSyn.HsWrap _ _ e  -> go ctx $ GhcPlugins.L loc e
      HsSyn.HsOverLit _ l -> Just $ lintHsOverLit ctx loc l
      _                   -> Nothing

listify :: Typeable r => (r -> Maybe a) -> SYB.GenericQ [a]
listify f = SYB.everything (++) $ [] `SYB.mkQ` \x -> toList $ f x

newtype LinterCtx = LinterCtx
  { getBSTyConNames :: [GhcPlugins.Name]
  }

getLinterCtx :: TcM.TcM LinterCtx
getLinterCtx = do
    hscEnv <- TcM.getTopEnv
    bsTns <- foldM collectBsTns []
      [ strictByteStringTyConNameM hscEnv
      , lazyByteStringTyConNameM hscEnv
      ]
    pure $ LinterCtx
      { getBSTyConNames = bsTns
      }
  where
    collectBsTns xs tnM = tnM <&> \case
      Just tn -> tn : xs
      Nothing -> xs

    strictByteStringTyConNameM hscEnv = do
      let strictByteStringName = GhcPlugins.mkModuleName "Data.ByteString.Internal"
      fr <- liftIO $ Finder.findImportedModule hscEnv strictByteStringName bytestringPackage
      case fr of
        Finder.Found _ md -> do
          bsTn <- IfaceEnv.lookupOrig md $ GhcPlugins.mkTcOcc "ByteString"
          pure $ Just bsTn
        _ -> pure Nothing

    lazyByteStringTyConNameM hscEnv = do
      let lazyByteStringModule = GhcPlugins.mkModuleName "Data.ByteString.Lazy.Internal"
      fr <- liftIO $ Finder.findImportedModule hscEnv lazyByteStringModule bytestringPackage
      case fr of
        Finder.Found _ md -> do
          bsTn <- IfaceEnv.lookupOrig md $ GhcPlugins.mkTcOcc "ByteString"
          pure $ Just bsTn
        _ -> pure Nothing

    bytestringPackage = Just $ GhcPlugins.fsLit "bytestring"

lintHsOverLit :: LinterCtx -> GhcPlugins.SrcSpan -> HsSyn.HsOverLit HsSyn.GhcTc -> TcM.TcM ()
lintHsOverLit ctx loc lit = case checkHsOverLit ctx lit of
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

checkHsOverLit :: LinterCtx -> HsSyn.HsOverLit HsSyn.GhcTc -> Either String ()
checkHsOverLit ctx lit = case getByteStringLiteral ctx lit of
  Just l | not $ isValidByteStringLiteral l -> Left l
  _ -> pure ()

isByteStringTyCon :: LinterCtx -> GhcPlugins.TyCon -> Bool
isByteStringTyCon ctx tyCon = any (tn ==) $ getBSTyConNames ctx
  where
    tn = GhcPlugins.tyConName tyCon

getByteStringLiteral :: LinterCtx -> HsSyn.HsOverLit HsSyn.GhcTc -> Maybe String
getByteStringLiteral ctx lit = do
  (l, tc) <- getIsStringLiteral lit
  if isByteStringTyCon ctx tc
    then pure l
    else Nothing

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
