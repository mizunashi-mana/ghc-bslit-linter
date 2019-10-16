module GHC.Plugin.BSLitLinter (plugin) where

import           Control.Monad
import           Control.Monad.IO.Class
import qualified System.IO              as System
import qualified Data.Char              as Char

import qualified GhcPlugins
import qualified Pretty
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
import qualified HsPat       as HsSyn


plugin :: GhcPlugins.Plugin
plugin = GhcPlugins.defaultPlugin
  { GhcPlugins.pluginRecompile = GhcPlugins.purePlugin
  , GhcPlugins.typeCheckResultAction = bsLitLinterPlugin
  }

bsLitLinterPlugin :: [GhcPlugins.CommandLineOption] -> GhcPlugins.ModSummary
  -> TcM.TcGblEnv -> TcM.TcM TcM.TcGblEnv
bsLitLinterPlugin _args mods tcEnv = do
  let lbinds = TcM.tcg_binds tcEnv
  lintLHsBinds lbinds
  pure tcEnv

lintLHsBinds :: HsSyn.LHsBinds HsSyn.GhcTc -> TcM.TcM ()
lintLHsBinds lbinds = forM_ lbinds lintLHsBind

lintLHsBind :: HsSyn.LHsBind HsSyn.GhcTc -> TcM.TcM ()
lintLHsBind lbind = go $ GhcPlugins.unLoc lbind
  where
    go HsSyn.FunBind{ HsSyn.fun_matches } = lintMatchGroup fun_matches
    go HsSyn.PatBind{ HsSyn.pat_rhs } = lintGRHSs pat_rhs
    go HsSyn.VarBind{ HsSyn.var_rhs } = lintLHsExpr var_rhs
    go HsSyn.AbsBinds{ HsSyn.abs_binds } = lintLHsBinds abs_binds
    go HsSyn.PatSynBind{} = pure ()
    go HsSyn.XHsBindsLR{} = pure ()

type ExprMatchGroup p = HsSyn.MatchGroup p (HsSyn.LHsExpr p)

lintMatchGroup :: ExprMatchGroup HsSyn.GhcTc -> TcM.TcM ()
lintMatchGroup mg = go mg
  where
    go HsSyn.MG{ HsSyn.mg_alts } = forM_ (GhcPlugins.unLoc mg_alts) lintLMatch
    go HsSyn.XMatchGroup{} = pure ()

type LExprMatch p = HsSyn.LMatch p (HsSyn.LHsExpr p)

lintLMatch :: LExprMatch HsSyn.GhcTc -> TcM.TcM ()
lintLMatch m = go $ GhcPlugins.unLoc m
  where
    go HsSyn.Match{ HsSyn.m_grhss } = lintGRHSs m_grhss
    go HsSyn.XMatch{} = pure ()

lintLHsLocalBinds :: HsSyn.LHsLocalBinds HsSyn.GhcTc -> TcM.TcM ()
lintLHsLocalBinds lbinds = go $ GhcPlugins.unLoc lbinds
  where
    go (HsSyn.HsValBinds _ binds) = lintHsValBinds binds
    go (HsSyn.HsIPBinds _ binds) = lintHsIPBinds binds
    go HsSyn.EmptyLocalBinds{} = pure ()
    go HsSyn.XHsLocalBindsLR{} = pure ()

lintHsValBinds :: HsSyn.HsValBinds HsSyn.GhcTc -> TcM.TcM ()
lintHsValBinds binds = go binds
  where
    go (HsSyn.ValBinds _ binds _) = lintLHsBinds binds
    go HsSyn.XValBindsLR{} = pure ()

lintHsIPBinds :: HsSyn.HsIPBinds HsSyn.GhcTc -> TcM.TcM ()
lintHsIPBinds ipbinds = go ipbinds
  where
    go (HsSyn.IPBinds _ binds) = forM_ binds lintLIPBind
    go HsSyn.XHsIPBinds{} = pure ()

lintLIPBind :: HsSyn.LIPBind HsSyn.GhcTc -> TcM.TcM ()
lintLIPBind lbind = go $ GhcPlugins.unLoc lbind
  where
    go (HsSyn.IPBind _ _ expr) = lintLHsExpr expr
    go HsSyn.XIPBind{} = pure ()

type GExprRHSs p = HsSyn.GRHSs p (HsSyn.LHsExpr p)

lintGRHSs :: GExprRHSs HsSyn.GhcTc -> TcM.TcM ()
lintGRHSs grhss = go grhss
  where
    go HsSyn.GRHSs{ HsSyn.grhssGRHSs, HsSyn.grhssLocalBinds } = do
      forM_ grhssGRHSs lintLGRHS
      lintLHsLocalBinds grhssLocalBinds
    go HsSyn.XGRHSs{} = pure ()

type LGExprRHS p = HsSyn.LGRHS p (HsSyn.LHsExpr p)

lintLGRHS :: LGExprRHS HsSyn.GhcTc -> TcM.TcM ()
lintLGRHS lgrhs = go $ GhcPlugins.unLoc lgrhs
  where
    go (HsSyn.GRHS _ _ rhs) = lintLHsExpr rhs
    go HsSyn.XGRHS{} = pure ()

lintLHsExpr :: HsSyn.LHsExpr HsSyn.GhcTc -> TcM.TcM ()
lintLHsExpr (GhcPlugins.L loc expr) = go expr
  where
    go HsSyn.HsVar{}        = pure ()
    go HsSyn.HsUnboundVar{} = pure ()
    go HsSyn.HsConLikeOut{} = pure ()
    go HsSyn.HsIPVar{}      = pure ()
    go HsSyn.HsOverLabel{}  = pure ()
    go HsSyn.HsLit{}        = pure ()

    go (HsSyn.HsOverLit _ l) = lintHsOverLit loc l

    go (HsSyn.HsPar _ e) = lintLHsExpr e
    go (HsSyn.HsCoreAnn _ _ _ e) = lintLHsExpr e
    go (HsSyn.HsApp _ e1 e2) = forM_ [e1, e2] lintLHsExpr
    go (HsSyn.HsAppType _ e _) = lintLHsExpr e
    go (HsSyn.OpApp _ e1 _op e2) = forM_ [e1, e2] lintLHsExpr
    go (HsSyn.NegApp _ e _) = lintLHsExpr e
    go (HsSyn.SectionL _ e _op) = lintLHsExpr e
    go (HsSyn.SectionR _ _op e) = lintLHsExpr e
    go (HsSyn.ExplicitTuple _ es _) = forM_ es lintLHsTupArg
    go (HsSyn.ExplicitSum _ _ _ e) = lintLHsExpr e
    go (HsSyn.HsLam _ ms) = lintMatchGroup ms
    go (HsSyn.HsLamCase _ ms) = lintMatchGroup ms
    go (HsSyn.HsCase _ e ms) = do
      lintLHsExpr e
      lintMatchGroup ms
    go (HsSyn.HsIf _ _ cond e1 e2) = forM_ [cond, e1, e2] lintLHsExpr
    go (HsSyn.HsMultiIf _ grhss) = forM_ grhss lintLGRHS
    go (HsSyn.HsLet _ lbinds e) = do
      lintLHsLocalBinds lbinds
      lintLHsExpr e
    go (HsSyn.HsDo _ _ lstmts) = forM_ (GhcPlugins.unLoc lstmts) lintExprLStmt
    go (HsSyn.ExplicitList _ _ es) = forM_ es lintLHsExpr
    go HsSyn.RecordCon{ HsSyn.rcon_flds } = lintHsRecordBinds rcon_flds
    go HsSyn.RecordUpd{ HsSyn.rupd_expr, HsSyn.rupd_flds } = do
      lintLHsExpr rupd_expr
      forM_ rupd_flds lintLHsRecUpdField
    go (HsSyn.ExprWithTySig _ e _) = lintLHsExpr e

    go HsSyn.EWildPat{} = pure ()
    go HsSyn.ELazyPat{} = pure ()
    go (HsSyn.EAsPat _ _ e) = lintLHsExpr e
    go (HsSyn.EViewPat _ p e) = forM_ [p, e] lintLHsExpr

    go (HsSyn.HsSCC _ _ _ e) = lintLHsExpr e
    go (HsSyn.HsWrap _ _ e) = lintLHsExpr $ GhcPlugins.L loc e

    go (HsSyn.HsSpliceE _ s) = lintHsSplice s
    go (HsSyn.HsBracket _ b) = lintHsBracket b
    go HsSyn.HsRnBracketOut{} = error "unreachable"
    go HsSyn.HsTcBracketOut{} = error "unreachable"

    go (HsSyn.HsProc _ p c) = do
      lintLPat p
      lintLHsCmdTop c
    go (HsSyn.HsStatic _ e) = lintLHsExpr e

    go (HsSyn.HsTick _ _ e) = lintLHsExpr e
    go (HsSyn.HsBinTick _ _ _ e) = lintLHsExpr e
    go (HsSyn.HsTickPragma _ _ _ _ e) = lintLHsExpr e
    go (HsSyn.HsArrApp _ e1 e2 _ _) = forM_ [e1, e2] lintLHsExpr
    go (HsSyn.HsArrForm _ _op _ cs) = forM_ cs lintLHsCmdTop
    go HsSyn.HsRecFld{} = pure ()
    go HsSyn.XExpr{} = pure ()

lintPendingTcSplice :: HsSyn.PendingTcSplice -> TcM.TcM ()
lintPendingTcSplice splice = go splice
  where
    go = undefined

lintHsSplice :: HsSyn.HsSplice HsSyn.GhcTc -> TcM.TcM ()
lintHsSplice splice = go splice
  where
    go = undefined

lintHsBracket :: HsSyn.HsBracket HsSyn.GhcTc -> TcM.TcM ()
lintHsBracket bracket = go bracket
  where
    go = undefined

lintLPat :: HsSyn.LPat HsSyn.GhcTc -> TcM.TcM ()
lintLPat lpat = go $ GhcPlugins.unLoc lpat
  where
    go = undefined

lintLHsCmdTop :: HsSyn.LHsCmdTop HsSyn.GhcTc -> TcM.TcM ()
lintLHsCmdTop cmd = go $ GhcPlugins.unLoc cmd
  where
    go = undefined

lintExprLStmt :: HsSyn.ExprLStmt HsSyn.GhcTc -> TcM.TcM ()
lintExprLStmt (GhcPlugins.L loc stmt) = go stmt
  where
    go (HsSyn.LastStmt _ e _ _) = lintLHsExpr e
    go (HsSyn.BindStmt _ p e _ _) = do
      lintLPat p
      lintLHsExpr e
    go (HsSyn.ApplicativeStmt _ _ _) = undefined
    go (HsSyn.BodyStmt _ e _ _) = lintLHsExpr e
    go (HsSyn.LetStmt _ bs) = lintLHsLocalBinds bs
    go (HsSyn.ParStmt _ ss e _) = do
      forM_ ss lintParStmtBlock
      lintLHsExpr $ GhcPlugins.L loc e
    go HsSyn.TransStmt{} = undefined
    go HsSyn.RecStmt{} = undefined
    go HsSyn.XStmtLR{} = pure ()

type HsParStmtBlock p = HsSyn.ParStmtBlock p p

lintParStmtBlock :: HsParStmtBlock HsSyn.GhcTc -> TcM.TcM ()
lintParStmtBlock block = go block
  where
    go = undefined

lintHsRecordBinds :: HsSyn.HsRecordBinds HsSyn.GhcTc -> TcM.TcM ()
lintHsRecordBinds binds = go binds
  where
    go = undefined

lintLHsRecUpdField :: HsSyn.LHsRecUpdField HsSyn.GhcTc -> TcM.TcM ()
lintLHsRecUpdField field = go field
  where
    go = undefined

lintLHsTupArg :: HsSyn.LHsTupArg HsSyn.GhcTc -> TcM.TcM ()
lintLHsTupArg arg = go $ GhcPlugins.unLoc arg
  where
    go (HsSyn.Present _ e) = lintLHsExpr e
    go HsSyn.Missing{} = pure ()
    go HsSyn.XTupArg{} = pure ()

lintHsOverLit :: GhcPlugins.SrcSpan -> HsSyn.HsOverLit HsSyn.GhcTc -> TcM.TcM ()
lintHsOverLit loc lit = getByteStringLiteral lit >>= \case
    Nothing -> pure ()
    Just l -> unless (isValidByteStringLiteral l) $ do
      dynFlags <- GhcPlugins.getDynFlags
      liftIO $ GhcPlugins.printOrThrowWarnings dynFlags $ warns dynFlags l
  where
    warns dynFlags l =
      let errDoc = ErrUtils.errDoc
            [ GhcPlugins.text "Illegal ByteString overloaded literal" ]
            [ GhcPlugins.text $ "In the overloaded literal: \"" ++ l ++ "\"" ]
            [ GhcPlugins.text "May crash here."
            , GhcPlugins.text "Avoit to use non-8bit characters or use Text instead."
            ]
          msg = ErrUtils.formatErrDoc dynFlags errDoc
          warnMsg = ErrUtils.mkPlainWarnMsg dynFlags loc msg
      in Bag.unitBag warnMsg

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

printOutputableForDebug :: GhcPlugins.Outputable a => a -> TcM.TcM ()
printOutputableForDebug x = do
  dynFlags <- GhcPlugins.getDynFlags
  let sdoc = GhcPlugins.ppr x
  let str = GhcPlugins.showSDocDebug dynFlags sdoc
  liftIO $ putStrLn str
