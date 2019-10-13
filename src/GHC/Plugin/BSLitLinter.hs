module GHC.Plugin.BSLitLinter (plugin) where

import           Control.Monad
import           Control.Monad.IO.Class
import qualified System.IO              as System

import qualified GhcPlugins
import qualified DynFlags
import qualified Outputable
import qualified Pretty
import qualified TcRnTypes
import qualified SrcLoc
import qualified HsExtension
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
  -> TcRnTypes.TcGblEnv -> TcRnTypes.TcM TcRnTypes.TcGblEnv
bsLitLinterPlugin _args mods tcEnv = do
  let lbinds = TcRnTypes.tcg_binds tcEnv
  lintLHsBinds lbinds
  liftIO $ putStrLn "lbinds:"
  printPprWithDebugStyle lbinds
  pure tcEnv

lintLHsBinds :: HsSyn.LHsBinds HsExtension.GhcTc -> TcRnTypes.TcM ()
lintLHsBinds lbinds = forM_ lbinds lintLHsBind

lintLHsBind :: HsSyn.LHsBind HsExtension.GhcTc -> TcRnTypes.TcM ()
lintLHsBind lbind = go $ SrcLoc.unLoc lbind
  where
    go HsSyn.FunBind{ HsSyn.fun_matches } = lintMatchGroup fun_matches
    go HsSyn.PatBind{ HsSyn.pat_rhs } = lintGRHSs pat_rhs
    go HsSyn.VarBind{ HsSyn.var_rhs } = lintLHsExpr var_rhs
    go HsSyn.AbsBinds{ HsSyn.abs_binds } = lintLHsBinds abs_binds
    go HsSyn.PatSynBind{} = pure ()
    go HsSyn.XHsBindsLR{} = pure ()

type ExprMatchGroup p = HsSyn.MatchGroup p (HsSyn.LHsExpr p)

lintMatchGroup :: ExprMatchGroup HsExtension.GhcTc -> TcRnTypes.TcM ()
lintMatchGroup mg = go mg
  where
    go HsSyn.MG{ HsSyn.mg_alts } = forM_ (SrcLoc.unLoc mg_alts) lintLMatch
    go HsSyn.XMatchGroup{} = pure ()

type LExprMatch p = HsSyn.LMatch p (HsSyn.LHsExpr p)

lintLMatch :: LExprMatch HsExtension.GhcTc -> TcRnTypes.TcM ()
lintLMatch m = go $ SrcLoc.unLoc m
  where
    go HsSyn.Match{ HsSyn.m_grhss } = lintGRHSs m_grhss
    go HsSyn.XMatch{} = pure ()

lintLHsLocalBinds :: HsSyn.LHsLocalBinds HsExtension.GhcTc -> TcRnTypes.TcM ()
lintLHsLocalBinds lbinds = go $ SrcLoc.unLoc lbinds
  where
    go (HsSyn.HsValBinds _ binds) = lintHsValBinds binds
    go (HsSyn.HsIPBinds _ binds) = lintHsIPBinds binds
    go HsSyn.EmptyLocalBinds{} = pure ()
    go HsSyn.XHsLocalBindsLR{} = pure ()

lintHsValBinds :: HsSyn.HsValBinds HsExtension.GhcTc -> TcRnTypes.TcM ()
lintHsValBinds binds = go binds
  where
    go (HsSyn.ValBinds _ binds _) = lintLHsBinds binds
    go HsSyn.XValBindsLR{} = pure ()

lintHsIPBinds :: HsSyn.HsIPBinds HsExtension.GhcTc -> TcRnTypes.TcM ()
lintHsIPBinds ipbinds = go ipbinds
  where
    go (HsSyn.IPBinds _ binds) = forM_ binds lintLIPBind
    go HsSyn.XHsIPBinds{} = pure ()

lintLIPBind :: HsSyn.LIPBind HsExtension.GhcTc -> TcRnTypes.TcM ()
lintLIPBind lbind = go $ SrcLoc.unLoc lbind
  where
    go (HsSyn.IPBind _ _ expr) = lintLHsExpr expr
    go HsSyn.XIPBind{} = pure ()

type GExprRHSs p = HsSyn.GRHSs p (HsSyn.LHsExpr p)

lintGRHSs :: GExprRHSs HsExtension.GhcTc -> TcRnTypes.TcM ()
lintGRHSs grhss = go grhss
  where
    go HsSyn.GRHSs{ HsSyn.grhssGRHSs, HsSyn.grhssLocalBinds } = do
      forM_ grhssGRHSs lintLGRHS
      lintLHsLocalBinds grhssLocalBinds
    go HsSyn.XGRHSs{} = pure ()

type LGExprRHS p = HsSyn.LGRHS p (HsSyn.LHsExpr p)

lintLGRHS :: LGExprRHS HsExtension.GhcTc -> TcRnTypes.TcM ()
lintLGRHS lgrhs = go $ SrcLoc.unLoc lgrhs
  where
    go (HsSyn.GRHS _ _ rhs) = lintLHsExpr rhs
    go HsSyn.XGRHS{} = pure ()

lintLHsExpr :: HsSyn.LHsExpr HsExtension.GhcTc -> TcRnTypes.TcM ()
lintLHsExpr lexpr = do
  liftIO $ putStrLn "lintHsExpr:"
  printPprWithDebugStyle lexpr
  lintHsExpr $ SrcLoc.unLoc lexpr

lintHsExpr :: HsSyn.HsExpr HsExtension.GhcTc -> TcRnTypes.TcM ()
lintHsExpr expr = go expr
  where
    go HsSyn.HsVar{}        = pure ()
    go HsSyn.HsUnboundVar{} = pure ()
    go HsSyn.HsConLikeOut{} = pure ()
    go HsSyn.HsIPVar{}      = pure ()
    go HsSyn.HsOverLabel{}  = pure ()
    go HsSyn.HsLit{}        = pure ()

    go (HsSyn.HsOverLit _ l) = lintHsOverLit l

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
    go (HsSyn.HsDo _ _ lstmts) = forM_ (SrcLoc.unLoc lstmts) lintExprLStmt
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
    go (HsSyn.HsWrap _ _ e) = lintHsExpr e

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

lintPendingTcSplice :: HsSyn.PendingTcSplice -> TcRnTypes.TcM ()
lintPendingTcSplice splice = go splice
  where
    go = undefined

lintHsSplice :: HsSyn.HsSplice HsExtension.GhcTc -> TcRnTypes.TcM ()
lintHsSplice splice = go splice
  where
    go = undefined

lintHsBracket :: HsSyn.HsBracket HsExtension.GhcTc -> TcRnTypes.TcM ()
lintHsBracket bracket = go bracket
  where
    go = undefined

lintLPat :: HsSyn.LPat HsExtension.GhcTc -> TcRnTypes.TcM ()
lintLPat lpat = go $ SrcLoc.unLoc lpat
  where
    go = undefined

lintLHsCmdTop :: HsSyn.LHsCmdTop HsExtension.GhcTc -> TcRnTypes.TcM ()
lintLHsCmdTop cmd = go $ SrcLoc.unLoc cmd
  where
    go = undefined

lintExprLStmt :: HsSyn.ExprLStmt HsExtension.GhcTc -> TcRnTypes.TcM ()
lintExprLStmt lstmt = go $ SrcLoc.unLoc lstmt
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
      lintHsExpr e
    go HsSyn.TransStmt{} = undefined
    go HsSyn.RecStmt{} = undefined
    go HsSyn.XStmtLR{} = pure ()

type HsParStmtBlock p = HsSyn.ParStmtBlock p p

lintParStmtBlock :: HsParStmtBlock HsExtension.GhcTc -> TcRnTypes.TcM ()
lintParStmtBlock block = go block
  where
    go = undefined

lintHsRecordBinds :: HsSyn.HsRecordBinds HsExtension.GhcTc -> TcRnTypes.TcM ()
lintHsRecordBinds binds = go binds
  where
    go = undefined

lintLHsRecUpdField :: HsSyn.LHsRecUpdField HsExtension.GhcTc -> TcRnTypes.TcM ()
lintLHsRecUpdField field = go field
  where
    go = undefined

lintLHsTupArg :: HsSyn.LHsTupArg HsExtension.GhcTc -> TcRnTypes.TcM ()
lintLHsTupArg arg = go $ SrcLoc.unLoc arg
  where
    go (HsSyn.Present _ e) = lintLHsExpr e
    go HsSyn.Missing{} = pure ()
    go HsSyn.XTupArg{} = pure ()

lintHsOverLit :: HsSyn.HsOverLit HsExtension.GhcTc -> TcRnTypes.TcM ()
lintHsOverLit lit = go lit
  where
    go HsSyn.OverLit{ HsSyn.ol_val } = do
      liftIO $ putStrLn "lintHsOverLit:"
      printPprWithDebugStyle ol_val
    go HsSyn.XOverLit{} = pure ()

printPprWithDebugStyle :: Outputable.Outputable a => a -> TcRnTypes.TcM ()
printPprWithDebugStyle x = do
  dynFlags <- DynFlags.getDynFlags
  let mode = Pretty.PageMode
      pprStyle = Outputable.defaultDumpStyle dynFlags
      sdoc = Outputable.ppr x
  liftIO $ Outputable.printSDocLn mode dynFlags System.stdout pprStyle sdoc
