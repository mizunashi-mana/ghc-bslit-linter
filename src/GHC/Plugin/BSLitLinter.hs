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


plugin :: GhcPlugins.Plugin
plugin = GhcPlugins.defaultPlugin
  { GhcPlugins.pluginRecompile = GhcPlugins.purePlugin
  , GhcPlugins.typeCheckResultAction = bsLitLinterPlugin
  }

bsLitLinterPlugin :: [GhcPlugins.CommandLineOption] -> GhcPlugins.ModSummary
  -> TcRnTypes.TcGblEnv -> TcRnTypes.TcM TcRnTypes.TcGblEnv
bsLitLinterPlugin _args mods tcEnv = do
  let lbinds = TcRnTypes.tcg_binds tcEnv
  --lintLHsBinds lbinds
  printPprWithDebugStyle lbinds
  pure tcEnv

lintLHsBinds :: HsSyn.LHsBinds HsExtension.GhcTc -> TcRnTypes.TcM ()
lintLHsBinds lbinds = forM_ lbinds lintLHsBind

lintLHsBind :: HsSyn.LHsBind HsExtension.GhcTc -> TcRnTypes.TcM ()
lintLHsBind lbind = go $ SrcLoc.unLoc lbind
  where
    go HsSyn.FunBind{ HsSyn.fun_matches } = undefined
    go HsSyn.PatBind{ HsSyn.pat_rhs } = lintGRHSs pat_rhs
    go HsSyn.VarBind{ HsSyn.var_rhs } = lintLHsExpr var_rhs
    go HsSyn.AbsBinds{ HsSyn.abs_binds } = lintLHsBinds abs_binds
    go HsSyn.PatSynBind{} = pure ()
    go HsSyn.XHsBindsLR{} = pure ()

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
lintLHsExpr lexpr = go $ SrcLoc.unLoc lexpr
  where
    go (HsSyn.HsOverLit _ lit) = lintHsOverLit lit
    go _                       = pure ()

lintHsOverLit :: HsSyn.HsOverLit HsExtension.GhcTc -> TcRnTypes.TcM ()
lintHsOverLit lit = go lit
  where
    go HsSyn.OverLit{ HsSyn.ol_val, HsSyn.ol_witness } = do
      printPprWithDebugStyle ol_val
      printPprWithDebugStyle ol_witness
    go HsSyn.XOverLit{} = pure ()

printPprWithDebugStyle :: Outputable.Outputable a => a -> TcRnTypes.TcM ()
printPprWithDebugStyle x = do
  dynFlags <- DynFlags.getDynFlags
  let mode = Pretty.PageMode
      pprStyle = Outputable.defaultDumpStyle dynFlags
      sdoc = Outputable.ppr x
  liftIO $ Outputable.printSDocLn mode dynFlags System.stdout pprStyle sdoc
