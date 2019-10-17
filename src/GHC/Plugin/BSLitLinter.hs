{-# LANGUAGE OverloadedStrings #-}

module GHC.Plugin.BSLitLinter (plugin) where

import GHC.Plugin.BSLitLinter.Internal

import qualified GhcPlugins
import qualified TcRnTypes   as TcM


plugin :: GhcPlugins.Plugin
plugin = GhcPlugins.defaultPlugin
  { GhcPlugins.pluginRecompile = GhcPlugins.purePlugin
  , GhcPlugins.typeCheckResultAction = bsLitLinterPlugin
  }

bsLitLinterPlugin :: [GhcPlugins.CommandLineOption] -> GhcPlugins.ModSummary
  -> TcM.TcGblEnv -> TcM.TcM TcM.TcGblEnv
bsLitLinterPlugin _args _mods tcEnv = do
  let lbinds = TcM.tcg_binds tcEnv
  lintLHsBinds lbinds
  pure tcEnv
