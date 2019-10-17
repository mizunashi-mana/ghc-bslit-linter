module GHC.Plugin.BSLitLinter.Util where

import Control.Monad.IO.Class

import qualified GhcPlugins
import qualified TcRnTypes   as TcM


printOutputableForDebug :: GhcPlugins.Outputable a => a -> TcM.TcM ()
printOutputableForDebug x = do
  dynFlags <- GhcPlugins.getDynFlags
  let sdoc = GhcPlugins.ppr x
  let str = GhcPlugins.showSDocDebug dynFlags sdoc
  liftIO $ putStrLn str
