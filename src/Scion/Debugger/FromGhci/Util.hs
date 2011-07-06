module Scion.Debugger.FromGhci.Util where

import Control.Monad
import GHC
import Scion.Debugger.FromGhci.Types
import System.Console.Haskeline.MonadException as Haskeline

getLoadedModules :: GHC.GhcMonad m => m [GHC.ModSummary]
getLoadedModules = do
  graph <- GHC.getModuleGraph
  filterM (GHC.isLoaded . GHC.ms_mod_name) graph

-- for convenience...
getPrelude :: DebuggerM Module
getPrelude = getDebuggerState >>= return . prelude

getDynFlags :: GhcMonad m => m DynFlags
getDynFlags = GHC.getSessionDynFlags

setDynFlags :: DynFlags -> DebuggerM [PackageId]
setDynFlags dflags = GHC.setSessionDynFlags dflags

getEnvironmentMod :: DebuggerM Module
getEnvironmentMod = lookupModule (mkModuleName "System.Environment") Nothing

debuggerHandle :: MonadException m => (SomeException -> m a) -> m a -> m a
debuggerHandle h m = Haskeline.catch m $ \e -> unblock (h e)

