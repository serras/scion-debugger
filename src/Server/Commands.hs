{-# LANGUAGE ScopedTypeVariables #-}

module Server.Commands where

import Control.Applicative
import Control.Monad
import Data.Aeson
import qualified Data.Map as M
import qualified Data.Text as T
import DynFlags hiding (String)
import Exception (gcatch)
import qualified GHC
import HscTypes
import Scion.Debugger.FromGhci

data Command = AddPackages [String]
             | SetSourceDirs [String]
             | LoadMain String {- main module -}
             | RunMain [String] {- args -}
             | SetBreakpoint String {- module -} Int {- line -} Bool {- on? -}

executeCommand :: Command -> DebuggerM Value
executeCommand (AddPackages _) =
  do return $ String (T.pack "ok")
executeCommand (SetSourceDirs dirs) =
  do dflags <- GHC.getSessionDynFlags
     GHC.setSessionDynFlags $ dflags { importPaths = dirs }
     return $ String (T.pack "ok")
executeCommand (LoadMain mname) =
  (do prev_context <- GHC.getContext
      GHC.setTargets [ Target (TargetModule (GHC.mkModuleName mname)) True Nothing ]
      GHC.load GHC.LoadAllTargets
      -- Do the "after load" as if we were GHCi
      afterLoad prev_context
      return $ String (T.pack "ok")
  )
  `gcatch`
  (\(e :: SourceError) -> return $ String (T.pack $ "error: " ++ show e))
executeCommand (SetBreakpoint mname line on) =
  do md <- lookupModule mname
     done <- breakByModuleLine md line on
     return $ Bool done
executeCommand (RunMain args) =
  do -- Add System.Environment
     runStmt2 ("System.Environment.withArgs " ++ show args ++ " (main)") GHC.RunToCompletion
     return $ String (T.pack "completed")

runStmt2 :: String -> GHC.SingleStep -> DebuggerM GHC.RunResult
runStmt2 expr step = handleSourceError (\e -> do GHC.printExceptionAndWarnings e
                                                 return GHC.RunFailed) $ 
                       do GHC.runStmt expr step

instance FromJSON Command where
  parseJSON (Object v) = case M.lookup (T.pack "command") v of
                           Just (String e) ->
                             case T.unpack e of
                               "add-packages"    -> AddPackages <$> v .: T.pack "packages"
                               "set-source-dirs" -> SetSourceDirs <$> v .: T.pack "dirs"
                               "load-main"       -> LoadMain <$> v .: T.pack "module"
                               "run-main"        -> RunMain <$> v .: T.pack "args"
                               "set-breakpoint"  -> SetBreakpoint <$> v .: T.pack "module"
                                                                  <*> v .: T.pack "line"
                                                                  <*> v .: T.pack "on"
                               _                 -> mzero
                           _ -> mzero
  parseJSON _          = mzero

