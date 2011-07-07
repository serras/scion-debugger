{-# LANGUAGE ScopedTypeVariables #-}

module Server.Commands where

import Bag (bagToList)
import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.List (sortBy)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Data.Vector (fromList)
import Debugger (showTerm)
import DynFlags hiding (String)
import Exception (gcatch)
import FastString (unpackFS)
import qualified GHC
import HscTypes
import Name
import Outputable
import PprTyThing
import Scion.Debugger.FromGhci

data Command = AddPackages [String]
             | SetSourceDirs [String]
             | LoadMain String {- main module -}
             | RunMain [String] {- args -}
             | RunMainStep [String] {- args -}
             | SetBreakpoint String {- module -} Int {- line -} Bool {- on? -}
             | Step
             | Continue

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
  do result <- runProtectedStatement $ GHC.runStmt ("System.Environment.withArgs " ++ show args ++ " (main)") GHC.RunAndLogSteps
     afterRun result
executeCommand (RunMainStep args) =
  do result <- runProtectedStatement $ GHC.runStmt ("System.Environment.withArgs " ++ show args ++ " (main)") GHC.SingleStep
     afterRun result
executeCommand Step =
  do result <- runProtectedStatement $ GHC.resume (const True) GHC.SingleStep
     afterRun result
executeCommand Continue =
  do result <- runProtectedStatement $ GHC.resume (const True) GHC.RunAndLogSteps
     afterRun result

afterRun :: Either String GHC.RunResult -> DebuggerM Value
afterRun (Left msg) =
  return $ object [ T.pack "result" .= T.pack "error", T.pack "message" .= T.pack msg ]
afterRun (Right (GHC.RunOk names)) = do
  names_info <- getNamesInfo names
  return $ object [ T.pack "result" .= T.pack "completed"
                  , T.pack "vars"   .= names_info
                  ]
afterRun (Right GHC.RunFailed) = 
  return $ object [ T.pack "result" .= T.pack "failed" ]
afterRun (Right (GHC.RunException e)) =
  return $ object [ T.pack "result" .= T.pack "exception", T.pack "message" .= T.pack (show e) ]
afterRun (Right (GHC.RunBreak _ names Nothing)) = do
  names_info <- getNamesInfo names
  history <- getHistory
  history_info <- mapM getHistoryInfo history
  return $ object [ T.pack "result"  .= T.pack "break"
                  , T.pack "vars"    .= names_info
                  , T.pack "place"   .= Null
                  , T.pack "history" .= Array (fromList history_info)
                  ]
afterRun (Right (GHC.RunBreak th names (Just info))) = do
  names_info <- getNamesInfo names
  bid_loc <- toBreakIdAndLocation info
  history <- getHistory
  history_info <- mapM getHistoryInfo history
  case bid_loc of
    Nothing       -> afterRun (Right (GHC.RunBreak th names Nothing))
    Just (_, loc) -> return $ object [ T.pack "result"  .= T.pack "break"
                                     , T.pack "vars"    .= names_info
                                     , T.pack "place"   .= getSrcSpanInfo (breakLoc loc)
                                     , T.pack "history" .= Array (fromList history_info)
                                     ]

runProtectedStatement :: DebuggerM GHC.RunResult -> DebuggerM (Either String GHC.RunResult)
runProtectedStatement action = handleSourceError (\e -> do let msg = show $ bagToList (srcErrorMessages e)
                                                           return $ Left msg)
                                                 (do r <- action
                                                     return $ Right r)

getNamesInfo :: [GHC.Name] -> DebuggerM Value
getNamesInfo names = 
  do let namesSorted = sortBy compareNames names
     tythings <- catMaybes `liftM` mapM GHC.lookupName namesSorted
     let things = [ident | AnId ident <- tythings]
     getIdsInfo things

getIdsInfo :: [GHC.Id] -> DebuggerM Value
getIdsInfo things = 
  do let names = map (ppr . getName) things
         types  = map (pprTypeForUser False . GHC.idType) things
     contents <- mapM (pprContents) things
     return $ Array $ fromList $ map (\(n, c, t) -> object [ T.pack "name"     .= T.pack (showSDocOneLine n)
                                                           , T.pack "contents" .= T.pack (showSDocOneLine c)
                                                           , T.pack "type"     .= T.pack (showSDocOneLine t)
                                                           ]) $
                                     zip3 names contents types

compareNames :: GHC.Name -> GHC.Name -> Ordering
n1 `compareNames` n2 = compareWith n1 `compare` compareWith n2
    where compareWith n = (getOccString n, getSrcSpan n)

pprContents :: GhcMonad m => GHC.Id -> m SDoc
pprContents ident = do
  let depthBound = 100
  term      <- GHC.obtainTermFromId depthBound False ident
  docs_term <- showTerm term
  return docs_term

getSrcSpanInfo :: GHC.SrcSpan -> Value
getSrcSpanInfo sp = object [ T.pack "file"       .= unpackFS (GHC.srcSpanFile sp)
                           , T.pack "start_line" .= GHC.srcSpanStartLine sp
                           , T.pack "start_col"  .= GHC.srcSpanStartCol sp
                           , T.pack "end_line"   .= GHC.srcSpanEndLine sp
                           , T.pack "end_col"    .= GHC.srcSpanEndCol sp
                           ]

getHistory :: DebuggerM [([GHC.Name], Int, GHC.SrcSpan)]
getHistory = do (ctx:_) <- GHC.getResumeContext
                let hist_length = (length (GHC.resumeHistory ctx)) - 1
                history <- mapM (\_ -> GHC.back) [1 .. hist_length]
                mapM_ (\_ -> GHC.forward) [1 .. hist_length]
                return history

getHistoryInfo :: ([GHC.Name], Int, GHC.SrcSpan) -> DebuggerM Value
getHistoryInfo (names, _, sp) = do vars <- getNamesInfo names
                                   return $ object [ T.pack "vars"      .= vars
                                                   , T.pack "place"     .= getSrcSpanInfo sp
                                                   ]

instance FromJSON Command where
  parseJSON (Object v) = case M.lookup (T.pack "command") v of
                           Just (String e) ->
                             case T.unpack e of
                               "add-packages"    -> AddPackages <$> v .: T.pack "packages"
                               "set-source-dirs" -> SetSourceDirs <$> v .: T.pack "dirs"
                               "load-main"       -> LoadMain <$> v .: T.pack "module"
                               "run-main"        -> RunMain <$> v .: T.pack "args"
                               "run-main-step"   -> RunMainStep <$> v .: T.pack "args"
                               "set-breakpoint"  -> SetBreakpoint <$> v .: T.pack "module"
                                                                  <*> v .: T.pack "line"
                                                                  <*> v .: T.pack "on"
                               "step"            -> pure Step
                               "continue"        -> pure Continue
                               _                 -> mzero
                           _ -> mzero
  parseJSON _          = mzero

