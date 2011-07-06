{-# LANGUAGE PatternGuards #-}

module Scion.Debugger.FromGhci.Load where

import Data.List (nubBy)
import Digraph
import GHC
import Scion.Debugger.FromGhci.Break
import Scion.Debugger.FromGhci.Types
import Scion.Debugger.FromGhci.Util

afterLoad :: ([Module],[(Module, Maybe (ImportDecl RdrName))]) -> DebuggerM ()
afterLoad prev_context = do
  discardTickArrays
  loaded_mod_summaries <- getLoadedModules
  setContextAfterLoad prev_context loaded_mod_summaries

setContextAfterLoad :: ([Module],[(Module, Maybe (ImportDecl RdrName))]) -> [GHC.ModSummary] -> DebuggerM ()
setContextAfterLoad prev [] = do
  prel_mod <- getPrelude
  setContextKeepingPackageModules prev ([], [(prel_mod, Nothing)])
setContextAfterLoad prev ms = do
  -- load a target if one is available, otherwise load the topmost module.
  targets <- GHC.getTargets
  case [ m | Just m <- map (findTarget ms) targets ] of
    []    -> 
      let graph' = flattenSCCs (GHC.topSortModuleGraph True ms Nothing) in
      load_this (last graph')      
    (m:_) -> 
      load_this m
 where
   findTarget ms t
    = case filter (`matches` t) ms of
    []    -> Nothing
    (m:_) -> Just m

   summary `matches` Target (TargetModule m) _ _
    = GHC.ms_mod_name summary == m
   summary `matches` Target (TargetFile f _) _ _ 
    | Just f' <- GHC.ml_hs_file (GHC.ms_location summary)    = f == f'
   _ `matches` _
    = False

   load_this summary | m <- GHC.ms_mod summary = do
    b <- GHC.moduleIsInterpreted m
    -- Add System.Environment to the scope, so we can run the program
    prel_mod <- getPrelude
    env_mod <- getEnvironmentMod
    idecl <- GHC.parseImportDecl "import System.Environment"
    if b then setContextKeepingPackageModules prev ([m], [(env_mod, Just idecl)])
         else setContextKeepingPackageModules prev ([],[(prel_mod,Nothing),(env_mod, Just idecl),(m,Nothing)])

-- | Keep any package modules (except Prelude) when changing the context.
setContextKeepingPackageModules
        :: ([Module],[(Module, Maybe (ImportDecl RdrName))])          -- previous context
        -> ([Module],[(Module, Maybe (ImportDecl RdrName))])          -- new context
        -> DebuggerM ()
setContextKeepingPackageModules prev_context (as,bs) = do
  let (_,bs0) = prev_context
  prel_mod <- getPrelude
  -- filter everything, not just lefts
  let pkg_modules = filter ((\p -> {- not (isHomeModule p) && -} p /= prel_mod) . fst) bs0
  let bs1 = if null as then nubBy sameFst ((prel_mod,Nothing) : bs) else bs
  GHC.setContext as (nubBy sameFst (bs1 ++ pkg_modules))
  return ()

sameFst :: (Module, Maybe (ImportDecl RdrName)) -> (Module, Maybe (ImportDecl RdrName)) -> Bool
sameFst x y = fst x == fst y

