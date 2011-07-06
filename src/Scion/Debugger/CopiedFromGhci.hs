{-# LANGUAGE PatternGuards #-}

module Scion.Debugger.CopiedFromGhci where

import Control.Monad
import Data.List (nubBy)
import Digraph
import GHC

-- These functions are copied and slightly modified versions
-- of the code used in GHCi

getLoadedModules :: GHC.GhcMonad m => m [GHC.ModSummary]
getLoadedModules = do
  graph <- GHC.getModuleGraph
  filterM (GHC.isLoaded . GHC.ms_mod_name) graph

afterLoad :: ([Module],[(Module, Maybe (ImportDecl RdrName))]) -> Ghc ()
afterLoad prev_context = do
  loaded_mod_summaries <- getLoadedModules
  -- let loaded_mods = map GHC.ms_mod loaded_mod_summaries
  --     loaded_mod_names = map GHC.moduleName loaded_mods
  setContextAfterLoad prev_context loaded_mod_summaries

setContextAfterLoad :: ([Module],[(Module, Maybe (ImportDecl RdrName))]) -> [GHC.ModSummary] -> Ghc ()
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
	| Just f' <- GHC.ml_hs_file (GHC.ms_location summary)	= f == f'
   _ `matches` _
	= False

   load_this summary | m <- GHC.ms_mod summary = do
	b <- GHC.moduleIsInterpreted m
	-- Add System.Environment to the scope, so we can run the program
	env_mod <- getEnvironmentMod
	idecl <- GHC.parseImportDecl "import System.Environment"
	if b then setContextKeepingPackageModules prev ([m], [(env_mod, Just idecl)])
       	     else do
                prel_mod <- getPrelude
                setContextKeepingPackageModules prev ([],[(prel_mod,Nothing),(env_mod, Just idecl),(m,Nothing)])

-- | Keep any package modules (except Prelude) when changing the context.
setContextKeepingPackageModules
        :: ([Module],[(Module, Maybe (ImportDecl RdrName))])          -- previous context
        -> ([Module],[(Module, Maybe (ImportDecl RdrName))])          -- new context
        -> Ghc ()
setContextKeepingPackageModules prev_context (as,bs) = do
  let (_,bs0) = prev_context
  prel_mod <- getPrelude
  -- filter everything, not just lefts
  let pkg_modules = filter ((\p -> {- not (isHomeModule p) && -} p /= prel_mod) . fst) bs0
  let bs1 = if null as then nubBy sameFst ((prel_mod,Nothing) : bs) else bs
  GHC.setContext as (nubBy sameFst (bs1 ++ pkg_modules))

getPrelude :: Ghc Module
getPrelude = lookupModule (mkModuleName "Prelude") Nothing

getEnvironmentMod :: Ghc Module
getEnvironmentMod = lookupModule (mkModuleName "System.Environment") Nothing

sameFst :: (Module, Maybe (ImportDecl RdrName)) -> (Module, Maybe (ImportDecl RdrName)) -> Bool
sameFst x y = fst x == fst y

