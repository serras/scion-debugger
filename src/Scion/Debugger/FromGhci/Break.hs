module Scion.Debugger.FromGhci.Break where

import Control.Monad
import Data.Array
import Data.Function (on)
import Data.List (sortBy, partition)
import Data.Maybe (listToMaybe)
import GHC
import Module
import MonadUtils
import Scion.Debugger.FromGhci.Types

breakByModuleLine :: Module -> Int -> Bool -> DebuggerM Bool
breakByModuleLine mod line on = findBreakAndSet mod (findBreakByLine line) on

findBreakAndSet :: Module -> (TickArray -> Maybe (Int, SrcSpan)) -> Bool -> DebuggerM Bool
findBreakAndSet mod lookupTickTree on = do 
   tickArray <- getTickArray mod
   (breakArray, _) <- getModBreak mod
   case lookupTickTree tickArray of 
      Nothing  -> return False
      Just (tick, span) -> do
         success <- liftIO $ setBreakFlag on breakArray tick
         if success 
            then do _ <- recordBreak $ BreakLocation
                                         { breakModule = mod
                                         , breakLoc = span
                                         , breakTick = tick
                                         }
                    return True
            else return False

-- When a line number is specified, the current policy for choosing
-- the best breakpoint is this:
--    - the leftmost complete subexpression on the specified line, or
--    - the leftmost subexpression starting on the specified line, or
--    - the rightmost subexpression enclosing the specified line
--
findBreakByLine :: Int -> TickArray -> Maybe (BreakIndex,SrcSpan)
findBreakByLine line arr
  | not (inRange (bounds arr) line) = Nothing
  | otherwise =
    listToMaybe (sortBy (leftmost_largest `on` snd)  complete)   `mplus`
    listToMaybe (sortBy (leftmost_smallest `on` snd) incomplete) `mplus`
    listToMaybe (sortBy (rightmost `on` snd) ticks)
  where 
        ticks = arr ! line

        starts_here = [ tick | tick@(_,span) <- ticks,
                               GHC.srcSpanStartLine span == line ]

        (complete,incomplete) = partition ends_here starts_here
            where ends_here (_,span) = GHC.srcSpanEndLine span == line

recordBreak :: BreakLocation -> DebuggerM (Bool{- was already present -}, Int)
recordBreak brkLoc = do
   st <- getDebuggerState
   let oldActiveBreaks = breaks st 
   -- don't store the same break point twice
   case [ nm | (nm, loc) <- oldActiveBreaks, loc == brkLoc ] of
     (nm:_) -> return (True, nm)
     [] -> do
      let oldCounter = break_ctr st
          newCounter = oldCounter + 1
      setDebuggerState $ st { break_ctr = newCounter,
                              breaks = (oldCounter, brkLoc) : oldActiveBreaks
                         }
      return (False, oldCounter)

-- --------------------------------------------------------------------------
-- Tick arrays

getTickArray :: Module -> DebuggerM TickArray
getTickArray modl = do
   st <- getDebuggerState
   let arrmap = tickarrays st
   case lookupModuleEnv arrmap modl of
      Just arr -> return arr
      Nothing  -> do
        (_breakArray, ticks) <- getModBreak modl 
        let arr = mkTickArray (assocs ticks)
        setDebuggerState $ st { tickarrays = extendModuleEnv arrmap modl arr }
        return arr

discardTickArrays :: DebuggerM ()
discardTickArrays = do
   st <- getDebuggerState
   setDebuggerState $ st { tickarrays = emptyModuleEnv }

mkTickArray :: [(BreakIndex,SrcSpan)] -> TickArray
mkTickArray ticks
  = accumArray (flip (:)) [] (1, max_line) 
        [ (line, (nm,span)) | (nm,span) <- ticks,
                              line <- srcSpanLines span ]
    where
        max_line = foldr max 0 (map GHC.srcSpanEndLine (map snd ticks))
        srcSpanLines span = [ GHC.srcSpanStartLine span .. 
                              GHC.srcSpanEndLine span ]

lookupModule :: GHC.GhcMonad m => String -> m Module
lookupModule modName
   = GHC.lookupModule (GHC.mkModuleName modName) Nothing

-- don't reset the counter back to zero?
discardActiveBreakPoints :: DebuggerM ()
discardActiveBreakPoints = do
   st <- getDebuggerState
   mapM_ (turnOffBreak.snd) (breaks st)
   setDebuggerState $ st { breaks = [] }

deleteBreak :: Int -> DebuggerM ()
deleteBreak identity = do
   st <- getDebuggerState
   let oldLocations    = breaks st
       (this,rest)     = partition (\loc -> fst loc == identity) oldLocations
   if null this 
      then return ()
      else do mapM_ (turnOffBreak.snd) this
              setDebuggerState $ st { breaks = rest }

turnOffBreak :: BreakLocation -> DebuggerM Bool
turnOffBreak loc = do
  (arr, _) <- getModBreak (breakModule loc)
  liftIO $ setBreakFlag False arr (breakTick loc)

getModBreak :: Module -> DebuggerM (GHC.BreakArray, Array Int SrcSpan)
getModBreak mod = do
   Just mod_info <- GHC.getModuleInfo mod
   let modBreaks  = GHC.modInfoModBreaks mod_info
   let array      = GHC.modBreaks_flags modBreaks
   let ticks      = GHC.modBreaks_locs  modBreaks
   return (array, ticks)

setBreakFlag :: Bool -> GHC.BreakArray -> Int -> IO Bool 
setBreakFlag toggle array index
   | toggle    = GHC.setBreakOn array index 
   | otherwise = GHC.setBreakOff array index

