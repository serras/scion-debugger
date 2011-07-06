{-# LANGUAGE FlexibleInstances #-}

module Scion.Debugger.FromGhci.Types where

import Bag (emptyBag)
import Control.Monad
import qualified Control.Monad.IO.Class as IOClass
import Control.Monad.Trans as Trans
import Data.Array
import Data.IORef
import Exception
import GHC
import HscTypes (reifyGhc, Session, unGhc, WarnLogMonad, setWarnings, getWarnings)
import Module
import qualified MonadUtils
import System.Console.Haskeline (InputT)
import qualified System.Console.Haskeline as Haskeline

data DebuggerState = DebuggerState
  { prelude        :: GHC.Module
  , break_ctr      :: !Int
  , breaks         :: ![(Int, BreakLocation)]
  , tickarrays     :: ModuleEnv TickArray
  }

data BreakLocation
   = BreakLocation
   { breakModule :: !GHC.Module
   , breakLoc    :: !SrcSpan
   , breakTick   :: {-# UNPACK #-} !Int
   }

instance Eq BreakLocation where
  loc1 == loc2 = breakModule loc1 == breakModule loc2 &&
                 breakTick loc1   == breakTick loc2

type TickArray = Array Int [(BreakIndex,SrcSpan)]

newtype DebuggerM a = DebuggerM { unDebugger :: IORef DebuggerState -> Ghc a }

reflectDebugger :: (Session, IORef DebuggerState) -> DebuggerM a -> IO a
reflectDebugger (s, gs) m = unGhc (unDebugger m gs) s

reifyDebugger :: ((Session, IORef DebuggerState) -> IO a) -> DebuggerM a
reifyDebugger f = DebuggerM f'
  where
    -- f' :: IORef DebuggerState -> Ghc a
    f' gs = reifyGhc (f'' gs)
    -- f'' :: IORef DebuggerState -> Session -> IO a
    f'' gs s = f (s, gs)

startDebugger :: DebuggerM a -> DebuggerState -> Ghc a
startDebugger g state = do ref <- MonadUtils.liftIO $ newIORef state
                           unDebugger g ref

instance Monad DebuggerM where
  (DebuggerM m) >>= k  =  DebuggerM $ \s -> m s >>= \a -> unDebugger (k a) s
  return a  = DebuggerM $ \_ -> return a

instance Functor DebuggerM where
    fmap f m = m >>= return . f

instance WarnLogMonad DebuggerM where
    setWarnings _ = return ()
    getWarnings   = return emptyBag

instance WarnLogMonad (InputT DebuggerM) where
    setWarnings _ = return ()
    getWarnings   = return emptyBag

getDebuggerState :: DebuggerM DebuggerState
getDebuggerState = DebuggerM $ \r -> MonadUtils.liftIO $ readIORef r
setDebuggerState :: DebuggerState -> DebuggerM ()
setDebuggerState s = DebuggerM $ \r -> MonadUtils.liftIO $ writeIORef r s

liftGhc :: Ghc a -> DebuggerM a
liftGhc m = DebuggerM $ \_ -> m

instance MonadUtils.MonadIO DebuggerM where
  liftIO = liftGhc . MonadUtils.liftIO

instance Trans.MonadIO Ghc where
  liftIO = MonadUtils.liftIO

instance GhcMonad DebuggerM where
  setSession s' = liftGhc $ setSession s'
  getSession    = liftGhc $ getSession

instance GhcMonad (InputT DebuggerM) where
  setSession = lift . setSession
  getSession = lift getSession

instance MonadUtils.MonadIO (InputT DebuggerM) where
  liftIO = Trans.liftIO

instance ExceptionMonad DebuggerM where
  gcatch m h = DebuggerM $ \r -> unDebugger m r `gcatch` (\e -> unDebugger (h e) r)
  gblock (DebuggerM m)   = DebuggerM $ \r -> gblock (m r)
  gunblock (DebuggerM m) = DebuggerM $ \r -> gunblock (m r)
  gmask f = DebuggerM $ \s -> gmask $ \io_restore ->
                                        let g_restore (DebuggerM m) = DebuggerM $ \s' -> io_restore (m s')
                                        in  unDebugger (f g_restore) s

instance IOClass.MonadIO DebuggerM where
  liftIO = MonadUtils.liftIO

instance Haskeline.MonadException DebuggerM where
  catch = gcatch
  block = gblock
  unblock = gunblock
  -- XXX when Haskeline's MonadException changes, we can drop our 
  -- deprecated block/unblock methods

instance ExceptionMonad (InputT DebuggerM) where
  gcatch   = Haskeline.catch
  gmask f  = Haskeline.block (f Haskeline.unblock) -- slightly wrong
  gblock   = Haskeline.block
  gunblock = Haskeline.unblock

