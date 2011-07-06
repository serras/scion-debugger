module Main where

import qualified Control.Monad.IO.Class as CMI
import Control.Monad.State
import Data.Aeson
import qualified Data.Aeson.Types as T
import qualified Data.Attoparsec.Char8 as Atto
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import DynFlags (defaultDynFlags)
import Exception
import GHC
import GHC.Paths (libdir)
import qualified MonadUtils
import Server.Commands
import System.Console.Haskeline
import System.Console.Haskeline.MonadException

main :: IO ()
main = defaultErrorHandler defaultDynFlags $ do
         runGhc (Just libdir) $ do
           dflags <- getSessionDynFlags
           setSessionDynFlags $ dflags { hscTarget = HscInterpreted, ghcLink = LinkInMemory }
           runInputT defaultSettings loop
           return ()

loop :: InputT Ghc ()
loop = do maybeLine <- getInputLine ""
          case maybeLine of
            Nothing -> return () -- ctrl+D or EOF
            Just line -> do
              case Atto.parse json (BS.pack line) of
                Atto.Fail _ _ e   -> outputStrLn ("error in command: " ++ e)
                Atto.Done _ value -> case T.parse parseJSON value of
                                       Error e     -> outputStrLn ("error in command: " ++ e)
                                       Success cmd -> do res <- lift $ executeCommand cmd
                                                         outputStr $ "$$DEBUG$$"
                                                         outputStrLn $ LBS.unpack (encode res)
          loop


-- These two instances make Haskeline happy

instance CMI.MonadIO Ghc where
  liftIO = MonadUtils.liftIO

instance MonadException Ghc where
  catch   = gcatch
  block   = gblock
  unblock = gunblock

