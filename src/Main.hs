module Main where

import Control.Monad.State
import Data.Aeson
import qualified Data.Aeson.Types as T
import qualified Data.Attoparsec.Char8 as Atto
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import DynFlags (defaultDynFlags)
import GHC
import GHC.Paths (libdir)
import Module
import MonadUtils
import Scion.Debugger.FromGhci
import Server.Commands
import System.Console.Haskeline

main :: IO ()
main = defaultErrorHandler defaultDynFlags $ do
         runGhc (Just libdir) $ do
           dflags <- getSessionDynFlags
           setSessionDynFlags $ dflags { hscTarget = HscInterpreted, ghcLink = LinkInMemory }
           prel_mod <- GHC.lookupModule (GHC.mkModuleName "Prelude") Nothing
           GHC.setContext [] [(prel_mod, Nothing)]
           startDebugger debuggerLoop
                         DebuggerState { prelude    = prel_mod
                                       , break_ctr  = 0
                                       , breaks     = []
                                       , tickarrays = emptyModuleEnv
                                       }
           return ()

debuggerLoop :: DebuggerM ()
debuggerLoop = debuggerHandle (\_ -> return ())
                              (runInputT defaultSettings loop)

loop :: InputT DebuggerM ()
loop = do maybeLine <- getInputLine "> "
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

