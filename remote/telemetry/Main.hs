module Main where

import Collector
import Control.Concurrent (forkIOWithUnmask, killThread, threadDelay)
import Control.Exception (bracket, finally)
import Control.Monad (forever, void)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Aeson (eitherDecodeFileStrict)
import Data.String (fromString)
import Data.Time (getCurrentTime)
import Network.Wai.Handler.Warp
import System.Environment (getArgs, getEnv)
import System.Posix.Signals (installHandler, sigTERM, sigINT, Handler(Catch))

main :: IO ()
main = do
 args <- getArgs
 path <- case args of [file] -> pure file; _ -> fail "Usage: sapiens-telemetry CONFIG.json (secret in SAPIENS_TELEMETRY_SECRET)"
 config <- either fail pure =<< eitherDecodeFileStrict path
 secret <- BC.pack <$> getEnv "SAPIENS_TELEMETRY_SECRET"
 if B.length secret < 32 then fail "SAPIENS_TELEMETRY_SECRET must contain at least 32 bytes" else pure ()
 collector <- newCollector config secret
 let settings = setInstallShutdownHandler installShutdown $ setGracefulShutdownTimeout (Just 5) $ setHost (fromString (bindAddress config)) $ setPort (listenPort config) defaultSettings
     flushExpired = forever $ do
      threadDelay 1000000
      now <- getCurrentTime
      flushCollector collector (due config now)
 (bracket (forkIOWithUnmask (\unmask -> unmask flushExpired)) killThread $ \_ -> runSettings settings (collectorApplication collector))
  `finally` flushCollector collector (const True)

installShutdown :: IO () -> IO ()
installShutdown action = do
 void (installHandler sigTERM (Catch action) Nothing)
 void (installHandler sigINT (Catch action) Nothing)
