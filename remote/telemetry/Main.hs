module Main where

import Collector
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Aeson (eitherDecodeFileStrict)
import Data.String (fromString)
import Network.Wai.Handler.Warp
import System.Environment (getArgs, getEnv)

main :: IO ()
main = do
 args <- getArgs
 path <- case args of [file] -> pure file; _ -> fail "Usage: sapiens-telemetry CONFIG.json (secret in SAPIENS_TELEMETRY_SECRET)"
 config <- either fail pure =<< eitherDecodeFileStrict path
 secret <- BC.pack <$> getEnv "SAPIENS_TELEMETRY_SECRET"
 if B.length secret < 32 then fail "SAPIENS_TELEMETRY_SECRET must contain at least 32 bytes" else pure ()
 collector <- newCollector config secret
 runSettings (setHost (fromString (bindAddress config)) $ setPort (listenPort config) defaultSettings) (collectorApplication collector)
