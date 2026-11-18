module Main where

import Control.Exception
import Control.Monad
import Data.Aeson
import System.Directory
import System.Environment
import System.FilePath
import System.Process
import System.Exit (exitFailure)
import UpdateState
import UpdateNative
import Installation

main :: IO ()
main = dispatch `catch` \(problem::SomeException) -> showUpdateError (displayException problem) >> exitFailure

dispatch :: IO ()
dispatch = do
 root <- takeDirectory <$> getExecutablePath >>= canonicalizePath
 args <- getArgs
 withLease (leasePath root) (not (null args)) $ \_ -> case args of
  ["--initialize",name] -> do
   version <- case fromJSON (toJSON name) of Success v -> pure v; Error e -> fail e
   exists <- doesFileExist (statePath root)
   when exists (fail "An installation decision already exists")
   payload <- doesFileExist (versionPath root version </> "Sapiens4.exe")
   unless payload (fail "Initial version is missing")
   writeUpdateState root (Settled (Selection version Nothing))
  [] -> do
   state <- readUpdateState root
   let (version,arguments) = case state of
        Settled selected -> (current selected,[])
        Pending pending -> (current (original pending),["--d2-recover"])
   void $ createProcess (proc (versionPath root version </> "Sapiens4.exe") arguments) {close_fds=True}
  _ -> fail "Unsupported Starter command"
