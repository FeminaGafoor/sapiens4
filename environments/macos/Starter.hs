module Main where

import Control.Exception
import Control.Monad
import Installation
import UpdatePlatform
import UpdateState
import System.Directory
import System.Environment
import System.FilePath
import System.Process
import System.Exit

main :: IO ()
main = (do
 args <- getArgs
 root <- starterDirectory
 createDirectoryIfMissing True root
 validatePath root
 initialized <- doesFileExist (statePath root)
 when (null args && not initialized) $ withLease (leasePath root) True $ \_ -> do
  exists <- doesFileExist (statePath root)
  unless exists $ do
   executable <- getExecutablePath
   callProcess "/usr/bin/codesign" ["--verify","--deep","--strict",takeDirectory (takeDirectory (takeDirectory executable))]
   let seed = takeDirectory executable </> "../Resources/Seed.dmg"
   version <- freshVersion
   createDirectoryIfMissing True (root </> "versions")
   stagePayload seed (versionPath root version)
   writeUpdateState root (Settled (Selection version Nothing))
 starter args) `catch` \(problem::IOException) -> showUpdateError (displayException problem) >> exitFailure
