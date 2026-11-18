module Main where
import Installation (leasePath, starter)
import UpdatePlatform (pathPresent, showUpdateError, starterDirectory, validatePath, withIdleLease)
import UpdateState (UpdateState(..), Version, readUpdateState, statePath)
import Control.Exception
import Control.Monad (filterM)
import Data.Aeson (parseJSON, toJSON)
import Data.Aeson.Types (parseEither)
import System.Directory (doesDirectoryExist, listDirectory, removeFile, removePathForcibly)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
main :: IO ()
main = do
 args <- getArgs
 (if args == ["--uninstall"] then uninstall else starter args) `catch` \(problem::IOException) -> do
  if args == ["--uninstall"] then hPutStrLn stderr (displayException problem) `catch` \(_::IOException) -> pure ()
   else showUpdateError (displayException problem)
  exitFailure

uninstall :: IO ()
uninstall = do
 root <- starterDirectory
 withIdleLease (leasePath root) $ do
  validatePath (statePath root)
  readUpdateState root >>= \state -> case state of
   Settled _ -> pure ()
   Pending _ -> fail "Recover the interrupted Sapiens update before uninstalling."
  let marker = root </> "sapiens-installation.id"
  validatePath marker
  identity <- readFile marker
  versions <- ownedDirectories (root </> "versions")
  ownedVersions <- filterM (\path -> do
   let versionMarker = path </> "sapiens-installation.id"
   marked <- pathPresent versionMarker
   if marked then (== identity) <$> readFile versionMarker else pure False) versions
  backups <- ownedDirectories (root </> "backups")
  mapM_ removePathForcibly (ownedVersions ++ backups)
  removeFile (statePath root)

ownedDirectories :: FilePath -> IO [FilePath]
ownedDirectories folder = do
 exists <- pathPresent folder
 if not exists then pure [] else do
  entries <- listDirectory folder
  let paths = [folder </> name | name <- entries, Right (_::Version) <- [parseEither parseJSON (toJSON name)]]
  mapM_ validatePath paths
  filterM doesDirectoryExist paths
