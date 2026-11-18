module Main where
import Installation (starter)
import UpdatePlatform (showUpdateError)
import Control.Exception
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
main :: IO ()
main = do
 args <- getArgs
 starter args `catch` \(problem::IOException) -> do
  if args == ["--uninstall"] then hPutStrLn stderr (displayException problem) `catch` \(_::IOException) -> pure ()
   else showUpdateError (displayException problem)
  exitFailure
