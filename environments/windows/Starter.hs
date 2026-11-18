module Main where
import Installation (starter)
import UpdatePlatform (showUpdateError)
import Control.Exception
import System.Environment (getArgs)
import System.Exit (exitFailure)
main :: IO ()
main = (getArgs >>= starter) `catch` \(problem::SomeException) -> showUpdateError (displayException problem) >> exitFailure
