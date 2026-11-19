module Main where

import qualified Data.Text as T
import Application
import Icon
import Installation (withConfigAccess, withInstalledVersion)
import RuntimePaths (configDirectory)
import Update (updateMode)
import UpdatePlatform (watchCoordinator)
import Control.Monad (unless)
import System.Environment

data Launch = Desktop | Fixture T.Text | Icon FilePath FilePath

launchOptions :: [String] -> Either String Launch
launchOptions arguments = case arguments of
    ["--fixture", address] -> Right (Fixture (T.pack address))
    [address] | take 4 address == "http" -> Right (Fixture (T.pack address))
    ["--icon", directory, source] -> Right (Icon directory source)
    [] -> Right Desktop
    _ -> Left "Usage: sapiens-macos [--fixture URL | --icon DIRECTORY SVG]"

main :: IO ()
main = do
    args <- getArgs
    handled <- updateMode args
    unless handled $ withInstalledVersion $ do
        watchCoordinator
        either (ioError . userError) runLaunch (launchOptions args)

runLaunch :: Launch -> IO ()
runLaunch Desktop = do
    config <- configDirectory
    withConfigAccess config $ runApplication False (T.pack "http://127.0.0.1:4174")
runLaunch (Fixture address) = runApplication True address
runLaunch (Icon directory source) = createIcon directory source
