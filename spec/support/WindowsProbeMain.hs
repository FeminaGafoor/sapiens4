module Main where
import Shell (runDesktop)
import WindowsUpdate (updateMode)
import System.Environment (getArgs)
import Control.Monad (unless)
main :: IO ()
main = getArgs >>= updateMode >>= \handled -> unless handled (runDesktop True)
