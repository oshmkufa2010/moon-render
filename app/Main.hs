module Main where

import System.Environment
import MoonRender (render)

main :: IO ()
main = do
  [filePath, widthStr] <- getArgs
  render filePath (read widthStr) >>= putStrLn
