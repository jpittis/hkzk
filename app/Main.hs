module Main where

import Lib

main :: IO ()
main = do
  err <- start 4444
  case err of
    Left err -> print err
    Right _ -> return ()
