module Main where

import Lib

import Network.Wai.Handler.Warp

main :: IO ()
main = do
  state <- initState
  run 4444 $ app state
