{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Wai.Handler.Warp (run)
import Server (application)

main :: IO ()
main =
  run 3000 application
