{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid (mconcat)
import Data.Text.Lazy (pack)
import Test
import Web.Scotty

main = scotty 3000 $ do
  get "/" $ do
    html $ mconcat ["<h1>Scotty, ", pack test, " me up!</h1>"]
  get "/:word" $ do
    beam <- param "word"
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
