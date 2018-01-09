{-# LANGUAGE DeriveGeneric #-}

module Test where

import Data.Text (Text)
import GHC.Generics


test :: String
test = "Test"


data Person = Person {
      name :: Text
    , age  :: Int
    } deriving (Generic, Show)
