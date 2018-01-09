{-# LANGUAGE DeriveGeneric #-}

module Test where

import Data.Aeson (ToJSON(..), FromJSON, genericToEncoding, defaultOptions, tagSingleConstructors)
import Data.Text (Text)
import GHC.Generics


test :: String
test = "Test"


data Person = Person {
      name :: Text
    , age  :: Int
    } deriving (Generic, Show)


instance ToJSON Person where
    toEncoding = genericToEncoding (defaultOptions { tagSingleConstructors = True })

instance FromJSON Person
