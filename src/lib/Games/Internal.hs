{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Games.Internal
  ( Game (..)
  , Progress (..)
  ) where


import Data.Aeson (FromJSON, ToJSON, toEncoding, genericToEncoding, defaultOptions)
import GHC.Generics


data Game st mv pl =
  Game
  { gamePlayers  :: [pl]
  , gameTurn     :: st -> Maybe pl
  , gameProgress :: st -> Progress
  , gameMakeMove :: mv -> st -> Maybe st
  }


data Progress
  = Running | Draw | Won [Int] | Lost [Int]
  deriving (Generic, Show)


instance ToJSON Progress where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Progress
