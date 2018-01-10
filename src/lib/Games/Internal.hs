{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric, DeriveFunctor #-}

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
  , gameProgress :: st -> Progress pl
  , gameMakeMove :: mv -> st -> Maybe st
  }


data Progress pl
  = Running | Draw | Won [pl] | Lost [pl]
  deriving (Generic, Show, Functor)


instance ToJSON pl => ToJSON (Progress pl) where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON pl => FromJSON (Progress pl)
