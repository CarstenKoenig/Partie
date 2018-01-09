{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}

module Games
  ( GenericGame (..)
  , Progress (..)
  , Player
  , GameState (..)
  , getState
  , move
  )where

import Control.Monad.IO.Class (MonadIO)
import Data.ByteString.Lazy (ByteString)
import Data.Aeson (FromJSON, ToJSON, decode, toEncoding, genericToEncoding, defaultOptions, tagSingleConstructors)
import GHC.Generics
import Web.Scotty.Trans

type Player = Int

data Progress
  = Running | Draw | Won [Player] | Lost [Player]
  deriving (Generic, Show)


instance ToJSON Progress where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Progress


data GenericGame =
  forall state move . (FromJSON state, ToJSON state, FromJSON move) =>
  GenericGame { currentState :: state
              , getGameState :: state -> GameState
              , applyMove    :: move -> state -> state
              }

data GameState =
  GameState
  { nrPlayers  :: Int
  , playerTurn :: Int
  , progress   :: Progress
  } deriving (Generic, Show)


instance ToJSON GameState where
    toEncoding = genericToEncoding (defaultOptions { tagSingleConstructors = True })

instance FromJSON GameState



getState :: GenericGame -> GameState
getState (GenericGame st getS _) = getS st


-- | updates the game and returns the game as JSON
move :: (ScottyError e, MonadIO m) => GenericGame -> ActionT e m GenericGame
move g@(GenericGame st get apply) = do
  move' <- jsonData
  let st' = apply move' st
  json st'
  return $ GenericGame st' get apply


invalidGameState :: (Monad m, ScottyError e) => ActionT e m a
invalidGameState = raise $ stringError "could not decode game state"
