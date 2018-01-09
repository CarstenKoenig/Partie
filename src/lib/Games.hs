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
  GenericGame { getGameState :: state -> GameState
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



getState :: (ScottyError e, MonadIO m) => GenericGame -> ByteString -> ActionT e m GameState
getState (GenericGame getS _) st =
  maybe invalidGameState (return . getS) $ decode st


move :: (ScottyError e, MonadIO m) => GenericGame -> ByteString -> ActionT e m ()
move (GenericGame _ apply) st = do
  move' <- jsonData
  maybe invalidGameState json $ do
    state' <- decode st
    pure $ apply move' state'


invalidGameState :: (Monad m, ScottyError e) => ActionT e m a
invalidGameState = raise $ stringError "could not decode game state"
