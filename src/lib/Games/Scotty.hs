{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}

module Games.Scotty
  ( GenericGame
  , Progress (..)
  , Player
  , GameInfo (nrPlayers, playerTurn, progress)
  , GameId
  , State
  , getState
  , move
  ) where


import Control.Concurrent.STM.TVar
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader
import Control.Monad.STM
import Data.Aeson (FromJSON, ToJSON, decode, toEncoding, genericToEncoding, defaultOptions, tagSingleConstructors)
import Data.ByteString.Lazy (ByteString)
import qualified Data.Map.Strict as Map
import qualified Data.UUID as UUID
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
              , getGameState :: state -> GameInfo
              , applyMove    :: move -> state -> state
              }

data GameInfo =
  GameInfo
  { nrPlayers  :: Int
  , playerTurn :: Int
  , progress   :: Progress
  } deriving (Generic, Show)


instance ToJSON GameInfo where
    toEncoding = genericToEncoding (defaultOptions { tagSingleConstructors = True })

instance FromJSON GameInfo


type GameId = UUID.UUID

data State =
  State { runningGames :: TVar (Map.Map GameId GenericGame)
        }


getState :: (ScottyError e, MonadReader State m, MonadIO m) => GameId -> ActionT e m GameInfo
getState gid =
  getState' <$> getGame gid
  where
    getState' (GenericGame st get _) = get st


-- | updates the game and returns the game as JSON
move :: (ScottyError e, MonadIO m, MonadReader State m) => GameId -> ActionT e m ()
move gid = do
  (GenericGame st get apply) <- getGame gid
  mv <- jsonData
  let st' = apply mv st
  setGame gid (GenericGame st' get apply)
  json st'


getGame :: (ScottyError e, MonadReader State m, MonadIO m) => GameId -> ActionT e m GenericGame
getGame gid = lift (asks runningGames) >>= game >>= maybe (gameNotFound gid) return
  where
    game map = liftIO $ atomically $ do
      map' <- readTVar map
      return $ Map.lookup gid map'


setGame :: (ScottyError e, MonadReader State m, MonadIO m) => GameId -> GenericGame -> ActionT e m ()
setGame gid gg = lift (asks runningGames) >>= setGame'
  where
    setGame' map = liftIO $ atomically $ modifyTVar' map (Map.insert gid gg)


gameNotFound :: (Monad m, ScottyError e) => GameId -> ActionT e m a
gameNotFound gid = raise $ stringError $ "game " ++ UUID.toString gid ++ " not found"



invalidGameState :: (Monad m, ScottyError e) => ActionT e m a
invalidGameState = raise $ stringError "could not decode game state"
