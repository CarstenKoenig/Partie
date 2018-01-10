{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Games.Scotty
  ( GenericGame
  , Progress (..)
  , Player
  , GameInfo (nrPlayers, playerTurn, progress)
  , GameId
  , State
  , getInfo
  , move
  ) where


import Control.Concurrent.STM.TVar
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader
import Control.Monad.STM
import Data.Aeson (FromJSON, ToJSON, encode, decode, toEncoding, genericToEncoding, defaultOptions, tagSingleConstructors)
import Data.ByteString.Lazy (ByteString)
import qualified Data.Map.Strict as Map
import qualified Data.UUID as UUID
import GHC.Generics
import Games.Internal
import Web.Scotty.Trans


data GenericGame =
  forall state move . (ToJSON state, FromJSON move) =>
  GenericGame { currentState :: state
              , getGameState :: state -> GameInfo
              , applyMove    :: move -> state -> Maybe state
              }


data GameInfo =
  GameInfo
  { nrPlayers  :: Player
  , playerTurn :: Maybe Player
  , progress   :: Progress Player
  } deriving (Generic, Show)

instance ToJSON GameInfo where
    toEncoding = genericToEncoding (defaultOptions { tagSingleConstructors = True })

instance FromJSON GameInfo


type Player = Int


-- | translates the game template into a GenericGame that does not expose the games types
fromGame :: forall st mv pl . (ToJSON st, FromJSON mv, Enum pl) => Game st mv pl -> st -> GenericGame
fromGame game curState =
  GenericGame curState getGameState' applyMove'
  where
    getGameState' :: st -> GameInfo
    getGameState' st =
      let nr = length $ gamePlayers game
          pl = fromEnum <$> gameTurn game st
          pr = fmap fromEnum $ gameProgress game st
      in GameInfo nr pl pr

    applyMove' :: mv -> st -> Maybe st
    applyMove' = gameMakeMove game


type GameId = UUID.UUID

data State =
  State { runningGames :: TVar (Map.Map GameId GenericGame)
        }


-- | gets the current game info
getInfo :: (ScottyError e, MonadReader State m, MonadIO m) => GameId -> ActionT e m GameInfo
getInfo gid =
  getState' <$> getGame gid
  where
    getState' (GenericGame st get _) = get st


jsonState :: GenericGame -> ByteString
jsonState (GenericGame st _ _) = encode st


jsonMove :: GenericGame -> ByteString -> Maybe ByteString
jsonMove (GenericGame st _ apply) m = do
  mv <- decode m
  encode <$> apply mv st


-- | updates the game and returns the game as JSON
move :: (ScottyError e, MonadIO m, MonadReader State m) => GameId -> ActionT e m ()
move gid = do
  (GenericGame st get apply) <- getGame gid
  mv <- jsonData
  case apply mv st of
    Just st' -> do
      setGame gid (GenericGame st' get apply)
      json st'
    Nothing -> raise $ stringError "invalid move"


----------------------------------------------------------------------
-- helpers

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
