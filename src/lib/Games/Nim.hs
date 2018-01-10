{-# LANGUAGE DeriveGeneric #-}

module Games.Nim
  ( Nim
  , Move (..)
  , Player
  , initNim
  , progressNim
  , move
  , gameNim
  ) where

import Control.Monad (guard)
import Data.Aeson (FromJSON, ToJSON, toEncoding, genericToEncoding, defaultOptions)
import GHC.Generics (Generic)
import Games.Internal


gameNim :: Game Nim Move Player
gameNim = Game [Player1, Player2] turn' progressNim move
  where
    turn' nm =
      case progressNim nm of
        Running -> Just $ turn nm
        _       -> Nothing


initNim :: Int -> Nim
initNim n = Nim Player1 [1..n]


progressNim :: Nim -> Progress Player
progressNim nm =
  if any (> 0) (rows nm)
  then Running
  else Won [ turn nm ]


move :: Move -> Nim -> Maybe Nim
move mv nm = do
  guard (cnt > 0)
  rows' <- go (row mv) (rows nm)
  return $ nm { turn = next (turn nm), rows = rows' }
  where
    go _ [] = Nothing
    go 0 (x:xs)
      | x >= cnt  = Just ((x-cnt) : xs)
      | otherwise = Nothing
    go n (x:xs)   = (x:) <$> go (n-1) xs
    cnt = count mv


data Nim =
  Nim
  { turn :: Player
  , rows :: [Int]
  } deriving (Generic, Show)


data Move =
  Move
  { row   :: Int
  , count :: Int
  } deriving (Generic, Show)


data Player = Player1 | Player2
  deriving (Generic, Show, Eq, Ord, Enum)


next :: Player -> Player
next Player1 = Player2
next Player2 = Player1


----------------------------------------------------------------------

instance ToJSON Move where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Move


instance ToJSON Nim where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Nim


instance ToJSON Player where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Player
