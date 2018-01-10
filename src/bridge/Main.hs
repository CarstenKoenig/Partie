{-# LANGUAGE RankNTypes #-}
module Main where

import Data.Proxy (Proxy(..))
import Data.Typeable (Typeable)
import Language.PureScript.Bridge
import Test
import Games.Scotty
import Games.Nim as Nim

main :: IO ()
main = do
  let myTypes = [ mkSumType (Proxy :: Proxy Person)
                , mkSumType (Proxy :: Proxy GameInfo)
                , mkSumType (Proxy :: Proxy Progress)
                , mkSumType (Proxy :: Proxy Nim)
                , mkSumType (Proxy :: Proxy Nim.Player)
                , mkSumType (Proxy :: Proxy Nim.Move)
                ]
  writePSTypes "./client/src/" (buildBridge defaultBridge) myTypes
