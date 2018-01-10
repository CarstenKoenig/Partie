module Main where

import Data.Proxy (Proxy(..))
import Language.PureScript.Bridge
import Test
import Games.Scotty

main :: IO ()
main = do
  let myTypes = [ mkSumType (Proxy :: Proxy Person)
                , mkSumType (Proxy :: Proxy GameInfo)
                , mkSumType (Proxy :: Proxy Progress)
                ]
  writePSTypes "./client/src/" (buildBridge defaultBridge) myTypes
