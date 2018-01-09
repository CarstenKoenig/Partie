module Main where

import Data.Proxy (Proxy(..))
import Language.PureScript.Bridge
import Test

main :: IO ()
main = do
  let myTypes = [ mkSumType (Proxy :: Proxy Person) ]
  writePSTypes "./client/src/" (buildBridge defaultBridge) myTypes
