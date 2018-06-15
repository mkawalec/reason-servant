module Main where

import Lib
import qualified WS as WS
import qualified Network.WebSockets as WebSockets

import Network.Wai.Handler.Warp
import Data.Acid
import qualified Data.Set as Set
import Control.Concurrent.STM.TVar
import Control.Concurrent (forkIO)

main :: IO ()
main = do 
  acid <- openLocalState (AccountsState Set.empty)
  state <- newTVarIO []

  forkIO $ WebSockets.runServer "127.0.0.1" 8082 $ WS.application state acid
  run 8081 (app acid)
