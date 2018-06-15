{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}

module WS where

import qualified Network.WebSockets as W
import Data.Acid
import Lib
import Data.Aeson
import Data.UUID (UUID)
import GHC.Generics
import Data.Text (Text)
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Control.Monad.IO.Class (liftIO)
import Control.Exception
import qualified Data.Set as Set
import Control.Monad (forM_, forever)

type ConnectedClient = (UUID, W.Connection)
type WSState = [ConnectedClient]

data Action = Connect | Connected | UserConnected deriving (Eq, Show, Generic)

instance FromJSON Action where
  parseJSON (String t) = case t of
                            "connect" -> pure Connect
                            "connected" -> pure Connected
                            "userConnected" -> pure UserConnected
                            _ -> mempty
  parseJSON _ = mempty

instance ToJSON Action where
  toJSON Connect = String "connect"
  toJSON Connected = String "connected"
  toJSON UserConnected = String "userConnected"

data ActionMessage = ActionMessage {
  action :: Action
, uuid :: Maybe UUID
} deriving (Eq, Show, Generic)

data Error = Error {
  message :: Text
} deriving (Eq, Show, Generic)

instance FromJSON ActionMessage
instance ToJSON ActionMessage
instance ToJSON Error

broadcast :: ToJSON a => UUID -> a -> WSState -> IO ()
broadcast uuid message state = forM_ accts sendToClient
  where serializedMsg = encode message
        accts = filter ((uuid /=) . fst) state
        sendToClient (_, conn) = W.sendTextData conn serializedMsg

hasAccount :: AcidState AccountsState -> UUID -> IO Bool
hasAccount accounts uuid = do
  users <- liftIO $ query accounts QueryState
  return . not . null $ filter (\(Account uid _ _) -> uuid == uid) (Set.toList users)

application :: TVar WSState -> AcidState AccountsState -> W.ServerApp
application clients accounts pending = do
  conn <- W.acceptRequest pending
  W.forkPingThread conn 30

  msg <- decode <$> W.receiveData conn
  case msg of
    Just (ActionMessage Connect (Just uuid)) -> flip finally disconnect $ do
      hasAccount accounts uuid >>= \acc -> case acc of
        False -> W.sendTextData conn (encode $ WS.Error 
                  "No such account exist, POST /register first")
        True -> do
          users <- atomically $ do
            users <- readTVar clients
            writeTVar clients ((uuid, conn):users)
            readTVar clients
          broadcast uuid (ActionMessage UserConnected Nothing) users
          mainLoop (uuid, conn) clients

      where disconnect = do
              users <- atomically $ do
                users <- readTVar clients
                writeTVar clients (filter ((uuid /=) . fst) users)
                readTVar clients
              broadcast uuid (ActionMessage UserConnected Nothing) users
        
    _ -> W.sendTextData conn (encode $ WS.Error "Wrong message format, bye")

    
mainLoop :: ConnectedClient -> TVar WSState -> IO ()
mainLoop (uuid, conn) clients = forever $ do
  (decode <$> W.receiveData conn) >>= processMsg

  where processMsg :: Maybe ActionMessage -> IO ()
        processMsg msg = case msg of
          Nothing -> return ()
          Just msg -> (atomically $ readTVar clients) >>= broadcast uuid msg

