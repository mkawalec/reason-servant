module WS where

{-# LANGUAGE OverloadedStrings #-}

import qualified Network.WebSockets as WS
import Data.Acid
import Lib
import Data.Aeson
import qualified Data.Text (Text)
import qualified Data.Set as Set

type ConnectedClient = (UUID, WS.Connection)
type WSState = [ConnectedClient]

data Action = Connect | Connected | UserConnected deriving (Eq, Show)

instance FromJSON Action where
  parseJSON (String t) -> case t of
                            "connect" -> pure Connect
                            "connected" -> pure Connected
                            "userConnected" -> pure UserConnected
                            _ -> empty
  parseJSON _ -> empty

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
instance ToJSON Error

broadcast :: ToJSON a => UUID -> a -> WSState -> IO ()
broadcast uuid message state = forM_ accts sendToClient
  where serializedMsg = encode message
        accts = filter ((uuid /=) . snd) state
        sendToClient (_, conn) = WS.sendTextData conn serializedMsg

hasAccount :: AcidState AccountsState -> UUID -> IO Bool
hasAccount accounts uuid = do
  users <- liftIO $ query accounts QueryState
  return $ filter \(Account uid _ _) -> uuid == uid (Set.toList users)

application :: TVar WSState -> AcidState AccountsState -> WS.ServerApp
application clients accounts pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30

  msg <- decode <$> WS.receiveData conn
  case msg of
    Just (ActionMessage Connect (Just uuid)) -> flip finally disconnect $ do
      hasAccount accounts uuid >>= \case
        False -> WS.sendTextData conn (encode $ Error 
                  "No such account exist, POST /register first")
        True -> do
          atomically $ do
            users <- readTVar clients
            writeTVar clients ((uuid, conn):clients)
          broadcast uuid (ActionMessage UserConnected Nothing) users
          mainLoop (uuid, conn) clients

      where disconnect = do
        atomically $ do
          users <- readTVar clients
          writeTVar clients (filter ((uuid /=) . uid) users)
        broadcast uuid (ActionMessage UserConnected Nothing) users
        
    _ -> WS.sendTextData conn (encode $ Error "Wrong message format, bye")

    
mainLoop :: ConnectedClient -> TVar WSState -> IO ()
mainLoop (uuid, conn) clients = forever $ do
  decode <$> WS.receiveData conn >>= \case
    Nothing -> ()
    Just msg -> atomically $ readTVar clients >>= broadcast uuid msg

