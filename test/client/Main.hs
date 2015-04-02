{-# LANGUAGE OverloadedStrings #-}

{-
    Test program for client side tests for ghcjs-base
 -}
module Main where

import Control.Concurrent

import qualified JavaScript.Web.WebSocket as WS
-- import qualified Jav

main :: IO ()
main = do
  let wsClose _ = putStrLn "connection closed"
      wsMessage _ = putStrLn "websocket message"
  ws <- WS.connect $ WS.WebSocketRequest "ws://localhost:3001/"
                                         []
                                         (Just wsClose)
                                         (Just wsMessage)
  WS.send "5" ws
  threadDelay 10000000

{-
  Warp.runSettings Warp.defaultSettings
         { Warp.settingsPort = 3001
         } $ WaiWS.websocketsOr WS.defaultConnectionOptions handleWebSocket handleStatic
-}
{-
handleStatic :: Network.Wai.Application
handleStatic = Static.staticApp (Static.defaultFileServerSettings ".")

handleWebSocket :: WS.ServerApp
handleWebSocket pending = do
  putStrLn "accepting WebSocket request"
  conn <- WS.acceptRequest pending
  let handleMessages = forever $ do
        d <- WS.receiveDataMessage conn
        case d of
          WS.Text t    -> do
            putStrLn "received text message"
            case reads . TL.unpack . TLE.decodeUtf8 $ t of
             [(i, [])] -> if i < 0
                          then replicateM_ (negate i) $
                                 WS.sendDataMessage conn (WS.Text "TestTextMessage")
                          else replicateM_ i $
                                 WS.sendDataMessage conn (WS.Binary "TestBinaryMessage")
             _         -> putStrLn "received non-numeric message"
          WS.Binary bs -> do
            putStrLn "received binary message"
      handleConnectionException :: WS.ConnectionException -> IO ()
      handleConnectionException e = print e
  handleMessages `E.catch` handleConnectionException
-}
