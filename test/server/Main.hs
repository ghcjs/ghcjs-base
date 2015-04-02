{-# LANGUAGE OverloadedStrings #-}

{-
    Server for testing WebSocket and XHR functionality
      GET  requests:
      POST requests:
 -}
module Main where

import Control.Monad

import qualified Control.Exception as E

import qualified Network.WebSockets as WS
import qualified Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.Wai.Application.Static as Static

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy.Encoding as TLE

main :: IO ()
main = Warp.runSettings Warp.defaultSettings
         { Warp.settingsPort = 3001
         } $ WaiWS.websocketsOr WS.defaultConnectionOptions handleWebSocket handleStatic

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
             [(i, [])] -> case i of
                            0         -> do
                                 putStrLn "closing connection"
                                 WS.sendClose conn (""::T.Text)
                            _ | i < 0 -> replicateM_ (negate i) $
                                 WS.sendDataMessage conn (WS.Text "TestTextMessage")
                            _         -> replicateM_ i $
                                 WS.sendDataMessage conn (WS.Binary "TestBinaryMessage")
             _         -> putStrLn "received non-numeric message"
          WS.Binary bs -> do
            putStrLn "received binary message"
      handleConnectionException :: WS.ConnectionException -> IO ()
      handleConnectionException e = print e
  handleMessages `E.catch` handleConnectionException

