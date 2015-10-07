{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE InterruptibleFFI #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE UnboxedTuples #-}

module JavaScript.Web.WebSocket ( WebSocket
                                , WebSocketRequest(..)
                                , ReadyState(..)
                                , BinaryType(..)
                                , connect
                                , close
                                , send
                                , getBufferedAmount  
                                , getExtensions
                                , getProtocol
                                , getReadyState
                                , getBinaryType
                                , getUrl
                                ) where

import           GHCJS.Concurrent
import           GHCJS.Prim
import           GHCJS.Foreign.Callback.Internal (Callback(..))
import qualified GHCJS.Foreign.Callback          as CB

import           GHC.Exts

import           Control.Exception
import           Control.Monad

import           Data.Data
import           Data.Maybe
import           Data.Typeable

import           System.IO

import           Data.JSString (JSString)
import qualified Data.JSString as JSS

import           JavaScript.Array (JSArray)
import qualified JavaScript.Array as JSA
import           JavaScript.Web.MessageEvent
import           JavaScript.Web.MessageEvent.Internal
import           JavaScript.Web.CloseEvent
import           JavaScript.Web.CloseEvent.Internal
import           JavaScript.Web.ErrorEvent
import           JavaScript.Web.ErrorEvent.Internal

import Unsafe.Coerce

data WebSocketRequest = WebSocketRequest
  { url       :: JSString
  , protocols :: [JSString]
  , onClose   :: Maybe (CloseEvent -> IO ()) -- ^ called when the connection closes (at most once)
  , onMessage :: Maybe (MessageEvent -> IO ()) -- ^ called for each message
  }

newtype WebSocket = WebSocket JSVal
-- instance IsJSVal WebSocket

data ReadyState = Closed | Connecting | Connected
  deriving (Data, Typeable, Enum, Eq, Ord, Show)

data BinaryType = Blob | ArrayBuffer
  deriving (Data, Typeable, Enum, Eq, Ord, Show)

{- | create a WebSocket -} 
connect :: WebSocketRequest -> IO WebSocket
connect req = do
  mcb <- maybeCallback MessageEvent (onMessage req)
  ccb <- maybeCallback CloseEvent   (onClose req)
  synchronously $ do
    ws <- case protocols req of
           []  -> js_createStr (url req) JSS.empty
           [x] -> js_createStr (url req) x
           xs  -> js_createArr (url req) (JSA.fromList $ unsafeCoerce xs) -- fixme
    (js_open ws mcb ccb >>= handleOpenErr >> return ws) `onException` js_close 1000 "Haskell Exception" ws

maybeCallback :: (JSVal -> a) -> Maybe (a -> IO ()) -> IO JSVal
maybeCallback _ Nothing = return jsNull
maybeCallback f (Just g) = do
  cb@(Callback cb') <- CB.syncCallback1 CB.ContinueAsync (g . f)
  CB.releaseCallback cb
  return cb'

handleOpenErr :: JSVal -> IO ()
handleOpenErr r
  | isNull r  = return ()
  | otherwise = throwIO (userError "WebSocket failed to connect") -- fixme

releaseMessageCallback :: WebSocket -> IO ()
releaseMessageCallback ws = js_getOnmessage ws >>=
  \cb -> unless (isNull cb) (CB.releaseCallback $ Callback cb)

{- | close a websocket and release the callbacks -}
close :: Maybe Int -> Maybe JSString -> WebSocket -> IO ()
close value reason ws =
  js_close (fromMaybe 1000 value) (fromMaybe JSS.empty reason) ws
{-# INLINE close #-}

send :: JSString -> WebSocket -> IO ()
send xs ws = js_send xs ws
{-# INLINE send #-}

getBufferedAmount :: WebSocket -> IO Int
getBufferedAmount ws = js_getBufferedAmount ws
{-# INLINE getBufferedAmount #-}

getExtensions :: WebSocket -> IO JSString
getExtensions ws = js_getExtensions ws
{-# INLINE getExtensions #-}

getProtocol :: WebSocket -> IO JSString
getProtocol ws = js_getProtocol ws
{-# INLINE getProtocol #-}

getReadyState :: WebSocket -> IO ReadyState
getReadyState ws = fmap toEnum (js_getReadyState ws)
{-# INLINE getReadyState #-}

getBinaryType :: WebSocket -> IO BinaryType
getBinaryType ws = fmap toEnum (js_getBinaryType ws)
{-# INLINE getBinaryType #-}

getUrl :: WebSocket -> JSString
getUrl ws = js_getUrl ws
{-# INLINE getUrl #-}

getLastError :: WebSocket -> IO (Maybe ErrorEvent)
getLastError ws = do
  le <- js_getLastError ws
  return $ if isNull le then Nothing else Just (ErrorEvent le)
{-# INLINE getLastError #-}

-- -----------------------------------------------------------------------------


foreign import javascript safe
  "new WebSocket($1, $2)" js_createStr :: JSString -> JSString -> IO WebSocket
foreign import javascript safe
  "new WebSocket($1, $2)" js_createArr :: JSString -> JSArray -> IO WebSocket
                                          
foreign import javascript interruptible
  "h$openWebSocket($1, $2, $3, $c);"
  js_open :: WebSocket -> JSVal -> JSVal -> IO JSVal
foreign import javascript safe
  "h$closeWebSocket($1, $2);"    js_close      :: Int -> JSString -> WebSocket -> IO ()
foreign import javascript unsafe
  "$2.send($1);"          js_send              :: JSString -> WebSocket -> IO ()
foreign import javascript unsafe
  "$1.bufferedAmount"     js_getBufferedAmount :: WebSocket -> IO Int
foreign import javascript unsafe
  "$1.readyState"         js_getReadyState     :: WebSocket -> IO Int
foreign import javascript unsafe
  "$1.protocol"           js_getProtocol       :: WebSocket -> IO JSString
foreign import javascript unsafe
  "$1.extensions"         js_getExtensions     :: WebSocket -> IO JSString
foreign import javascript unsafe
  "$1.url"                js_getUrl            :: WebSocket -> JSString
foreign import javascript unsafe
  "$1.binaryType === 'blob' ? 1 : 2"
  js_getBinaryType                             :: WebSocket -> IO Int

foreign import javascript unsafe
  "$2.onopen = $1;"       js_setOnopen         :: Callback a -> WebSocket -> IO ()
foreign import javascript unsafe
  "$2.onclose = $1;"      js_setOnclose        :: Callback a -> WebSocket -> IO ()
foreign import javascript unsafe
  "$2.onopen = $1;"       js_setOnerror        :: Callback a -> WebSocket -> IO ()
foreign import javascript unsafe
  "$2.onmessage = $1;"    js_setOnmessage      :: Callback a -> WebSocket -> IO ()
foreign import javascript unsafe
  "$1.onmessage"          js_getOnmessage      :: WebSocket -> IO JSVal
foreign import javascript unsafe
  "$1.lastError"          js_getLastError      :: WebSocket -> IO JSVal
