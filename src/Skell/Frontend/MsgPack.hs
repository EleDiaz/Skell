{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, TupleSections #-}
module Skell.Frontend.MsgPack where

import           Control.Lens
import           Control.Monad
import           Control.Exception
import           Control.Monad.Logger
import           Control.Monad.State.Strict
import           Control.Concurrent
import           Control.Concurrent.STM

import qualified Data.Map as M
import           Data.MessagePack
import           Data.Default
import           Data.ByteString.Lazy hiding (unpack, pack)
import qualified Data.ByteString as BS

import qualified Network.Socket as N
import           Network.Transport
import           Network.Transport.TCP

import           Skell.Types

data RPCMsg = Request Int String [Object]
            -- ^ Represent a 'request' from a client, 
            -- (0,msgID,methodName,parameters)
            | Response Int Object Object
            -- ^ Represent the response from server to client, 
            -- (1,msgID==msgID_client_request,errorObject,resultObject)
            | Notification String [Object]
            -- ^ Represent an notification, not is so clear its use. 
            -- Client to Server and is like a Request but response by server
            deriving (Eq, Show)

-- | By default it's connect to '127.0.0.1' in the port 4000
msgPackFrontend :: (ISkell -> IOSkell OSkell) -> IO ()
msgPackFrontend = serve "127.0.0.1" "4000"

-- | Add feature of filter logger messages by log level "filterLogger" 
serve :: N.HostName -> N.ServiceName -> (ISkell -> IOSkell OSkell) -> IO ()
serve host port model = N.withSocketsDo $ do
  serverDone <- newEmptyMVar
  msgs       <- atomically $ newTChan 
  Right transport <- createTransport host port defaultTCPParameters
  Right endpoint <- liftIO $ newEndPoint transport

  forkIO . runStdoutLoggingT . flip evalStateT def $ do
    getRequest' endpoint msgs serverDone
    forever $ processMsg msgs >>= wrap model >>= sendResponse

  readMVar serverDone `onCtrlC` closeTransport transport
  where
    wrap :: Monad m => (b -> m c) -> (a,b) -> m (a,c)
    wrap mbc (a, b) = mbc b >>= return . (a,)


getRequest' :: EndPoint -> TChan (ConnectionId, ByteString) -> MVar () -> IOSkell ()
getRequest' endpoint msgs serverDone = do
  conns <- liftIO $ newMVar M.empty
  connections .= conns
  _ <- liftIO . forkIO $ go conns
  return ()

  where
    go :: MVar (M.Map ConnectionId (MVar Connection)) -> IO ()
    go cs = do
      event <- liftIO $ receive endpoint
      case event of 
        ConnectionOpened cid rel addr -> do
          connMVar <- newEmptyMVar
          forkIO $ do
            Right conn <- connect endpoint addr rel defaultConnectHints
            putMVar connMVar conn
            modifyMVar_ cs (return . M.insert cid connMVar) 
          go cs
        Received cid (payload:_) -> do 
            if not $ BS.null payload 
            then do
              atomically $ writeTChan msgs (cid, fromStrict payload)
            else return ()
        ConnectionClosed cid -> do
          forkIO $ do
            cs' <- readMVar cs
            conn <- readMVar (cs' M.! cid)
            close conn 
            putMVar cs (M.delete cid cs')
          go cs
        EndPointClosed -> do
          putMVar serverDone ()
      go cs

-- | Process ByteString get the RPCMsg with msgpack format. It can be a Request or a Notification
processMsg :: TChan (ConnectionId, ByteString) -> IOSkell (RPCMsg, ISkell)
processMsg msgs = do
  (cid,bs) <- liftIO . atomically $ readTChan msgs
  workOnConn .= cid
  let bs' = unpack bs
  let request = bs' >>= fromObject >>= isRequest
  let notif   = bs' >>= fromObject >>= isNotification
  case request of
    Just x -> return x
    Nothing -> case notif of
      Just x -> return x
      Nothing -> lift $ logWarnN "Bad RPC format" >> return (undefined, INone)

  where 
    isRequest :: (Int, Int, String, [Object]) -> Maybe (RPCMsg, ISkell)
    isRequest (i, msgid, method, params) =
      if i == 0 then do
        Just (Request msgid method params, IAction method params)
      else 
        Nothing

    isNotification :: (Int, String, [Object]) -> Maybe (RPCMsg, ISkell)
    isNotification (i, method, params) =
      if i == 2 then do
        Just (Notification method params, IAction method params)
      else
        Nothing


sendResponse :: (RPCMsg, OSkell) -> IOSkell ()
sendResponse (Request msgid _ _, oSkell) = do
  let response = toStrict $ pack (1::Int, msgid, toObject (), toObject ("Hello"::String))
  st <- get
  conns <- liftIO $ readMVar (st^.connections)
  case M.lookup (st^.workOnConn) conns of
    Just mConn -> do 
      conn <- liftIO $ readMVar mConn
      err  <- liftIO $ send conn [response]
      case err of
        Right () -> return ()
        Left _   -> lift $ logWarnNS "MsgPack.hs" "Fail on sending response"
    Nothing -> lift $ logWarnNS "MsgPack.hs" "Not exist connection"

onCtrlC :: IO a -> IO () -> IO a
p `onCtrlC` q = catchJust isUserInterrupt p (const $ q >> p `onCtrlC` q)
  where
    isUserInterrupt :: AsyncException -> Maybe () 
    isUserInterrupt UserInterrupt = Just ()
    isUserInterrupt _             = Nothing