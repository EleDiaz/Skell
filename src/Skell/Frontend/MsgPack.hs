{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

module Skell.Frontend.MsgPack where

-- Con este metodo se puede crear una tercera aplicacion que solo realize pequeñas funcionalidades
-- Para permitir mas de conexiones simultaneas al servidor hay que estar pendiente de siempre actualizar
-- a todos los clientes.....multicast?
import           Control.Monad
import           Control.Exception
import           Control.Monad.Logger

import           Data.Maybe (fromMaybe)
import           Data.MessagePack
import           Data.Default
import           Data.ByteString.Lazy hiding (unpack, pack)
import qualified Data.ByteString as BS

import           Network.Socket hiding (send, sendTo, recv, recvFrom)
import           Network.Socket.ByteString

import           Pipes
import           Pipes.Lift
import qualified Pipes.Concurrent as P

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
msgPackFrontend :: PSkell -> IO ()
msgPackFrontend = serve "127.0.0.1" 4000

type Connection = (Socket, SockAddr, RPCMsg)

serve :: HostName -> PortNumber -> PSkell -> IO ()
serve host port model = withSocketsDo $ do 
  sock <- socket AF_INET Stream defaultProtocol 
  addr <- inet_addr host
  bind sock $ SockAddrInet port addr
  listen sock 10

  runStdoutLoggingT . runEffect $ do
    getRequest' sock >-> processMsg 
    >-> forever (wrapModel $ evalStateP def model)
    >-> sendResponse

    where
      wrapModel :: Monad s => Pipe a b s () -> Pipe (x, a) (x, b) s ()
      wrapModel m = do 
        (msgid, iSkell) <- await
        ((lift $ return iSkell) >~ m) >-> helper msgid

      helper ::Monad s => a -> Pipe b (a, b) s ()
      helper i = do x <- await
                    yield (i, x)


getRequest' :: Socket -> Producer (Socket, SockAddr, ByteString) (LoggingT IO) ()
getRequest' sock = do
  (output, input) <- liftIO . P.spawn $ P.bounded 400
  liftIO . P.forkIO $ go output sock
  P.fromInput input
  where
    go :: P.Output (Socket, SockAddr, ByteString) -> Socket -> IO ()
    go output sock = do
      (sock', addr') <- liftIO $ accept sock
      P.forkIO $ go' sock' addr' output
      go output sock

    go' :: Socket -> SockAddr -> P.Output (Socket, SockAddr, ByteString) -> IO ()
    go' sock' addr' output' = do
      bsOrError <- try (recv sock' 4096)
      case bsOrError of
        Right bs -> 
          if not $ BS.null bs 
            then do
              P.atomically $ (P.send output') (sock', addr', fromStrict bs)
              go' sock' addr' output'
            else return ()
        Left (_::SomeException) -> return ()

-- | Process ByteString get the RPCMsg with msgpack format. It can be a Request or a Notification
processMsg :: Pipe (Socket, SockAddr, ByteString) (Connection, ISkell) (LoggingT IO) ()
processMsg = do
  (sock,addr,bs) <- await
  let bs' = unpack bs
  let request = bs' >>= fromObject >>= isRequest sock addr
  let notif   = bs' >>= fromObject >>= isNotification sock addr
  case request of 
    Just x -> yield x
    Nothing -> fromMaybe (lift $ logWarnN "Bad RPC format") (notif >>= return . yield)
  processMsg

  where 
    isRequest :: Socket -> SockAddr -> (Int, Int, String, [Object]) -> Maybe (Connection, ISkell)
    isRequest sock addr (i, msgid, method, params) =
      if i == 0 then do
        Just ((sock, addr, Request msgid method params), IAction method params)
      else 
        Nothing

    isNotification :: Socket -> SockAddr -> (Int, String, [Object]) -> Maybe (Connection, ISkell)
    isNotification sock addr (i, method, params) =
      if i == 2 then do
        Just ((sock, addr, Notification method params), IAction method params)
      else
        Nothing


sendResponse :: Consumer (Connection, OSkell) (LoggingT IO) ()
sendResponse = do
  ((sock, addr, Request msgid _ _), _) <- await -- TODO End OSkell, make a standard.
  let response = toStrict $ pack (1::Int, msgid, toObject (), toObject ("Hello"::String))
  liftIO $ sendAllTo sock response addr
  sendResponse

-- TODO:
processResponse :: OSkell -> Object
processResponse _ = undefined