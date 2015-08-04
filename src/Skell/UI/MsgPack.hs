module Skell.Frontend.MsgPack where

-- Con este metodo se puede crear una tercera aplicacion que solo realize pequeñas funcionalidades
-- 

import           Control.Monad (forever)
import           Control.Monad.State.Strict (evalStateT)

import           Data.MessagePack
import           Data.Binary
import           Data.Default

import           Pipes
import           Pipes.Safe
import           Pipes.Lift
import           Pipes.Network.TCP.Safe
import qualified Pipes.Binary      as P

import           Skell.Types

-- Solicitud
-- _1 Int -> 0 -> tipo solicitud, msgid = para poder enlazar los mensajes, nombre del metodo y params
-- Respuesta
-- _1 Int -> 1 -> respuesta, msgid, Object -> error sino nil, Objeto -> resultado sino nil
-- Notificacion
-- _1 Int -> 2 -> Notificacion, Method, Params, este no espera una repuesta por parte del cliente
data RPCMsg = Request (Int, Int, String, [Object])
            | Response (Int, Int, Object, Object)
            | Notification (Int, String, [Object])
            deriving (Eq, Show)

-- TODO: El tipo `Int´ varia segun la plataforma usar Word32|8... 
instance Binary RPCMsg where
  put (Request x)      = put x
  put (Response x)     = put x
  put (Notification x) = put x

  get = do t <- get :: Get Int
           case t of
              0 -> do msgid  <- get -- :: Int
                      method <- get -- :: String
                      params <- get -- :: [Object]
                      return $ Request (0, msgid, method, params) 
              1 -> do msgid  <- get -- :: Int
                      err    <- get -- :: Object
                      result <- get -- :: Object
                      return $ Response (1, msgid, err, result)
              2 -> do method <- get -- :: String
                      params <- get -- :: [Object]
                      return $ Notification (2, method, params)
              x -> error $ "Fail to parse RPCMsg in Binary: " ++ show x

msgPackFrontend :: PSkell -> IO ()
msgPackFrontend = server "127.0.0.1" "4000"

-- TODO: Fromserver y toServer no se sabe que es lo que terminan por hacer implementarlo con sockets
server :: HostName -> ServiceName -> PSkell -> IO ()
-- serve host port model = P.withSocketsDo $ runEffect $ evalStateP def $ do
--   (sock, _sockAddr) <- liftIO $ P.bindSock (P.Host host) port
--   liftIO $ print "HEllo"
--   P.fromSocket sock 4096 >-> getRequest >-> wrapModel model >-> sendResponse >-> P.toSocket sock
--   liftIO $ P.closeSock sock
server host port model = runSafeT . runEffect . evalStateP def $ forever $
    fromServe 4096 (HostAny) port
    >-> getRequest
    >-> wrapModel model 
    >-> sendResponse 
    >-> toServe (Host host) port
    where
      wrapModel :: Pipe ISkell OSkell IOSkell () -> Pipe (Int, ISkell) (Int, OSkell) IOSkell ()
      wrapModel m = do 
        (msgid, iSkell) <- await
        ((lift $ return iSkell) >~ m) >-> helper msgid

      helper :: Int -> Pipe OSkell (Int, OSkell) IOSkell ()
      helper i = do x <- await
                    yield (i, x) 

-- Hay que tener en cuenta msgid que tiene que enviar el Output. Tiene que haber mayor comunicacion entre
-- getRequest <-> sendResponse 
getRequest :: Pipe P.ByteString (Int, ISkell) IOSkell ()
getRequest = do
  liftIO $ print "GetRequest"
  bs <- await
  liftIO $ print "GetRequest"
  request <- evalStateT P.decode (yield bs) 
  case request of
    Left _ -> yield (-1,undefined) -- Throw an error to IOSkell to debug
    Right x -> yield (1,processMsg x)

-- Para el diccionario de metodos disponibles se pueden sacar de IOSkell para permitir que los plugins añadan mayor funcionalidad
processMsg :: RPCMsg -> ISkell
processMsg = undefined


sendResponse :: Pipe (Int, OSkell) P.ByteString IOSkell ()
sendResponse = do
  liftIO $ print "SendResponse"
  (i, _) <- await
  liftIO $ print "send"
  P.encode $ Response (1, i, toObject (), toObject ("Hello i expect that this string arrive to its destiny"::String))

processResponse :: OSkell -> RPCMsg
processResponse _ = undefined