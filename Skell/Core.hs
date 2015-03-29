module Skell.Core where

import           Control.Lens
import           Control.Monad.State
import qualified Data.Sequence       as S
import           Pipes

import           Skell.Buffer        (Buffer)
import           Skell.Types
-- import           Skell.Debug

-- Definir distintos modelos de nucleo quiza??
-- para debuger extremo?
-- U otra idea que se te ocurra
--
coreModel :: Pipe ISkell OSkell IOSkell ()
coreModel = do
  -- iSk <- await
  -- lift $ get >>= flip _keymap iSk
  -- modify (\st -> st^.keymap iSk) -- Pasamos los eventos de entrada al keymap definido

  yield $ OSkell "Hola que tal"
  return ()

-- coreModelDebug :: Mode -> Pipe ISkell OSkell IOSkell ()
-- coreModelDebug mode = do
--   iSk <- await
--   st <- lift $ get
--   liftIO $ do debug mode iSk
--               debug mode st
--   st^.keymap iSk -- TODO: Catch posibles errores
--   return ()



-- | Aplica una funcion al buffer en uso, si no
-- ningun buffer no hace nada
withCurrentBuffer :: (Buffer -> Buffer) -> Pipe ISkell OSkell IOSkell ()
withCurrentBuffer f = do
  lift $ buffers %= (\ss ->
                      case S.viewl ss of
                       S.EmptyL -> S.empty
                       a S.:< se -> f a S.<| se
                    )
  return ()

