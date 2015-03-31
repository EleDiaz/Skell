module Skell.Core where

import           Control.Lens
import           Control.Monad.State
import qualified Data.Sequence       as S
import           Pipes

import qualified Yi.Rope             as YS

import           Skell.Buffer
import           Skell.Types
import           Skell.Keymap
-- import           Skell.Debug

-- Definir distintos modelos de nucleo quiza??
-- para debuger extremo?
-- U otra idea que se te ocurra
--
coreModel :: Pipe ISkell OSkell IOSkell ()
coreModel = do
  iSk <- await
  -- lift $ get >>= flip _keymap iSk
  st <- lift $ get
  lift $ defaultEmacsKeymap (iSk^.keyI)
  yield $ OSkell (case S.viewl (st^.buffers) of
                    S.EmptyL -> ""
                    a S.:< _ -> YS.toString (a^.content)
                 )
  when (not $ st^.exit) coreModel

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
withCurrentBuffer :: (Buffer -> Buffer) -> IOSkell ()
withCurrentBuffer f = do
  buffers %= (\ss ->
                case S.viewl ss of
                    S.EmptyL -> S.empty
                    a S.:< se -> f a S.<| se
             )

defaultEmacsKeymap :: S.Seq Keys -> IOSkell ()
defaultEmacsKeymap sq
    | S.drop (S.length sq - 1) sq == ctrlG = return ()
    | isChar /= Nothing = let Just a = isChar
                          in withCurrentBuffer (insertStr_ [a])
    | otherwise = mkKeymap
                  [ ("C-x-s", exit .= True) -- TODO
                  , ("Left",  withCurrentBuffer moveLeft_)
                  , ("Right", withCurrentBuffer moveRight_)
                  -- , ("C-Left", edit.buffers._1._1 %= prevToken >> return True)
                  -- , ("C-Right", edit.buffers._1._1 %= nextToken >> return True)
                  ] sq

    where Right ctrlG = parseKeys "C-g"
          isChar = case S.viewl sq of
                        (KChar a) S.:< _ -> Just a
                        _ -> Nothing

