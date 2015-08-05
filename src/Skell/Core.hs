module Skell.Core where

import           Control.Lens
import           Control.Monad.State
import qualified Data.Sequence       as S
import           Pipes

import           Skell.Buffer
import           Skell.Types


coreModel :: PSkell
coreModel = do
  iSk <- await
  st <- lift $ get
  yield $ OSkell (case S.viewl (st^.buffers) of
                    S.EmptyL -> ""
                    a S.:< _ -> a^.content
                 )

-- | Aplica una funcion al buffer en uso, si no
-- ningun buffer no hace nada
withCurrentBuffer :: (Buffer -> Buffer) -> IOSkell ()
withCurrentBuffer f = do
  buffers %= (\ss ->
                case S.viewl ss of
                    S.EmptyL -> S.empty
                    a S.:< se -> f a S.<| se
             )

-- defaultEmacsKeymap :: S.Seq Keys -> IOSkell ()
-- defaultEmacsKeymap sq
--     | S.drop (S.length sq - 1) sq == ctrlG = return ()
--     | isChar /= Nothing = let Just a = isChar
--                           in withCurrentBuffer (insertStr_ [a])
--     | otherwise = mkKeymap
--                   [ -- ("C-x-s", exit .= True) -- TODO
--                   ("Left",  withCurrentBuffer moveLeft_)
--                   , ("Right", withCurrentBuffer moveRight_)
--                   -- , ("C-Left", edit.buffers._1._1 %= prevToken >> return True)
--                   -- , ("C-Right", edit.buffers._1._1 %= nextToken >> return True)
--                   ] sq

--     where Right ctrlG = parseKeys "C-g"
--           isChar = case S.viewl sq of
--                         (KChar a) S.:< _ -> Just a
--                         _ -> Nothing

