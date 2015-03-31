{-# LANGUAGE TemplateHaskell #-}
module Skell.Buffer where

import           Control.Lens
import           Control.Monad.State
import           Data.Default

import           Yi.Rope             (YiString)
import qualified Yi.Rope             as YS


-- | TODO:
data Buffer = Buffer
    { _filepath :: String
    , _content  :: YiString
    , _cursor   :: Int
    } deriving (Show)
instance Default Buffer where
    def = Buffer "**none**" YS.empty 0

makeLenses ''Buffer

-- | Tipo intermedio para facilitar la ejecucion de varias operaciones sobre
-- el buffer
type BufferM a = State Buffer a

insertStr :: String -> BufferM ()
insertStr str = modify $ insertStr_ str

deleteChar :: BufferM ()
deleteChar = modify deleteChar_

moveLeft :: BufferM ()
moveLeft = modify moveLeft_

moveRight :: BufferM ()
moveRight = modify moveRight_

insertStr_ :: String -> Buffer -> Buffer
insertStr_ new (Buffer n str pos) = Buffer n (YS.concat [before, YS.fromString new,after]) (pos+length new)
  where (before,after) = YS.splitAt pos str

deleteChar_ :: Buffer -> Buffer
deleteChar_ b@(Buffer _ _ 0) = b
deleteChar_ (Buffer n str pos) = case YS.init before of
                                  Just before' -> Buffer n (YS.concat [before',  after]) (pos-1)
                                  Nothing      -> Buffer n (YS.concat [before,  after]) (pos-1)
  where (before,after) = YS.splitAt pos str

moveLeft_ :: Buffer -> Buffer
moveLeft_ b@(Buffer n str pos)
    | pos==0 = b
    | otherwise = Buffer n str (pos-1)

moveRight_ :: Buffer -> Buffer
moveRight_ b@(Buffer n str pos)
    | pos==YS.length str = b
    | otherwise = Buffer n str (pos+1)
