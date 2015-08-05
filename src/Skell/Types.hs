{-# LANGUAGE TemplateHaskell, OverloadedStrings, FlexibleInstances #-}
module Skell.Types where

import           Control.Lens
import           Control.Monad.State.Strict
import           Control.Monad.Logger

import           Data.Default
import           Data.Sequence       (Seq)
import qualified Data.Sequence       as S
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Data.MessagePack

import           Pipes



import           Skell.Buffer        (Buffer)

-- TODO:
type IOSkell = StateT Skell (LoggingT IO)

type PSkell = Pipe ISkell OSkell IOSkell ()

-- | TODO:
data ISkell
  = IAction String [Object]
  | INone
  deriving (Show)

instance Default ISkell where
  def = INone

-- TODO: Este debe ser cambiado a mas preciso...
data OSkell =
  OSkell { _textDisplayO :: String
         }

instance Default OSkell where
    def = OSkell ""

-- | Editor's State 
data Skell = Skell
    { _buffers :: Seq Buffer
    -- ^ Buffers available to clients
    , _methods :: Map String Method
    -- ^ Dictionary of all methods expose to client
    }

instance Default Skell where
    def = Skell (S.singleton def) M.empty

-- Note: The methods have to return (), is possible return any more useful like Output perhaps.
-- | Represent a function of a variable number of arguments(all arguments ought be in MessagePack class)
newtype Method = Method { unMethod :: [Object] -> IOSkell () }

class MethodType a where
  applyMethod :: a -> [Object] -> IOSkell ()

-- This would works but, i have short experience with FlexibleInstances
instance MethodType (StateT Skell (LoggingT IO) ())  where
  applyMethod f ls =
    case ls of
      [] -> f
      _ -> logWarnN "Bad number of arguments, expected 0" >> return ()

instance (MessagePack a, MethodType r) => MethodType (a -> r) where
  applyMethod f ls = 
    case ls of 
      [] -> logWarnN "Bad number of arguments, expected 1..*"
      x:xs -> case fromObject x of 
        Just obj -> applyMethod (f obj) xs
        Nothing -> logWarnN "Bad argument format"

makeLenses ''ISkell
makeLenses ''OSkell
makeLenses ''Skell
