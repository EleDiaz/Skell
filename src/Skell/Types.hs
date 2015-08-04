{-# LANGUAGE TemplateHaskell #-}
module Skell.Types where

import           Control.Lens
import           Control.Monad.State.Strict

import           Data.Default
import           Data.Sequence       (Seq)
import qualified Data.Sequence       as S

import           Pipes

--import           Yi.Rope             (YiString)
--import qualified Yi.Rope             as R

import           Skell.Buffer        (Buffer)
-- import           Skell.Debug

data Keys = KCtrl | KMeta | KShift | KAlt | KEsc | KBS | KEnter | KUpLeft
          | KUpRight | KDownLeft | KDownRight | KLeft | KRight | KUp | KDown
          | KCenter | KBackTab | KPrtScr | KPause | KIns | KHome | KPageUp
          | KPageDown | KDel | KEnd | KBegin | KMenu | KChar Char | KFun Int
          deriving (Show, Eq)

data Mouse = MLeftButton | MRightButton | MCenterButton
           deriving (Show, Eq)

-- | TODO:
data ISkell
  = IKey (Seq Keys)
  | IMouse (Mouse, (Int, Int))
  | IClose
  | INone
instance Default ISkell where
  def = INone

instance Show ISkell where
  show _ = "ISkell"

-- TODO:
data OSkell =
  OSkell { _textDisplayO :: String
         }
instance Default OSkell where
    def = OSkell "HOLA"

-- TODO:
type IOSkell = StateT Skell IO

type PSkell = Pipe ISkell OSkell IOSkell ()

-- | Estado del editor
data Skell = Skell
    { _buffers   :: Seq Buffer
    -- ^ Buferes disponibles, el _1 es el buffer actual
    , _clipboard :: String
    , _keymap    :: ISkell -> IOSkell ()
    -- ^ Combinaciones de teclas
    , _exit      :: Bool
    }
instance Default Skell where
    def = Skell (S.singleton def) "" (const (return ())) False

makeLenses ''ISkell
makeLenses ''OSkell
makeLenses ''Skell
