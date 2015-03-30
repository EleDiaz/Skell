{-# LANGUAGE TemplateHaskell #-}
module Skell.Types where

import           Control.Lens
import           Control.Monad.State.Strict
import           Data.Default
import           Data.Sequence       (Seq)
import qualified Data.Sequence       as S

import           Yi.Rope             (YiString)
import qualified Yi.Rope             as R

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
data ISkell =
  ISkell { _keyI   :: Seq Keys
         , _mouseI :: Seq Mouse
         , _closeI :: Bool
         }
instance Default ISkell where
    def = ISkell S.empty S.empty False

-- TODO:
data OSkell =
  OSkell { _textDisplayO :: String
         }
instance Default OSkell where
    def = OSkell "HOLA"

-- TODO:
type IOSkell = StateT Skell IO

-- | Estado del editor
data Skell = Skell
    { _buffers   :: Seq Buffer
    -- ^ Buferes disponibles, el _1 es el buffer actual
    , _clipboard :: YiString
    , _keymap    :: ISkell -> IOSkell ()
    -- ^ Combinaciones de teclas
    }
instance Default Skell where
    def = Skell S.empty R.empty (const (return ()))

makeLenses ''ISkell
makeLenses ''OSkell
makeLenses ''Skell
