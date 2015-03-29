module Skell.UI.Gtk.Keys where

import qualified Data.Sequence       as S
import           Data.Default

import           Skell.Types

-- TODO:
convertKeys :: (Maybe String, Maybe String) -> ISkell
convertKeys _ = def { _keyI = S.singleton $ KChar 'a' }
