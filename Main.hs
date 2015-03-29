module Main where

import           Skell.UI.Gtk
import           Skell.Core

main :: IO ()
main = gtkFrontend coreModel
