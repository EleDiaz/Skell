module Main where

import           Skell.Frontend.MsgPack
import           Skell.Core

main :: IO ()
main = msgPackFrontend coreModel
