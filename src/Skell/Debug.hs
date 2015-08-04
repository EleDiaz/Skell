module Skell.Debug where

data Mode = File | Terminal

class Debugable a where
    debug :: a -> Mode -> IO ()

