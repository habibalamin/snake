module Movement where

import System.IO (hReady, stdin)
import Control.Concurrent (threadDelay)
import Control.Monad (replicateM)

data Move = Up | Down | Left | Right | Noop deriving (Show, Eq, Enum)

data Direction = North | East | South | West deriving (Show, Eq, Enum)
data Coords = Coords Integer Integer

instance Show Coords where
    show (Coords x y) = "(x: " ++ show x ++ ", y: " ++ show y ++ ")"

mkMove :: String -> Move
mkMove "\ESC[A" = Up
mkMove "\ESC[B" = Down
mkMove "\ESC[C" = Movement.Right
mkMove "\ESC[D" = Movement.Left
mkMove _        = Noop

getNextMove :: IO Move
getNextMove = mkMove <$> keys where
    -- MAGICNUMBER: this feels like a good pace when repeated, but it
    -- should probably be customisable & labelled for why I chose it?
    -- SRP: I think this function does too much, and assumes too much
    -- about its callers.
    keys = threadDelay 100000 >> hReady stdin >>= getFirstChar >>= getRestChars >>= flushInput where
        getFirstChar ready = if ready then sequence [getChar] else return "\NUL"
        getRestChars firstChar = if firstChar == "\ESC" then
            (firstChar ++) <$> replicateM 2 getChar else
            return firstChar
        flushInput caughtChars = hReady stdin >>=
            \ready -> if ready then getChar >> flushInput caughtChars else return caughtChars
