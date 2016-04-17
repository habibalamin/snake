module Main where

import Terminal
import World
import Movement

main :: IO ()
main = catchInput $ randomWorld >>= snake

snake :: World -> IO ()
snake world = print world >> getNextMove >>= return . flip updateWorld world >>= snake
