module Main where

import System.Exit (exitSuccess)

import Terminal
import World
import Movement

main :: IO ()
main = catchInput $ randomWorld >>= snake

snake :: World -> IO ()
snake world = if gameOver world
    then printWorld world >>
        getChar >> clearScreenWorldPassthru world >>
        exitSuccess
    else printWorld world >>
        getNextMove >>= updateWorldIO world >>=
        clearScreenWorldPassthru >>= snake where
            printWorld = putStr . show
            updateWorldIO world = return . flip updateWorld world
            clearScreenWorldPassthru world = clearScreen world >> return world where
                -- We add 1 to the height to account for the top border. We
                -- are already on the same line as the bottom border (as we
                -- use putStr to avoid printing a newline after the world),
                -- so no need to need to account for that.
                clearScreen world = putStr $ "\r\ESC[" ++ show (worldHeight world + 1) ++ "A"
