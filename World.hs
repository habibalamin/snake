module World where

import System.Random (randomRIO)

import Terminal
import Movement
import Snake

data World = World { worldWidth :: Integer
                   , worldHeight :: Integer
                   , worldSnake :: Snake } deriving Show

randomWorld :: IO World
randomWorld = getTerminalWidthHeight >>= return . newWorld >>= seedWorld

newWorld :: Integral a => (a, a) -> World
newWorld (width, height) = World { worldWidth = fromIntegral width
                                 , worldHeight = fromIntegral height
                                 , worldSnake = Snake North [] }

-- OPTIMIZE: this should really be done with a Random instance for the
-- Snake, Coords, World, and Direction types.
seedWorld :: World -> IO World
seedWorld world = (,) <$> randomRIO (0, worldWidth world) <*> randomRIO (0, worldHeight world) >>=
    \(x, y) -> return world { worldSnake = Snake North [Coords x y] }
