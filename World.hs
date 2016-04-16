module World where

import System.Random (randomRIO)

import Data.List.Split (chunksOf)

import Terminal
import Movement
import Snake

data World = World { worldWidth :: Integer
                   , worldHeight :: Integer
                   , worldSnake :: Snake }

instance Show World where
    -- OPTIMIZE: this is a hella long function.
    show World { worldWidth = width 
               , worldHeight = height
               , worldSnake = snake } = showChunked $ map drawPoint rowReverseList where
        rowReverseList = concat $ reverse $ chunksOf (fromIntegral width) [0..(size - 1)]
        size = width * height

        drawPoint index = if snakeBodyIsAt index snake then 'o' else ' '
        snakeBodyIsAt index snake = indexToCoords width index `elem` getCoords snake

        showChunked list = boxWorld $ chunksOf (fromIntegral width) list
        boxWorld lineList = "/" ++ horizontalEdges ++ "\\\n" ++
            concatMap (\line -> "|" ++ line ++ "|\n") lineList ++
            "\\" ++ horizontalEdges ++ "/"
        horizontalEdges = replicate (fromIntegral width) '-'

randomWorld :: IO World
randomWorld = getTerminalWidthHeight >>= (\(x,y) -> return (x - 2, y - 2)) >>=
    return . newWorld >>= seedWorld

newWorld :: Integral a => (a, a) -> World
newWorld (width, height) = World { worldWidth = fromIntegral width
                                 , worldHeight = fromIntegral height
                                 , worldSnake = Snake North [] }

-- OPTIMIZE: this should really be done with a Random instance for the
-- Snake, Coords, World, and Direction types.
seedWorld :: World -> IO World
seedWorld world = (,) <$> randomRIO (0, worldWidth world) <*> randomRIO (0, worldHeight world) >>=
    \(x, y) -> return world { worldSnake = Snake North [Coords x y] }
