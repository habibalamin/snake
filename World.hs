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

-- OPTIMIE: this is a very large function (albeit mainly pattern
-- matching and other simple constructs). It seems to be working
-- well, however, it is not unlikely that I've missed something.
updateWorld :: Move -> World -> World
updateWorld move world@World { worldWidth = width
                             , worldHeight = height
                             , worldSnake = snake } = world { worldSnake = updateSnake
    move width height snake } where
        updateSnake m w h Snake { getDirection = d
                                , getCoords = c } = Snake { getDirection = newDirection m d
                                                          , getCoords = newCoords m w h d c } where
            newDirection Noop           d = d
            newDirection Up             d = if d == South then d else North
            newDirection Down           d = if d == North then d else South
            newDirection Movement.Left  d = if d == East then d else West
            newDirection Movement.Right d = if d == West then d else East
            newCoords m w h d coords = let newHeadCoords = updateHead w h d (head coords) in
                newHeadCoords : updateTail newHeadCoords coords where
                    updateTail nhc cs = if length cs == 1 then []
                        else if nhc == head cs then tail cs else init cs
                    updateHead w h d head = progressHead w h d head where
                        progressHead w h d head = if outOfBounds w h $ newHead d head
                            then head
                            else newHead d head where
                            outOfBounds w h (Coords headX headY) = headX > (w - 1) ||
                                                                   headY > (h - 1) ||
                                                                   headX < 0 ||
                                                                   headY < 0
                            newHead North (Coords x y) = Coords x (y + 1)
                            newHead East  (Coords x y) = Coords (x + 1) y
                            newHead South (Coords x y) = Coords x (y - 1)
                            newHead West  (Coords x y) = Coords (x - 1) y
