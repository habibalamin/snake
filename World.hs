module World where

import System.Random (randomRIO, Random)

import Data.List.Split (chunksOf)

import Terminal
import Movement
import Snake

data World = World { worldWidth :: Integer
                   , worldHeight :: Integer
                   , worldSnake :: Snake
                   , pellet :: Coords
                   , gameOver :: Bool } deriving Eq

instance Show World where
    -- OPTIMIZE: this is a hella long function.
    show World { worldWidth = width 
               , worldHeight = height
               , worldSnake = snake
               , pellet = pellet
               , gameOver = gameOver } = if gameOver
    then "Game Over!\n"
    else showChunked $ map drawPoint rowReverseList where
        rowReverseList = concat $ reverse $ chunksOf (fromIntegral width) [0..(size - 1)]
        size = width * height

        drawPoint index = if snakeBodyIsAt index snake || pelletIsAt index pellet then 'o' else ' '
        snakeBodyIsAt index snake = indexToCoords width index `elem` getCoords snake
        pelletIsAt index pellet = indexToCoords width index == pellet

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
                                 , worldSnake = Snake North []
                                 , pellet = Coords pelletX pelletY
                                 , gameOver = False } where
    pelletX = fromIntegral $ width `div` 2
    pelletY = fromIntegral $ height `div` 2

-- OPTIMIZE: this should really be done with a Random instance for the
-- Snake, Coords, World, and Direction types.
seedWorld :: World -> IO World
seedWorld world = randomCoords (worldWidth world) (worldHeight world) >>=
    \coords -> return world { worldSnake = Snake North [coords] }

randomCoords :: (Integral a, Random a) => a -> a -> IO Coords
randomCoords width height = (,) <$> randomRIO (0, width) <*> randomRIO (0, height) >>=
    \(x, y) -> return $ Coords (fromIntegral x) (fromIntegral y)

getSnakeCoords :: World -> [Coords]
getSnakeCoords = getCoords . worldSnake

updatePelletCoords :: World -> IO Coords
updatePelletCoords world = if pellet world `elem` getSnakeCoords world
    then randomCoords (worldWidth world) (worldHeight world)
    else return $ pellet world

-- OPTIMIE: this is a very large function (albeit mainly pattern
-- matching and other simple constructs). It seems to be working
-- well, however, it is not unlikely that I've missed something.
updateWorld :: Move -> Coords -> World -> World
updateWorld move pelletCoords world@World { worldWidth = width
                                          , worldHeight = height
                                          , worldSnake = snake } = if newWorld == world
    then world { gameOver = True }
    else newWorld where
        newWorld = world { pellet = pelletCoords
                         , worldSnake = updateSnake move width height snake } where
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
                        updateTail nhc cs = if nhc == pelletCoords then cs
                            else if nhc == head cs then tail cs else init cs
                        updateHead w h d head = progressHead w h d head where
                            progressHead w h d head = if outOfBounds w h $ newHead d head
                                then head
                                else newHead d head where
                                outOfBounds w h coords@(Coords headX headY) = headX > (w - 1) ||
                                                                            headY > (h - 1) ||
                                                                            headX < 0 ||
                                                                            headY < 0 ||
                                                                            coords `elem` c
                                newHead North (Coords x y) = Coords x (y + 1)
                                newHead East  (Coords x y) = Coords (x + 1) y
                                newHead South (Coords x y) = Coords x (y - 1)
                                newHead West  (Coords x y) = Coords (x - 1) y
