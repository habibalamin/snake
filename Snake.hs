module Snake where

import Movement

data Snake = Snake { getDirection :: Direction
                   , getCoords :: [Coords] }

instance Show Snake where
    show (Snake direction []) = "Snake " ++ show direction ++ " []"
    show (Snake direction coords) = "Snake " ++ show direction ++ " [" ++ unwords (map showSnakeCoord coords) ++ "]" where
        showSnakeCoord (Coords x y) = "(" ++ show x ++ " " ++ show y ++ ")"
