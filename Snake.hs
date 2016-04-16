module Snake where

data Coords = Coords Integer Integer

instance Show Coords where
    show (Coords x y) = "(x: " ++ show x ++ ", y: " ++ show y ++ ")"

data Snake = Snake { getCoords :: [Coords] }

instance Show Snake where
    show (Snake []) = "Snake []"
    show (Snake coords) = "Snake [" ++ unwords (map showSnakeCoord coords) ++ "]" where
        showSnakeCoord (Coords x y) = "(" ++ show x ++ " " ++ show y ++ ")"
