module Movement where

data Move = Up | Down | Left | Right | Noop deriving Show

mkMove :: String -> Move
mkMove "\ESC[A" = Up
mkMove "\ESC[B" = Down
mkMove "\ESC[C" = Movement.Right
mkMove "\ESC[D" = Movement.Left
mkMove _        = Noop
