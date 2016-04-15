module Terminal where

import System.IO (stdin
                , hGetEcho
                , hGetBuffering
                , hSetEcho
                , hSetBuffering
                , BufferMode(..))
import Data.Maybe (fromJust)

import qualified System.Console.Terminal.Size as T

catchInput :: IO a -> IO a
catchInput action = (,) <$> hGetEcho stdin <*> hGetBuffering stdin >>=
    \(echo, buffering) -> hSetEcho stdin False >> hSetBuffering stdin NoBuffering >> action >>=
    \result -> hSetEcho stdin echo >> hSetBuffering stdin buffering >> return result

getTerminalWidthHeight :: Integral a => IO (a, a)
getTerminalWidthHeight = maybeWindowToWidthHeight <$> T.size where
    maybeWindowToWidthHeight maybeWindow = (T.width window, T.height window) where
        window = fromJust maybeWindow
