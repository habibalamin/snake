module Terminal where

import System.IO (stdin
                , hGetEcho
                , hGetBuffering
                , hSetEcho
                , hSetBuffering
                , BufferMode(..))

catchInput :: IO a -> IO a
catchInput action = (,) <$> hGetEcho stdin <*> hGetBuffering stdin >>=
    \(echo, buffering) -> hSetEcho stdin False >> hSetBuffering stdin NoBuffering >> action >>=
    \result -> hSetEcho stdin echo >> hSetBuffering stdin buffering >> return result
