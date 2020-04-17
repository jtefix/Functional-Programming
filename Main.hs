module Main where

import Tokens
import System.Environment
import Control.Exception
import System.IO

main :: IO ()
main = catch main' noParse

main' = do 
    (file : _) <- getArgs
    contents <- readFile file
    putStrLn ("Text to be parsed: " ++ contents)
    putStrLn ("Parsed text: " ++ show (alexScanTokens contents))

noParse :: ErrorCall -> IO ()
noParse e = do
    let err = show e
    hPutStr stderr err
    return ()