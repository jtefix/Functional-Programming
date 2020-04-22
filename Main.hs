module Main where

import Tokens
import Grammar
import TypeCheck
import System.Environment
import Control.Exception
import System.IO

main :: IO ()
main = catch main' noParse

main' = do 
    (file : _) <- getArgs
    contents <- readFile file
    putStrLn ("Text to be parsed: " ++ contents)
    let string = parseCalc (alexScanTokens contents)
    putStrLn ("Parsed text: " ++ show string)
    let typeOf = checker [] string
    putStrLn ("typeCheck; " ++ show typeOf) 

noParse :: ErrorCall -> IO ()
noParse e = do
    let err = show e
    hPutStr stderr err
    return ()