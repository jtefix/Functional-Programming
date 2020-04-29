module Main where

import Tokens
import Grammar
import TypeCheck
import Eval
import System.Environment
import Control.Exception
import System.IO
import Data.List
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

main :: IO ()
main = catch main' noParse

main' = do 
    (file1 : _) <- getArgs
    problem <- readFile file1
    input <- fmap lines getContents
    putStrLn ("Text to be parsed: " ++ problem)
    let string = parseCalc (alexScanTokens problem)
    putStrLn ("Parsed text: " ++ show string)
    let typeOf = checker [] string
    putStrLn ("typeCheck: " ++ show typeOf) 
    putStrLn ("Type Checking Passed with type " ++ (printType typeOf) ++ "\n") 
    let s1 = map (splitOn ' ') input
    let s2 = map (map read) s1 :: [[Int]]
    let s3 = multiZip s2
    let result = evalLoop (string) s3
    putStrLn (show s3)
    putStrLn ("Evaluates to " ++ (unparse result) ++ "\n")


noParse :: ErrorCall -> IO ()
noParse e = do
    let err = show e
    hPutStr stderr err
    return ()

multiZip :: [[a]] -> [[a]]
multiZip [] = []
multiZip ([]:xss) = multiZip xss
multiZip ((x:xs):xss) = (x:[z|(z:_) <- xss]) : multiZip (xs : [t | (_:t) <- xss])

splitOn :: Char -> String -> [String]
splitOn c [] = []
splitOn c ls = (takeWhile (/=c) ls) : splitOn' c (dropWhile (/=c) ls)
 where splitOn' c [] = []
       splitOn' c (x:[]) | x==c = [[]]
       splitOn' c (x:xs) | x==c = splitOn c xs
                         | otherwise = []

-- main = do 
--     (file : _) <- getArgs
--     contents <- fmap lines (readFile file)
--     let s1 = map (splitOn ' ') contents
--     let s2 = map (map read) s1 :: [[Int]]
--     let s3 = multiZip s2
--     putStrLn (show s3)
  --  print $ splitOn "\n" contents