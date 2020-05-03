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
    (file :  _) <- getArgs
    problem <- readFile file
    input <- fmap lines getContents
    let string = parseCalc (alexScanTokens problem)
    let typeOf = checker [] string 
    let s1 = map (splitOn ' ') input
    let s2 = map (map read) s1 :: [[Int]]
    let s3 = multiZip s2
    let result = evalLoop string s3
    if (null result) then (return ()) else (putStr result) 
     
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