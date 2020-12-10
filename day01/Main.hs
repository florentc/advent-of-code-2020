module Main where

import System.IO (getContents)
import Text.Read (readMaybe)
import Data.Maybe (catMaybes)
import Data.List (lookup)

parseInput :: Read a => String -> [a]
parseInput = catMaybes . (fmap readMaybe) . lines 

couples :: Eq a => [a] -> [[a]]
couples l = [[x, y] | x <- l, y <- l, x /= y]

triples :: Eq a => [a] -> [[a]]
triples l = [[x, y, z] | x <- l, y <- l, z <- l, x /= y, y /= z, x /= z]

sumProduct :: Num a => [a] -> (a, a)
sumProduct l = (sum l, product l)

solve :: (Eq a, Num a) => [[a]] -> Maybe a
solve = (lookup 2020) . (fmap sumProduct)

main :: IO ()
main =
    do
        input <- parseInput <$> getContents
        putStrLn (show . solve . couples $ input) 
        putStrLn (show . solve . triples $ input) 
