module Main where

import Text.Read (readMaybe)
import Data.List (sort)
import Data.Maybe (catMaybes)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

joltDiff :: [Int] -> Int
joltDiff = (uncurry (*)) . (diffCount (0, 1))
    where diffCount :: (Int, Int) -> [Int] -> (Int, Int)
          diffCount n [] = n
          diffCount n [x] = n
          diffCount (n1, n3) (x1 : x2 : xs)
            | x2 - x1 == 1 = diffCount (n1 + 1, n3) (x2 : xs)
            | x2 - x1 == 3 = diffCount (n1, n3 + 1) (x2 : xs)
            | otherwise    = diffCount (n1, n3)     (x2 : xs)

type Graph a = Map a (Set a)

buildGraph :: Graph Int -> [Int] -> Graph Int
buildGraph g [] = g
buildGraph g (x : xs) =
    buildGraph
        (Map.insert x
            (Set.fromList $ takeWhile (\y -> 1 <= y - x && y - x <= 3)
            xs) g) xs

nbArrangements :: Graph Int -> Maybe Int
nbArrangements g = pathCount g (Map.keys g) (Map.singleton 0 1)
    where
        pathCount :: Graph Int -> [Int] -> Map Int Int -> Maybe Int
        pathCount g [] acc = Nothing
        pathCount g [x] acc = Map.lookup x acc
        pathCount g (x : xs) acc = 
            do
                dests <- Map.lookup x g
                n <- Map.lookup x acc
                pathCount g xs (foldr (\x -> Map.insertWith (+) x n) acc dests)

parseInput :: String -> [Int]
parseInput = sort . (0 :) . catMaybes . (fmap readMaybe) . lines

main :: IO ()
main =
    do
        input <- parseInput <$> getContents
        putStrLn . show $ joltDiff input
        putStrLn . show $ nbArrangements . (buildGraph Map.empty) $ input
