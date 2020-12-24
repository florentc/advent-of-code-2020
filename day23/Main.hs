{-# LANGUAGE Strict #-}
module Main where

import Text.Read (readMaybe)
import Data.Maybe (catMaybes, fromMaybe)

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

type Ring = IntMap Int

fromList :: [Int] -> Ring
fromList [] = IntMap.empty
fromList l@(x : xs) = IntMap.fromList (zip l (xs <> [x]))

next :: Ring -> Int -> Int
next ring focus = fromMaybe focus $ IntMap.lookup focus ring

detachThree :: Ring -> Int -> ((Int, Int, Int), Ring)
detachThree ring focus =
    let first = next ring focus
        middle = next ring first
        last = next ring middle
     in ((first, middle, last), IntMap.insert focus (next ring last) ring)

squeezeAfter :: Ring -> Int -> (Int, Int) -> Ring
squeezeAfter ring target (first, last) =
    IntMap.insert target first $ IntMap.insert last (next ring target) ring

toList :: Ring -> Int -> [Int]
toList ring focus = focus : aux focus
    where
        aux x = let x' = next ring x
                 in if x' == focus then [] else x' : aux x'

destination :: Int -> (Int, Int) -> [Int] -> Int
destination target (min, max) extracted
  | target < min = destination max (min, max) extracted
  | elem target extracted = destination (target - 1) (min, max) extracted
  | otherwise = target

step :: (Int, Int) -> (Int, Ring) -> (Int, Ring)
step bounds (focus, ring) =
    let ((x1, x2, x3), ring') = detachThree ring focus
        dest = destination (focus - 1) bounds [x1, x2, x3]
        ring'' = squeezeAfter ring' dest (x1, x3)
     in (next ring'' focus, ring'')

solve1 :: (Int, Int) -> Int -> [Int] -> String
solve1 _ _ [] = "empty"
solve1 bounds n l@(x : _) =
    case toList (snd $ (iterate (step bounds) (x, fromList l)) !! n) 1 of
      [] -> "empty"
      (x : xs) -> mconcat $ show <$> xs

solve2 :: (Int, Int) -> Int -> [Int] -> Maybe Int
solve2 _ _ [] = Nothing
solve2 bounds n l@(x : _) =
    let endRing = (snd $ (iterate (step bounds) (x, fromList l)) !! n)
        ((x1, x2, _), _) = detachThree endRing 1
     in Just (x1 * x2)

parseInput :: String -> [Int]
parseInput = catMaybes . fmap (readMaybe . (: []))

main :: IO ()
main =
    do
        input <- parseInput <$> getContents
        putStrLn . show $ solve1 (1, 9) 100 input
        putStrLn . show $ solve2 (1, 1000000) 10000000 (input <> [10 .. 1000000])
