module Main where

import Control.Applicative
import Data.Maybe (listToMaybe, catMaybes)
import Data.Vector (Vector, (!?))
import qualified Data.Vector as Vector
import qualified Text.ParserCombinators.ReadP as P

-- Problem

data Cell = Snow | Tree
type Line = Vector Cell
type Area = Vector Line
type Position = (Int, Int)
type Slope = (Int, Int)

positions :: Slope -> [Position]
positions (dx, dz) = iterate (\(x, z) -> (x + dx, z + dz)) (0, 0)

isTree :: Cell -> Bool
isTree Tree = True
isTree _ = False

lineHasTree :: Line -> Int -> Maybe Bool
lineHasTree l x = isTree <$> (l !? (mod x (Vector.length l)))

areaHasTree :: Area -> Position -> Maybe Bool
areaHasTree area (x, z) = area !? z >>= (\l -> lineHasTree l x)

nbTrees :: Area -> Slope -> Int
nbTrees area = (countTrees 0) . (fmap (areaHasTree area)) . positions
    where
        countTrees :: Int -> [Maybe Bool] -> Int
        countTrees acc ((Just b) : xs) = countTrees (acc + (fromEnum b)) xs
        countTrees acc _ = acc

-- Parsers

cellP :: P.ReadP Cell
cellP = (Tree <$ (P.char '#')) P.+++ (Snow <$ (P.char '.'))

lineP :: P.ReadP Line
lineP = Vector.fromList <$> (P.many1 cellP) <* P.eof

parseLine :: String -> Maybe Line
parseLine = (fmap fst) . listToMaybe . (P.readP_to_S lineP)

parseInput :: String -> Vector Line
parseInput = Vector.fromList . catMaybes . (fmap parseLine) . lines 

-- Main

slopes :: [Slope]
slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

main :: IO ()
main =
    do
        input <- parseInput <$> getContents
        let treeHazard = nbTrees input <$> slopes
        putStrLn . show $ treeHazard
        putStrLn . show $ product treeHazard
