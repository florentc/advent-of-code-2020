module Main where

import Data.Char (isDigit)
import Data.Maybe (catMaybes, listToMaybe)
import Text.Read (readMaybe)
import Text.ParserCombinators.ReadP

-- Problem

data Policy = Policy Int Int Char
data Item = Item Policy String

oldValid :: Item -> Bool
oldValid (Item (Policy min max char) password) =
    let n = length (filter (== char) password) in min <= n && n <= max

newValid :: Item -> Bool
newValid (Item (Policy i j char) password) = newValid' char i j False password

newValid' :: Char -> Int -> Int -> Bool -> String -> Bool
newValid' c i j b [] = b
newValid' c i j b (x:xs)
    | i > 0 = newValid' c (i - 1) (j - 1) (if i == 1 then c == x else b) xs
    | i == 0 && j > 1 = newValid' c 0 (j - 1) b xs
    | i == 0 && j == 1 = b /= (c == x)
    | otherwise = False

-- Parsers

number :: ReadP Int
number = munch1 isDigit >>= (maybe pfail return) . readMaybe

policy :: ReadP Policy
policy = Policy <$> number <* char '-' <*> number <* skipSpaces <*> get

item :: ReadP Item
item = Item <$> policy <* char ':' <* skipSpaces <*> look

parseItem :: String -> Maybe Item
parseItem = (fmap fst) . listToMaybe . (readP_to_S item)

parseInput :: String -> [Item]
parseInput = catMaybes . (fmap parseItem) . lines 

-- Main

main :: IO ()
main =
    do
        input <- parseInput <$> getContents
        putStrLn $ show $ length . (filter oldValid) $ input
        putStrLn $ show $ length . (filter newValid) $ input
