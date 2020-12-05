module Main where

import Numeric (readInt)
import Data.Maybe (listToMaybe, catMaybes)
import Data.List (sort)
import Safe.Foldable (maximumMay)

type Seat = (Int, Int)
type SeatId = Int

parseBin :: Char -> Char -> String -> Maybe Int
parseBin zero one =
    (fmap fst) . listToMaybe . (readInt 2 (`elem` [zero, one]) (fromEnum . (== one)))

parseSeat :: String -> Maybe Seat
parseSeat str =
    let (strRow, strCol) = splitAt 7 str
     in (,) <$> (parseBin 'F' 'B' strRow) <*> (parseBin 'L' 'R' strCol)

seatId :: Seat -> SeatId
seatId (row, col) = row * 8 + col

highestId :: [Seat] -> Maybe SeatId
highestId = maximumMay . (fmap seatId)

mySeat :: [Seat] -> Maybe SeatId
mySeat =  aux . sort . (fmap seatId)
    where 
        aux :: [SeatId] -> Maybe SeatId
        aux (x1 : x2 : xs) = if x1 + 1 == x2 then aux (x2 : xs) else Just (x1 + 1)
        aux _ = Nothing

parseInput :: String -> [Seat]
parseInput = catMaybes . (fmap parseSeat) . lines

main :: IO ()
main =
    do
        input <- parseInput <$> getContents
        putStrLn . show $ highestId input
        putStrLn . show $ mySeat input
