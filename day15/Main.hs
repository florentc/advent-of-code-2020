module Main where

import Text.Read (readMaybe)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)
import Data.Foldable (toList)
import Data.Sequence (Seq, Seq((:<|), (:|>)))
import qualified Data.Sequence as Seq
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap

type Position = Int
type Turn = Int
data State = State
    { _positions :: !(IntMap Position)
    , _turn :: !Turn
    , _submission :: !Int }

initial :: Seq Int -> Maybe State
initial Seq.Empty = Nothing
initial input@(ns :|> last) =
    Just $ State (IntMap.fromList (zip (toList ns) [1..])) (length input) last

turn :: State -> State
turn (State positions turn submission) =
    State
    (IntMap.insert submission turn positions)
    (turn + 1)
    (maybe 0 (turn -) $ IntMap.lookup submission positions)

atTurn :: Seq Int -> Turn -> (Maybe Int)
atTurn input target =
        _submission . (until ((\s -> _turn s == target)) turn) <$> initial input

parseInput :: String -> Seq Int
parseInput = Seq.fromList . catMaybes . (fmap readMaybe) . splitOn ","

main :: IO ()
main =
    do
        input <- parseInput <$> getContents
        putStrLn . show $ input `atTurn` 2020
        putStrLn . show $ input `atTurn` 30000000
