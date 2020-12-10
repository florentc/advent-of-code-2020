module Main where

import Data.Sequence (Seq, Seq((:<|), (:|>), Empty))
import qualified Data.Sequence as Seq
import Data.Maybe (catMaybes)
import Text.Read (readMaybe)

summableFrom :: Int -> Seq Int -> Bool
summableFrom n Empty = False
summableFrom n (x :<| xs) = any ((== n) . (+ x)) xs || summableFrom n xs

firstInvalid :: Int -> Seq Int -> Maybe Int
firstInvalid step = (uncurry aux) . (Seq.splitAt step)
    where aux :: Seq Int -> Seq Int -> Maybe Int
          aux preds@(p :<| ps) (x :<| xs)
            | summableFrom x preds = aux (ps :|> x) xs
            | otherwise = Just x
          aux _ _ = Nothing

weakness :: Int -> Seq Int -> Maybe Int
weakness target Empty = Nothing
weakness target l@(x :<| xs) =
    maybe (weakness target xs) (\(x, y) -> Just (x + y)) (aux target (x, x) l)
        where aux :: Int -> (Int, Int) -> Seq Int -> Maybe (Int, Int)
              aux _ (a, b) Empty = Nothing
              aux n (a, b) (x :<| xs)
                | n - x < 0 = Nothing
                | n - x == 0 = Just (min a x, max b x)
                | otherwise = aux (n - x) (min a x, max b x) xs

solve :: Int -> Seq Int -> Maybe (Int, Int)
solve step input = do target <- firstInvalid step input
                      weakness <- weakness target input
                      return (target, weakness)

parseInput :: String -> Seq Int
parseInput = Seq.fromList . catMaybes . (fmap readMaybe) . lines

main :: IO ()
main = putStrLn . show . (solve 25) . parseInput =<< getContents
