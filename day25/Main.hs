module Main where

import Text.Read (readMaybe)
import Data.List (findIndex)

loopStep :: Int -> Int -> Int
loopStep subject value = rem (value * subject) 20201227

loopSize :: Int -> Maybe Int
loopSize publicKey =
    findIndex (== publicKey) (iterate (loopStep 7) 1) 

encryptionKey :: Int -> Int -> Int
encryptionKey publicKey loopSize =
    (iterate (loopStep publicKey) 1) !! loopSize

parseInput :: String -> Maybe (Int, Int)
parseInput s = case lines s of
                 [s1, s2] -> (,) <$> readMaybe s1 <*> readMaybe s2
                 _ -> Nothing

main :: IO ()
main =
    do
        input <- parseInput <$> getContents
        case input of
          Nothing -> putStrLn "invalid input"
          Just (pk1, pk2) ->
                  let (ls1, ls2) = (loopSize pk1, loopSize pk2)
                   in putStrLn . show $ encryptionKey pk1 <$> ls2
