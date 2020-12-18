{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Text.Parsec (Parsec)
import qualified Text.Parsec as P
import qualified Text.Parsec.Char as P
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.Hashable (Hashable)
import Data.Ix (range)
import Safe (atMay)

-- Problem

newtype Pos3D = Pos3D (Int, Int, Int) deriving (Eq, Hashable)
newtype Pos4D = Pos4D (Int, Int, Int, Int) deriving (Eq, Hashable)
type Space p = HashMap p (Int, Bool)

class (Eq a, Hashable a) => Position a where
    vicinity :: a -> [a]

instance (Position Pos3D) where
    vicinity (Pos3D (x0, y0, z0)) =
        [Pos3D (x, y, z)
          | x <- [x0 - 1 .. x0 + 1]
          , y <- [y0 - 1 .. y0 + 1]
          , z <- [z0 - 1 .. z0 + 1]
          , (x, y, z) /= (x0, y0, z0)]

instance (Position Pos4D) where
    vicinity (Pos4D (x0, y0, z0, w0)) =
        [Pos4D (x, y, z, w)
          | x <- [x0 - 1 .. x0 + 1]
          , y <- [y0 - 1 .. y0 + 1]
          , z <- [z0 - 1 .. z0 + 1]
          , w <- [w0 - 1 .. w0 + 1]
          , (x, y, z, w) /= (x0, y0, z0, w0)]

tellAboutActivation :: Position p => p -> Space p -> Space p
tellAboutActivation i s0 = foldr notify s0 (vicinity i)
    where notify :: Position p => p -> Space p -> Space p
          notify j s =
              case HashMap.lookup j s of
                Nothing -> HashMap.insert j (1, False) s
                Just (n, b) -> HashMap.insert j (n + 1, b) s

tellAboutDeactivation :: Position p => p -> Space p -> Space p
tellAboutDeactivation i s0 = foldr notify s0 (vicinity i)
    where notify :: Position p => p -> Space p -> Space p
          notify j s =
              case HashMap.lookup j s of
                Nothing -> s
                Just (n, b)
                  | n > 1 || b -> HashMap.insert j (n - 1, b) s
                  | otherwise -> HashMap.delete j s

activate :: Position p => p -> Space p -> Space p
activate i s =
    case HashMap.lookup i s of
      Nothing -> tellAboutActivation i $ HashMap.insert i (0, True) s
      Just (n, False) -> tellAboutActivation i $ HashMap.insert i (n, True) s
      Just (_, True) -> s

deactivate :: Position p => p -> Space p -> Space p
deactivate i s =
    case HashMap.lookup i s of
      Just (n, True) -> tellAboutDeactivation i $ HashMap.insert i (n, False) s
      _ -> s

activePositions :: Position p => Space p -> [p]
activePositions = (fmap fst) . filter (snd . snd) . HashMap.toList

hasToSwitch :: (Int, Bool) -> Bool
hasToSwitch (n, b) = (b && not (n == 2 || n == 3)) || (not b && n == 3)

step :: forall p. Position p => Space p -> Space p
step s = foldr set s operations
    where
        operations :: [(p, Bool)]
        operations =
            (\(i, (_, b)) -> (i, not b)) <$>
                filter (hasToSwitch . snd) (HashMap.toList s)
        set :: (p, Bool) -> Space p -> Space p
        set (i, True) = activate i
        set (i, False) = deactivate i

fromGrid :: Position p => [[Bool]] -> [p] -> Space p
fromGrid grid range = (foldr activate HashMap.empty)
                    . (fmap fst)
                    . (filter snd)
                    $ (zip range (mconcat grid))

solve :: Int -> [[Bool]] -> (Maybe Int, Maybe Int)
solve _ [] = (Nothing, Nothing)
solve n grid@(line0 : lines) =
    let (width, height)  = (length line0, 1 + length lines)
        range3d = Pos3D <$> range ((0, 1, 1), (0, height, width))
        range4d = Pos4D <$> range ((0, 0, 1, 1), (0, 0, height, width))
        f :: Position p => [p] -> Maybe Int
        f = (fmap (length . activePositions)) .
            (`atMay` n) . (iterate step) . (fromGrid grid)
     in (f range3d, f range4d)

-- Parsers

pLine :: Parsec String u [Bool]
pLine = (P.many1 $ True <$ P.char '#' P.<|> False <$ P.char '.') <* P.endOfLine

pInput :: Parsec String u [[Bool]]
pInput = P.many1 pLine <* P.eof

-- Main

main :: IO ()
main =
    do
        eInput <- P.parse pInput "input" <$> getContents
        case eInput of
          Left parsingError -> putStrLn . show $ parsingError
          Right grid -> putStrLn . show $ solve 6 grid
