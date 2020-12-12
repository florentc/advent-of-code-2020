module Main where

import Text.Parsec (Parsec, (<|>))
import qualified Text.Parsec as P
import qualified Text.Parsec.Char as P
import Data.Maybe (catMaybes)
import Vector2D (Vector2D)
import qualified Vector2D as Vector2D
import qualified Data.Vector as Vector

-- Problem

data Cell = Floor | Empty | Occupied
type Grid = Vector2D Cell
type Position = (Int, Int)
type Direction = (Int, Int)
type Modification = (Position, Cell)

neighbors :: Position -> Grid -> [Cell]
neighbors (x, y) g =
    catMaybes $ (\i -> Vector2D.lookup i g) <$>
        [(i, j) | i <- [x - 1 .. x + 1], j <- [y - 1 .. y + 1], i /= x || j /= y]

closestSeat :: Position -> Grid -> Direction -> Maybe Cell
closestSeat (x, y) g (dx, dy) =
    do
        cell <- Vector2D.lookup (x + dx, y + dy) g
        case cell of
          Floor -> closestSeat (x + dx, y + dy) g (dx, dy)
          _ -> return cell

closestSeats :: Position -> Grid -> [Cell]
closestSeats pos g =
    catMaybes $ closestSeat pos g <$>
        [(dx, dy) | dx <- [-1 .. 1], dy <- [-1 .. 1], dx /= 0 || dy /= 0]

isOccupied :: Cell -> Bool
isOccupied Occupied = True
isOccupied _ = False

nbOccupied :: [Cell] -> Int
nbOccupied = length . (filter isOccupied)

rule1 :: Grid -> Position -> Maybe Modification
rule1 g i = 
    do
        cell <- Vector2D.lookup i g
        case cell of
          Empty | nbOccupied (neighbors i g) == 0 -> Just (i, Occupied)
          Occupied | nbOccupied (neighbors i g) >= 4 -> Just (i, Empty)
          _ -> Nothing

rule2 :: Grid -> Position -> Maybe Modification
rule2 g i = 
    do
        cell <- Vector2D.lookup i g
        case cell of
          Empty | nbOccupied (closestSeats i g) == 0 -> Just (i, Occupied)
          Occupied | nbOccupied (closestSeats i g) >= 5 -> Just (i, Empty)
          _ -> Nothing

next :: (Grid -> Position -> Maybe Modification) -> Grid -> Maybe Grid
next rule g =
    let modifications = catMaybes $ (rule g) <$> Vector2D.indexes g
     in if null modifications
           then Nothing
           else Just $ Vector2D.update g modifications

stableStep :: (Grid -> Position -> Maybe Modification) -> Grid -> Grid
stableStep rule g = maybe g (stableStep rule) (next rule g)

-- Parsers

pCell :: Parsec String u Cell
pCell = Floor <$ P.char '.' <|> Empty <$ P.char 'L' <|> Occupied <$ P.char '#'

pGrid :: Parsec String u Grid
pGrid =
    ((Vector2D.fromList) <$> P.many1 (P.many1 pCell <* P.endOfLine) <* P.eof)
    >>= maybe (P.unexpected "line size") return

-- Main

main :: IO ()
main =
    do
        eInput <- P.parse pGrid "input" <$> getContents
        case eInput of
          Left parsingError -> putStrLn . show $ parsingError
          Right input ->
              do
                  putStrLn . show $
                      ( nbOccupied . Vector2D.elems $ stableStep rule1 input
                      , nbOccupied . Vector2D.elems $ stableStep rule2 input )
