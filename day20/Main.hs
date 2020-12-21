{-# LANGUAGE TupleSections #-}
module Main where

import Text.Read (readMaybe)
import Text.Parsec (Parsec)
import qualified Text.Parsec as P
import qualified Text.Parsec.Char as P

import Vector2D (Vector2D)
import qualified Vector2D as Vector2D

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Maybe (catMaybes)
import Data.List (find)

-- Problem

type Cell = Int
type Id = Int
type Grid = Vector2D Cell

type Flipped = Bool
data Rotation = R0 | R1 | R2 | R3 deriving Show
type Transformation = (Rotation, Flipped)

data Side = T | R | B | L deriving (Show, Eq, Ord)

type Border = [Cell]
type BorderInfo = (Side, Flipped)

type GridCatalog = Map Id Grid
type BordersCatalog = Map Id [(Border, BorderInfo)]

tileBorders :: Grid -> [(Border, BorderInfo)]
tileBorders grid =
    let (bt, br, bb, bl) = Vector2D.borders grid
     in [ (bt, (T, False))
        , (br, (R, False))
        , (bb, (B, False))
        , (bl, (L, False))
        , (reverse bt, (T, True))
        , (reverse br, (R, True))
        , (reverse bb, (B, True))
        , (reverse bl, (L, True))
        ]

bordersMap :: GridCatalog -> BordersCatalog
bordersMap = Map.map tileBorders

allBorders :: BordersCatalog -> [(Id, Border, BorderInfo)]
allBorders = mconcat . (fmap aux) . Map.assocs
    where aux (i, l) = (\(x, y) -> (i, x, y)) <$> l

matchBorder :: Side -> Border -> BordersCatalog -> Maybe (Id, Transformation)
matchBorder side border =
    (fmap (\(id, b, bInfo) -> (id, transformationFromMatch side bInfo)))
    . (find (\(_, b, _) -> b == border))
    . allBorders

findMatches :: Id -> GridCatalog -> BordersCatalog -> Map Side (Id, Transformation)
findMatches id gridCatalog bordersCatalog =
    case Map.lookup id gridCatalog of
      Nothing -> Map.empty
      Just grid ->
          let (bt, br, bb, bl) = Vector2D.borders grid
              candidates = Map.delete id bordersCatalog
           in Map.fromList . catMaybes $
              [ (T,) <$> matchBorder T bt candidates
              , (R,) <$> matchBorder R br candidates
              , (B,) <$> matchBorder B bb candidates
              , (L,) <$> matchBorder L bl candidates
              ]

matchesCatalog :: GridCatalog -> Map Id (Map Side (Id, Transformation))
matchesCatalog gridCat =
    let borderCat = bordersMap gridCat
    in Map.mapWithKey (\id _ -> findMatches id gridCat borderCat) gridCat

solve1 :: GridCatalog -> Int
solve1 = product . Map.keys . (Map.filter ((== 2) . length)) . matchesCatalog

transformationFromMatch :: Side -> BorderInfo -> Transformation
transformationFromMatch T (T, False) = (R2, True)
transformationFromMatch T (R, False) = (R3, True)
transformationFromMatch T (B, False) = (R0, False)
transformationFromMatch T (L, False) = (R3, False)
transformationFromMatch T (T, True)  = (R2, False)
transformationFromMatch T (R, True)  = (R1, False)
transformationFromMatch T (B, True)  = (R0, True)
transformationFromMatch T (L, True)  = (R1, True)
transformationFromMatch R (T, False) = (R3, True)
transformationFromMatch R (R, False) = (R0, True)
transformationFromMatch R (B, False) = (R1, False)
transformationFromMatch R (L, False) = (R0, False)
transformationFromMatch R (T, True)  = (R3, False)
transformationFromMatch R (R, True)  = (R2, False)
transformationFromMatch R (B, True)  = (R1, True)
transformationFromMatch R (L, True)  = (R2, True)
transformationFromMatch B (T, False) = (R0, False)
transformationFromMatch B (R, False) = (R3, False)
transformationFromMatch B (B, False) = (R2, True)
transformationFromMatch B (L, False) = (R3, True)
transformationFromMatch B (T, True)  = (R0, True)
transformationFromMatch B (R, True)  = (R1, True)
transformationFromMatch B (B, True)  = (R2, False)
transformationFromMatch B (L, True)  = (R1, False)
transformationFromMatch L (T, False) = (R1, False)
transformationFromMatch L (R, False) = (R0, False)
transformationFromMatch L (B, False) = (R3, True)
transformationFromMatch L (L, False) = (R0, True)
transformationFromMatch L (T, True)  = (R1, True)
transformationFromMatch L (R, True)  = (R2, True)
transformationFromMatch L (B, True)  = (R3, False)
transformationFromMatch L (L, True)  = (R2, False)

-- Parsers

pNum :: Parsec String u Int
pNum = P.many1 P.digit
    >>= (maybe (P.unexpected "invalid nat") return) . readMaybe

pCell :: Parsec String u Cell
pCell = 0 <$ P.char '.' P.<|> 1 <$ P.char '#'

pGrid :: Parsec String u (Vector2D Cell)
pGrid =
    do
        mGrid <- ((Vector2D.fromList) <$> P.many1 (P.many1 pCell <* P.endOfLine))
        case mGrid of
          Nothing -> P.unexpected "grid"
          Just g -> return $ g

pTile :: Parsec String u (Id, Grid)
pTile =
    (,) <$> (P.string "Tile " *> pNum <* P.char ':' <* P.endOfLine)
        <*> pGrid

pInput :: Parsec String u (Map Id Grid)
pInput = Map.fromList <$> P.sepBy1 pTile P.endOfLine <* P.eof

-- Main

main :: IO ()
main =
    do
        eInput <- P.parse pInput "input" <$> getContents
        case eInput of
          Left parsingError -> putStrLn . show $ parsingError
          Right input -> do
              putStrLn . show $ solve1 input
