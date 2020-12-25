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

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Maybe (catMaybes, listToMaybe)
import Data.List (find)
import Data.List.Split (chunksOf)
import Data.Ix (range)

-- Problem

type Cell = Int
type Id = Int
type Grid = Vector2D Cell

type Flipped = Bool
data Rotation = R0 | R1 | R2 | R3 deriving (Show, Enum)
type Transformation = (Rotation, Flipped)

data Side = T | R | B | L deriving (Show, Eq, Ord, Enum)

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
              , (L,) <$> matchBorder L bl candidates ]

matchesCatalog :: GridCatalog -> Map Id (Map Side (Id, Transformation))
matchesCatalog gridCat =
    let borderCat = bordersMap gridCat
    in Map.mapWithKey (\id _ -> findMatches id gridCat borderCat) gridCat

-- Metagrid: a metagrid is a grid of reference to individual tiles and how they
-- are to be oriented and flipped. Coordinates are transformed accordingly when
-- accessing individual cells to target the right position in the raw
-- non-transformed initial tiles in the catalog

data Metagrid = Metagrid
    { _mgTileSize :: Int
    , _mgCatalog :: GridCatalog
    , _mgContent :: Vector2D (Id, Transformation) } deriving Show

metaSize :: Metagrid -> Int
metaSize (Metagrid tilesize _ grid) = tilesize * Vector2D.width grid

applyTransformation :: Int -> Transformation -> (Int, Int) -> (Int, Int)
applyTransformation n (R0, False) i = i
applyTransformation n (R1, False) (x, y) = (y, n - 1 - x)
applyTransformation n (R2, False) (x, y) = (n - 1 - x, n - 1 - y)
applyTransformation n (R3, False) (x, y) = (n - 1 - y, x)
applyTransformation n (r, True) (x, y) =
    (\(i, j) -> (n - 1 - i, j)) (applyTransformation n (r, False) (x, y))

metaLookup :: Metagrid -> (Int, Int) -> Maybe Cell
metaLookup (Metagrid tilesize gridCatalog grid) (x, y) =
    do
        let (metaX, localX) = (divMod x tilesize)
        let (metaY, localY) = (divMod y tilesize)
        (id, transformation) <- Vector2D.lookup (metaX, metaY) grid
        localGrid <- Map.lookup id gridCatalog
        Vector2D.lookup
            (applyTransformation tilesize transformation (localX, localY))
            localGrid

-- Building a metagrid consists in finding a corner tile, orienting it top
-- left, and iterate to the right and downwards through the neighbors graph

buildMetagrid :: GridCatalog -> Maybe Metagrid
buildMetagrid gridCatalog =
    do
        let matches = matchesCatalog gridCatalog
        (cornerId, (s1, s2)) <- corner matches
        grid <- Vector2D.fromList $
            array (cornerId, (makeCornerTopLeft (s1, s2), False)) matches
        (firstTileId, _) <- Vector2D.lookup (1, 1) grid
        tilesize <- Vector2D.width <$> Map.lookup firstTileId gridCatalog
        return $ Metagrid tilesize gridCatalog grid
            where
                makeCornerTopLeft :: (Side, Side) -> Rotation
                makeCornerTopLeft (R, B) = R0
                makeCornerTopLeft (B, R) = R0
                makeCornerTopLeft (L, B) = R3
                makeCornerTopLeft (B, L) = R3
                makeCornerTopLeft (L, T) = R3
                makeCornerTopLeft (T, L) = R2
                makeCornerTopLeft (R, T) = R1
                makeCornerTopLeft (T, R) = R1

-- Find a corner tile among the graph of neighbors

corner :: Map Id (Map Side (Id, Transformation)) -> Maybe (Id, (Side, Side))
corner matches =
    do
        let sides = (Map.filter ((== 2) . length)) $ matches
        cornerId <- listToMaybe $ Map.keys sides
        sides <- Map.keys <$> (Map.lookup cornerId matches)
        (s1, s2) <- case sides of
                      [s1, s2] -> Just $ (s1, s2)
                      _ -> Nothing
        return (cornerId, (s1, s2))

-- Adjust the direction individual transformations when iterating through the
-- neighboring tiles

compensateDirection :: Side -> Transformation -> Side
compensateDirection s (r, f) =
    case (f, (toEnum $ (fromEnum s - fromEnum r) `mod` 4)) of
      (True, L) -> R
      (True, R) -> L
      (_, s') -> s'

concatTransform :: Transformation -> Transformation -> Transformation
concatTransform (r1, f1) (r2, f2) =
    let f' = toEnum $ (fromEnum f1 + fromEnum f2) `mod` 2
        r' = (fromEnum r1 + fromEnum r2) 
        r'' = (fromEnum r1 - fromEnum r2) 
     in (toEnum $ (if f1 then r'' else r') `mod` 4, f')

-- Build the lines of a metagrid

line :: (Id, Transformation)
     -> Map Id (Map Side (Id, Transformation))
     -> [(Id, Transformation)]
line (id, trans) matchesCatalog =
    let newDirection = compensateDirection R trans
        rem = case Map.lookup id matchesCatalog of
                Nothing -> []
                Just sides ->
                    case Map.lookup newDirection sides of
                      Nothing -> []
                      Just (id, t) ->
                          line (id, concatTransform trans t) matchesCatalog
     in (id, trans) : rem

array :: (Id, Transformation)
      -> Map Id (Map Side (Id, Transformation))
      -> [[(Id, Transformation)]]
array (id, trans) matchesCatalog =
    let (s1', s2') = (compensateDirection B trans, compensateDirection R trans)
        rem = case Map.lookup id matchesCatalog of
                Nothing -> []
                Just sides ->
                    case Map.lookup s1' sides of
                      Nothing -> []
                      Just (id, t) ->
                          array (id, concatTransform trans t) matchesCatalog
     in (line (id, trans) matchesCatalog) : rem

-- Helper functions to ignore individual tiles' borders in a metagrid

metaLookupNoBorder :: Metagrid -> (Int, Int) -> Maybe Cell
metaLookupNoBorder metagrid =
    (metaLookup metagrid) . (realCoordFromNoBorderCoord (_mgTileSize metagrid))

realCoordFromNoBorderCoord :: Int -> (Int, Int) -> (Int, Int)
realCoordFromNoBorderCoord tilesize (x, y) = (aux x, aux y)
    where aux x = let (q, r) = divMod x (tilesize - 2) in tilesize * q + r + 1

metaSizeNoBorder :: Metagrid -> Int
metaSizeNoBorder (Metagrid tilesize _ grid) = (tilesize - 2) * Vector2D.width grid

metaRangeNoBorder :: Metagrid -> [(Int, Int)]
metaRangeNoBorder metagrid =
    let n = metaSizeNoBorder metagrid in range ((0, 0), (n - 1, n - 1))

-- Monster hunting
type Monster = [(Int, Int)]

--                   # 
-- #    ##    ##    ###
--  #  #  #  #  #  #   
seaMonster :: Monster
seaMonster =
    [ (18,0)
    , (0, 1), (5, 1), (6, 1), (11, 1), (12, 1), (17, 1), (18, 1), (19, 1)
    , (1, 2), (4, 2), (7, 2), (10, 2), (13, 2), (16, 2) ]

miniMonster :: [(Int, Int)]
miniMonster = [(0, 0)]

checkMonster :: Monster
             -> (Metagrid, Transformation)
             -> (Int, Int)
             -> Set (Int, Int)
checkMonster monster (metagrid, trans) (x, y) =
    maybe Set.empty id $
    do
        let n = metaSizeNoBorder metagrid
        let monsterCoords = (\(i, j) -> (x + i, y + j)) <$> monster
        cells <- sequence $
            (\i -> metaLookupNoBorder metagrid (applyTransformation n trans i))
            <$> monsterCoords
        if all (== 1) cells
           then return $ Set.fromList monsterCoords
           else Nothing

checkMonsterMetagrid :: Monster -> Metagrid -> Maybe Int
checkMonsterMetagrid monster metagrid =
    let aux m t =
            Set.size . Set.unions $
                (checkMonster m (metagrid, t)) <$> (metaRangeNoBorder metagrid)
        nbFull = aux miniMonster (R0, False)
        seaMonstersCells =
            (aux monster) <$> ((,) <$> [R0, R1, R2, R3] <*> [False, True])
     in (nbFull -) <$> find (> 0) seaMonstersCells
            
solve1 :: GridCatalog -> Int
solve1 = product . Map.keys . (Map.filter ((== 2) . length)) . matchesCatalog

solve2 :: GridCatalog -> Maybe Int
solve2 input = buildMetagrid input >>= checkMonsterMetagrid seaMonster

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
              putStrLn . show $ solve2 input
