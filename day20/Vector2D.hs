{-# LANGUAGE TupleSections #-}
module Vector2D
    ( Vector2D
    , Vector2D.lookup
    , (!?)
    , fromList
    , inRange
    , assocs
    , elems
    , indexes
    , update
    , borders
    , width
    , height
    )
    where

import Data.Ix (range)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Safe (headMay)
import Data.Maybe (catMaybes)

data Vector2D a = Vector2D
    { width :: Int
    , height :: Int
    , content :: Vector a}

instance Show a => (Show (Vector2D a)) where
    show (Vector2D w h c)
      | h == 0 = "\n"
      | otherwise =
          let (line, rest) = Vector.splitAt w c
           in (mconcat $ (show <$> (Vector.toList line)))
           <> "\n"
           <> (show (Vector2D w (h - 1) rest))


lookup :: (Int, Int) -> Vector2D a -> Maybe a
lookup (i, j) x@(Vector2D w h content) =
    if inRange (i, j) x
       then content Vector.!? (w * j + i)
       else Nothing

(!?) :: Vector2D a -> (Int, Int) -> Maybe a
(!?) = flip Vector2D.lookup

borders :: Vector2D a -> ([a], [a], [a], [a])
borders x@(Vector2D w h _) =
    ( catMaybes $ (x !?) <$> (range ((0, 0), (w - 1, 0)))
    , catMaybes $ (x !?) <$> (range ((w - 1, 0), (w - 1, h - 1)))
    , catMaybes $ (x !?) <$> (range ((0, h - 1), (w - 1, h - 1)))
    , catMaybes $ (x !?) <$> (range ((0, 0), (0, h - 1)))
    )


fromList :: [[a]] -> Maybe (Vector2D a)
fromList l =
    do
        let lines = Vector.fromList <$> l
        width <- Vector.length <$> headMay lines
        if all (== width) (Vector.length <$> lines)
           then Just $ Vector2D width (length l) (Vector.concat lines)
           else Nothing

inRange :: (Int, Int) -> Vector2D a -> Bool
inRange (i, j) (Vector2D w h _) = 0 <= i && 0 <= j && i < w && j < h

assocs :: Vector2D a -> [((Int, Int), a)]
assocs vector@(Vector2D w h v) = zip (indexes vector) (Vector.toList v)

elems :: Vector2D a -> [a]
elems (Vector2D _ _ v) = Vector.toList v

indexes :: Vector2D a -> [(Int, Int)]
indexes (Vector2D w h _) = range ((0, 0), (w - 1, h - 1))

update :: Vector2D a -> [((Int, Int), a)] -> Vector2D a
update (Vector2D w h v) values =
    Vector2D w h
    (v Vector.// ((\((i , j), x) -> (w * j + i, x)) <$> values))
