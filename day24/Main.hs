module Main where

import Text.Parsec (Parsec)
import qualified Text.Parsec as P
import qualified Text.Parsec.Char as P
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.List (foldl')

-- Problem (based on day 17)

data Direction = NW | NE | E | SE | SW | W deriving Show
type PosHex = (Int, Int)
type Space = HashMap PosHex (Int, Bool)

disp :: PosHex -> Direction -> PosHex
disp (x, y) NW = (x, y + 1)
disp (x, y) NE = (x + 1, y + 1)
disp (x, y) E  = (x + 1, y)
disp (x, y) SE = (x, y - 1)
disp (x, y) SW = (x - 1, y - 1)
disp (x, y) W  = (x - 1, y)

tileCoord :: [Direction] -> PosHex
tileCoord = foldl' disp (0, 0)

vicinity :: PosHex -> [PosHex]
vicinity (x, y) = disp (x, y) <$> [NW, NE, E, SE, SW, W]

tellAboutActivation :: PosHex -> Space -> Space
tellAboutActivation i s0 = foldr notify s0 (vicinity i)
    where notify :: PosHex -> Space -> Space
          notify j s =
              case HashMap.lookup j s of
                Nothing -> HashMap.insert j (1, False) s
                Just (n, b) -> HashMap.insert j (n + 1, b) s

tellAboutDeactivation :: PosHex -> Space -> Space
tellAboutDeactivation i s0 = foldr notify s0 (vicinity i)
    where notify :: PosHex -> Space -> Space
          notify j s =
              case HashMap.lookup j s of
                Nothing -> s
                Just (n, b)
                  | n > 1 || b -> HashMap.insert j (n - 1, b) s
                  | otherwise -> HashMap.delete j s

activate :: PosHex -> Space -> Space
activate i s =
    case HashMap.lookup i s of
      Nothing -> tellAboutActivation i $ HashMap.insert i (0, True) s
      Just (n, False) -> tellAboutActivation i $ HashMap.insert i (n, True) s
      Just (_, True) -> s

deactivate :: PosHex -> Space -> Space
deactivate i s =
    case HashMap.lookup i s of
      Just (n, True) -> tellAboutDeactivation i $ HashMap.insert i (n, False) s
      _ -> s

flipTile :: PosHex -> Space -> Space
flipTile i s =
    case HashMap.lookup i s of
      Nothing -> activate i s
      Just (_, True) -> deactivate i s
      Just (_, False) -> activate i s

setupSpace :: [[Direction]] -> Space
setupSpace =
    foldr
    (\dirs -> flipTile (tileCoord dirs))
    HashMap.empty


hasToSwitch :: (Int, Bool) -> Bool
hasToSwitch (n, b) = (b && (n == 0 || n > 2)) || (not b && n == 2)

step :: Space -> Space
step s = foldr set s operations
    where
        operations :: [(PosHex, Bool)]
        operations =
            (\(i, (_, b)) -> (i, not b)) <$>
                filter (hasToSwitch . snd) (HashMap.toList s)
        set :: (PosHex, Bool) -> Space -> Space
        set (i, True) = activate i
        set (i, False) = deactivate i

activePositions :: Space -> Int
activePositions = length . (fmap fst) . filter (snd . snd) . HashMap.toList

solve2 :: Space -> Int
solve2 = activePositions . (!! 100) . (iterate step)

-- Parsers

pDirection :: Parsec String u Direction
pDirection =
    P.choice [ P.try $ NW <$ P.string "nw"
             , P.try $ NE <$ P.string "ne"
             , P.try $ E <$ P.string "e"
             , P.try $ SE <$ P.string "se"
             , P.try $ SW <$ P.string "sw"
             , P.try $ W <$ P.string "w"
             ]

pTile :: Parsec String u [Direction] 
pTile = P.many1 pDirection <* P.endOfLine

pInput :: Parsec String u [[Direction]]
pInput = P.many1 pTile <* P.eof

-- Main

main :: IO ()
main =
    do
        eInput <- P.parse pInput "input" <$> getContents
        case eInput of
          Left parsingError -> putStrLn . show $ parsingError
          Right input ->
              let space = setupSpace input
               in putStrLn . show $ (activePositions space, solve2 space)
