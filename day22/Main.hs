module Main where

import Text.Read (readMaybe)
import Text.Parsec (Parsec)
import qualified Text.Parsec as P
import qualified Text.Parsec.Char as P

import Data.Sequence (Seq, Seq((:<|), (:|>), Empty))
import qualified Data.Sequence as Seq

import Data.Set (Set)
import qualified Data.Set as Set

-- Problem

type Card = Int
type Score = Int
type Deck = Seq Card
data Player = P1 | P2 deriving Show

score :: Deck -> Score
score = fst . foldr (\c (score, mult) -> (score + c * mult, mult + 1)) (0, 1)

regularCombat :: (Deck, Deck) -> (Player, Int)
regularCombat (Empty, d2) = (P2, score d2)
regularCombat (d1, Empty) = (P1, score d1)
regularCombat (d1, d2) = regularCombat (turn (d1, d2))
    where turn :: (Deck, Deck) -> (Deck, Deck)
          turn d@(Empty, _) = d
          turn d@(_, Empty) = d
          turn (c1 :<| d1, c2 :<| d2) = if c1 > c2 then (d1 :|> c1 :|> c2, d2)
                                                   else (d1, d2 :|> c2 :|> c1)

recCombat :: (Deck, Deck) -> Set (Deck, Deck) -> (Player, Int)
recCombat (Empty, d2) _ = (P2, score d2)
recCombat (d1, Empty) _ = (P1, score d1)
recCombat d@(c1 :<| d1, c2 :<| d2) previousDecks
  | Set.member d previousDecks = (P1, score (fst d))
  | otherwise =
      let winner =
              if (c1 <= Seq.length d1) && (c2 <= Seq.length d2)
                 then fst (recCombat (Seq.take c1 d1, Seq.take c2 d2) Set.empty)
                 else if c1 > c2 then P1 else P2
       in case winner of
            P1 -> recCombat (d1 :|> c1 :|> c2, d2) (Set.insert d previousDecks)
            P2 -> recCombat (d1, d2 :|> c2 :|> c1) (Set.insert d previousDecks)

-- Parsers

pNum :: Parsec String u Int
pNum = P.many1 P.digit
    >>= (maybe (P.unexpected "invalid nat") return) . readMaybe

pDeck :: Parsec String u Deck
pDeck = (Seq.fromList <$> P.many1 (pNum <* P.endOfLine))

pPlayer :: Parsec String u Deck 
pPlayer = P.string "Player " *> pNum *> P.char ':' *> P.endOfLine *> pDeck

pInput :: Parsec String u (Deck, Deck)
pInput = (,) <$> pPlayer <* P.endOfLine <*> pPlayer <* P.eof

-- Main

main :: IO ()
main =
    do
        eInput <- P.parse pInput "input" <$> getContents
        case eInput of
          Left parsingError -> putStrLn . show $ parsingError
          Right input -> do
              putStrLn . show $ regularCombat input
              putStrLn . show $ recCombat input Set.empty
