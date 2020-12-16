module Main where

import Text.Read (readMaybe)
import Text.Parsec (Parsec)
import qualified Text.Parsec as P
import qualified Text.Parsec.Char as P
import Data.Ix (inRange)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (find, isPrefixOf)

-- Problem

type Ticket = [Int]
type Fieldname = String
type Rule = (Fieldname, Set (Int, Int))

fieldConformsTo :: Int -> Rule -> Bool
fieldConformsTo n (_, ranges) = any (`inRange` n) ranges

fieldConformsToRules :: Int -> Set Rule -> Bool
fieldConformsToRules field rules = any (fieldConformsTo field) rules

possibleFields :: Set Rule -> Int -> Set Fieldname
possibleFields rules value =
    (Set.map fst) . (Set.filter (fieldConformsTo value)) $ rules

nonConformingFields :: Set Rule -> Ticket -> [Int]
nonConformingFields rules =
    filter (\f -> not $ fieldConformsToRules f rules)

scanningErrorRate :: Set Rule -> [Ticket] -> Int
scanningErrorRate rules = sum . mconcat . (fmap $ nonConformingFields rules)

conformingTickets :: Set Rule -> [Ticket] -> [Ticket]
conformingTickets rules = filter (null . (nonConformingFields rules))

collapsePossibilities :: [[Set Fieldname]] -> [Set Fieldname]
collapsePossibilities [] = []
collapsePossibilities (x : xs) = foldr (zipWith Set.intersection) x xs

solvePossibilities :: Set Fieldname -> [Set Fieldname] -> Maybe [Fieldname]
solvePossibilities solved l
  | all ((== 1) . Set.size) l = sequence (Set.lookupMin <$> l)
  | otherwise =
      do
          singleton <-
              find (\s -> Set.size s == 1 && not (Set.isSubsetOf s solved)) l
          field <- Set.lookupMin singleton
          solvePossibilities (Set.insert field solved) (subtleDelete field <$> l)
      where
          subtleDelete :: Ord a => a -> Set a -> Set a
          subtleDelete x s = if Set.size s > 1 then Set.delete x s else s

solveFields :: Set Rule -> [Ticket] -> Maybe [Fieldname]
solveFields rules tickets =
    (solvePossibilities Set.empty) . collapsePossibilities $
        (fmap $ possibleFields rules) <$> (conformingTickets rules tickets)

solve :: Set Rule -> [Ticket] -> Ticket -> Maybe Int
solve rules tickets ticket =
    do
        fields <- solveFields rules tickets
        return $ product $ (fmap snd) $
                (filter ((isPrefixOf "departure") . fst))
                (zip fields ticket)

-- Parsers

pNat :: Parsec String u Int
pNat = P.many1 P.digit
    >>= (maybe (P.unexpected "invalid nat") return) . readMaybe

pTicket :: Parsec String u Ticket
pTicket =  P.sepBy1 pNat (P.char ',') <* P.endOfLine

pRule :: Parsec String u Rule
pRule =
    do
        name <- (:) <$> P.letter <*> P.many1 (P.letter P.<|> P.space)
        P.string ": "
        range1 <- (,) <$> pNat <* P.char '-' <*> pNat
        P.string " or "
        range2 <- (,) <$> pNat <* P.char '-' <*> pNat
        P.endOfLine
        return (name, Set.fromList [range1, range2])

pInput :: Parsec String u (Set Rule, Ticket, [Ticket])
pInput =
    do
        rules <- Set.fromList <$> P.many1 pRule
        P.endOfLine >> P.string "your ticket:" >> P.endOfLine
        myTicket <- pTicket
        P.endOfLine >> P.string "nearby tickets:" >> P.endOfLine
        nearbyTickets <- P.many1 pTicket
        P.eof
        return (rules, myTicket, nearbyTickets)

-- Main

main :: IO ()
main =
    do
        eInput <- P.parse pInput "input" <$> getContents
        case eInput of
          Left parsingError -> putStrLn . show $ parsingError
          Right (rules, myTicket, nearbyTickets) ->
              putStrLn . show $ ( scanningErrorRate rules nearbyTickets
                                , solve rules nearbyTickets myTicket )
