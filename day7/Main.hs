{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Text.Parsec (Parsec)
import qualified Text.Parsec as P
import qualified Text.Parsec.Char as P
import Text.Read (readMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Monoid (Sum(..), getSum)

-- Graphs

type LabeledGraph node label = Map node (Set (label, node))
type WeightedGraph node = LabeledGraph node Int
type Graph node = LabeledGraph node ()

succEdges :: (Ord node) => node -> LabeledGraph node label -> Set (label, node)
succEdges node = (fromMaybe Set.empty) . (Map.lookup node)

reverseEdges :: forall l n. (Ord n) => LabeledGraph n l -> Graph n
reverseEdges = foldl insertReversedEdge Map.empty . Map.toList
    where
        insertReversedEdge :: Graph n -> (n, Set (l, n)) -> Graph n
        insertReversedEdge graph (node, succNodes) =
            foldr (aux node) graph (Set.map snd succNodes)
        aux :: n -> n -> Graph n -> Graph n
        aux to from = Map.insertWith Set.union from (Set.singleton ((), to))

foldSucc :: forall node label res. (Ord node, Monoid res) =>
    node -> ((label, node) -> res -> res) -> LabeledGraph node label -> res
foldSucc root f graph = visitSucc root (Set.empty)
    where
        visitSucc :: node -> Set node -> res
        visitSucc node visited =
            let toVisit = Set.toList $ succEdges node graph
             in mconcat $ (visitNode graph visited) <$> toVisit
        visitNode :: LabeledGraph node label -> Set node -> (label, node) -> res
        visitNode graph visited edge@(_, node)
          | Set.member node visited = mempty
          | otherwise = (f edge) (visitSucc node (Set.insert node visited))

-- Problem

type Color = (String, String)
type Requirement = (Int, Color)
type Rule = (Color, Set Requirement)

nbThatContain :: Color -> WeightedGraph Color -> Int
nbThatContain c = Set.size . (foldSucc c (\(_, n) -> Set.insert n)) . reverseEdges

nbRequiredInside :: Color -> WeightedGraph Color -> Int
nbRequiredInside c = getSum . (foldSucc c (\(w, _) (Sum x) -> Sum $ w * (1 + x)))

-- Parsers

pColor :: Parsec String u Color
pColor = (,) <$> P.many1 P.lower <* P.space <*> P.many1 P.lower <* P.spaces

pNat :: Parsec String u Int
pNat =
    P.many1 P.digit <* P.spaces
    >>= (maybe (P.unexpected "invalid nat") return) . readMaybe

pRequirement :: Parsec String u Requirement
pRequirement = (,) <$>
    pNat <*> pColor <* P.string "bag" <* P.optional (P.char 's') <* P.spaces

pRequirements :: Parsec String u (Set Requirement)
pRequirements =
    Set.fromList <$> (P.sepBy1 pRequirement (P.char ',' <* P.spaces))
    P.<|> Set.empty <$ P.string "no other bags"

pRule :: Parsec String u Rule
pRule = (,) <$>
    pColor <* P.string "bags" <* P.spaces <* P.string "contain" <* P.spaces
    <*> pRequirements <* P.char '.' <* P.endOfLine

pInput :: Parsec String u (WeightedGraph Color)
pInput = Map.fromList <$> P.many pRule <* P.eof

-- Main

shinyGold :: Color
shinyGold = ("shiny", "gold")

main :: IO ()
main =
    do
        eInput <- P.parse pInput "input" <$> getContents
        case eInput of
          Left parsingError -> putStrLn . show $ parsingError
          Right input ->
              do
                  putStrLn . show $ nbThatContain shinyGold input
                  putStrLn . show $ nbRequiredInside shinyGold input
