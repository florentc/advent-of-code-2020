module Main where

import Text.Parsec (Parsec)
import qualified Text.Parsec as P
import qualified Text.Parsec.Char as P
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (intercalate)

-- Problem

type Ingredient = String
type Allergen = String
type Food = (Set Ingredient, Set Allergen)

allIngredients :: [Food] -> Set Ingredient
allIngredients = Set.unions . (fmap fst)

allAllergens :: [Food] -> Set Allergen
allAllergens = Set.unions . (fmap snd)

intersections :: Ord a => [Set a] -> Set a
intersections [] = Set.empty
intersections (s : ss) = foldr Set.intersection s ss

allergenMap :: [Food] -> Maybe (Map Allergen Ingredient)
allergenMap food = aux food (Set.toList (allAllergens food)) Map.empty
    where
        aux :: [Food] -> [Allergen] -> Map Allergen Ingredient
            -> Maybe (Map Allergen Ingredient)
        aux _ [] m = Just m
        aux foods (allergen : allergens) m =
            let candidates :: Set Ingredient
                candidates =
                    intersections $ fst <$>
                        (filter (\(_, as) -> Set.member allergen as) foods)
                tryCandidates :: [Allergen] -> Maybe (Map Allergen Ingredient)
                tryCandidates [] = Nothing
                tryCandidates (c : cs) =
                    maybe (tryCandidates cs) return $
                        aux ((\(is, as) -> (Set.delete c is, as)) <$> foods)
                            allergens (Map.insert allergen c m)
             in tryCandidates (Set.toList candidates)

solve1 :: [Food] -> Maybe Int
solve1 food =
    do
        xs <- Set.fromList . Map.elems <$> allergenMap food
        return $
            foldr (\(is, _) n -> n + (Set.size $ Set.difference is xs)) 0 food

solve2 :: [Food] -> Maybe String
solve2 = fmap (intercalate "," . Map.elems) . allergenMap

-- Parsers

pWord :: Parsec String u String
pWord = P.many1 P.letter <* P.spaces

pIngredients :: Parsec String u (Set Ingredient)
pIngredients = Set.fromList <$> P.many1 pWord

pAllergens :: Parsec String u (Set Allergen)
pAllergens = Set.fromList <$> (P.string "(contains "
                           *> P.sepBy1 pWord (P.char ','
                           <* P.spaces) <* P.char ')')

pFood :: Parsec String u Food
pFood = (,) <$> pIngredients <*> pAllergens <* P.endOfLine

pInput :: Parsec String u [Food]
pInput = P.many1 pFood <* P.eof

-- Main

main :: IO ()
main =
    do
        eInput <- P.parse pInput "input" <$> getContents
        case eInput of
          Left parsingError -> putStrLn . show $ parsingError
          Right input -> putStrLn . show $ (solve1 input, solve2 input)
