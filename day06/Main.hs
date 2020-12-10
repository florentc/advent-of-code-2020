module Main where

import Data.Set (Set)
import qualified Data.Set as Set
import Text.Parsec (Parsec)
import qualified Text.Parsec as P
import qualified Text.Parsec.Char as P

type Answer = Char
type PersonAnswers = Set Answer
type GroupAnswers = [PersonAnswers]

pInput :: Parsec String u [GroupAnswers]
pInput = P.sepBy1 pGroupAnswers P.endOfLine <* P.eof
    where pGroupAnswers  = P.many1 pPersonAnswers
          pPersonAnswers = Set.fromList <$> P.many1 pAnswer <* P.endOfLine
          pAnswer        = P.oneOf ['a'..'z']

nbYesAny :: [GroupAnswers] -> Int
nbYesAny = sum . (fmap (Set.size . Set.unions))

nbYesAll :: [GroupAnswers] -> Int
nbYesAll = sum . (fmap (Set.size . (foldl Set.intersection (Set.fromList ['a'..'z']))))

main :: IO ()
main =
    do
        eInput <- P.parse pInput "input" <$> getContents
        case eInput of
          Left e -> putStrLn . show $ e
          Right input -> putStrLn . show $ (nbYesAny input, nbYesAll input)
