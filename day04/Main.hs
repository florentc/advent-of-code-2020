module Main where

import Control.Applicative (liftA2)
import Text.Read (readMaybe)
import Text.Regex.TDFA ((=~))
import qualified Text.ParserCombinators.ReadP as P
import Data.Char (isDigit, isAlphaNum)
import Data.Maybe (listToMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

-- Problem

data Entry = BirthYear
           | IssueYear
           | ExpirationYear
           | Height
           | HairColor
           | EyeColor
           | PassportId
           | CountryId
           deriving (Eq, Ord)

type Passport = Set (Entry, String)

requiredEntries :: Set Entry
requiredEntries = Set.fromList [BirthYear, IssueYear, ExpirationYear, Height, HairColor, EyeColor, PassportId]

hasRequiredFields :: Passport -> Bool
hasRequiredFields p = Set.isSubsetOf requiredEntries (Set.map fst p)

readBetween :: (Ord a, Read a) => a -> a -> String -> Bool
readBetween min max = (all (liftA2 (&&) (>= min) (<= max))) . readMaybe

isYearValid :: Int -> Int -> String -> Bool
isYearValid min max s = s =~ "^[0-9]{4}$" && readBetween min max s

isHeightValid :: String -> Bool
isHeightValid value =
    case span isDigit value of
      (height, "cm") -> readBetween 150 193 height
      (height, "in") -> readBetween 59 76 height
      _ -> False

isValueValid :: Entry -> String -> Bool
isValueValid BirthYear      = isYearValid 1920 2002
isValueValid IssueYear      = isYearValid 2010 2020
isValueValid ExpirationYear = isYearValid 2020 2030
isValueValid Height         = isHeightValid
isValueValid HairColor      = (=~ "^#[0-9a-f]{6}$")
isValueValid EyeColor       = (flip elem) ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
isValueValid PassportId     = (=~ "^[0-9]{9}$")
isValueValid CountryId      = const True

isPassportValid :: Passport -> Bool
isPassportValid p = hasRequiredFields p && (and . (fmap (uncurry isValueValid)) . Set.toList) p

-- Parsers

word :: P.ReadP String
word = (P.munch1 (\c -> isAlphaNum c || c == '#'))

field :: String -> Entry -> P.ReadP (Entry, String)
field str entry = P.string str >> word >>= (\value -> return (entry, value))

entry :: P.ReadP (Entry, String)
entry = P.choice
    [ field "byr:" BirthYear
    , field "iyr:" IssueYear
    , field "eyr:" ExpirationYear
    , field "hgt:" Height
    , field "hcl:" HairColor
    , field "ecl:" EyeColor
    , field "pid:" PassportId
    , field "cid:" CountryId ]

passport :: P.ReadP Passport
passport = Set.fromList <$> P.sepBy1 entry (P.char ' ' P.+++ P.char '\n')

passports :: P.ReadP [Passport]
passports = (P.sepBy1 passport (P.char '\n' >> P.char '\n')) <* P.skipSpaces <* P.eof

parseInput :: String -> Maybe [Passport]
parseInput = (fmap fst) . listToMaybe . (P.readP_to_S passports)

-- Main

main :: IO ()
main =
    do
        input <- parseInput <$> getContents
        putStrLn . show $ length . (filter id) . (fmap hasRequiredFields) <$> input
        putStrLn . show $ length . (filter id) . (fmap isPassportValid) <$> input
