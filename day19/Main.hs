module Main where

import Text.Read (readMaybe)
import Text.Parsec (Parsec)
import qualified Text.Parsec as P
import qualified Text.Parsec.Char as P
import Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as RP
import Data.Maybe (catMaybes)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

-- Problem

type Message = String

data Entry = EChar Char
           | ESeq [Int]
           | EAlt [Int] [Int]
           deriving Show

type Rulebook = IntMap Entry

data RuleTree = TChar Char
              | TSeq [RuleTree]
              | TAlt RuleTree RuleTree

expandFromRulebook :: Rulebook -> Int -> Maybe RuleTree
expandFromRulebook rb n = IntMap.lookup n rb >>= return . aux
    where
        aux (EChar c) = TChar c
        aux (ESeq ns) = let rs = (catMaybes $ (\n -> IntMap.lookup n rb) <$> ns)
                         in TSeq (aux <$> rs)
        aux (EAlt n1s n2s) = TAlt (aux (ESeq n1s)) (aux (ESeq n2s))

ruleParser :: RuleTree -> ReadP ()
ruleParser (TChar c) = () <$ RP.char c
ruleParser (TSeq rs) = () <$ (sequence (ruleParser <$> rs))
ruleParser (TAlt r1 r2) = () <$ ((ruleParser r1) RP.+++ (ruleParser r2))

isValid :: RuleTree -> String -> Bool
isValid rule = not . null . (filter (null . snd))
             . (RP.readP_to_S (ruleParser rule))

solve :: Rulebook -> [Message] -> Maybe Int
solve rulebook messages =
    do
        rule0 <- expandFromRulebook rulebook 0
        return $ length . (filter id) $ (isValid rule0) <$> messages

-- Parsers

pSpaces :: Parsec String u String
pSpaces = P.many (P.char ' ')

pNum :: Parsec String u Int
pNum = P.many1 P.digit <* pSpaces
    >>= (maybe (P.unexpected "invalid nat") return) . readMaybe

pRule :: Parsec String u (Int, Entry)
pRule = (,) <$> pNum <* P.char ':' <* pSpaces
            <*> P.choice
                [ EChar <$> (P.char '"' *> P.letter <* P.char '"') <* pSpaces
                , P.try $ EAlt <$> pSeq <* P.char '|' <* pSpaces <*> pSeq
                , ESeq <$> pSeq ]
            <* P.endOfLine
        where pSeq = P.many1 pNum

pInput :: Parsec String u (Rulebook, [String])
pInput = (,) <$> (IntMap.fromList <$> P.many1 pRule) <* P.endOfLine
             <*> P.many1 (P.many1 P.letter <* P.endOfLine) <* P.eof

-- Main

main :: IO ()
main =
    do
        eInput <- P.parse pInput "input" <$> getContents
        case eInput of
          Left parsingError -> putStrLn . show $ parsingError
          Right (rules, messages) -> putStrLn . show $ solve rules messages
