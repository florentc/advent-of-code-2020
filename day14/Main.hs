module Main where

import Text.Read (readMaybe)
import Text.Parsec (Parsec)
import qualified Text.Parsec as P
import qualified Text.Parsec.Char as P
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq, Seq((:<|), (:|>)))
import qualified Data.Sequence as Seq
import Data.List (foldl')

-- Problem

type Bit = Int
type BinInteger = Seq Bit
type Mask = Seq (Maybe Bit)
type BinTemplate = Seq (Maybe Bit)
type Address = Integer
type Value = Integer
data Instruction = Mask Mask | Write Address Value
type Program = [Instruction]
type Memory = Map Address Value

toBin :: Integer -> BinInteger
toBin n =
    let (q, r) = divMod n 2 in
        (if q == 0 then Seq.Empty else (toBin q)) :|> fromInteger r

fromBin :: BinInteger -> Integer
fromBin Seq.Empty = 0
fromBin (bs :|> b) = (toInteger b) + 2 * (fromBin bs)

fromBinTemplate :: BinTemplate -> [Integer]
fromBinTemplate Seq.Empty = [0]
fromBinTemplate (bs :|> Just b) =
    (\x -> (toInteger b) + 2 * x) <$> (fromBinTemplate bs)
fromBinTemplate (bs :|> Nothing) =
    let xs = fromBinTemplate bs in ((* 2) <$> xs) <> (((+ 1) . (* 2)) <$> xs)

applyMask :: Mask -> BinInteger -> BinInteger
applyMask Seq.Empty bs = bs
applyMask (ms :|> Nothing) (bs :|> b) = (applyMask ms bs) :|> b
applyMask (ms :|> Just b) (bs :|> _) = (applyMask ms bs) :|> b
applyMask ms Seq.Empty = applyMask ms (Seq.singleton 0)

applyMask2 :: Mask -> BinInteger -> BinTemplate
applyMask2 Seq.Empty bs = Just <$> bs
applyMask2 (ms :|> Just 1) (bs :|> _) = (applyMask2 ms bs) :|> Just 1
applyMask2 (ms :|> Just _) (bs :|> b) = (applyMask2 ms bs) :|> Just b
applyMask2 (ms :|> Nothing) (bs :|> _) = (applyMask2 ms bs) :|> Nothing
applyMask2 ms Seq.Empty = applyMask2 ms (Seq.singleton 0)

data State = State
    { _sMemory :: Memory
    , _sMask :: Mask }

rule1 :: State -> Instruction -> State
rule1 (State memory _) (Mask mask) = State memory mask
rule1 (State memory mask) (Write addr value) =
    let maskedValue = (fromBin . (applyMask mask) . toBin $ value)
     in State (Map.insert addr maskedValue memory) mask

rule2 :: State -> Instruction -> State
rule2 (State memory _) (Mask mask) = State memory mask
rule2 (State memory mask) (Write addr value) =
    let addresses = fromBinTemplate . (applyMask2 mask) . toBin $ addr
    in State (foldr (\a -> Map.insert a value) memory addresses) mask

initialState :: State
initialState = State Map.empty Seq.Empty

solve :: (State -> Instruction -> State) -> Program -> Integer
solve rule = sum . Map.elems . _sMemory . foldl' rule initialState

-- Parsers

pNat :: Parsec String u Integer
pNat = P.many1 P.digit <* P.spaces
    >>= (maybe (P.unexpected "invalid nat") return) . readMaybe

pMask :: Parsec String u Instruction
pMask =
    P.string "mask"
    *> P.spaces *> P.char '=' *> P.spaces *>
        ((Mask . Seq.fromList) <$> P.many (P.choice
            [ Just 1 <$ P.char '1'
            , Just 0 <$ P.char '0'
            , Nothing <$ P.char 'X']))
    <* P.spaces

pWrite :: Parsec String u Instruction
pWrite =
    Write <$>
        (P.string "mem" *> P.between (P.char '[') (P.char ']') pNat)
        <*> (P.spaces *> P.char '=' *> P.spaces *> pNat)

pInput :: Parsec String u Program
pInput = P.many1 ((P.try $ pMask) P.<|> pWrite) <* P.eof

-- Main

main :: IO ()
main =
    do
        eInput <- P.parse pInput "input" <$> getContents
        case eInput of
          Left parsingError -> putStrLn . show $ parsingError
          Right input -> putStrLn . show $
              (solve rule1 input, solve rule2 input)
