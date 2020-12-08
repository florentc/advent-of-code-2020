{-# LANGUAGE TemplateHaskell #-}
module Main where

import Text.Parsec (Parsec)
import qualified Text.Parsec as P
import qualified Text.Parsec.Char as P
import Text.Read (readMaybe)
import Data.Vector (Vector, (!?))
import qualified Data.Vector as Vector
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Either (lefts, rights)
import Lens.Micro.Platform (makeLenses, (%~), (^.), (.~), (&))

-- Instructions

data Operation = Acc | Jmp | Nop deriving (Show)
type Instruction = (Operation, Int)

alt :: Instruction -> [Instruction]
alt (Acc, _) = []
alt (Jmp, n) = [(Nop, n)]
alt (Nop, n) = [(Jmp, n)]

-- Program execution

type Program = Vector Instruction
data Context = Context
    { _sVisited :: Set Int
    , _sAcc :: Int
    , _sPc :: Int
    , _sAlterable :: Bool
    , _sProgram :: Program }
makeLenses ''Context

apply :: Instruction -> Context -> Context
apply (Nop, _) = sPc %~ (+1)
apply (Acc, n) = (sPc %~ (+1)) . (sAcc %~ (+n))
apply (Jmp, n) = sPc %~ (+n)

data Ending = Error | Termination Int | Loop Int Int deriving Show

isTermination :: Ending -> Bool
isTermination (Termination _) = True
isTermination _ = False

doInstr :: Context -> Int -> Instruction ->  Either Ending Context
doInstr ctx pc instruction =
    aux . (sVisited %~ (Set.insert pc)) . (apply instruction) $ ctx
        where
            aux :: Context -> Either Ending Context
            aux s@(Context visited acc pc _ program)
              | Set.member pc visited       = Left $ Loop pc acc
              | pc == Vector.length program = Left $ Termination acc
              | otherwise                   = Right s

doStep :: Context -> [Either Ending Context]
doStep ctx@(Context _ _ pc alterable program) = 
    case program !? pc of
        Nothing -> [Left Error]
        Just instr -> doInstr ctx pc instr : altPaths
            where altPaths =
                    if alterable
                       then doInstr (ctx & sAlterable .~ False) pc <$> (alt instr)
                       else []

run :: Bool -> Program -> [Ending]
run alterable = (aux []) . (:[]) .  Right . (Context Set.empty 0 0 alterable)
    where
        aux :: [Ending] -> [Either Ending Context] -> [Ending]
        aux endings [] = endings
        aux endings eContexts =
            let (es, ctxs) = (endings <> lefts eContexts, rights eContexts)
             in aux es (mconcat $ doStep <$> ctxs)

-- Parsers

pOperation :: Parsec String u Operation
pOperation = Acc <$ P.string "acc"
       P.<|> Jmp <$ P.string "jmp"
       P.<|> Nop <$ P.string "nop"

pInt :: Parsec String u Int
pInt = (negate <$ (P.char '-') P.<|> id <$ (P.char '+'))
    <*> ((P.many1 P.digit) >>= (maybe (P.unexpected "nat") return) . readMaybe)

pInstruction :: Parsec String u Instruction
pInstruction = (,) <$> pOperation <* P.space <*> pInt <* P.endOfLine

pInput :: Parsec String u Program
pInput = Vector.fromList <$> P.many1 pInstruction <* P.eof

-- Main

main :: IO ()
main =
    do
        eInput <- P.parse pInput "input" <$> getContents
        case eInput of
          Left parsingError -> putStrLn . show $ parsingError
          Right input ->
              do
                  putStrLn . show $ run False input
                  putStrLn . show $ filter isTermination (run True input)
