module Main where

import Text.Read (readMaybe)
import Text.Parsec (Parsec)
import qualified Text.Parsec as P
import qualified Text.Parsec.Char as P
import Data.List (foldl')

-- Problem

data Op = Add | Mult deriving Show
data Term = Number Int | Paren Expr deriving Show
data Expr = Expr Term [(Op, Term)] deriving Show
data Priority = LeftAssoc | PrioAdd

evalOp :: Op -> (Int -> Int -> Int)
evalOp Add = (+)
evalOp Mult = (*)

evalTerm :: Priority -> Term -> Int
evalTerm _ (Number n) = n
evalTerm p (Paren expr) = evalExpr p expr

evalExpr :: Priority -> Expr -> Int
evalExpr p (Expr term []) = evalTerm p term
evalExpr p (Expr term opTerms) =
    foldl'
    (\n (op, term) -> (evalOp op) n (evalTerm p term))
    (evalTerm p term)
    (case p of PrioAdd   -> evalAdd opTerms
               LeftAssoc -> opTerms)

evalAdd :: [(Op, Term)] -> [(Op, Term)]
evalAdd [] = []
evalAdd [x] = [x]
evalAdd ((op, t1) : (Add, t2) : opTerms) =
    let t = Number $ (evalTerm PrioAdd t1) + (evalTerm PrioAdd t2)
     in evalAdd ((op, t) : opTerms)
evalAdd ((op, t1) : (Mult, t2) : opTerms) =
    (op, t1) : (evalAdd ((Mult, t2) : opTerms))

solve :: Priority -> [Expr] -> Int
solve p = sum . (fmap (evalExpr p))

-- Parsers

pSpaces :: Parsec String u String
pSpaces = P.many P.space

pNum :: Parsec String u Term
pNum = P.many1 P.digit <* pSpaces
    >>= (maybe (P.unexpected "invalid nat") (return . Number)) . readMaybe

pParen :: Parsec String u Term
pParen = Paren <$> (P.char '(' *> pSpaces *> pExpr <* P.char ')' <* pSpaces)

pTerm :: Parsec String u Term
pTerm = pNum P.<|> pParen

pOp :: Parsec String u Op
pOp = (Add <$ P.char '+' P.<|> Mult <$ P.char '*') <* pSpaces

pExpr :: Parsec String u Expr
pExpr = Expr <$> pTerm <*> P.many ((,) <$> pOp <*> pTerm)

pInput :: Parsec String u [Expr]
pInput = P.many1 pExpr <* P.eof

-- Main

main :: IO ()
main =
    do
        eInput <- P.parse pInput "input" <$> getContents
        case eInput of
          Left parsingError -> putStrLn . show $ parsingError
          Right input -> putStrLn . show $
              (solve LeftAssoc input, solve PrioAdd input)
