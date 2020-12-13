module Main where

import Text.Read (readMaybe)
import Text.Parsec (Parsec)
import qualified Text.Parsec as P
import qualified Text.Parsec.Char as P
import Data.Maybe (catMaybes)
import Data.List (sortOn)
import Safe (headMay)

-- Problem

type Timestamp = Integer
type Offset = Integer
type Period = Integer
type BusId = Integer
type Timetable = [Maybe Timestamp]

firstBusFrom :: Timestamp -> BusId -> Timestamp
firstBusFrom t b = b * divCeiling t b
    where
        divCeiling a b =
            uncurry (+) $ (toInteger . fromEnum . (> 0)) <$> quotRem a b

solve1 :: Timestamp -> Timetable -> Maybe Integer
solve1 t =
      (>>= \(busId, tDeparture) -> return (busId * (tDeparture - t)))
    . headMay . sortOn snd . (fmap (\x -> (x, x))) . catMaybes

solve2 :: Timetable -> (Timestamp, Period)
solve2 tt =
    let l = catMaybes . (zipWith (\i -> (>>= (\x -> return (x, i)))) [0..]) $ tt
     in foldr (\x (t0, period) -> firstSync x (period, (period - t0))) (1, 1) l

firstSync :: (BusId, Offset) -> (BusId, Offset) -> (Timestamp, Period)
firstSync (a, φa) (b, φb) =
    let (pgcd, k) = euclidWithCoeff a b
        ppcm = lcm a b
     in ((mod (a * (div (- (φb - φa) * k) pgcd)) ppcm) - φa, ppcm)

euclidWithCoeff :: Integer -> Integer -> (Integer, Integer)
euclidWithCoeff = aux 1 0
    where aux :: Integer -> Integer -> Integer -> Integer -> (Integer, Integer)
          aux k k' a b
            | b == 0 = (a, k)
            | otherwise = let (q, r) = quotRem a b in aux k' (k - q * k') b r

-- Parsers

pNat :: Parsec String u Integer
pNat = P.many1 P.digit <* P.spaces
    >>= (maybe (P.unexpected "invalid nat") return) . readMaybe

pInput :: Parsec String u (Timestamp, Timetable)
pInput = (,) <$>
    pNat <*>
    P.sepBy ((Just <$> pNat) P.<|> (Nothing <$ P.char 'x')) (P.char ',')

-- Main

main :: IO ()
main =
    do
        eInput <- P.parse pInput "input" <$> getContents
        case eInput of
          Left parsingError -> putStrLn . show $ parsingError
          Right (tFirstAvailable, timetable) -> putStrLn . show $
                  (solve1 tFirstAvailable timetable, solve2 timetable)
