{-# LANGUAGE TemplateHaskell #-}
module Main where

import Text.Read (readMaybe)
import Text.Parsec (Parsec, (<|>))
import qualified Text.Parsec as P
import qualified Text.Parsec.Char as P
import Data.Maybe (catMaybes)
import Lens.Micro.Platform (makeLenses, (&), (^.), (%~), _1, _2)

-- Problem

data Orientation = North | East | South | West deriving (Show, Enum)
data Direction = Port | Starboard deriving Show
type Distance = Int
type Angle = Int
data Action = Go Orientation Distance
            | Turn Direction Angle
            | Forward Distance 
            deriving Show

data State = State
    { _orientation :: Orientation
    , _waypoint :: (Distance, Distance)
    , _deviation :: (Distance, Distance) } deriving Show

makeLenses ''State

initialState :: State
initialState = State East (10, 1) (0, 0)

forward :: Distance -> State -> State
forward d s = s & move (s ^. orientation) d

forwardWaypoint :: Int -> State -> State
forwardWaypoint n s =
    let (wx, wy) = s ^. waypoint
     in s & (deviation . _1 %~ (+ (n * wx))) . (deviation . _2 %~ (+ (n * wy)))

move :: Orientation -> Distance -> State -> State
move North d = deviation . _2 %~ (+ d)
move South d = deviation . _2 %~ (subtract d)
move West d = deviation . _1 %~ (subtract d)
move East d = deviation . _1 %~ (+ d)

moveWaypoint :: Orientation -> Distance -> State -> State
moveWaypoint North d = waypoint . _2 %~ (+ d)
moveWaypoint South d = waypoint . _2 %~ (subtract d)
moveWaypoint West d = waypoint . _1 %~ (subtract d)
moveWaypoint East d = waypoint . _1 %~ (+ d)

turn :: Angle -> State -> State
turn angle = orientation %~ toEnum . (`mod` 4). (+ (div angle 90)) . fromEnum

turnWaypoint :: Angle -> State -> State
turnWaypoint angle =
    case mod (div angle 90) 4 of
      1 -> waypoint %~ (\(x, y) -> (y , -x))
      2 -> waypoint %~ (\(x, y) -> (-x , -y))
      3 -> waypoint %~ (\(x, y) -> (-y , x))
      _ -> id

stepBasic :: Action -> State -> State
stepBasic (Go o d) = (move o d)
stepBasic (Turn Port a) = turn (-a)
stepBasic (Turn Starboard a) = (turn a)
stepBasic (Forward d) = forward d

stepWaypoint :: Action -> State -> State
stepWaypoint (Go o d) = (moveWaypoint o d)
stepWaypoint (Turn Port a) = turnWaypoint (-a)
stepWaypoint (Turn Starboard a) = (turnWaypoint a)
stepWaypoint (Forward n) = forwardWaypoint n

finalState :: (Action -> State -> State) -> State -> [Action] -> State
finalState _ s [] = s
finalState step s (a : actions) = finalState step (step a s) actions

distance :: State -> Distance
distance = (\(x, y) -> (abs x) + (abs y)) . (^. deviation)

-- Parsers

pNat :: Parsec String u Int
pNat =
    P.many1 P.digit <* P.spaces
    >>= (maybe (P.unexpected "invalid nat") return) . readMaybe

pOrientation :: Parsec String u Orientation
pOrientation = P.choice
    [ North <$ P.char 'N'
    , East <$ P.char 'E'
    , South <$ P.char 'S'
    , West <$ P.char 'W' ]

pAction :: Parsec String u Action
pAction = P.choice
    [ Go <$> pOrientation <*> pNat
    , Turn <$> (Port <$ P.char 'L' <|> Starboard <$ P.char 'R') <*> pNat
    , P.char 'F' *> (Forward <$> pNat) ]

pInput :: Parsec String u [Action]
pInput = P.many1 pAction <* P.eof

-- Main

main :: IO ()
main =
    do
        eInput <- P.parse pInput "input" <$> getContents
        case eInput of
          Left parsingError -> putStrLn . show $ parsingError
          Right input ->
              putStrLn . show $
                  ( distance . (finalState stepBasic initialState) $ input
                  , distance . (finalState stepWaypoint initialState) $ input )
