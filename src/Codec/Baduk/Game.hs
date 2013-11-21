module Codec.Baduk.Game where

import Data.Time.Clock
import Data.Time.Format

data RankType = Dan | Kyu | Pro 
              deriving (Show)

data Rank = Rank Int RankType deriving (Show)

data Player = Player String Rank deriving (Show)

data Color  = Black | White deriving (Show)
data Margin = Time
            | Resignation
            | Points Double
            deriving (Show)

data Result = Unfinished
            | Draw
            | Win Color Margin
            deriving (Show)

data Move = Stone Color Int Int
          | Pass Color
          deriving (Show)

data Game = Game { size        :: Int
                 , black       :: Player
                 , white       :: Player
                 , handicap    :: Int
                 , komi        :: Double
                 , time        :: UTCTime
                 , moves       :: [Move]
                 , result      :: Result
                 } deriving (Show)

