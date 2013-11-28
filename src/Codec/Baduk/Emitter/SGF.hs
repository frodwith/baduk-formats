{-# LANGUAGE OverloadedStrings #-}
module Codec.Baduk.Emitter.SGF (game2sgf) where

import Codec.Baduk.Game
import Data.Time.Format
import Data.Time.Clock
import System.Locale
import qualified Data.Text as T

game2sgf :: Game -> T.Text
game2sgf g = let (Player bn br) = (black g)
                 (Player wn wr) = (white g)
             in T.concat [ "(;"
                         , tag "FF" "4"
                         , tag "GM" "1"
                         , tag "SZ" (showT (size g))
                         , tag "PB" bn
                         , tag "BR" (printRank br)
                         , tag "PW" wn
                         , tag "WR" (printRank wr)
                         , tag "HA" (showT (handicap g))
                         , tag "KM" (showT (komi g))
                         , tag "DT" (printTime (time g))
                         , tag "RE" (printResult (result g))
                         , T.concat (map printMove (moves g))
                         , ")"
                         ]

tag :: T.Text -> T.Text -> T.Text
tag name val = T.concat [name, "[", val, "]"]

printRank :: Rank -> T.Text
printRank (Rank n c) = T.concat [showT n, kind c]
    where kind Kyu = "k"
          kind Dan = "d"
          kind Pro = "p"

showT :: (Show a) => a -> T.Text
showT = T.pack . show

printTime :: UTCTime -> T.Text
printTime = T.pack . formatTime defaultTimeLocale "%F"

printResult :: Result -> T.Text
printResult Draw = "0"
printResult (Win c r) = T.concat [ printColor c, "+", pmar r]
    where pmar Time        = "Time"
          pmar Resignation = "Resign"
          pmar (Points n)  = showT n

printColor :: Color -> T.Text
printColor Black = "B"
printColor White = "W"

printMove :: Move -> T.Text
printMove (Pass  bw)     = T.concat [ ";", printColor bw, "[]" ]
printMove (Stone bw c r) = T.concat [ ";" , tag (printColor bw) coord ]
    where coord   = T.concat [co c, co r]
          co n    = T.singleton $ letters !! (n-1)
          letters = ['a'..'z']
