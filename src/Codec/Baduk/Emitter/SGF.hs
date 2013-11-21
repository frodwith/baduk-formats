module Codec.Baduk.Emitter.SGF (game2sgf) where

import Codec.Baduk.Game
import Data.Time.Format
import Data.Time.Clock
import System.Locale

game2sgf g = let (Player bn br) = (black g)
                 (Player wn wr) = (white g)
             in "(;" ++ p "FF" "4"
                     ++ p "GM" "1"
                     ++ p "SZ" (show (size g))
                     ++ p "PB" bn
                     ++ p "BR" (pr br)
                     ++ p "PW" wn
                     ++ p "WR" (pr wr)
                     ++ p "HA" (show (handicap g))
                     ++ p "KM" (show (komi g))
                     ++ p "DT" (pt (time g))
                     ++ p "RE" (pres (result g))
                     ++ concat (map pm (moves g))
                     ++ ")"
    where p name val = name ++ "[" ++ val ++ "]"
          pr (Rank n c) = (show n) ++ case c of
                                          Kyu -> "k"
                                          Dan -> "d"
                                          Pro -> "p"

          pt = formatTime defaultTimeLocale "%F"

          pcol Black = "B"
          pcol White = "W"

          pres Draw = "0"
          pres (Win c r) = (pcol c) ++ "+" ++ pmar r
              where pmar Time        = "Time"
                    pmar Resignation = "Resign"
                    pmar (Points n)  = show n

          pm (Pass  bw)     = ";" ++ (pcol bw) ++ "[]"
          pm (Stone bw c r) = ";" ++ (p (pcol bw) ((co c):(co r):[]))
             where co n    = letters !! (n-1)
                   letters = ['a'..'z']
