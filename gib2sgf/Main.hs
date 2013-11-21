module Main where

import Codec.Baduk.Emitter.SGF
import Codec.Baduk.Parser.Tygem

gibo2sgf text = case (parseGibo text) of
                   Left  e -> error (show e)
                   Right g -> game2sgf g

main = interact gibo2sgf
