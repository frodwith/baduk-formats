module Main where

import Codec.Baduk.Emitter.SGF
import Codec.Baduk.Parser.Tygem
import qualified Data.Text.IO as TIO

gibo2sgf text = case (parseGibo text) of
                   Left  e -> error (show e)
                   Right g -> game2sgf g

main = TIO.interact gibo2sgf
