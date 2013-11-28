module Main where

import Codec.Baduk.Emitter.SGF
import Codec.Baduk.Parser.Tygem
import qualified Data.Text.IO as TIO
import System.IO

gibo2sgf text = case (parseGibo text) of
                   Left  e -> error (show e)
                   Right g -> game2sgf g

main = do hSetEncoding stdin latin1
          TIO.interact gibo2sgf
