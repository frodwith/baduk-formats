module Main where

import Codec.Baduk.Emitter.SGF
import Codec.Baduk.Parser.WBaduk
import qualified Data.Text.IO as TIO

ngf2sgf text = case (parseNGF text) of
                   Left  e -> error (show e)
                   Right g -> game2sgf g

main = TIO.interact ngf2sgf
