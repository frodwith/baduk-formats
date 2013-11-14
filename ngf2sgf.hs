import SGF
import NGF

ngf2sgf text = case (parseNGF text) of
                   Left  e -> error (show e)
                   Right g -> game2sgf g

main = interact ngf2sgf
