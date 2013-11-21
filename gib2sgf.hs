import SGF
import Gibo

gibo2sgf text = case (parseGibo text) of
                   Left  e -> error (show e)
                   Right g -> game2sgf g

main = interact gibo2sgf
