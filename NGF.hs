module NGF (parseNGF) where

import Game

import Text.ParserCombinators.Parsec
import Data.Time.Format
import System.Locale
import qualified Data.Map as M

readRankType 'D' = Dan
readRankType 'K' = Kyu
readRankType 'P' = Pro

eol :: CharParser st String
eol = string "\r\n"

chomp = do skipMany $ noneOf "\r\n"
           eol

numberLine = do n <- many1 digit
                chomp
                return (read n :: Int)

playerLine = do name <- many1 alphaNum
                spaces
                q <- many1 digit
                t <- oneOf "DKP"
                chomp
                return $ Player name (Rank (read q) (readRankType t))


coords = "BCDEFGHIJKLMNOPQRST"
cmap   = M.fromList $ zip coords [1..19]

cnum :: CharParser st Int
cnum = do c <- oneOf coords
          case M.lookup c cmap of
              Just n  -> return n
              Nothing -> fail "coordinate"

moveLine = do string "PM"
              count 2 anyChar
              color <- oneOf "BW"
              col   <- cnum
              row   <- cnum
              chomp
              return $ Stone (ch color) col row
    where ch 'B' = Black
          ch 'W' = White

resultLine = do color <- (string "White") <|> (string "Black")
                space
                wins  <- (string "wins") <|> (string "loses")
                let tloss  = wins == "loses"
                let winner = if tloss
                             then if color == "White" then Black else White
                             else if color == "White" then White else Black
                margin <- do if tloss
                             then return Time
                             else do spaces
                                     string "by"
                                     spaces
                                     m <- (string "resign")
                                          <|> do whole <- many1 digit
                                                 half  <- option ' ' (char '.')
                                                 return $ if half == '.'
                                                          then whole ++ ".5"
                                                          else whole
                                          <?> "game result"
                                     return $ if m == "resign" 
                                              then Resignation 
                                              else Points (read m)
                chomp
                return $ Win winner margin                                      

ngf = do chomp
         size  <- numberLine
         black <- playerLine
         white <- playerLine
         chomp
         handi <- numberLine
         chomp
         komii <- numberLine
         let komi = if handi == 0 then 0.5 else fromIntegral komii
         tline <- many1 $ noneOf "]"
         let time = readTime defaultTimeLocale "%0Y%m%d [%R" tline
         chomp
         chomp -- This field might be minutes of main time?
         result <- resultLine
         nMoves <- numberLine
         moves  <- count nMoves moveLine
         return $ Game size black white handi komi time moves result

parseNGF = parse ngf "NGF error" 
