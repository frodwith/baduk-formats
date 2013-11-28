{-# LANGUAGE NoMonomorphismRestriction #-}

module Codec.Baduk.Parser.WBaduk (parseNGF) where

import Codec.Baduk.Game
import Text.Parsec
import Text.Parsec.Text
import Control.Applicative hiding (optional, (<|>))
import Data.Time.Format
import System.Locale
import qualified Data.Text as T
import qualified Data.Map as M

readRankType 'D' = Dan
readRankType 'K' = Kyu
readRankType 'P' = Pro

eol = string "\r\n"

chomp = do skipMany $ noneOf "\r\n"
           eol

numberLine = read <$> many1 digit <* eol

playerLine = Player <$> name <*> rank
    where name = T.pack <$> many1 (noneOf " ")
          rank = make <$> (spaces *> many1 digit)
                      <*> oneOf "DKP" <* optional (char '*') <* eol
          make r t = Rank (read r) (readRankType t)

coords = "ABCDEFGHIJKLMNOPQRST"
cmap   = M.fromList $ zip coords [0..19]
pCoord = (cmap M.!) <$> oneOf coords

moveLine = (mv . ch2c) <$> (string "PM" *> count 2 anyChar *> oneOf "BW")
                       <*> pCoord <*> pCoord <* count 2 anyChar <* eol
    where mv c 0 0 = Pass c
          mv c x y = Stone c x y
          ch2c 'B' = Black
          ch2c 'W' = White

resultLine = Win <$> pWinner <*> pMargin
    where pWinner = (flip winner) <$> (((string "White") <|> (string "Black"))                                         <* space)
                                  <*> ((string "wins")  <|> (string "loses"))

          pMargin = margin <$> (spaces *> ((string "on") <|> (string "by")))
                           <*> (spaces *> ((string "time")
                                           <|> (string "resign")
                                           <|> pPoints))
                           <* chomp

          pPoints = do whole <- many1 digit
                       half  <- optionMaybe (char '.')
                       return $ case half of
                                    Nothing  -> whole
                                    Just '.' -> whole ++ ".5"

          winner "wins"  = s2c
          winner "loses" = other . s2c

          s2c "White" = White
          s2c "Black" = Black

          other Black = White
          other White = Black

          margin "on" "time"   = Time
          margin "by" "resign" = Resignation
          margin "by" p        = Points (read p)

ngf = do chomp
         size  <- numberLine
         white <- playerLine
         black <- playerLine
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

parseNGF = parse ngf "parseNGF"
