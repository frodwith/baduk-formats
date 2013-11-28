{-# LANGUAGE NoMonomorphismRestriction #-}

module Codec.Baduk.Parser.Tygem (parseGibo) where

import Text.Read
import Codec.Baduk.Game hiding (moves, result, handicap)
import Text.Parsec
import Text.Parsec.Text
import Text.Parsec.Error
import Data.Maybe
import Data.Time.Format
import System.Locale
import Control.Applicative hiding (many, (<|>))
import qualified Data.Text as T

int1 = (read :: String -> Int) <$> many1 digit

eol = string "\n"
      <|> try (string "\r\n")
      <|> string "\r"
      <?> "end of line"

header = start *> eol *> (endBy (try tag) eol) <* end
    where start = string "\\HS"
          end   = string "\\HE"

tag = (,) <$> (start *> name) <*> ((string "=") *> value <* end)
    where start = string "\\["
          end   = string "\\]"
          name  = many1 upper
          value = many (noneOf "\\")

moves = start *> eol *> (catMaybes <$> (endBy move eol)) <* end
    where start = string "\\GS"
          end   = string "\\GE"

move = (Just <$> stone) <|> stuff
    where stuff = (\_ -> Nothing) <$> many1 (noneOf "\\\r\n")

stone = Stone <$> (string "STO 0 " *> many1 digit *> space *> player)
              <*> (space *> int1p)
              <*> (space *> int1p <* space)
    where player  = n2c <$> oneOf "12"
          n2c '1' = Black
          n2c '2' = White
          int1p   = (+1) <$> int1

data Gibo = Gibo [(String,String)] [Move] deriving (Show)

showParseEither e = case e of
                       Left pe -> Left (show pe)
                       Right v -> Right v

data GiboError = MissingPropertyError String
               | ParsecError ParseError
               | DateFormatError String
               | GiboParseError String

instance Show GiboError where
    show (MissingPropertyError p) = "Missing required property: " ++ p
    show (ParsecError pe)       = show pe
    show (DateFormatError d)      = "Bad gibo date: \"" ++ d ++ "\""
    show (GiboParseError e)       = "Gibo parse error: " ++ e

giboParse p s i = case parse p s i of
                      Left pe -> Left (ParsecError pe)
                      Right v -> Right v

gibo2game (Gibo tags moves) =
    let g = get tags
    in  do rWhite   <- g "GAMEWHITENAME"
           rBlack   <- g "GAMEBLACKNAME"
           rGongje  <- g "GAMEGONGJE"
           rDum     <- g "GAMEDUM"
           rDate    <- g "GAMEDATE"
           rResult  <- g "GAMERESULT"
           rHandi   <- g "GAMECONDITION"
           white    <- giboParse player "white player" rWhite
           black    <- giboParse player "black player" rBlack
           gongje   <- gread rGongje
           dum      <- gread rDum
           time     <- pTime rDate
           result   <- giboParse result "result" rResult
           handicap <- giboParse handicap "handicap" rHandi
           let komi = (gongje/10) - dum
           return $ Game 19 black white handicap komi time moves result

    where get m s = case lookup s m of
                        Just v  -> Right v
                        Nothing -> Left (MissingPropertyError s)

          gread s = case readMaybe s of
                        Just v  -> Right v
                        Nothing -> Left $ GiboParseError
                                        $ "Could not read \"" ++ s ++ "\""

          pTime s = case parseTime defaultTimeLocale "%0Y-%m-%d-%H-%M-%S" s of
                        Just t  -> Right t
                        Nothing -> Left $ DateFormatError s

player = Player <$> name <*> ((many space) *> rank)
    where name    = T.pack <$> many1 (noneOf " ")
          rank    = Rank <$> (char '(' *> int1)
                         <*> (letter <* char ')')
          letter  = dkp <$> oneOf "DKP"
          dkp 'D' = Dan
          dkp 'K' = Kyu
          dkp 'P' = Pro


-- TODO: figure out how handicaps are represented in this field
handicap = tr <$> string "Even : "
    where tr _ = 0

result = Win <$> winner <*> (space *> margin)
    where winner = s2c <$> ((string "black") <|> string ("white"))
          margin = r2m <$> (try (string "wins by time")
                            <|> (string "wins by resignation")
                            <|> (points <* (string " win")))

          r2m "wins by time"        = Time
          r2m "wins by resignation" = Resignation
          r2m n                     = Points (read n)

          s2c "black" = Black
          s2c "white" = White

          points = (++) <$> (many1 digit) <*> option "" (string ".5")

gibo = Gibo <$> (header <* eol) <*> moves

parseGibo s = do gibo <- giboParse gibo "parseGibo" s
                 game <- gibo2game gibo
                 return game
