{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task2 where

import Parser
import ParserCombinators ( char, string, spaces, choice)
import Task1
import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
import Data.List (elemIndex)

-- | Date representation
--
-- Date parts are expected to be in following ranges
--
-- 'Day' in @[1..31]@
-- 'Month' in @[1..12]@
-- 'Year' is any non-negative integer
--
data Date = Date Day Month Year
  deriving (Show, Eq)

newtype Day   = Day   Int deriving (Show, Eq)
newtype Month = Month Int deriving (Show, Eq)
newtype Year  = Year  Int deriving (Show, Eq)

-- | Parses date in one of three formats given as BNF
--
-- @
-- date ::= dotFormat | hyphenFormat | usFormat
--
-- dotFormat ::= day "." month "." year
-- hyphenFormat ::= day "-" month "-" year
-- usFormat ::= monthName " " usDay " " year
--
-- usDay ::= nonZeroDigit | "1" digit | "2" digit | "30" | "31"
-- day ::= "0" nonZeroDigit | "1" digit | "2" digit | "30" | "31"
-- month ::= "0" nonZeroDigit | "10" | "11" | "12"
-- year ::= number
--
-- number ::= digit | number digit
-- digit ::= "0" | nonZeroDigit
-- nonZeroDigit ::= "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
--
-- monthName ::= "Jan" | "Feb" | "Mar" | "Apr" | "May" | "Jun" | "Jul" | "Aug" | "Sep" | "Oct" | "Nov" | "Dec"
-- @
--
-- Usage example:
--
-- >>> parse date "01.01.2012"
-- Parsed (Date (Day 1) (Month 1) (Year 2012)) (Input 10 "")
-- >>> parse date "12.12.2012"
-- Parsed (Date (Day 12) (Month 12) (Year 2012)) (Input 10 "")
-- >>> parse date "12-12-2012"
-- Parsed (Date (Day 12) (Month 12) (Year 2012)) (Input 10 "")
-- >>> parse date "Dec 12 2012"
-- Parsed (Date (Day 12) (Month 12) (Year 2012)) (Input 11 "")
-- >>> parse date "Jan 1 2012"
-- Parsed (Date (Day 1) (Month 1) (Year 2012)) (Input 10 "")
-- >>> parse date "Feb 31 2012"
-- Parsed (Date (Day 31) (Month 2) (Year 2012)) (Input 11 "")
-- >>> parse date "12/12/2012"
-- Failed [PosError 2 (Unexpected '/'),PosError 0 (Unexpected '1')]
--
date :: Parser Date
date = choice [dotFormat, hyphenFormat, usFormat]

dotFormat :: Parser Date
dotFormat = dotOrHyphenFormat '.'

hyphenFormat :: Parser Date
hyphenFormat = dotOrHyphenFormat '-'

dotOrHyphenFormat :: Char -> Parser Date
dotOrHyphenFormat ch = do
  d <- day
  _ <- char ch
  m <- month
  _ <- char ch
  Date d m <$> year


usFormat :: Parser Date
usFormat = do
  m <- monthName
  _ <- char ' '
  _ <- spaces
  d <- usDay
  _ <- char ' '
  Date d m <$> year


-- day, month, year   

day :: Parser Day
day = do
  dayStr <- dayChoice <|> decade '0' nonZeroDigit
  let d = read dayStr :: Int
  pure $ Day d


usDay :: Parser Day
usDay = do
  dayStr <- dayChoice <|> charToString nonZeroDigit
  let d = read dayStr :: Int
  pure $ Day d


dayChoice :: Parser String
dayChoice = choice [decade '1' digit, decade '2' digit, string "30", string "31"]


month :: Parser Month
month = do
  monthStr <- choice [decade '0' nonZeroDigit, string "10", string "11", string "12"]
  let m = read monthStr :: Int
  pure $ Month m

monthNames :: [String]
monthNames = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]

monthName :: Parser Month
monthName = do
  name <- choice (map string monthNames)
  let index = fromMaybe 0 (elemIndex name monthNames) + 1
  pure (Month index)

year :: Parser Year
year = Year <$> nat


--- helper functions 

charToString :: Parser Char -> Parser String
charToString = fmap (: [])

nonZeroDigit :: Parser Char
nonZeroDigit = satisfy (\c -> c >= '1' && c <= '9')

decade :: Char -> Parser Char -> Parser String
decade ch parser = do
  _ <- char ch
  d <- charToString parser
  pure (ch : d)