{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task1 where

import Parser ( Parser, satisfy)
import Data.Char (isDigit)

import Control.Applicative

-- | Parses natural number (including zero)
--
-- Usage example:
--
-- >>> parse nat "0"
-- Parsed 0 (Input 1 "")
-- >>> parse nat "123"
-- Parsed 123 (Input 3 "")
-- >>> parse nat "-123"
-- Failed [PosError 0 (Unexpected '-')]
-- >>> parse nat "abc"
-- Failed [PosError 0 (Unexpected 'a')]
-- >>> parse nat "123abc"
-- Parsed 123 (Input 3 "abc")
--
nat :: Parser Int
nat = read <$> some digit

digit :: Parser Char
digit = satisfy isDigit

-- | Parses integer number
--
-- Usage example:
--
-- >>> parse int "0"
-- Parsed 0 (Input 1 "")
-- >>> parse int "123"
-- Parsed 123 (Input 3 "")
-- >>> parse int "-123"
-- Parsed (-123) (Input 4 "")
-- >>> parse int "abc"
-- Failed [PosError 0 (Unexpected 'a')]
-- >>> parse int "123abc"
-- Parsed 123 (Input 3 "abc")
--
int :: Parser Int
int = negateNat <|> nat


-- todo: implement opt? 
negateNat :: Parser Int
negateNat = do
    _ <- satisfy (== '-') 
    n <- nat                
    return (-n)            

