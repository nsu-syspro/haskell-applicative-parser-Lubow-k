{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task3 where

import Parser ( Parser, Parsed(..), parse, satisfy, option)
import Data.Char (toLower, isControl)
import Data.List (intercalate)

import ParserCombinators ( char, string, choice)
import Control.Applicative ((<|>), many, some)

import Task1 (digit)
import Task2 (charToString)

-- | JSON representation
--
-- See <https://www.json.org>
--
data JValue =
    JObject [(String, JValue)]
  | JArray [JValue]
  | JString String
  | JNumber Double
  | JBool Bool
  | JNull
 deriving (Show, Eq)

-- | Parses JSON value
--
-- See full grammar at <https://www.json.org>
--
-- Usage example:
--
-- >>> parse json "{}"
-- Parsed (JObject []) (Input 2 "")
-- >>> parse json "null"
-- Parsed JNull (Input 4 "")
-- >>> parse json "true"
-- Parsed (JBool True) (Input 4 "")
-- >>> parse json "3.14"
-- Parsed (JNumber 3.14) (Input 4 "")
-- >>> parse json "{{}}"
-- Failed [PosError 0 (Unexpected '{'),PosError 1 (Unexpected '{')]
--
json :: Parser JValue
json = element

element :: Parser JValue
element = whiteSpace *> value <* whiteSpace

value :: Parser JValue
value = choice [parseObject, parseNull, parseBool, parseNumber, parseString, parseArray]

parseNull :: Parser JValue
parseNull = string "null" >> pure JNull

parseBool :: Parser JValue
parseBool = choice [string "true" >> pure (JBool True), string "false" >> pure (JBool False)]

parseNumber :: Parser JValue
parseNumber = do
  s        <- option (string "-")
  intPart  <- integerPart
  fracPart <- option fraction
  expPart  <- option expon

  let numStr = s ++ intPart ++ fracPart ++ expPart
  let n = read numStr :: Double
  pure $ JNumber n

integerPart :: Parser String
integerPart = some digit

fraction :: Parser String
fraction = do
  let dot = '.'
  _  <- char dot
  ds <- some digit
  pure (dot : ds)

expon :: Parser String
expon = do
  e  <- choice [char 'E', char 'e']
  s  <- option sign
  ds <- some digit
  pure (e : s ++ ds)

sign :: Parser String
sign = choice [string "+", string "-"]

parseString :: Parser JValue
parseString = fmap JString pString

pString :: Parser String
pString = do
  _ <- char '\"'
  content <- many pChar
  _ <- char '\"'
  let res = concat content
  pure res

pChar :: Parser String
pChar = charToString (satisfy isNormalChar) <|> pEscape

isNormalChar :: Char -> Bool
isNormalChar c = not (isControl c || c == '\"' || c == '\\')

pEscape :: Parser String
pEscape = do
  start <- char '\\'
  end   <- satisfy isEscapeChar
  pure (start : [end])

isEscapeChar :: Char -> Bool
isEscapeChar c = c `elem` "\"/\\bfnrt"

parseContainer :: Monoid a => Char -> Char -> Parser a -> (a -> JValue) -> Parser JValue
parseContainer open close parser constructor = do
  _ <- char open
  values <- option parser <* whiteSpace
  _ <- char close
  return $ constructor values

parseContent :: Parser a -> Parser [a]
parseContent parser = do
  e  <- parser
  es <- option (char ',' *> parseContent parser)
  return (e : es)

parseArray :: Parser JValue
parseArray = parseContainer '[' ']' (parseContent element) JArray

parseObject :: Parser JValue
parseObject = parseContainer '{' '}' (parseContent member) JObject

member :: Parser (String, JValue)
member = do
  key <- whiteSpace *> pString <* whiteSpace
  _   <- char ':'
  val <- element
  return (key, val)

whiteSpace :: Parser String
whiteSpace = many (choice (char <$> [' ', '\n', '\r', '\t']))


-- * Rendering helpers

-- | Renders given JSON value as oneline string
render :: JValue -> String
render = concatMap readable . renderTokens
  where
    -- Adds some nice spacing for readability
    readable ":" = ": "
    readable "," = ", "
    readable s   = s

-- | Renders given JSON value as list of separate tokens ready for pretty printing
renderTokens :: JValue -> [String]
renderTokens JNull        = ["null"]
renderTokens (JBool b)    = [map toLower $ show b]
renderTokens (JNumber d)  = [show d]
renderTokens (JString s)  = ["\"" ++ s ++ "\""]
renderTokens (JArray xs)  = ["["] ++ intercalate [","] (map renderTokens xs) ++ ["]"]
renderTokens (JObject xs) = ["{"] ++ intercalate [","] (map renderPair xs) ++ ["}"]
 where
  renderPair :: (String, JValue) -> [String]
  renderPair (k, v) = ["\"" ++ k ++ "\""] ++ [":"] ++ renderTokens v

-- | Renders 'Parsed' or 'Failed' value as string
renderParsed :: Parsed JValue -> String
renderParsed (Parsed v _) = render v
renderParsed (Failed err) = show err

-- | Parses given file as JSON and renders result
renderJSONFile :: String -> IO String
renderJSONFile file = renderParsed <$> parseJSONFile file

-- | Parses given file as JSON
parseJSONFile :: String -> IO (Parsed JValue)
parseJSONFile file = parse json <$> readFile file
