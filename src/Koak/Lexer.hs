--
-- EPITECH PROJECT, 2022
-- koak
-- File description:
-- Koak.Lexer
--

module Koak.Lexer           ( Token(..)
                            , tokenizeKoak
                            ) where

import Data.Char            ( isAlphaNum
                            , isAlpha
                            , isSpace
                            , isDigit
                            )
import Text.Read            ( readMaybe )
import Control.Exception    ( throw )

import Exception            ( KoakException(KoakUnknownTokenException, KoakInvalidNumberException) )

data Token  = Word String           -- 'if', 'def', 'foobar', 'i'
            | FloatingNumber Double -- '3.14159265', '.01'
            | IntegerNumber Int     -- '0', '0123456789'
            | OpenParenthesis       -- '('
            | ClosedParenthesis     -- ')'
            | SemiColon             -- ';'
            | Colon                 -- ':'
            | Comma                 -- ','
            deriving (Show, Eq)

tokenizeKoak :: String -> [Token]
tokenizeKoak []           = []
tokenizeKoak ('(':xs)     = OpenParenthesis   : tokenizeKoak xs
tokenizeKoak (')':xs)     = ClosedParenthesis : tokenizeKoak xs
tokenizeKoak (';':xs)     = SemiColon         : tokenizeKoak xs
tokenizeKoak (':':xs)     = Colon             : tokenizeKoak xs
tokenizeKoak (',':xs)     = Comma             : tokenizeKoak xs
tokenizeKoak line@('.':_) = let (token, leftover) = parseDot line in token : tokenizeKoak leftover
tokenizeKoak line@(x:xs)
    | isSpace x           = tokenizeKoak xs
    | isAlphaWordChar x   = let (token, leftover) = parseAlphaWord line     in token : tokenizeKoak leftover
    | isDigit x           = let (token, leftover) = parseNumber line False  in token : tokenizeKoak leftover
    | otherwise           = let (token, leftover) = parseSpecialWord line   in token : tokenizeKoak leftover

isSyntaxToken :: Char -> Bool
isSyntaxToken c = c `elem` "();:,"

isAlphaWordChar :: Char -> Bool
isAlphaWordChar c = isAlpha c || c == '\'' || c == '_'

isAlphaNumWordChar :: Char -> Bool
isAlphaNumWordChar c = isAlphaNum c || c == '\'' || c == '_'

isSpecialWordChar :: Char -> Bool
isSpecialWordChar c = not (isAlphaNumWordChar c) && not (isSpace c) && not (isSyntaxToken c)

parseAlphaWord :: String -> (Token, String)
parseAlphaWord unparsed = let (parsed, rest) = span isAlphaNumWordChar unparsed in (Word parsed, rest)

parseSpecialWord :: String -> (Token, String)
parseSpecialWord unparsed = let (parsed, rest) = span isSpecialWordChar unparsed in (Word parsed, rest)

parseDot :: String -> (Token, String)
parseDot line@(_:x2:xs)
    | isDigit x2        = parseNumber line True
    | otherwise         = parseSpecialWord line
parseDot line           = parseSpecialWord line

parseNumber :: String -> Bool -> (Token, String)
parseNumber unparsed floating = parseNumber' $ parseNumber'' "" unparsed floating

parseNumber' :: (String, String, Bool) -> (Token, String)
parseNumber' (parsed, rest, floating) = (refineNumber parsed floating, rest)

parseNumber'' :: String -> String -> Bool -> (String, String, Bool)
parseNumber'' parsed []          floating = (reverse parsed, [], floating)
parseNumber'' parsed ('.':rs)    _        = parseNumber'' ('.':parsed) rs True
parseNumber'' parsed rest@(r:rs) floating
    | isDigit r                           = parseNumber'' (r:parsed) rs floating
    | otherwise                           = (reverse parsed, rest, floating)

refineNumber :: String -> Bool -> Token
refineNumber rawNumber@('.':_) _ = FloatingNumber $ refineFloatingNumber ('0':rawNumber) $ readMaybe $ '0':rawNumber
refineNumber rawNumber False     = IntegerNumber  $ refineIntegerNumber  rawNumber       $ readMaybe       rawNumber
refineNumber rawNumber True      = FloatingNumber $ refineFloatingNumber rawNumber       $ readMaybe       rawNumber

refineIntegerNumber :: String -> Maybe Int -> Int
refineIntegerNumber _               (Just x) = x
refineIntegerNumber impureRawNumber Nothing  = throw $ KoakInvalidNumberException impureRawNumber

refineFloatingNumber :: String -> Maybe Double -> Double
refineFloatingNumber _               (Just x) = x
refineFloatingNumber impureRawNumber Nothing  = throw $ KoakInvalidNumberException impureRawNumber
