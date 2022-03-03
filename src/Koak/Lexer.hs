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

data Token  = Word String               -- 'if', 'def', 'foobar', 'i'
            | FloatingNumber Double     -- '3.14159265', '.01'
            | Number Int                -- '0', '0123456789'
            | OpenParenthesis           -- '('
            | ClosedParenthesis         -- ')'
            | Plus                      -- '+'
            | Minus                     -- '-'
            | Multiply                  -- '*'
            | Divide                    -- '/'
            | Modulo                    -- '%'
            | Power                     -- '^'
            | GreaterEqual              -- '>='
            | Greater                   -- '>'
            | LowerEqual                -- '<='
            | Lower                     -- '<'
            | Equal                     -- '=='
            | NotEqual                  -- '!='
            | LogicalNot                -- '!'
            | Assign                    -- '='
            | Comma                     -- ','
            | Colon                     -- ':'
            | SemiColon                 -- ';'
            | Dot                       -- '.'
            deriving(Eq, Show)

tokenizeKoak :: String -> [Token]
tokenizeKoak []            = []
tokenizeKoak ('('     :xs) = OpenParenthesis   : tokenizeKoak xs
tokenizeKoak (')'     :xs) = ClosedParenthesis : tokenizeKoak xs
tokenizeKoak ('+'     :xs) = Plus              : tokenizeKoak xs
tokenizeKoak ('-'     :xs) = Minus             : tokenizeKoak xs
tokenizeKoak ('*'     :xs) = Multiply          : tokenizeKoak xs
tokenizeKoak ('/'     :xs) = Divide            : tokenizeKoak xs
tokenizeKoak ('%'     :xs) = Modulo            : tokenizeKoak xs
tokenizeKoak ('^'     :xs) = Power             : tokenizeKoak xs
tokenizeKoak ('>':'=' :xs) = GreaterEqual      : tokenizeKoak xs
tokenizeKoak ('>'     :xs) = Greater           : tokenizeKoak xs
tokenizeKoak ('<':'=' :xs) = LowerEqual        : tokenizeKoak xs
tokenizeKoak ('<'     :xs) = Lower             : tokenizeKoak xs
tokenizeKoak ('=':'=' :xs) = Equal             : tokenizeKoak xs
tokenizeKoak ('='     :xs) = Assign            : tokenizeKoak xs
tokenizeKoak ('!':'=' :xs) = NotEqual          : tokenizeKoak xs
tokenizeKoak ('!'     :xs) = LogicalNot        : tokenizeKoak xs
tokenizeKoak (','     :xs) = Comma             : tokenizeKoak xs
tokenizeKoak (':'     :xs) = Colon             : tokenizeKoak xs
tokenizeKoak (';'     :xs) = SemiColon         : tokenizeKoak xs
tokenizeKoak line@('.':_)  = let (token, leftover) = parseDot    line in token : tokenizeKoak leftover
tokenizeKoak line@(x:xs)
    | isSpace x            = tokenizeKoak xs
    | isAlpha x            = let (token, leftover) = parseWord   line in token : tokenizeKoak leftover
    | isDigit x            = let (token, leftover) = parseNumber line False in token : tokenizeKoak leftover
    | otherwise            = throw $ KoakUnknownTokenException x

parseWord :: String -> (Token, String)
parseWord unparsed = let (parsed, rest) = parseWord' "" unparsed in (Word parsed, rest)

parseWord' :: String -> String -> (String, String)
parseWord' parsed []          = (reverse parsed, [])
parseWord' parsed rest@(r:rs)
    | isAlphaNum r            = parseWord' (r:parsed) rs
    | otherwise               = (reverse parsed, rest)

parseDot :: String -> (Token, String)
parseDot line@(_:x2:xs)
    | isDigit x2 = parseNumber line True
    | otherwise  = (Dot, x2:xs)
parseDot (_:xs)  = (Dot, xs)
parseDot _       = (Dot, [])

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
refineNumber rawNumber False     = Number         $ refineIntegerNumber  rawNumber       $ readMaybe       rawNumber
refineNumber rawNumber True      = FloatingNumber $ refineFloatingNumber rawNumber       $ readMaybe       rawNumber

refineIntegerNumber :: String -> Maybe Int -> Int
refineIntegerNumber _               (Just x) = x
refineIntegerNumber impureRawNumber Nothing  = throw $ KoakInvalidNumberException impureRawNumber

refineFloatingNumber :: String -> Maybe Double -> Double
refineFloatingNumber _               (Just x) = x
refineFloatingNumber impureRawNumber Nothing  = throw $ KoakInvalidNumberException impureRawNumber
