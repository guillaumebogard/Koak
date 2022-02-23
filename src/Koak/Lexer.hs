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
            | Number Double             -- '0', '0123456789', '3.14159265', '.01'
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
            | Semicolon                 -- ';'
            | Dot                       -- '.'
<<<<<<< HEAD
            deriving(Eq, Show)
=======
            deriving (Show, Eq)
>>>>>>> origin

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
tokenizeKoak (';'     :xs) = Semicolon         : tokenizeKoak xs
tokenizeKoak line@('.':_)  = let (token, leftover) = parseDot    line in token : tokenizeKoak leftover
tokenizeKoak line@(x:xs)
    | isSpace x            = tokenizeKoak xs
    | isAlpha x            = let (token, leftover) = parseWord   line in token : tokenizeKoak leftover
    | isDigit x            = let (token, leftover) = parseNumber line in token : tokenizeKoak leftover
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
    | isDigit x2 = parseNumber line
    | otherwise  = (Dot, x2:xs)
parseDot (_:xs)  = (Dot, xs)
parseDot _       = (Dot, [])

parseNumber :: String -> (Token, String)
parseNumber unparsed = let (parsed, rest) = parseNumber' "" unparsed in (Koak.Lexer.Number $ refineNumber parsed, rest)

parseNumber' :: String -> String -> (String, String)
parseNumber' parsed []          = (reverse parsed, [])
parseNumber' parsed ('.':rs)    = parseNumber' ('.':parsed) rs
parseNumber' parsed rest@(r:rs)
    | isDigit r                 = parseNumber' (r:parsed) rs
    | otherwise                 = (reverse parsed, rest)

refineNumber :: String -> Double
refineNumber rawNumber@('.':_) = refineNumber' ('0':rawNumber) $ readMaybe $ '0':rawNumber
refineNumber rawNumber         = refineNumber' rawNumber       $ readMaybe rawNumber

refineNumber' :: String -> Maybe Double -> Double
refineNumber' _               (Just x) = x
refineNumber' impureRawNumber Nothing  = throw $ KoakInvalidNumberException impureRawNumber
