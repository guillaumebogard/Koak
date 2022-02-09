--
-- EPITECH PROJECT, 2022
-- koak
-- File description:
-- Koak.Lexer
--

module Koak.Lexer   ( Token(..)
                    , tokenizeKoak
                    ) where

import Data.Char (isAlphaNum, isAlpha, isSpace, isDigit)
import Text.Read (readMaybe)
import Control.Exception (throw)

import Error ( KoakError(KoaKUnknownToken, KoaKInvalidNumber) )

data Token  = Word String              -- 'if', 'def', 'FooBar', 'i'
            | Number Double             -- '0', '0123456789', '3.14159265'
            | OpenParenthesis           -- '('
            | ClosedParenthesis         -- '('
            | Plus                      -- '+'
            | Minus                     -- '-'
            | Multiply                  -- '*'
            | Divide                    -- '/'
            | Power                     -- '^'
            | Greater                   -- '>'
            | GreaterEqual              -- '>='
            | Lower                     -- '<'
            | LowerEqual                -- '<='
            | Equal                     -- '=='
            | NotEqual                  -- '!='
            | LogicalNot                -- '!'
            | Assign                    -- '='
            | Comma                     -- ','
            | Colon                     -- ':'
            | SemiColon                 -- ';'
            | Dot                       -- '.'
            deriving(Show)

tokenizeKoak :: String -> [Token]
tokenizeKoak []                         = []
tokenizeKoak ('('      :xs)             = OpenParenthesis   : tokenizeKoak xs
tokenizeKoak (')'      :xs)             = ClosedParenthesis : tokenizeKoak xs
tokenizeKoak ('+'      :xs)             = Plus              : tokenizeKoak xs
tokenizeKoak ('-'      :xs)             = Minus             : tokenizeKoak xs
tokenizeKoak ('*'      :xs)             = Multiply          : tokenizeKoak xs
tokenizeKoak ('/'      :xs)             = Divide            : tokenizeKoak xs
tokenizeKoak ('^'      :xs)             = Power             : tokenizeKoak xs
tokenizeKoak ('>':'='  :xs)             = GreaterEqual      : tokenizeKoak xs
tokenizeKoak ('>'      :xs)             = Greater           : tokenizeKoak xs
tokenizeKoak ('<':'='  :xs)             = LowerEqual        : tokenizeKoak xs
tokenizeKoak ('<'      :xs)             = Lower             : tokenizeKoak xs
tokenizeKoak ('=':'='  :xs)             = Equal             : tokenizeKoak xs
tokenizeKoak ('='      :xs)             = Assign            : tokenizeKoak xs
tokenizeKoak ('!':'='  :xs)             = NotEqual          : tokenizeKoak xs
tokenizeKoak ('!'      :xs)             = LogicalNot        : tokenizeKoak xs
tokenizeKoak (','      :xs)             = Comma             : tokenizeKoak xs
tokenizeKoak (':'      :xs)             = Colon             : tokenizeKoak xs
tokenizeKoak (';'      :xs)             = SemiColon         : tokenizeKoak xs
tokenizeKoak line@('.':x:xs)
    | isDigit x = let (token, leftover) = parseNumber line in token : tokenizeKoak leftover
    | otherwise                         = Dot               : tokenizeKoak (x:xs)
tokenizeKoak ('.'      :xs)             = Dot               : tokenizeKoak xs
tokenizeKoak line@(x:xs)
    | isSpace x = tokenizeKoak xs
    | isAlpha x = let (token, leftover) = parseWord   line in token : tokenizeKoak leftover
    | isDigit x = let (token, leftover) = parseNumber line in token : tokenizeKoak leftover
    | otherwise = throw $ KoaKUnknownToken [x]

parseWord :: String -> (Token, String)
parseWord s = let (l, r) = parseWord' ("", s) in (Word l, r)

parseWord' :: (String, String) -> (String, String)
parseWord' (l, [])              = (l, [])
parseWord' (l, line@(r:rs))
    | isAlphaNum r              = parseWord' (l ++ [r], rs)
    | otherwise                 = (l, line)

parseNumber :: String -> (Token, String)
parseNumber s = let (l, r) = parseNumber' ("", s) in (Koak.Lexer.Number $ readAndCheck l, r)

parseNumber' :: (String, String) -> (String, String)
parseNumber' (l, [])            = (l, [])
parseNumber' (l, '.':rs)        = parseNumber' (l ++ ".", rs)
parseNumber' (l, line@(r:rs))
    | isDigit r                 = parseNumber' (l ++ [r], rs)
    | otherwise                 = (l, line)

readAndCheck :: String -> Double
readAndCheck n@('.':_) = readAndCheck $ '0':n
readAndCheck t          = readAndCheck' (readMaybe t) t

readAndCheck' :: Maybe Double -> String -> Double
readAndCheck' (Just x) _ = x
readAndCheck' Nothing  t = throw $ KoaKInvalidNumber t