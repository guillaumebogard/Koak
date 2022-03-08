--
-- EPITECH PROJECT, 2022
-- koak
-- File description:
-- Koak.Lexer
--

module Koak.Lexer           ( Token(..)
                            , tokenizeKoak
                            ) where

import Data.Char            ( isDigit
                            , isSpace
                            )
import Control.Exception    ( throw )

import Koak.Lexer.Exception ( KoakLexerException( KoakLexerInvalidNumberException ) )
import Koak.Grammar.Utils   ( isAlphaWordChar
                            , isAlphaNumWordChar
                            , isSpecialWordChar
                            )

data Token  = Word String           -- 'if', 'def', 'foobar', 'i'
            | FloatingNumber Double -- '3.14159265', '.01'
            | IntegerNumber Int     -- '0', '0123456789'
            | OpenedParenthesis     -- '('
            | ClosedParenthesis     -- ')'
            | SemiColon             -- ';'
            | Colon                 -- ':'
            | Comma                 -- ','
            deriving (Show, Eq)

tokenizeKoak :: String -> [Token]
tokenizeKoak      []       = []
tokenizeKoak      ('(':xs) = OpenedParenthesis : tokenizeKoak xs
tokenizeKoak      (')':xs) = ClosedParenthesis : tokenizeKoak xs
tokenizeKoak      (';':xs) = SemiColon         : tokenizeKoak xs
tokenizeKoak      (':':xs) = Colon             : tokenizeKoak xs
tokenizeKoak      (',':xs) = Comma             : tokenizeKoak xs
tokenizeKoak line@('.':_ ) = uncurry addToTokensAndContinueTokenize $ parseDot                     line
tokenizeKoak line@(x:xs)
    | isSpace         x    = tokenizeKoak xs
    | isDigit         x    = uncurry addToTokensAndContinueTokenize $ parseNumberStartingWithDigit line
    | isAlphaWordChar x    = uncurry addToTokensAndContinueTokenize $ parseAlphaWord               line
    | otherwise            = uncurry addToTokensAndContinueTokenize $ parseSpecialWord             line

addToTokensAndContinueTokenize :: Token -> String -> [Token]
addToTokensAndContinueTokenize token rest = token : tokenizeKoak rest

parseAlphaWord :: String -> (Token, String)
parseAlphaWord   unparsed = uncurry wrapParsedAroundWord $ span isAlphaNumWordChar unparsed

parseSpecialWord :: String -> (Token, String)
parseSpecialWord unparsed = uncurry wrapParsedAroundWord $ span isSpecialWordChar  unparsed

wrapParsedAroundWord :: String -> String -> (Token, String)
wrapParsedAroundWord parsed rest = (Word parsed, rest)

parseDot :: String -> (Token, String)
parseDot line@(_:lineWithoutDot@(x2:_))
    | isDigit   x2 = parseNumberStartingWithDot lineWithoutDot
    | otherwise    = parseSpecialWord           line
parseDot line      = parseSpecialWord           line

parseNumberStartingWithDot :: String -> (Token, String)
parseNumberStartingWithDot = parseNumberStartingWithDot' . span isDigit

parseNumberStartingWithDot' :: (String, String) -> (Token, String)
parseNumberStartingWithDot'  (fractionalPart, rest) = parseNumberStartingWithDot'' ('.' : fractionalPart, rest)

parseNumberStartingWithDot'' :: (String, String) -> (Token, String)
parseNumberStartingWithDot'' (fractionalPart, rest) = (FloatingNumber $ read $ '0' : fractionalPart, rest)

parseNumberStartingWithDigit :: String -> (Token, String)
parseNumberStartingWithDigit = parseNumberStartingWithDigit' . span isDigit

parseNumberStartingWithDigit' :: (String, String) -> (Token, String)
parseNumberStartingWithDigit' (dp, '.':fp@(x2:_))
    | isDigit   x2                       = uncurry (parseNumberStartingWithDigit'' dp) $ span isDigit fp
    | otherwise                          = throw $ KoakLexerInvalidNumberException $ dp ++ "."
parseNumberStartingWithDigit' (dp, rest) = (IntegerNumber $ read dp, rest)

parseNumberStartingWithDigit'' :: String -> String -> String -> (Token, String)
parseNumberStartingWithDigit'' dp fp rest = (FloatingNumber $ read $ dp ++ '.' : fp, rest)
