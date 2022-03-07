--
-- EPITECH PROJECT, 2022
-- koak
-- File description:
-- Koak.Grammar.Utils
--

module Koak.Grammar.Utils ( syntaxTokens
                          , isSyntaxToken
                          , isAlphaWordChar
                          , isAlphaWord
                          , isAlphaNumWordChar
                          , isAlphaNumWord
                          , isSpecialWordChar
                          , isSpecialWord
                          ) where

import Data.Char          ( isAlphaNum
                          , isAlpha
                          , isSpace
                          )

syntaxTokens :: String
syntaxTokens = [ '('
               , ')'
               , ';'
               , ':'
               , ','
               ]

isSyntaxToken :: Char -> Bool
isSyntaxToken c = c `elem` syntaxTokens

isAlphaWordChar :: Char -> Bool
isAlphaWordChar c = isAlpha c || c == '\'' || c == '_'

isAlphaWord :: String -> Bool
isAlphaWord = all isAlphaWordChar

isAlphaNumWordChar :: Char -> Bool
isAlphaNumWordChar c = isAlphaNum c || c == '\'' || c == '_'

isAlphaNumWord :: String -> Bool
isAlphaNumWord = all isAlphaWordChar

isSpecialWordChar :: Char -> Bool
isSpecialWordChar c = not (isAlphaNumWordChar c) && not (isSpace c) && not (isSyntaxToken c)

isSpecialWord :: String -> Bool
isSpecialWord = all isAlphaWordChar
