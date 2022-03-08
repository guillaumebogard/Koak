--
-- EPITECH PROJECT, 2022
-- koak
-- File description:
-- Koak.Lexer.Exception
--

module Koak.Lexer.Exception ( KoakLexerException(..) ) where

import GHC.Exception        ( Exception )


newtype KoakLexerException = KoakLexerInvalidNumberException String

instance Exception KoakLexerException

instance Show      KoakLexerException where
    show (KoakLexerInvalidNumberException value) = "Invalid number: '" ++ value ++ "'"

instance Eq        KoakLexerException where
    (KoakLexerInvalidNumberException left) == (KoakLexerInvalidNumberException right) = left == right
