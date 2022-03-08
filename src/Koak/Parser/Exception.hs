--
-- EPITECH PROJECT, 2022
-- koak
-- File description:
-- Koak.Parser.Exception
--

module Koak.Parser.Exception      ( KoakParserException(..) ) where

import GHC.Exception              ( Exception )


data KoakParserException = KoakParserMissingTokenException String String String String

instance Exception KoakParserException

instance Show      KoakParserException where
    show (KoakParserMissingTokenException at expected actual []  ) = "At '" ++ at ++ "'. Expected: '" ++ expected ++ ". Got: '" ++ actual ++ "'. No token left to be parsed."
    show (KoakParserMissingTokenException at expected actual rest) = "At '" ++ at ++ "'. Expected: '" ++ expected ++ ". Got: '" ++ actual ++ "'. Tokens left to be parsed: '" ++ rest ++ "'."

instance Eq        KoakParserException where
    (KoakParserMissingTokenException lAt lExpected lActual lRest) == (KoakParserMissingTokenException rAt rExpected rActual rRest) = lAt == rAt && lExpected == rExpected && lActual == rActual && lRest == rRest
