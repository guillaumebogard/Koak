--
-- EPITECH PROJECT, 2022
-- koak
-- File description:
-- Koak.Parser.Exception
--

module Koak.Parser.Exception      ( KoakParserException(..) ) where

import GHC.Exception              ( Exception )

import qualified Koak.Lexer as KL ( Token(..) )


data KoakParserException = KoakParserMissingTokenException String [KL.Token] (Maybe KL.Token) [KL.Token]

instance Exception KoakParserException

instance Show      KoakParserException where
    show (KoakParserMissingTokenException at expected actual [])   = "At '" ++ at ++ "'. Expected: '" ++ show expected ++ ". Got: '" ++ show actual ++ "'. No token left to be parsed."
    show (KoakParserMissingTokenException at expected actual rest) = "At '" ++ at ++ "'. Expected: '" ++ show expected ++ ". Got: '" ++ show actual ++ "'. Tokens left to be parsed: '" ++ show rest ++ "'."

instance Eq        KoakParserException where
    (KoakParserMissingTokenException lAt lExpected lActual lRest) == (KoakParserMissingTokenException rAt rExpected rActual rRest) = lAt == rAt && lExpected == rExpected && lActual == rActual && lRest == rRest
