--
-- EPITECH PROJECT, 2022
-- koak
-- File description:
-- Argument.Parser
--

module Argument.Parser      ( Filepath(..)
                            , KoakArguments(..)
                            , parseArguments
                            ) where

import GHC.Exception        ( throw )

import Error                ( KoakError(KoakArgumentParserError, KoakHelpError) )
import Argument.Lexer as AL ( Token(..)
                            , tokenizeArguments
                            )

newtype Filepath = Filepath String

newtype KoakArguments = KoakArguments Filepath

parseArguments :: [String] -> KoakArguments
parseArguments = parseTokenizedArguments . tokenizeArguments

parseTokenizedArguments :: [Token] -> KoakArguments
parseTokenizedArguments [AL.Filepath filepath] = KoakArguments $ Argument.Parser.Filepath filepath
parseTokenizedArguments tokens
    | hasHelpToken tokens = throw KoakHelpError
    | otherwise           = throw $ KoakArgumentParserError "Incorrect arguments, retry with -h to display usage"

hasHelpToken :: [Token] -> Bool
hasHelpToken []           = False
hasHelpToken (AL.Help:_)  = True
hasHelpToken (_:xs)       = hasHelpToken xs
