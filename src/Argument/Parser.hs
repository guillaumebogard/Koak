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

import Exception            ( KoakException(KoakArgumentParserException, KoakHelpException) )
import Argument.Lexer as AL ( Token(..)
                            , tokenizeArguments
                            )

newtype Filepath      = Filepath String

newtype KoakArguments = KoakArguments Filepath

parseArguments :: [String] -> KoakArguments
parseArguments = parseTokenizedArguments . tokenizeArguments

parseTokenizedArguments :: [Token] -> KoakArguments
parseTokenizedArguments = checkTokensInLayers

checkTokensInLayers :: [Token] -> KoakArguments
checkTokensInLayers []     = throw $ KoakArgumentParserException "Invalid arguments: requires at least 1 filepath"
checkTokensInLayers tokens = checkTokensInLayers' tokens

checkTokensInLayers' :: [Token] -> KoakArguments
checkTokensInLayers' = multipleFilepathsHandler . filter helpHandler . filter unknownOptionHandler

helpHandler :: Token -> Bool
helpHandler AL.Help = throw KoakHelpException
helpHandler _       = True

unknownOptionHandler :: Token -> Bool
unknownOptionHandler (AL.UnknownOption option) = throw $ KoakArgumentParserException $ "Invalid arguments: unknown option '" ++ option ++ "'"
unknownOptionHandler _                         = True

multipleFilepathsHandler :: [Token] -> KoakArguments
multipleFilepathsHandler [AL.Filepath filepath] = KoakArguments $ Argument.Parser.Filepath filepath
multipleFilepathsHandler _                      = throw $ KoakArgumentParserException   "Invalid arguments: can only take 1 filepath"
