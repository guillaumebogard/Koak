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
checkTokensInLayers tokens = checkTokensInLayers' tokens tokens

checkTokensInLayers' :: [Token] -> [Token] -> KoakArguments
checkTokensInLayers' tokens []          = checkTokensInLayers'' tokens
checkTokensInLayers' _      (AL.Help:_) = throw KoakHelpException
checkTokensInLayers' tokens (_:xs)      = checkTokensInLayers' tokens xs

checkTokensInLayers'' :: [Token] -> KoakArguments
checkTokensInLayers'' [AL.Filepath filepath] = KoakArguments $ Argument.Parser.Filepath filepath
checkTokensInLayers'' tokens                 = checkTokensInLayers''' tokens

checkTokensInLayers''' :: [Token] -> KoakArguments
checkTokensInLayers''' []                            = throw $ KoakArgumentParserException   "Invalid arguments: can only take 1 filepath"
checkTokensInLayers''' ((AL.UnknownOption option):_) = throw $ KoakArgumentParserException $ "Invalid arguments: unknown option '" ++ option ++ "'"
checkTokensInLayers''' (_:xs)                        = checkTokensInLayers''' xs
