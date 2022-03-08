--
-- EPITECH PROJECT, 2022
-- koak
-- File description:
-- Argument.Parser
--

module Argument.Parser                            ( Filepath(..)
                                                  , KoakArguments(..)
                                                  , parseArguments
                                                  ) where

import GHC.Exception                              ( throw )

import Exception                                  ( KoakException(KoakAPE) )
import qualified Argument.Parser.Exception as APE ( KoakArgumentParserException( KoakHelpException
                                                                               , KoakArgumentParserException
                                                                               )
                                                  )
import qualified Argument.Lexer            as AL  ( Token(..)
                                                  , tokenizeArguments
                                                  )

newtype Filepath      = Filepath String

newtype KoakArguments = KoakArguments Filepath

parseArguments :: [String] -> KoakArguments
parseArguments = parseTokenizedArguments . AL.tokenizeArguments

parseTokenizedArguments :: [AL.Token] -> KoakArguments
parseTokenizedArguments = checkTokensInLayers

checkTokensInLayers :: [AL.Token] -> KoakArguments
checkTokensInLayers []     = throw $ KoakAPE $ APE.KoakArgumentParserException "Requires at least 1 filepath"
checkTokensInLayers tokens = checkTokensInLayers' tokens

checkTokensInLayers' :: [AL.Token] -> KoakArguments
checkTokensInLayers' = multipleFilepathsHandler . filter helpHandler . filter unknownOptionHandler

helpHandler :: AL.Token -> Bool
helpHandler AL.Help = throw $ KoakAPE APE.KoakHelpException
helpHandler _       = True

unknownOptionHandler :: AL.Token -> Bool
unknownOptionHandler (AL.UnknownOption option) = throw $ KoakAPE $ APE.KoakArgumentParserException $ "Unknown option '" ++ option ++ "'"
unknownOptionHandler _                         = True

multipleFilepathsHandler :: [AL.Token] -> KoakArguments
multipleFilepathsHandler [AL.Filepath filepath] = KoakArguments $ Argument.Parser.Filepath filepath
multipleFilepathsHandler _                      = throw $ KoakAPE $ APE.KoakArgumentParserException "Can only take 1 filepath"
