--
-- EPITECH PROJECT, 2022
-- koak
-- File description:
-- Argument.Lexer
--

module Argument.Lexer ( Token(..)
                      , tokenizeArguments
                      ) where

data Token = Filepath String
           | UnknownOption String
           | Help

tokenizeArguments :: [String] -> [Token]
tokenizeArguments []            = []
tokenizeArguments ("-h":xs)     = Help : tokenizeArguments xs
tokenizeArguments ("--help":xs) = Help : tokenizeArguments xs
tokenizeArguments (x:xs)
    | head x == '-'             = UnknownOption x : tokenizeArguments xs
    | otherwise                 = Filepath x      : tokenizeArguments xs
