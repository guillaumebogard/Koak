--
-- EPITECH PROJECT, 2022
-- koak
-- File description:
-- Argument.Lexer
--

module Argument.Lexer ( Token(..)
                      , tokenizeArguments
                      ) where

data Token = Help
           | UnknownOption String
           | Filepath String

tokenizeArguments :: [String] -> [Token]
tokenizeArguments []                = []
tokenizeArguments ("-h"        :xs) = Help              : tokenizeArguments xs
tokenizeArguments ("--help"    :xs) = Help              : tokenizeArguments xs
tokenizeArguments (opt@('-':_) :xs) = UnknownOption opt : tokenizeArguments xs
tokenizeArguments (x           :xs) = Filepath x        : tokenizeArguments xs
