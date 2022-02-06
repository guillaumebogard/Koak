--
-- EPITECH PROJECT, 2022
-- koak
-- File description:
-- Argument.Parser
--
 
module Argument.Parser ( KoakArguments(..)
                       , parseArguments
                       ) where
 
type Filepath = String
 
newtype KoakArguments = KoakArguments Filepath
 
parseArguments :: [String] -> KoakArguments
parseArguments _ = KoakArguments ""
