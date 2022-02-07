--
-- EPITECH PROJECT, 2022
-- KOAK
-- File description:
-- Main
--

module Main where

import Control.Exception     ( handle )
import System.Environment    ( getArgs )

import Error                 ( errorHandler )
import Argument.Parser as AP ( Filepath(..)
                             , KoakArguments(..)
                             , parseArguments
                             )

main :: IO ()
main = handle errorHandler $ getArgs >>= handleExecution . parseArguments

handleExecution :: KoakArguments -> IO ()
handleExecution (KoakArguments (AP.Filepath file)) = readFile file >>= putStrLn
