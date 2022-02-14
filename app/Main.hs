--
-- EPITECH PROJECT, 2022
-- KOAK
-- File description:
-- Main
--

module Main where

import Control.Exception  ( handle )
import System.Environment ( getArgs )

import Error              ( errorHandler )
import Argument.Parser    ( KoakArguments(..)
                          , parseArguments
                          )

main :: IO ()
main = handle errorHandler $ getArgs >>= handleExecution . parseArguments

handleExecution :: KoakArguments -> IO ()
handleExecution (KoakArguments file) = readFile file >>= putStrLn
