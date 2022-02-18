--
-- EPITECH PROJECT, 2022
-- KOAK
-- File description:
-- Main
--

module Main where

import System.Environment    ( getArgs )
import System.Exit           ( ExitCode(ExitFailure)
                             , exitWith
                             , exitSuccess
                             )
import Control.Exception     ( handle )

import Exception             ( KoakException(..) )
import Argument.Parser as AP ( KoakArguments(..)
                             , Filepath(..)
                             , parseArguments
                             )

main :: IO ()
main = handle exceptionHandler $ getArgs >>= handleExecution . parseArguments

handleExecution :: KoakArguments -> IO ()
handleExecution (KoakArguments (AP.Filepath file)) = readFile file >>= putStrLn

exceptionHandler :: KoakException -> IO ()
exceptionHandler KoakHelpException = print KoakHelpException >> exitSuccess
exceptionHandler err               = print err               >> exitWith (ExitFailure 84)
