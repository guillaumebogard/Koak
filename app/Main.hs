--
-- EPITECH PROJECT, 2022
-- KOAK
-- File description:
-- Main
--

module Main where

import System.Environment                         ( getArgs )
import System.Exit                                ( ExitCode(ExitFailure)
                                                  , exitWith
                                                  , exitSuccess
                                                  )
import Control.Exception                          ( handle )

import Exception                                  ( KoakException(..) )
import qualified Argument.Parser.Exception as APE ( KoakArgumentParserException( KoakHelpException ) )
import qualified Argument.Parser as AP            ( KoakArguments(..)
                                                  , Filepath(..)
                                                  , parseArguments
                                                  )

main :: IO ()
main = handle exceptionHandler $ getArgs >>= handleExecution . AP.parseArguments

handleExecution :: AP.KoakArguments -> IO ()
handleExecution (AP.KoakArguments (AP.Filepath file)) = readFile file >>= putStrLn

exceptionHandler :: KoakException -> IO ()
exceptionHandler (KoakAPE APE.KoakHelpException) = print APE.KoakHelpException >> exitSuccess
exceptionHandler (KoakAPE err)                   = print err                   >> exitWith (ExitFailure 84)
exceptionHandler (KoakKLE err)                   = print err                   >> exitWith (ExitFailure 84)
exceptionHandler (KoakKPE err)                   = print err                   >> exitWith (ExitFailure 84)
