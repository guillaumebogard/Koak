--
-- EPITECH PROJECT, 2022
-- KOAK
-- File description:
-- CLIArguments Error
--

module CLIArguments.Error ( ArgException(..)
                          , exceptionHandler
                          ) where

import GHC.Exception      ( Exception )
import System.Exit        ( ExitCode ( ExitSuccess, ExitFailure )
                          , exitWith
                          )

data ArgException = InvalidOption String
                  | Help

instance Exception ArgException

instance Show ArgException where
    show (InvalidOption optionName) = "Invalid Option: '" ++ optionName ++ "'"
    show Help                       = usage

usage :: String
usage = "Help message"

exceptionHandler :: ArgException -> IO ()
exceptionHandler Help      = print Help      >> exitWith  ExitSuccess
exceptionHandler exception = print exception >> exitWith (ExitFailure 84)
