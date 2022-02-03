--
-- EPITECH PROJECT, 2022
-- KOAK
-- File description:
-- CLIArguments Error
--

module CLIArguments.Error ( ArgException(..), exceptionHandler ) where

import GHC.Exception      ( Exception )
import System.Exit (ExitCode (ExitFailure), exitWith)

type OptionName = String

data ArgException = ArgumentParsingError String
           | InvalidOption OptionName
           | Help

instance Exception ArgException

exceptionHandler :: ArgException -> IO ()
exceptionHandler Help = print Help >> exitWith (ExitFailure 0)
exceptionHandler exception = print exception >> exitWith (ExitFailure 84)

instance Show ArgException where
  show (InvalidOption         optionName) = "Invalid option :'" ++ optionName ++ "'"
  show (ArgumentParsingError  str)        = str
  show Help                               = "help message"

