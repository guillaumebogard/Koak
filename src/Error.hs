--
-- EPITECH PROJECT, 2022
-- koak
-- File description:
-- Error
--

module Error         ( KoakError(..)
                     , errorHandler
                     ) where

import GHC.Exception ( Exception )
import System.Exit   ( ExitCode ( ExitFailure )
                     , exitWith
                     , exitSuccess
                     )

data KoakError = KoakArgumentParserError String
               | KoakHelpError
               | KoaKUnknownToken Char
               | KoaKInvalidNumber String

instance Exception KoakError

instance Show KoakError where
    show (KoakArgumentParserError err) = "Argument Parser Error: " ++ err
    show  (KoaKUnknownToken token)     = "Unknown token: " ++ [token]
    show  (KoaKInvalidNumber token)    = "Invalid number: " ++ token
    show  KoakHelpError                = usage

usage :: String
usage = "Usage: ./koak file\n" ++
        "Description:\n" ++
        "\tAn interpreter of the KOAK language.\n" ++
        "Options:\n" ++
        "\t--help\t\tDisplay this information."

errorHandler :: KoakError -> IO ()
errorHandler KoakHelpError = print KoakHelpError >> exitSuccess
errorHandler err           = print err           >> exitWith (ExitFailure 84)
