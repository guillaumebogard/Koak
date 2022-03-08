--
-- EPITECH PROJECT, 2022
-- koak
-- File description:
-- Argument.Parser.Exception
--

module Argument.Parser.Exception ( KoakArgumentParserException(..) ) where

import GHC.Exception             ( Exception )


data KoakArgumentParserException = KoakHelpException
                                 | KoakArgumentParserException String

instance Exception KoakArgumentParserException

instance Show      KoakArgumentParserException where
    show  KoakHelpException                  = usage
    show (KoakArgumentParserException value) = "Argument Parsing Exception: " ++ value

instance Eq        KoakArgumentParserException where
    KoakHelpException                  == KoakHelpException                   = True
    (KoakArgumentParserException left) == (KoakArgumentParserException right) = left == right
    _                                  == _                                   = False

usage :: String
usage = "Usage: ./koak file\n"                     ++
        "Description:\n"                           ++
        "\tAn interpreter of the KOAK language.\n" ++
        "Options:\n"                               ++
        "\t--help\t\tDisplay this information."
