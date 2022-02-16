--
-- EPITECH PROJECT, 2022
-- koak
-- File description:
-- Error
--

module Exception     ( KoakException(..) ) where

import GHC.Exception ( Exception )

data KoakException = KoakArgumentParserException String
                   | KoakHelpException

instance Exception KoakException

instance Show KoakException where
    show (KoakArgumentParserException err) = "Argument Parser Exception: " ++ err
    show  KoakHelpException                = usage

usage :: String
usage = "Usage: ./koak file\n"                     ++
        "Description:\n"                           ++
        "\tAn interpreter of the KOAK language.\n" ++
        "Options:\n"                               ++
        "\t--help\t\tDisplay this information."
