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
                   | KoakUnknownTokenException Char
                   | KoakInvalidNumberException String

instance Exception KoakException

instance Show KoakException where
    show (KoakArgumentParserException err)  = "Argument Parser Exception: " ++ err
    show (KoakUnknownTokenException token)  = "Unknown token: "             ++ [token]
    show (KoakInvalidNumberException token) = "Invalid number: "            ++ token
    show  KoakHelpException                 = usage

instance Eq KoakException where
    (KoakArgumentParserException left) == (KoakArgumentParserException right) = left == right
    KoakHelpException                  == KoakHelpException                   = True
    _                                  == _                                   = False

usage :: String
usage = "Usage: ./koak file\n"                     ++
        "Description:\n"                           ++
        "\tAn interpreter of the KOAK language.\n" ++
        "Options:\n"                               ++
        "\t--help\t\tDisplay this information."
