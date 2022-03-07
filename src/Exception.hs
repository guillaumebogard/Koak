--
-- EPITECH PROJECT, 2022
-- koak
-- File description:
-- Error
--

module Exception     ( KoakException(..) ) where

import GHC.Exception ( Exception )

data KoakException = KoakHelpException
                   | KoakArgumentParserException String
                   | KoakUnknownTokenException Char
                   | KoakInvalidNumberException String
                   | KoakParserMissingTokenException String String String String

instance Exception KoakException

instance Show KoakException where
    show  KoakHelpException                                        = usage
    show (KoakArgumentParserException err)                         = "Argument Parser Exception: " ++ err
    show (KoakUnknownTokenException token)                         = "Unknown token: "             ++ [token]
    show (KoakInvalidNumberException token)                        = "Invalid number: "            ++ token
    show (KoakParserMissingTokenException at expected actual [])   = "At " ++ at ++ ", expected: " ++ expected ++ ". Got: " ++ actual ++ "."
    show (KoakParserMissingTokenException at expected actual rest) = "At " ++ at ++ ", expected: " ++ expected ++ ". Got: " ++ actual ++ ". " ++ rest

instance Eq KoakException where
    KoakHelpException                                             == KoakHelpException                                             = True
    (KoakArgumentParserException left)                            == (KoakArgumentParserException right)                           = left == right
    (KoakParserMissingTokenException lAt lExpected lActual lRest) == (KoakParserMissingTokenException rAt rExpected rActual rRest) = lAt == rAt && lExpected == rExpected && lActual == rActual && lRest == rRest
    _                                                             == _                                                             = False

usage :: String
usage = "Usage: ./koak file\n"                     ++
        "Description:\n"                           ++
        "\tAn interpreter of the KOAK language.\n" ++
        "Options:\n"                               ++
        "\t--help\t\tDisplay this information."
