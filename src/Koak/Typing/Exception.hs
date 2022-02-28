--
-- EPITECH PROJECT, 2022
-- B-YEP-500-BDX-5-1-koak-matheo.lucak
-- File description:
-- Exception
--

module Koak.Typing.Exception    ( KoakTypingException(..) ) where

import GHC.Exception            ( Exception )

import Koak.Parser              ( TYPE(..) )

data KoakTypingException    = MismatchedArgumentType TYPE TYPE 
                            | MismatchedReturnType  TYPE TYPE 
                            | MismatchedThenElseType TYPE TYPE
    deriving (Eq)

instance Exception KoakTypingException

instance Show KoakTypingException where
    show (MismatchedArgumentType got expected) = "Mismatched argument type. Got type: '" ++ show got ++ "', but expected type '" ++ show expected ++ "'."
    show (MismatchedReturnType   got expected) = "Mismatched return type. Got type: '" ++ show got ++ "', but expected type '" ++ show expected ++ "'."
    show (MismatchedThenElseType got expected) = "Mismatched type between then and else expression. In else, got type: '" ++ show got ++ "', but expected same type as then: '" ++ show expected ++ "'."
