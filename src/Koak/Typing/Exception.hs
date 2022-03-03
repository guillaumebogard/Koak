--
-- EPITECH PROJECT, 2022
-- B-YEP-500-BDX-5-1-koak-matheo.lucak
-- File description:
-- Exception
--

module Koak.Typing.Exception    ( KoakTypingException(..) ) where

import GHC.Exception            ( Exception )

import Koak.Parser              ( TYPE(..)
                                , PROTOTYPE(..)
                                , VAR_SIGNATURE(..)
                                )

data KoakTypingException    = MismatchedArgumentType TYPE TYPE
                            | MismatchedReturnType   TYPE TYPE
                            | MismatchedThenElseType TYPE TYPE
                            | ShadowedVariableByVariable     VAR_SIGNATURE VAR_SIGNATURE
                            | ShadowedVariableByDefinition   VAR_SIGNATURE PROTOTYPE
                            | ShadowedDefinitionByVariable   PROTOTYPE     VAR_SIGNATURE
                            | ShadowedDefinitionByDefinition PROTOTYPE     PROTOTYPE
    deriving (Eq)

instance Exception KoakTypingException

instance Show KoakTypingException where
    show (MismatchedArgumentType got expected)   = "Mismatched argument type. Got type: '" ++ show got ++ "', but expected type '" ++ show expected ++ "'."
    show (MismatchedReturnType   got expected)   = "Mismatched return type. Got type: '"   ++ show got ++ "', but expected type '" ++ show expected ++ "'."
    show (MismatchedThenElseType got expected)   = "Mismatched type between then and else expression. In else, got type: '" ++ show got ++ "', but expected same type as then: '" ++ show expected ++ "'."
    show (ShadowedVariableByVariable     old_var new_var) = "Shadowed variable "   ++ show new_var ++ " by a variable "   ++ show old_var ++ "."
    show (ShadowedVariableByDefinition   old_def new_var) = "Shadowed variable "   ++ show new_var ++ " by a definition " ++ show old_def ++ "."
    show (ShadowedDefinitionByVariable   old_var new_def) = "Shadowed definition " ++ show new_def ++ " by a variable "   ++ show old_var ++ "."
    show (ShadowedDefinitionByDefinition old_def new_def) = "Shadowed definition " ++ show new_def ++ " by a definition " ++ show old_def ++ "."
