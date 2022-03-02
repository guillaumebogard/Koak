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
                            | ShadowedDefinitionByVariable   PROTOTYPE     VAR_SIGNATURE
                            | ShadowedVariableByDefinition   VAR_SIGNATURE PROTOTYPE
                            | ShadowedDefinitionByDefinition PROTOTYPE     PROTOTYPE
    deriving (Eq)

-- Interdire le shadowing
-- 

instance Exception KoakTypingException

instance Show KoakTypingException where
    show (MismatchedArgumentType got expected)   = "Mismatched argument type. Got type: '" ++ show got ++ "', but expected type '" ++ show expected ++ "'."
    show (MismatchedReturnType   got expected)   = "Mismatched return type. Got type: '"   ++ show got ++ "', but expected type '" ++ show expected ++ "'."
    show (MismatchedThenElseType got expected)   = "Mismatched type between then and else expression. In else, got type: '" ++ show got ++ "', but expected same type as then: '" ++ show expected ++ "'."
    show (ShadowedVariableByVariable     new_var old_var) = "Shadowed variable "   ++ show new_var ++ " by a variable "   ++ show old_var ++ "."
    show (ShadowedVariableByDefinition   new_var old_def) = "Shadowed variable "   ++ show new_var ++ " by a definition " ++ show old_def ++ "."
    show (ShadowedDefinitionByVariable   new_def old_var) = "Shadowed definition " ++ show new_def ++ " by a variable "   ++ show old_var ++ "."
    show (ShadowedDefinitionByDefinition new_def old_def) = "Shadowed definition " ++ show new_def ++ " by a definition " ++ show old_def ++ "."
