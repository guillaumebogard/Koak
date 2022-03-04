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
                                , PROTOTYPE_ID
                                )

data KoakTypingException    = MismatchedArgumentType TYPE TYPE
                            | MismatchedUnaryType    TYPE TYPE
                            | MismatchedBinaryType   TYPE TYPE TYPE TYPE
                            | MismatchedReturnType   TYPE TYPE
                            | MismatchedThenElseType TYPE TYPE
                            | ShadowedVariableByVariable     PROTOTYPE_ID
                            | ShadowedVariableByDefinition   PROTOTYPE_ID
                            | ShadowedDefinitionByVariable   PROTOTYPE
                            | ShadowedDefinitionByDefinition PROTOTYPE
                            | UnaryFunctionInvalidArgumentNumber    PROTOTYPE Int
                            | BinaryFunctionInvalidArgumentNumber   PROTOTYPE Int
    deriving (Eq)

instance Exception KoakTypingException

instance Show KoakTypingException where
    show (MismatchedArgumentType got expected)                  = "Mismatched argument type. Got type: '" ++ show got ++ "', but expected type '" ++ show expected ++ "'."
    show (MismatchedUnaryType    got expected)                  = "Mismatched unary type. Got type: '" ++ show got ++ "', but unary operator expected type '" ++ show expected ++ "'."
    show (MismatchedBinaryType got got' expected expected')     = "Mismatched binary type. Got type: '" ++ show got ++ "' and '" ++ show got' ++ "', but binary operator expected types '" ++ show expected ++ "' and '" ++ show expected' ++ "'."
    show (MismatchedReturnType   got expected)                  = "Mismatched return type. Got type: '"   ++ show got ++ "', but expected type '" ++ show expected ++ "'."
    show (MismatchedThenElseType got expected)                  = "Mismatched type between then and else expression. In else, got type: '" ++ show got ++ "', but expected same type as then: '" ++ show expected ++ "'."
    show (ShadowedVariableByVariable        var)                = "Shadowed variable "   ++ show var ++ " by another variable."
    show (ShadowedVariableByDefinition      var)                = "Shadowed variable "   ++ show var ++ " by a definition."
    show (ShadowedDefinitionByVariable      def)                = "Shadowed definition " ++ show def ++ " by a variable."
    show (ShadowedDefinitionByDefinition    def)                = "Shadowed definition " ++ show def ++ " by another definition."
    show (UnaryFunctionInvalidArgumentNumber  prototype arg_nb) = "Invalid argument number on unary function definition: "  ++ show prototype ++ ". Expected 1 argument. Got" ++ show arg_nb ++ " ."
    show (BinaryFunctionInvalidArgumentNumber prototype arg_nb) = "Invalid argument number on binary function definition: " ++ show prototype ++ ". Expected 2 argument. Got" ++ show arg_nb ++ " ."