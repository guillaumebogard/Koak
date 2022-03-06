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
                                , VAR_ASSIGNMENT
                                , IDENTIFIER(..)
                                )

data KoakTypingException    = MismatchedArgumentType TYPE TYPE
                            | MismatchedUnaryType    TYPE TYPE
                            | MismatchedBinaryType   TYPE TYPE TYPE TYPE
                            | MismatchedReturnType   TYPE TYPE
                            | MismatchedThenElseType TYPE TYPE
                            | ShadowedVariableByVariable     IDENTIFIER VAR_ASSIGNMENT
                            | ShadowedVariableByDefinition   IDENTIFIER PROTOTYPE
                            | ShadowedDefinitionByVariable   IDENTIFIER VAR_ASSIGNMENT
                            | ShadowedDefinitionByDefinition IDENTIFIER PROTOTYPE
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
    show (ShadowedVariableByVariable     name shadowed_var)     = "New variable named "   ++ show name ++ " is shadowing previous variable: "   ++ show shadowed_var ++ " ."
    show (ShadowedVariableByDefinition   name shadowed_def)     = "New variable named "   ++ show name ++ " is shadowing previous definition: " ++ show shadowed_def ++ " ."
    show (ShadowedDefinitionByVariable   name shadowed_var)     = "New definition named " ++ show name ++ " is shadowing previous variable: "   ++ show shadowed_var ++ " ."
    show (ShadowedDefinitionByDefinition name shadowed_def)     = "New definition named " ++ show name ++ " is shadowing previous definition: " ++ show shadowed_def ++ " ."
    show (UnaryFunctionInvalidArgumentNumber  prototype arg_nb) = "Invalid argument number on unary function definition: "  ++ show prototype ++ ". Expected 1 argument. Got" ++ show arg_nb ++ " ."
    show (BinaryFunctionInvalidArgumentNumber prototype arg_nb) = "Invalid argument number on binary function definition: " ++ show prototype ++ ". Expected 2 argument. Got" ++ show arg_nb ++ " ."