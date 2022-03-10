--
-- EPITECH PROJECT, 2022
-- B-YEP-500-BDX-5-1-koak-matheo.lucak
-- File description:
-- Exception
--

module Koak.Typing.Exception    ( KoakTypingException(..) ) where

import GHC.Exception            ( Exception )

import Koak.Parser              ( Type(..)
                                , Prototype(..)
                                , VarAssignment(..)
                                , Identifier(..)
                                )

data KoakTypingException    = UnknownDefinition  Identifier
                            | NotAUnaryFunction  Identifier
                            | NotABinaryFunction Identifier
                            | NotAFunction       Identifier
                            | NotAVar            Identifier
                            | MismatchedArgumentNumber  Identifier Int
                            | MismatchedArgumentType    Identifier [Type]
                            | MismatchedReturnType      Identifier Type Type
                            | MismatchedThenElseType    Type Type
                            | AssignmentToRValue
                            | ShadowedVariableByVariable     Identifier VarAssignment
                            | ShadowedVariableByDefinition   Identifier Prototype
                            | ShadowedDefinitionByVariable   Identifier VarAssignment
                            | ShadowedDefinitionByDefinition Identifier Prototype
    deriving (Eq)

instance Exception KoakTypingException

instance Show KoakTypingException where
    show (UnknownDefinition  identifier)                        = show identifier ++ "is Undefined."
    show (NotAUnaryFunction  identifier)                        = show identifier ++ " is not a unary function."
    show (NotABinaryFunction identifier)                        = show identifier ++ " is not a binary function."
    show (NotAFunction       identifier)                        = show identifier ++ " is not a function."
    show (NotAVar            identifier)                        = show identifier ++ " is not a variable."
    show (MismatchedArgumentNumber identifier arguments_nb)     = "Mismatched arguments number in a function call " ++ show identifier ++ " . No matching function with" ++ show arguments_nb ++ " arguments."
    show (MismatchedArgumentType identifier arguments_type)     = "Mismatched arguments type in a function call " ++ show identifier ++ " . No matching function with these following types" ++ show arguments_type ++ "."
    show (MismatchedReturnType   identifier got expected)       = "Mismatched return type in a function called "  ++ show identifier ++ " . Got: '" ++ show got ++ "', but expected '" ++ show expected ++ "'."
    show (MismatchedThenElseType got expected)                  = "Mismatched type between then and else expression. In else, got type: '" ++ show got ++ "', but expected same type as then: '" ++ show expected ++ "'."
    show AssignmentToRValue                                     = "Assignment is not allowed for RValue."
    show (ShadowedVariableByVariable     old_name new_var)      = "New variable   " ++ show new_var ++ " is shadowing previous named: "   ++ show old_name ++ " ."
    show (ShadowedVariableByDefinition   old_name new_def)      = "New variable   " ++ show new_def ++ " is shadowing previous named: "   ++ show old_name ++ " ."
    show (ShadowedDefinitionByVariable   old_name new_var)      = "New definition " ++ show new_var ++ " is shadowing previous named: "   ++ show old_name ++ " ."
    show (ShadowedDefinitionByDefinition old_name new_def)      = "New definition " ++ show new_def ++ " is shadowing previous named: "   ++ show old_name ++ " ."
