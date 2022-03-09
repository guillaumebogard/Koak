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
                            | MismatchedThenElseType Type Type
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
    show (ShadowedVariableByVariable     name shadowed_var)     = "New variable named "   ++ show name ++ " is shadowing previous variable: "   ++ show shadowed_var ++ " ."
    show (ShadowedVariableByDefinition   name shadowed_def)     = "New variable named "   ++ show name ++ " is shadowing previous definition: " ++ show shadowed_def ++ " ."
    show (ShadowedDefinitionByVariable   name shadowed_var)     = "New definition named " ++ show name ++ " is shadowing previous variable: "   ++ show shadowed_var ++ " ."
    show (ShadowedDefinitionByDefinition name shadowed_def)     = "New definition named " ++ show name ++ " is shadowing previous definition: " ++ show shadowed_def ++ " ."