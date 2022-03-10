--
-- EPITECH PROJECT, 2022
-- B-YEP-500-BDX-5-1-koak-matheo.lucak
-- File description:
-- Exception
--

module Koak.Typing.Exception ( KoakTypingException(..) ) where

import GHC.Exception         ( Exception )

import Koak.Parser           ( Type(..)
                             , Prototype(..)
                             , VarAssignment(..)
                             , Identifier(..)
                             )


data KoakTypingException = KoakTypingUnknownDefinition              Identifier
                         | KoakTypingNotAUnaryFunction              Identifier
                         | KoakTypingNotABinaryFunction             Identifier
                         | KoakTypingNotAFunction                   Identifier
                         | KoakTypingNotAVar                        Identifier
                         | KoakTypingMismatchedArgumentNumber       Identifier Int
                         | KoakTypingMismatchedArgumentType         Identifier [Type]
                         | KoakTypingMismatchedReturnType           Identifier Type          Type
                         | KoakTypingMismatchedThenElseType         Type       Type
                         | KoakTypingShadowedVariableByVariable     Identifier VarAssignment
                         | KoakTypingShadowedVariableByDefinition   Identifier Prototype
                         | KoakTypingShadowedDefinitionByVariable   Identifier VarAssignment
                         | KoakTypingShadowedDefinitionByDefinition Identifier Prototype
    deriving Eq

instance Exception KoakTypingException

instance Show      KoakTypingException where
    show (KoakTypingUnknownDefinition              identifier                         ) = '\'' : show identifier ++ "' is undefined."
    show (KoakTypingNotAUnaryFunction              identifier                         ) = '\'' : show identifier ++ "' is not a unary function."
    show (KoakTypingNotABinaryFunction             identifier                         ) = '\'' : show identifier ++ "' is not a binary function."
    show (KoakTypingNotAFunction                   identifier                         ) = '\'' : show identifier ++ "' is not a function."
    show (KoakTypingNotAVar                        identifier                         ) = '\'' : show identifier ++ "' is not a variable."
    show (KoakTypingMismatchedArgumentNumber       identifier   argumentsNb           ) = "Mismatched arguments number in function call " ++ show identifier ++ ". No matching function with the following arguments: " ++ show argumentsNb   ++ "."
    show (KoakTypingMismatchedArgumentType         identifier   argumentsType         ) = "Mismatched arguments type in function call "   ++ show identifier ++ ". No matching function with the following types: "     ++ show argumentsType ++ "."
    show (KoakTypingMismatchedReturnType           identifier   got           expected) = "Mismatched return type in function called "    ++ show identifier ++ ". Got: '"                                              ++ show got           ++ "', but expected '" ++ show expected ++ "'."
    show (KoakTypingMismatchedThenElseType         got          expected              ) = "Mismatched type between 'then' and 'else' expression. In 'else', got type: '" ++ show got ++ "', but expected same type as in 'then': '" ++ show expected ++ "'."
    show (KoakTypingShadowedVariableByVariable     name         shadowedVar           ) = "New variable named "   ++ show name ++ " is shadowing previous variable: '"   ++ show shadowedVar ++ "'."
    show (KoakTypingShadowedVariableByDefinition   name         shadowedDef           ) = "New variable named "   ++ show name ++ " is shadowing previous definition: '" ++ show shadowedDef ++ "'."
    show (KoakTypingShadowedDefinitionByVariable   name         shadowedVar           ) = "New definition named " ++ show name ++ " is shadowing previous variable: '"   ++ show shadowedVar ++ "'."
    show (KoakTypingShadowedDefinitionByDefinition name         shadowedDef           ) = "New definition named " ++ show name ++ " is shadowing previous definition: '" ++ show shadowedDef ++ "'."
