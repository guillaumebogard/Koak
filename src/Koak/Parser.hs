--
-- EPITECH PROJECT, 2022
-- koak
-- File description:
-- Koak.Parser
--

module Koak.Parser   ( parseKoak
                    ) where

import Koak.Lexer ( Token   (..)   )

data KDEFS          = KDEFS_DEFS DEFS
                    | KDEFS_EXPR EXPRESSIONS

data DEFS           = DEFS PROTOTYPE EXPRESSIONS

newtype PRECEDENCE  = PRECEDENCE Int

data PROTOTYPE      = PROTOTYPE_REDEFINE_UNARY  UN_OP  (Maybe PRECEDENCE) IDENTIFIER PROTOTYPE_ARGS
                    | PROTOTYPE_REDEFINE_BINARY BIN_OP (Maybe PRECEDENCE) IDENTIFIER PROTOTYPE_ARGS
                    | PROTOTYPE IDENTIFIER PROTOTYPE_ARGS

data PROTOTYPE_ARGS = PROTOTYPE_ARGS [PROTOTYPE_ID] TYPE

data PROTOTYPE_ID   = PROTOTYPE_ID IDENTIFIER TYPE

data TYPE           = INT
                    | DOUBLE
                    | VOID

data FOR            = FOR IDENTIFIER EXPRESSION IDENTIFIER EXPRESSION EXPRESSION EXPRESSION

data IF             = IF EXPRESSION EXPRESSIONS (Maybe EXPRESSIONS)

data WHILE          = WHILE EXPRESSION EXPRESSIONS

data EXPRESSIONS    = FOR_EXPR FOR
                    | IF_EXPR IF
                    | WHILE_EXPR WHILE
                    | EXPRESSIONS EXPRESSION [EXPRESSION]

data BIN_OP         = PLUS
                    | MINUS
                    | MULT
                    | DIV
                    | MOD
                    | LT
                    | GT
                    | EQ
                    | NEQ
                    | ASSIGN

data BINARY_OP      = BINARY_OP_UN BIN_OP UNARY
                    | BINARY_OP_EXPR BIN_OP EXPRESSION

data EXPRESSION     = EXPRESSION UNARY [BINARY_OP]

data UN_OP          = NOT
                    | NEG

data UNARY          = UNARY_UN UN_OP UNARY
                    | UNARY_POSTFIX POSTFIX

data POSTFIX        = POSTFIX PRIMARY (Maybe CALL_EXPR)

newtype CALL_EXPR   = CALL_EXPR (Maybe CALL_EXPR_ARGS)

data CALL_EXPR_ARGS = CALL_EXPR_ARGS EXPRESSION [EXPRESSION]

data PRIMARY        = PRIMARY_IDENTIFIER IDENTIFIER
                    | PRIMARY_LITERAL LITERAL
                    | PRIMARY_EXPRS EXPRESSIONS

newtype IDENTIFIER  = IDENTIFIER String

data DOT

newtype DECIMAL_CONST = DECIMAL_CONST Int

newtype DOUBLE_CONST  = DOUBLE_CONST Double

data LITERAL          = LITERAL_DECIMAL DECIMAL_CONST
                      | LITERAL_DOUBLE DOUBLE_CONST

parseKoak :: [Token] -> [KDEFS]
parseKoak [] = []
parseKoak tokens = let (kdefs, rest) = parseKdefs tokens in kdefs : parseKoak rest

parseKdefs :: [Token] -> (KDEFS, [Token])
parseKdefs [] = error "parseKdefs: empty list"
parseKdefs list@(x:xs)
    | x == Word "def" = let (def, rest) = parseDefs xs in (KDEFS_DEFS def, rest)
    | otherwise = let (expr, rest) = parseExpressions list in (KDEFS_EXPR expr, rest)

parseDefs :: [Token] -> (DEFS, [Token])
parseDefs [] = error "parseDefs: empty list"
parseDefs tokens = let (proto, rest) = parsePrototype tokens in
                   let (expr, rest2) = parseExpressions rest in
                   DEFS proto expr

parseExpressions :: [Token] -> (EXPRESSIONS, [Token])
parseExpressions [] = error "parseExpressions: empty list"
parseExpressions list@(x:xs)
    | x == Word "for" = let (for, rest) = parseFor xs in (FOR_EXPR for, rest)
    | x == Word "if" = let (if_, rest) = parseIf xs in (IF_EXPR if_, rest)
    | x == Word "while" = let (while, rest) = parseWhile xs in (WHILE_EXPR while, rest)
    | otherwise = let (expr, rest) = parseExpression list in (EXPRESSIONS expr [], rest)
