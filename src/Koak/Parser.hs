--
-- EPITECH PROJECT, 2022
-- koak
-- File description:
-- Koak.Parser
--

module Koak.Parser  ( parseKoak
                    , KDEFS(..)
                    , DEFS(..)
                    , PRECEDENCE(..)
                    , PROTOTYPE(..)
                    , PROTOTYPE_ARGS(..)
                    , PROTOTYPE_ID(..)
                    , TYPE(..)
                    , FOR(..)
                    , IF(..)
                    , WHILE(..)
                    , EXPRESSIONS(..)
                    , BIN_OP(..)
                    , BINARY_OP(..)
                    , EXPRESSION(..)
                    , UN_OP(..)
                    , UNARY(..)
                    , POSTFIX(..)
                    , CALL_EXPR(..)
                    , CALL_EXPR_ARGS(..)
                    , PRIMARY(..)
                    , IDENTIFIER(..)
                    , DOT
                    , DECIMAL_CONST(..)
                    , DOUBLE_CONST(..)
                    , LITERAL(..)
                    ) where

import Koak.Lexer as KL

data KDEFS          = KDEFS_DEFS DEFS
                    | KDEFS_EXPR EXPRESSIONS
    deriving (Eq, Show)

data DEFS           = DEFS PROTOTYPE EXPRESSIONS
    deriving (Eq, Show)

newtype PRECEDENCE  = PRECEDENCE Int
    deriving (Eq, Show)

data PROTOTYPE      = PROTOTYPE_UNARY  UN_OP  PRECEDENCE IDENTIFIER PROTOTYPE_ARGS
                    | PROTOTYPE_BINARY BIN_OP PRECEDENCE IDENTIFIER PROTOTYPE_ARGS
                    | PROTOTYPE IDENTIFIER PROTOTYPE_ARGS
    deriving (Eq, Show)

data PROTOTYPE_ARGS = PROTOTYPE_ARGS [PROTOTYPE_ID] TYPE
    deriving (Eq, Show)

data PROTOTYPE_ID   = PROTOTYPE_ID IDENTIFIER TYPE
    deriving (Eq, Show)

data TYPE           = INT
                    | DOUBLE
                    | VOID
    deriving (Eq, Show)

data FOR            = FOR IDENTIFIER EXPRESSION IDENTIFIER EXPRESSION EXPRESSION EXPRESSIONS
    deriving (Eq, Show)

data IF             = IF EXPRESSION EXPRESSIONS (Maybe EXPRESSIONS)
    deriving (Eq, Show)

data WHILE          = WHILE EXPRESSION EXPRESSIONS
    deriving (Eq, Show)

data EXPRESSIONS    = FOR_EXPR FOR
                    | IF_EXPR IF
                    | WHILE_EXPR WHILE
                    | EXPRESSIONS EXPRESSION [EXPRESSION]
    deriving (Eq, Show)

data BIN_OP         = BIN_PLUS
                    | BIN_MINUS
                    | BIN_MULT
                    | BIN_DIV
                    | BIN_MOD
                    | BIN_LT
                    | BIN_LTE
                    | BIN_GT
                    | BIN_GTE
                    | BIN_EQ
                    | BIN_NEQ
                    | BIN_ASSIGN
    deriving (Eq, Show)

data EXPRESSION     = EXPRESSION UNARY [(BIN_OP, UNARY)]
    deriving (Eq, Show)

data UN_OP          = U_NOT
                    | U_NEG
                    | U_MINUS
                    | U_PLUS
    deriving (Eq, Show)

data UNARY          = UNARY_UN UN_OP UNARY
                    | UNARY_POSTFIX POSTFIX
    deriving (Eq, Show)

data POSTFIX        = POSTFIX PRIMARY (Maybe CALL_EXPR)
    deriving (Eq, Show)

newtype CALL_EXPR   = CALL_EXPR (Maybe CALL_EXPR_ARGS)
    deriving (Eq, Show)

data CALL_EXPR_ARGS = CALL_EXPR_ARGS EXPRESSION [EXPRESSION]
    deriving (Eq, Show)

data PRIMARY        = PRIMARY_IDENTIFIER IDENTIFIER
                    | PRIMARY_LITERAL LITERAL
                    | PRIMARY_EXPRS EXPRESSIONS
    deriving (Eq, Show)

newtype IDENTIFIER  = IDENTIFIER String
    deriving (Eq, Show)

data DOT

newtype DECIMAL_CONST = DECIMAL_CONST Int
    deriving (Eq, Show)

newtype DOUBLE_CONST  = DOUBLE_CONST Double
    deriving (Eq, Show)

data LITERAL          = LITERAL_DECIMAL DECIMAL_CONST
                      | LITERAL_DOUBLE DOUBLE_CONST
    deriving (Eq, Show)

parseKoak :: [Token] -> [KDEFS]
parseKoak [] = []
parseKoak tokens = let (kdefs, rest) = parseKdefs tokens in kdefs : parseKoak rest

parseKdefs :: [Token] -> (KDEFS, [Token])
parseKdefs []                = error "parseKdefs: empty list"
parseKdefs ((Word "def"):xs) = let (def, rest)  = parseDefs xs          in (KDEFS_DEFS def, rest)
parseKdefs (x:xs)            = let (expr, rest) = parseExpressions list in (KDEFS_EXPR expr, rest)

parseDefs :: [Token] -> (DEFS, [Token])
parseDefs []     = error "parseDefs: empty list"
parseDefs tokens = let (prototype  , rest1) = parsePrototype   tokens in
                   let (expressions, rest2) = parseExpressions rest1  in
                   (DEFS prototype expressions, rest2)

parsePrototype :: [Token] -> (PROTOTYPE, [Token])
parsePrototype []                   = error "parseDefs: empty list"
parsePrototype ((Word "unary"):xs)  = parsePrototypeUnary  xs
parsePrototype ((Word "binary"):xs) = parsePrototypeBinary xs
parsePrototype ((Word w):xs)        = parsePrototype'      xs

parsePrototypeUnary :: [Token] -> (PROTOTYPE, [Token])
parsePrototypeUnary []     = error "parsePrototypeUnary: empty list"
parsePrototypeUnary tokens = let (unop,       rest1) = parseUnOp            tokens in
                             let (prec,       rest2) = parseMaybePrecedence rest1  in
                             let (id,         rest3) = parseIdentifier      rest2  in
                             let (proto_args, rest4) = parsePrototypeArgs   rest3  in
                             parsePrototypeUnary' unop prec id proto_args rest4

parsePrototypeUnary' :: UN_OP -> Maybe PRECEDENCE -> IDENTIFIER -> PROTOTYPE_ARGS -> [Token] -> (PROTOTYPE, [Token])
parsePrototypeUnary' unop (Nothing)   id proto_args list = (PROTOTYPE_UNARY unop (getDefaultUnaryPrecedence unop) id proto_args, list)
parsePrototypeUnary' unop (Just prec) id proto_args list = (PROTOTYPE_UNARY unop prec id proto_args, list)

parsePrototypeBinary :: [Token] -> (PROTOTYPE, [Token])
parsePrototypeBinary [] = error "parsePrototypeUnary: empty list"
parsePrototypeBinary list = let (binop,      rest1) = parseBinOp            list  in
                            let (prec,       rest2) = parseMaybePrecedence  rest1 in
                            let (id,         rest3) = parseIdentifier       rest2 in
                            let (proto_args, rest4) = parsePrototypeArgs    rest3 in
                            parsePrototypeBinary' binop prec id proto_args rest4

parsePrototypeBinary' :: BIN_OP -> Maybe PRECEDENCE -> IDENTIFIER -> PROTOTYPE_ARGS -> [Token] -> (PROTOTYPE, [Token])
parsePrototypeBinary' binop (Nothing)   id proto_args list = (PROTOTYPE_BINARY binop (getDefaultBinaryPrecedence binop) id proto_args, list)
parsePrototypeBinary' binop (Just prec) id proto_args list = (PROTOTYPE_BINARY binop prec id proto_args, list)

parsePrototype' :: [Token] -> (PROTOTYPE, [Token])
parsePrototype' [] = error "parsePrototypeBinary: empty list"
parsePrototype' list    = let (id,    rest1) = parseIdentifier    list  in
                          let (args,  rest2) = parsePrototypeArgs rest1 in
                          (PROTOTYPE id args, rest2)

parsePrototypeArgs :: [Token] -> (PROTOTYPE_ARGS, [Token])
parsePrototypeArgs []     = error "parsePrototypeArgs: empty list"
parsePrototypeArgs (x:xs) = let (p_list,      rest1) = parsePrototypeArgsList xs    in
                            let (return_type, rest2) = parsePrototypeArgsType rest1 in
                            parsePrototypeArgs' p_list return_type rest2

parsePrototypeArgs' :: [PROTOTYPE_ID] -> TYPE -> [Token] -> (PROTOTYPE_ARGS, [Token])
parsePrototypeArgs' p_list return_type (OpenParenthesis:ClosedParenthesis:xs) = (PROTOTYPE_ARGS p_list return_type, xs)
parsePrototypeArgs' _      _    (_:ClosedParenthesis:xs)                      = error "parsePrototypeArgs: missing '('"
parsePrototypeArgs' _      _    (OpenParenthesis:_:xs)                        = error "parsePrototypeArgs: missing ')'"
parsePrototypeArgs' _      _    _                                             = error "parsePrototypeArgs: missing '(' ')'"

parsePrototypeArgsList :: [Token] -> ([PROTOTYPE_ID], [Token])
parsePrototypeArgsList list = let (p_list, tokens) = parsePrototypeArgsList' list [] in (reverse p_list, tokens)

parsePrototypeArgsList' :: [Token] -> [PROTOTYPE_ID] -> ([PROTOTYPE_ID], [Token])
parsePrototypeArgsList' list@(Word _:_) p_list = let (proto_id, rest) = parsePrototypeId list in parsePrototypeArgsList' rest (proto_id:p_list)
parsePrototypeArgsList' list            p_list = (p_list, list)

parsePrototypeArgsType :: [Token] -> (TYPE, [Token])
parsePrototypeArgsType []         = error "parsePrototypeArgs: empty list"
parsePrototypeArgsType (Colon:xs) = parseType xs

parsePrecedence :: [Token] -> (PRECEDENCE, [Token])
parsePrecedence list = parsePrecedence' $ parseMaybePrecedence list

parsePrecedence' :: (Maybe PRECEDENCE, [Token]) -> (PRECEDENCE, [Token])
parsePrecedence' (Nothing,   list) = error "parsePrecedence: precedence is missing"
parsePrecedence' (Just p,    list) = (p, list)

parseMaybePrecedence :: [Token] -> (Maybe PRECEDENCE, [Token])
parseMaybePrecedence (Number n:xs)   = (Just $ PRECEDENCE $ round n, xs)
parseMaybePrecedence (_:list)        = (Nothing,                     list)

parsePrototypeId :: [Token] -> (PROTOTYPE_ID, [Token])
parsePrototypeId [] = error "parsePrototypeId: empty list"
parsePrototypeId _ = error "Not Implemented"

parseType :: [Token] -> (TYPE, [Token])
parseType []                 = error "parseType: empty list"
parseType (Word "int":xs)    = (INT, xs)
parseType (Word "double":xs) = (DOUBLE, xs)
parseType (Word "void":xs)   = (VOID, xs)
parseType _                  = error "parseType: Invalid Token"

parseFor :: [Token] -> (FOR, [Token])
parseFor [] = error "parseFor: empty list"
parseFor _ = error "Not Implemented"

parseIf :: [Token] -> (IF, [Token])
parseIf [] = error "parseIf: empty list"
parseIf _ = error "Not Implemented"

parseWhile :: [Token] -> (WHILE, [Token])
parseWhile [] = error "parseWhile: empty list"
parseWhile _ = error "Not Implemented"

parseExpressions :: [Token] -> (EXPRESSIONS, [Token])
parseExpressions [] = error "parseExpressions: empty list"
parseExpressions list@(x : xs)
  | x == Word "for" = let (for, rest) = parseFor xs in (FOR_EXPR for, rest)
  | x == Word "if" = let (if_, rest) = parseIf xs in (IF_EXPR if_, rest)
  | x == Word "while" = let (while, rest) = parseWhile xs in (WHILE_EXPR while, rest)
  | otherwise = let (expr, rest) = parseExpression list in
                let (arrExpr, rest2) = parseArrExpression rest in (EXPRESSIONS expr arrExpr, rest2)


parseExpression :: [Token] -> (EXPRESSION, [Token])
parseExpression [] = error "parseExpression: empty list"
parseExpression tokens = let (unary, rest) = parseUnary tokens in
                         let (binops, rest2) = parseExpression' rest in
                         (EXPRESSION unary binops, rest2)

parseExpression' :: [Token] -> ([(BIN_OP, UNARY)], [Token])
parseExpression' [] = ([], [])
parseExpression' (x:xs)
    | isBinOp x = let (unary, rest) = parseUnary rest in
                  let (binops, rest2) = parseExpression' rest2 in
                  ((getBinOp x, unary) : binops, rest3)
    | otherwise = ([], tokens)

isBinOp :: Token -> Bool
isBinOp Plus = True
isBinOp Minus = True
isBinOp Multiply = True
isBinOp Divide = True
isBinOp Lower = True
isBinOp Greater = True
isBinOp Equal = True
isBinOp NotEqual = True
isBinOp Assign = True
isBinOp _ = False

getBinOp :: Token -> BIN_OP
getBinOp Plus = BIN_PLUS
getBinOp Minus = BIN_MINUS
getBinOp Multiply = BIN_MULT
getBinOp Divide = BIN_DIV
getBinOp Lower = BIN_LT
getBinOp Greater = BIN_GT
getBinOp Equal = BIN_EQ
getBinOp NotEqual = BIN_NEQ
getBinOp Assign = BIN_ASSIGN

parseArrExpression :: [Token] -> ([EXPRESSION], [Token])
parseArrExpression [] = error "parseArrExpression: empty list"
parseArrExpression tokens = let (first, rest) = parseExpression tokens in
                              let (next, rest2) = parseArrExpression' rest in (first : next, rest2)

parseArrExpression' :: [Token] -> ([EXPRESSION], [Token])
parseArrExpression' [] = error "parseArrExpression: empty list"
parseArrExpression' (x : xs)
  | x == Comma = let (first, rest) = parseExpression xs in
                 let (next, rest2) = parseArrExpression' rest in (first : next, rest2)
  | otherwise = ([], x:xs)

parseUnary :: [Token] -> (UNARY, [Token])
parseUnary [] = error "parseUnary: empty list"
parseUnary list@(x : xs)
  | x == LogicalNot = let (un, rest) = parseUnary xs in (UNARY_OP NOT un, rest)
  | x == Minus = let (un, rest) = parseUnary xs in (UNARY_OP NEG un, rest)
  | otherwise = let (postfix, rest) = parsePostfix list in (UNARY_POSTFIX postfix, rest)

parsePostfix :: [Token] -> (POSTFIX, [Token])
parsePostfix [] = error "parsePostfix: empty list"
parsePostfix tokens = let (prim, rest) = parsePrimary tokens in
  let (callExpr, rest2) = parseMaybeCallExpr rest in
  (POSTFIX prim callExpr, rest)

parseMaybeCallExpr :: [Token] -> (Maybe CALL_EXPR, [Token])
parseMaybeCallExpr [] = (Nothing, [])
parseMaybeCallExpr list@(x : xs)
  | x == OpenParenthesis = let (callExpr, rest) = parseCallExpr list in (Just callExpr, rest)
  | otherwise = (Nothing, list)

parseCallExpr :: [Token] -> (CALL_EXPR, [Token])
parseCallExpr [] = error "parseCallExpr: empty list"
parseCallExpr (OpenParenthesis : ClosedParenthesis : xs) = (CALL_EXPR Nothing, xs)
parseCallExpr (OpenParenthesis : xs) =
  let (callExpr, rest) = parseCallExprArg xs
   in (CALL_EXPR (Just callExpr), rest)
parseCallExpr _ = error "parseCallExpr: invalid syntax"

parseCallExprArg :: [Token] -> (CALL_EXPR_ARGS, [Token])
parseCallExprArg [] = error "parseCallExprArg: empty list"
parseCallExprArg tokens@(x : xs) = let (x : xs, rest) = parseArrExpression tokens in (CALL_EXPR_ARGS x xs, rest)


parsePrimary :: [Token] -> (PRIMARY, [Token])
parsePrimary [] = error "parsePrimary: empty list"
parsePrimary (Word x : xs)          = (PRIMARY_IDENTIFIER (IDENTIFIER x), xs)
parsePrimary (Number x : xs)        = (PRIMARY_LITERAL x, xs)
parsePrimary (OpenParenthesis : xs) =
                let (expr, rest)    = parseExpressions xs
                in (PRIMARY_EXPRS expr, rest)
parsePrimary _                      = error "parsePrimary: unexpected token"

parseIdentifier :: [Token] -> (IDENTIFIER, [Token])
parseIdentifier [] = error "parseIdentifier: empty list"
parseIdentifier ((Word w):xs) = (IDENTIFIER w, xs)
parseIdentifier _  = error "parseIdentifier: expecting ';'"

isValidIdentifier :: [Token] -> Bool
isValidIdentifier ((Word w):xs) = True
isValidIdentifier _             = False

{--
parseDot :: [Token] -> (DOT, [Token])
parseDot [] = error "parseDot: empty list"
parseDot _ = error "Not Implemented"

parseDecimalConst :: [Token] -> (DECIMAL_CONST, [Token])
parseDecimalConst [] = error "parseDecimalConst: empty list"
parseDecimalConst _ = error "Not Implemented"

parseDoubleConst :: [Token] -> (DOUBLE_CONST, [Token])
parseDoubleConst [] = error "parseDoubleConst: empty list"
parseDoubleConst _ = error "Not Implemented"
--}

parseLitteral :: [Token] -> (LITERAL', [Token])
parseLitteral []                = error "parseLitteral: empty list"
parseLitteral ((Number x) : xs) = (LITERAL' x, xs)
parseLitteral _                 = error "parseLitteral: not a number"

-- parseExpressions :: [Token] -> (EXPRESSIONS, [Token])
-- parseExpressions _ = error "parseExpressions: empty list"
-- parseExpressions list@(x:xs)
--     | x == Word "for"   = let (for, rest)   = parseFor xs in (FOR_EXPR for, rest)
--     | x == Word "if"    = let (if_, rest)   = parseIf xs in (IF_EXPR if_, rest)
--     | x == Word "while" = let (while, rest) = parseWhile xs in (WHILE_EXPR while, rest)
--     | otherwise = let (expr, rest) = parseExpression list in (EXPRESSIONS expr [], rest)

isBinaryOp :: Token -> Bool
isBinaryOp KL.Plus          = True
isBinaryOp KL.Minus         = True
isBinaryOp KL.Multiply      = True
isBinaryOp KL.Divide        = True
isBinaryOp KL.Modulo        = True
isBinaryOp KL.Lower         = True
isBinaryOp KL.LowerEqual    = True
isBinaryOp KL.Greater       = True
isBinaryOp KL.GreaterEqual  = True
isBinaryOp KL.Equal         = True
isBinaryOp KL.NotEqual      = True
isBinaryOp KL.Assign        = True
isBinaryOp _                = False

isUnaryOp :: Token -> Bool
isUnaryOp KL.Plus       = True
isUnaryOp KL.Minus      = True
isUnaryOp KL.LogicalNot = True
isUnaryOp _             = False

getDefaultUnaryPrecedence :: UN_OP -> PRECEDENCE
getDefaultUnaryPrecedence U_PLUS    = PRECEDENCE 0
getDefaultUnaryPrecedence U_MINUS   = PRECEDENCE 0
getDefaultUnaryPrecedence U_NOT     = PRECEDENCE 0
getDefaultUnaryPrecedence U_NEG     = PRECEDENCE 0

getDefaultBinaryPrecedence :: BIN_OP -> PRECEDENCE
getDefaultBinaryPrecedence BI_PLUS     = PRECEDENCE 0
getDefaultBinaryPrecedence BI_MINUS    = PRECEDENCE 0
getDefaultBinaryPrecedence BI_MULT     = PRECEDENCE 0
getDefaultBinaryPrecedence BI_DIV      = PRECEDENCE 0
getDefaultBinaryPrecedence BI_MOD      = PRECEDENCE 0
getDefaultBinaryPrecedence BI_LT       = PRECEDENCE 0
getDefaultBinaryPrecedence BI_LTE      = PRECEDENCE 0
getDefaultBinaryPrecedence BI_GT       = PRECEDENCE 0
getDefaultBinaryPrecedence BI_GTE      = PRECEDENCE 0
getDefaultBinaryPrecedence BI_EQ       = PRECEDENCE 0
getDefaultBinaryPrecedence BI_NEQ      = PRECEDENCE 0
getDefaultBinaryPrecedence BI_ASSIGN   = PRECEDENCE 0
