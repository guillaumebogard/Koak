--
-- EPITECH PROJECT, 2022
-- koak
-- File description:
-- Koak.Parser
--

module Koak.Parser          ( parseKoak
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

import Exception            ( KoakException(KoakParserMissingToken))

import Control.Exception    ( throw )

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

data UN_OP          = UN_NOT
                    | UN_MINUS
                    | UN_PLUS
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
parseKdefs []                = throw $ newParsingError "parseKdefs" [Word "def"] Nothing []
parseKdefs ((Word "def"):xs) = let (def, rest)  = parseDefs xs          in (KDEFS_DEFS def, rest)
parseKdefs list              = let (expr, rest) = parseExpressions list in (KDEFS_EXPR expr, parseKDefsSemiColon rest)

parseDefs :: [Token] -> (DEFS, [Token])
parseDefs []     = throw $ newParsingError "parseDefs" [] Nothing []
parseDefs tokens = let (prototype  , rest ) = parsePrototype   tokens in
                   let (expressions, rest') = parseExpressions rest   in
                   (DEFS prototype expressions, parseKDefsSemiColon rest')

parseKDefsSemiColon :: [Token] -> [Token]
parseKDefsSemiColon []             = throw $ newParsingError "parseDefs" [SemiColon] Nothing []
parseKDefsSemiColon (SemiColon:xs) = xs
parseKDefsSemiColon (x:xs)         = throw $ newParsingError "parseDefs" [SemiColon] (Just x) xs

parsePrototype :: [Token] -> (PROTOTYPE, [Token])
parsePrototype []                   = throw $ newParsingError "parsePrototype" [Word "unary", Word "binary", Word "{function name}"] Nothing []
parsePrototype ((Word "unary"):xs)  = parsePrototypeUnary  xs
parsePrototype ((Word "binary"):xs) = parsePrototypeBinary xs
parsePrototype list@((Word _):_)    = parsePrototype'      list
parsePrototype (x:xs)               = throw $ newParsingError "parsePrototype" [Word "unary", Word "binary", Word "{function name}"] (Just x) xs

parsePrototypeUnary :: [Token] -> (PROTOTYPE, [Token])
parsePrototypeUnary []     = throw $ newParsingError "parsePrototypeUnary" [] Nothing []
parsePrototypeUnary tokens = let (unop,       rest   ) = parseUnOp            tokens in
                             let (precedence,       rest'  ) = parseMaybePrecedence rest   in
                             let (identifier, rest'' ) = parseIdentifier      rest'  in
                             let (proto_args, rest''') = parsePrototypeArgs   rest'' in
                             parsePrototypeUnary' unop precedence identifier proto_args rest'''

parsePrototypeUnary' :: UN_OP -> Maybe PRECEDENCE -> IDENTIFIER -> PROTOTYPE_ARGS -> [Token] -> (PROTOTYPE, [Token])
parsePrototypeUnary' unop (Nothing)         identifier proto_args list = (PROTOTYPE_UNARY unop (getDefaultUnaryPrecedence unop) identifier proto_args, list)
parsePrototypeUnary' unop (Just precedence) identifier proto_args list = (PROTOTYPE_UNARY unop precedence                       identifier proto_args, list)

parsePrototypeBinary :: [Token] -> (PROTOTYPE, [Token])
parsePrototypeBinary []     = throw $ newParsingError "parsePrototypeBinary" [] Nothing []
parsePrototypeBinary list = let (binop,      rest   ) = parseBinOp            list   in
                            let (precedence, rest'  ) = parseMaybePrecedence  rest   in
                            let (identifier, rest'' ) = parseIdentifier       rest'  in
                            let (proto_args, rest''') = parsePrototypeArgs    rest'' in
                            parsePrototypeBinary' binop precedence identifier proto_args rest'''

parsePrototypeBinary' :: BIN_OP -> Maybe PRECEDENCE -> IDENTIFIER -> PROTOTYPE_ARGS -> [Token] -> (PROTOTYPE, [Token])
parsePrototypeBinary' binop (Nothing)         identifier proto_args list = (PROTOTYPE_BINARY binop (getDefaultBinaryPrecedence binop) identifier proto_args, list)
parsePrototypeBinary' binop (Just precedence) identifier proto_args list = (PROTOTYPE_BINARY binop precedence                         identifier proto_args, list)

parsePrototype' :: [Token] -> (PROTOTYPE, [Token])
parsePrototype' []      = throw $ newParsingError "parsePrototype'" [] Nothing []
parsePrototype' list    = let (identifier, rest ) = parseIdentifier    list  in
                          let (arguments,  rest') = parsePrototypeArgs rest  in
                          (PROTOTYPE identifier arguments, rest')

parsePrototypeArgs :: [Token] -> (PROTOTYPE_ARGS, [Token])
parsePrototypeArgs []     = throw $ newParsingError "parsePrototypeArgs" [] Nothing []
parsePrototypeArgs (x:xs) = let (p_list,      rest ) = parsePrototypeArgsList xs in
                            let (return_type, rest') = parsePrototypeArgsType $ parsePrototypeArgsParenthesis $ x:rest  in
                            (PROTOTYPE_ARGS p_list return_type, rest')

parsePrototypeArgsParenthesis :: [Token] -> [Token]
parsePrototypeArgsParenthesis (OpenParenthesis:ClosedParenthesis:xs) = xs
parsePrototypeArgsParenthesis (x:ClosedParenthesis:xs)               = throw $ newParsingError "parsePrototypeArgsParenthesis" [OpenParenthesis] (Just x) xs
parsePrototypeArgsParenthesis (OpenParenthesis:x:xs)                 = throw $ newParsingError "parsePrototypeArgsParenthesis" [ClosedParenthesis] (Just x) xs
parsePrototypeArgsParenthesis list                                   = throw $ newParsingError "parsePrototypeArgsParenthesis" [OpenParenthesis, ClosedParenthesis] Nothing list

parsePrototypeArgsList :: [Token] -> ([PROTOTYPE_ID], [Token])
parsePrototypeArgsList list = let (p_list, tokens) = parsePrototypeArgsList' list [] in (reverse p_list, tokens)

parsePrototypeArgsList' :: [Token] -> [PROTOTYPE_ID] -> ([PROTOTYPE_ID], [Token])
parsePrototypeArgsList' list@(Word _:_) p_list = let (proto_id, rest) = parsePrototypeId list in parsePrototypeArgsList' rest (proto_id:p_list)
parsePrototypeArgsList' list            p_list = (p_list, list)

parsePrototypeArgsType :: [Token] -> (TYPE, [Token])
parsePrototypeArgsType []         = throw $ newParsingError "parsePrototypeArgsType" [Colon] Nothing []
parsePrototypeArgsType (Colon:xs) = parseType xs
parsePrototypeArgsType (x:xs)     = throw $ newParsingError "parsePrototypeArgsType" [Colon] (Just x) xs

parsePrecedence :: [Token] -> (PRECEDENCE, [Token])
parsePrecedence list = parsePrecedence' $ parseMaybePrecedence list

parsePrecedence' :: (Maybe PRECEDENCE, [Token]) -> (PRECEDENCE, [Token])
parsePrecedence' (Nothing,   list) = throw $ newParsingError "parsePrecedence" [Number 0] Nothing list
parsePrecedence' (Just p,    list) = (p, list)

parseMaybePrecedence :: [Token] -> (Maybe PRECEDENCE, [Token])
parseMaybePrecedence (Number n:xs) = (Just $ PRECEDENCE $ round n, xs)
parseMaybePrecedence list          = (Nothing, list)

parsePrototypeId :: [Token] -> (PROTOTYPE_ID, [Token])
parsePrototypeId []   = throw $ newParsingError "parsePrototypeId" [] Nothing []
parsePrototypeId list = let (identifier,     rest ) = parseIdentifier list in
                        let (prototype_type, rest') = parseType rest       in
                        (PROTOTYPE_ID identifier prototype_type, rest')

parseType :: [Token] -> (TYPE, [Token])
parseType []                 = throw $ newParsingError "parseType" [Word "int", Word "double", Word "void"] Nothing []
parseType (Word "int":xs)    = (INT, xs)
parseType (Word "double":xs) = (DOUBLE, xs)
parseType (Word "void":xs)   = (VOID, xs)
parseType (x:xs)             = throw $ newParsingError "parseType" [Word "int", Word "double", Word "void"] (Just x) xs

parseFor :: [Token] -> (FOR, [Token])
parseFor [] = error "parseFor: empty list"
parseFor _  = error "Not Implemented"

parseIf :: [Token] -> (IF, [Token])
parseIf [] = error "parseIf: empty list"
parseIf _  = error "Not Implemented"

parseWhile :: [Token] -> (WHILE, [Token])
parseWhile [] = error "parseWhile: empty list"
parseWhile _  = error "Not Implemented"


parseExpressions :: [Token] -> (EXPRESSIONS, [Token])
parseExpressions []                = throw $ newParsingError "parseExpressions" [Word "for", Word "if", Word "while", Word "{expression}"] Nothing []
parseExpressions (Word "for":  xs) = let (for,     rest)  = parseFor xs             in (FOR_EXPR for,     rest)
parseExpressions (Word "if":   xs) = let (if_,     rest)  = parseIf xs              in (IF_EXPR if_,      rest)
parseExpressions (Word "while":xs) = let (while,   rest)  = parseWhile xs           in (WHILE_EXPR while, rest)
parseExpressions list              = let (expr,    rest)  = parseExpression list    in
                                     let (arrExpr, rest') = parseArrExpression rest in (EXPRESSIONS expr arrExpr, rest')

parseExpression :: [Token] -> (EXPRESSION, [Token])
parseExpression []     = throw $ newParsingError "parseExpression" [] Nothing []
parseExpression tokens = let (unary, rest)   = parseUnary tokens     in
                         let (binops, rest') = parseExpression' rest in
                         (EXPRESSION unary binops, rest')

parseExpression' :: [Token] -> ([(BIN_OP, UNARY)], [Token])
parseExpression' []            = ([], [])
parseExpression' tokens@(x:xs)
    | isBinaryOp x             = let (unary, rest  ) = parseUnary xs          in
                                 let (binops, rest') = parseExpression' rest  in
                                 ((getBinaryOp x, unary) : binops, rest')
    | otherwise                = ([], tokens)

parseArrExpression :: [Token] -> ([EXPRESSION], [Token])
parseArrExpression list = let (arrExpr, rest) = parseArrExpression' list in (reverse arrExpr, rest)

parseArrExpression' :: [Token] -> ([EXPRESSION], [Token])
parseArrExpression' (Colon:xs) = let (first, rest ) = parseExpression xs       in
                                 let (next,  rest') = parseArrExpression' rest in (first : next, rest')
parseArrExpression' xs         = ([], xs)

parseUnary :: [Token] -> (UNARY, [Token])
parseUnary []          = throw $ newParsingError "parseUnary" [] Nothing []
parseUnary list@(x:xs)
  | isBinaryOp x       = let (unop, rest)    = parseUnary xs     in
                      (UNARY_UN (getUnaryOp x) unop, rest)
  | otherwise          = let (postfix, rest) = parsePostfix list in
                      (UNARY_POSTFIX postfix, rest)

parsePostfix :: [Token] -> (POSTFIX, [Token])
parsePostfix []     = throw $ newParsingError "parsePostfix" [] Nothing []
parsePostfix tokens = let (prim, rest)      = parsePrimary tokens     in
                      let (callExpr, rest') = parseMaybeCallExpr rest in
                      (POSTFIX prim callExpr, rest')

parseMaybeCallExpr :: [Token] -> (Maybe CALL_EXPR, [Token])
parseMaybeCallExpr []                       = (Nothing, [])
parseMaybeCallExpr list@(OpenParenthesis:_) = let (callExpr, rest) = parseCallExpr list in (Just callExpr, rest)
parseMaybeCallExpr list                     = (Nothing, list)

parseCallExpr :: [Token] -> (CALL_EXPR, [Token])
parseCallExpr []                                         = throw $ newParsingError "parseCallExpr" [OpenParenthesis, ClosedParenthesis] Nothing []
parseCallExpr (OpenParenthesis : ClosedParenthesis : xs) = (CALL_EXPR Nothing, xs)
parseCallExpr (OpenParenthesis : xs)                     = let (callExpr, rest) = parseCallExprArg xs in
                                                           (CALL_EXPR (Just callExpr), rest)
parseCallExpr (x:xs) = throw $ newParsingError "parseCallExpr" [OpenParenthesis, ClosedParenthesis] (Just x) xs

parseCallExprArg :: [Token] -> (CALL_EXPR_ARGS, [Token])
parseCallExprArg []            = throw $ newParsingError "parseCallExprArg" [] Nothing []
parseCallExprArg tokens@(x:xs) = let (first, other , rest) = parseCallExprArg' tokens in (CALL_EXPR_ARGS first other, rest)

parseCallExprArg' :: [Token] -> (EXPRESSION, [EXPRESSION], [Token])
parseCallExprArg' list = let (expr,     rest ) = parseExpression list    in
                         let (exprList, rest') = parseCallExprArg'' rest in
                         (expr, reverse exprList, rest')

parseCallExprArg'' :: [Token] -> ([EXPRESSION], [Token])
parseCallExprArg'' []                     = throw $ newParsingError "parseCallExprArg" [Comma, ClosedParenthesis] Nothing []
parseCallExprArg'' (ClosedParenthesis:xs) = ([], xs)
parseCallExprArg'' (Comma:xs)             = let (expr, rest ) = parseExpression xs      in
                                            let (next, rest') = parseCallExprArg'' rest in
                                            (expr : next, rest')
parseCallExprArg'' (x:xs)                 = throw $ newParsingError "parseCallExprArg" [Comma, ClosedParenthesis] (Just x) xs

parsePrimary :: [Token] -> (PRIMARY, [Token])
parsePrimary []                     = throw $ newParsingError "parsePrimary" [Word "{variable}", Number 0, OpenParenthesis] Nothing []
parsePrimary (Word x : xs)          = (PRIMARY_IDENTIFIER (IDENTIFIER x), xs)
parsePrimary (Number x : xs)        = (PRIMARY_LITERAL $ LITERAL_DOUBLE $ DOUBLE_CONST  x, xs)
parsePrimary (OpenParenthesis : xs) =
                let (expr, rest)    = parseExpressions xs
                in (PRIMARY_EXPRS expr, rest)
parsePrimary (x:xs)                 = throw $ newParsingError "parsePrimary" [Word "{variable}", Number 0, OpenParenthesis] (Just x) xs

parseIdentifier :: [Token] -> (IDENTIFIER, [Token])
parseIdentifier []            = throw $ newParsingError "parseIdentifier" [Word "{any}"] Nothing []
parseIdentifier ((Word w):xs) = (IDENTIFIER w, xs)
parseIdentifier (x:xs)        = throw $ newParsingError "parseIdentifier" [Word "{any}"] (Just x) xs

isValidIdentifier :: [Token] -> Bool
isValidIdentifier ((Word _):_) = True
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

parseLitteral :: [Token] -> (LITERAL, [Token])
parseLitteral []                = throw $ newParsingError "parseLitteral" [Number 0] Nothing []
parseLitteral ((Number x) : xs) = (LITERAL_DOUBLE $ DOUBLE_CONST x, xs)
parseLitteral (x:xs)            = throw $ newParsingError "parseLitteral" [Number 0] (Just x) xs

-- parseExpressions :: [Token] -> (EXPRESSIONS, [Token])
-- parseExpressions _ = error "parseExpressions: empty list"
-- parseExpressions list@(x:xs)
--     | x == Word "for"   = let (for, rest)   = parseFor xs in (FOR_EXPR for, rest)
--     | x == Word "if"    = let (if_, rest)   = parseIf xs in (IF_EXPR if_, rest)
--     | x == Word "while" = let (while, rest) = parseWhile xs in (WHILE_EXPR while, rest)
--     | otherwise = let (expr, rest) = parseExpression list in (EXPRESSIONS expr [], rest)

parseBinOp :: [Token] -> (BIN_OP, [Token])
parseBinOp []                = throw $ newParsingError "parseBinOp" [Plus, Minus, Multiply, Divide, Modulo, Lower, LowerEqual, Greater, GreaterEqual, Equal, NotEqual, Assign] Nothing []
parseBinOp (Plus:xs)         = (BIN_PLUS,   xs)
parseBinOp (Minus:xs)        = (BIN_MINUS,  xs)
parseBinOp (Multiply:xs)     = (BIN_MULT,   xs)
parseBinOp (Divide:xs)       = (BIN_DIV,    xs)
parseBinOp (Modulo:xs)       = (BIN_MOD,    xs)
parseBinOp (Lower:xs)        = (BIN_LT,     xs)
parseBinOp (LowerEqual:xs)   = (BIN_LTE,    xs)
parseBinOp (Greater:xs)      = (BIN_GT,     xs)
parseBinOp (GreaterEqual:xs) = (BIN_GTE,    xs)
parseBinOp (Equal:xs)        = (BIN_EQ,     xs)
parseBinOp (NotEqual:xs)     = (BIN_NEQ,    xs)
parseBinOp (Assign:xs)       = (BIN_ASSIGN, xs)
parseBinOp (x:xs)            = throw $ newParsingError "parseBinOp" [Plus, Minus, Multiply, Divide, Modulo, Lower, LowerEqual, Greater, GreaterEqual, Equal, NotEqual, Assign] (Just x) xs

parseUnOp :: [Token] -> (UN_OP, [Token])
parseUnOp []              = throw $ newParsingError "parseUnOp" [Plus, Minus, LogicalNot] Nothing []
parseUnOp (Plus:xs)       = (UN_PLUS,  xs)
parseUnOp (Minus:xs)      = (UN_MINUS, xs)
parseUnOp (LogicalNot:xs) = (UN_NOT,   xs)
parseUnOp (x:xs)          = throw $ newParsingError "parseUnOp" [Plus, Minus, LogicalNot] (Just x) xs

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

getBinaryOp :: Token -> BIN_OP
getBinaryOp KL.Plus          = BIN_PLUS
getBinaryOp KL.Minus         = BIN_MINUS
getBinaryOp KL.Multiply      = BIN_MULT
getBinaryOp KL.Divide        = BIN_DIV
getBinaryOp KL.Modulo        = BIN_MOD
getBinaryOp KL.Lower         = BIN_LT
getBinaryOp KL.LowerEqual    = BIN_LTE
getBinaryOp KL.Greater       = BIN_GT
getBinaryOp KL.GreaterEqual  = BIN_GTE
getBinaryOp KL.Equal         = BIN_EQ
getBinaryOp KL.NotEqual      = BIN_NEQ
getBinaryOp KL.Assign        = BIN_ASSIGN
getBinaryOp t                = throw $ newParsingError "getBinaryOp" [Plus, Minus, Multiply, Divide, Modulo, Lower, LowerEqual, Greater, GreaterEqual, Equal, NotEqual, Assign] (Just t) []

isUnaryOp :: Token -> Bool
isUnaryOp KL.Plus       = True
isUnaryOp KL.Minus      = True
isUnaryOp KL.LogicalNot = True
isUnaryOp _             = False

getUnaryOp :: Token -> UN_OP
getUnaryOp KL.Plus          = UN_PLUS
getUnaryOp KL.Minus         = UN_MINUS
getUnaryOp KL.LogicalNot    = UN_NOT
getUnaryOp t                = throw $ newParsingError "getUnaryOp" [Plus, Minus, LogicalNot] (Just t) []

getDefaultUnaryPrecedence :: UN_OP -> PRECEDENCE
getDefaultUnaryPrecedence UN_PLUS    = PRECEDENCE 0
getDefaultUnaryPrecedence UN_MINUS   = PRECEDENCE 0
getDefaultUnaryPrecedence UN_NOT     = PRECEDENCE 0

getDefaultBinaryPrecedence :: BIN_OP -> PRECEDENCE
getDefaultBinaryPrecedence BIN_PLUS     = PRECEDENCE 0
getDefaultBinaryPrecedence BIN_MINUS    = PRECEDENCE 0
getDefaultBinaryPrecedence BIN_MULT     = PRECEDENCE 0
getDefaultBinaryPrecedence BIN_DIV      = PRECEDENCE 0
getDefaultBinaryPrecedence BIN_MOD      = PRECEDENCE 0
getDefaultBinaryPrecedence BIN_LT       = PRECEDENCE 0
getDefaultBinaryPrecedence BIN_LTE      = PRECEDENCE 0
getDefaultBinaryPrecedence BIN_GT       = PRECEDENCE 0
getDefaultBinaryPrecedence BIN_GTE      = PRECEDENCE 0
getDefaultBinaryPrecedence BIN_EQ       = PRECEDENCE 0
getDefaultBinaryPrecedence BIN_NEQ      = PRECEDENCE 0
getDefaultBinaryPrecedence BIN_ASSIGN   = PRECEDENCE 0

newParsingError :: String -> [Token] -> (Maybe Token) -> [Token] -> KoakException
newParsingError at expected actual rest = KoakParserMissingToken at (show expected) (show actual) (show rest)
