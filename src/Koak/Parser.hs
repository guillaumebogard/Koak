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

import Koak.Lexer as KL     (Token(..))

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
parseKoak tokens = let (kdefs, rest) = parseKDefs tokens in kdefs : parseKoak rest

parseKDefs :: [Token] -> (KDEFS, [Token])
parseKDefs []                = throw $ newParsingError "parseKdefs" [Word "def"] Nothing []
parseKDefs ((Word "def"):xs) = let (def, rest)  = parseDefs xs          in (KDEFS_DEFS def, rest)
parseKDefs list              = let (expr, rest) = parseExpressions list in (KDEFS_EXPR expr, parseKDefsSemiColon rest)

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
parsePrototypeUnary' unop Nothing           identifier proto_args list = (PROTOTYPE_UNARY unop (getDefaultUnaryPrecedence unop) identifier proto_args, list)
parsePrototypeUnary' unop (Just precedence) identifier proto_args list = (PROTOTYPE_UNARY unop precedence                       identifier proto_args, list)

parsePrototypeBinary :: [Token] -> (PROTOTYPE, [Token])
parsePrototypeBinary []     = throw $ newParsingError "parsePrototypeBinary" [] Nothing []
parsePrototypeBinary list = let (binop,      rest   ) = parseBinOp            list   in
                            let (precedence, rest'  ) = parseMaybePrecedence  rest   in
                            let (identifier, rest'' ) = parseIdentifier       rest'  in
                            let (proto_args, rest''') = parsePrototypeArgs    rest'' in
                            parsePrototypeBinary' binop precedence identifier proto_args rest'''

parsePrototypeBinary' :: BIN_OP -> Maybe PRECEDENCE -> IDENTIFIER -> PROTOTYPE_ARGS -> [Token] -> (PROTOTYPE, [Token])
parsePrototypeBinary' binop Nothing         identifier proto_args list = (PROTOTYPE_BINARY binop (getDefaultBinaryPrecedence binop) identifier proto_args, list)
parsePrototypeBinary' binop (Just precedence) identifier proto_args list = (PROTOTYPE_BINARY binop precedence                         identifier proto_args, list)

parsePrototype' :: [Token] -> (PROTOTYPE, [Token])
parsePrototype' []      = throw $ newParsingError "parsePrototype'" [] Nothing []
parsePrototype' list    = let (identifier, rest ) = parseIdentifier    list  in
                          let (arguments,  rest') = parsePrototypeArgs rest  in
                          (PROTOTYPE identifier arguments, rest')

parsePrototypeArgs :: [Token] -> (PROTOTYPE_ARGS, [Token])
parsePrototypeArgs []     = throw $ newParsingError "parsePrototypeArgs" [] Nothing []
parsePrototypeArgs (x:xs) = let (p_list,      rest ) = parsePrototypeArgsList xs in
                            let (return_type, rest') = parsePrototypeArgsType $ parsePrototypeArgsParenthesis (x:rest)  in
                            (PROTOTYPE_ARGS p_list return_type, rest')

parsePrototypeArgsParenthesis :: [Token] -> [Token]
parsePrototypeArgsParenthesis (OpenParenthesis:ClosedParenthesis:xs) = xs
parsePrototypeArgsParenthesis (x:ClosedParenthesis:xs)               = throw $ newParsingError "parsePrototypeArgsParenthesis" [OpenParenthesis]                    (Just x) xs
parsePrototypeArgsParenthesis (OpenParenthesis:x:xs)                 = throw $ newParsingError "parsePrototypeArgsParenthesis" [ClosedParenthesis]                  (Just x) xs
parsePrototypeArgsParenthesis list                                   = throw $ newParsingError "parsePrototypeArgsParenthesis" [OpenParenthesis, ClosedParenthesis] Nothing  list

parsePrototypeArgsList :: [Token] -> ([PROTOTYPE_ID], [Token])
parsePrototypeArgsList list = let (p_list, tokens) = parsePrototypeArgsList' list [] in (reverse p_list, tokens)

parsePrototypeArgsList' :: [Token] -> [PROTOTYPE_ID] -> ([PROTOTYPE_ID], [Token])
parsePrototypeArgsList' list@(Word _:_) p_list = let (proto_id, rest) = parsePrototypeId list in parsePrototypeArgsList' rest (proto_id:p_list)
parsePrototypeArgsList' list            p_list = (p_list, list)

parsePrototypeArgsType :: [Token] -> (TYPE, [Token])
parsePrototypeArgsType []         = throw $ newParsingError "parsePrototypeArgsType" [Colon] Nothing []
parsePrototypeArgsType (Colon:xs) = parseType xs
parsePrototypeArgsType (x:xs)     = throw $ newParsingError "parsePrototypeArgsType" [Colon] (Just x) xs

parseMaybePrecedence :: [Token] -> (Maybe PRECEDENCE, [Token])
parseMaybePrecedence (Number n:xs) = (Just $ PRECEDENCE n, xs)
parseMaybePrecedence list          = (Nothing, list)

parsePrototypeId :: [Token] -> (PROTOTYPE_ID, [Token])
parsePrototypeId []   = throw $ newParsingError "parsePrototypeId" [] Nothing []
parsePrototypeId list = let (identifier, rest) = parseIdentifier list in parsePrototypeId' rest identifier

parsePrototypeId' :: [Token] -> IDENTIFIER -> (PROTOTYPE_ID, [Token])
parsePrototypeId' []         _          = throw $ newParsingError "parsePrototypeId" [Colon] Nothing []
parsePrototypeId' (Colon:xs) identifier = let (prototype_type, rest) = parseType xs in (PROTOTYPE_ID identifier prototype_type, rest)
parsePrototypeId' (x:xs)     _          = throw $ newParsingError "parsePrototypeId" [Colon] (Just x) xs

parseType :: [Token] -> (TYPE, [Token])
parseType []                 = throw $ newParsingError "parseType" [Word "int", Word "double", Word "void"] Nothing []
parseType (Word "int":xs)    = (INT, xs)
parseType (Word "double":xs) = (DOUBLE, xs)
parseType (Word "void":xs)   = (VOID, xs)
parseType (x:xs)             = throw $ newParsingError "parseType" [Word "int", Word "double", Word "void"] (Just x) xs

parseFor :: [Token] -> (FOR, [Token])
parseFor []              = throw $ newParsingError "parseIf" [Word "for"] Nothing []
parseFor (Word "for":xs) = let (assign_id, rest) = parseIdentifier xs in parseFor' rest assign_id
parseFor (x:xs)          = throw $ newParsingError "parseIf" [Word "for"] (Just x) xs

parseFor' :: [Token] -> IDENTIFIER -> (FOR, [Token])
parseFor' []         _          = throw $ newParsingError "parseIf" [Assign] Nothing []
parseFor' (Assign:xs) assign_id = let (assign_expr, rest) = parseExpression xs in parseFor'' rest assign_id assign_expr
parseFor' (x:xs)     _          = throw $ newParsingError "parseIf" [Assign] (Just x) xs

parseFor'' :: [Token] -> IDENTIFIER -> EXPRESSION -> (FOR, [Token])
parseFor'' []         _          _          = throw $ newParsingError "parseIf" [Comma] Nothing []
parseFor'' (Comma:xs) assign_id assign_expr = let (cmp_id, rest) = parseIdentifier xs in parseFor''' rest assign_id assign_expr cmp_id
parseFor'' (x:xs)     _          _          = throw $ newParsingError "parseIf" [Comma] (Just x) xs

parseFor''' :: [Token] -> IDENTIFIER -> EXPRESSION -> IDENTIFIER -> (FOR, [Token])
parseFor''' []         _          _          _      = throw $ newParsingError "parseIf" [Lower] Nothing []
parseFor''' (Lower:xs) assign_id assign_expr cmp_id = let (cmp_expr, rest) = parseExpression xs in parseFor'''' rest assign_id assign_expr cmp_id cmp_expr
parseFor''' (x:xs)     _          _          _      = throw $ newParsingError "parseIf" [Lower] (Just x) xs

parseFor'''' :: [Token] -> IDENTIFIER -> EXPRESSION -> IDENTIFIER -> EXPRESSION -> (FOR, [Token])
parseFor'''' []             _          _          _      _    = throw $ newParsingError "parseIf" [Comma] Nothing []
parseFor'''' (Comma:xs) assign_id assign_expr cmp_id cmp_expr = let (inc_expr, rest) = parseExpression xs in parseFor''''' rest assign_id assign_expr cmp_id cmp_expr inc_expr
parseFor'''' (x:xs)         _          _          _      _    = throw $ newParsingError "parseIf" [Comma] (Just x) xs

parseFor''''' :: [Token] -> IDENTIFIER -> EXPRESSION -> IDENTIFIER -> EXPRESSION -> EXPRESSION -> (FOR, [Token])
parseFor''''' []             _          _           _     _        _        = throw $ newParsingError "parseIf" [Word "in"] Nothing []
parseFor''''' (Word "in":xs) assign_id assign_expr cmp_id cmp_expr inc_expr =
    let (core_expr, rest) = parseExpressions xs in
    (FOR assign_id assign_expr cmp_id cmp_expr inc_expr core_expr, rest)
parseFor''''' (x:xs)         _          _           _     _        _        = throw $ newParsingError "parseIf" [Word "in"] (Just x) xs

parseIf :: [Token] -> (IF, [Token])
parseIf []             = throw $ newParsingError "parseIf" [Word "if"] Nothing []
parseIf (Word "if":xs) = let (expr, rest) = parseExpression xs in parseIfThen rest expr
parseIf (x:xs)         = throw $ newParsingError "parseIf" [Word "if"] (Just x) xs

parseIfThen :: [Token] -> EXPRESSION -> (IF, [Token])
parseIfThen []               _    = throw $ newParsingError "parseIf" [Word "then"] Nothing []
parseIfThen (Word "then":xs) expr = let (exprs, rest) = parseExpressions xs in parseIf' rest expr exprs
parseIfThen (x:xs)           _    = throw $ newParsingError "parseIf" [Word "then"] (Just x) xs

parseIf' :: [Token] -> EXPRESSION -> EXPRESSIONS -> (IF, [Token])
parseIf' (Word "else":xs) expr exprs = let (exprs', rest) = parseExpressions xs in
                                       (IF expr exprs (Just exprs'), rest)
parseIf' rest             expr exprs = (IF expr exprs Nothing,       rest)

parseWhile :: [Token] -> (WHILE, [Token])
parseWhile []                = throw $ newParsingError "parseWhile" [Word "while"] Nothing []
parseWhile (Word "while":xs) = let (expr, rest) = parseExpression xs in parseWhileDo rest expr
parseWhile (x:xs)            = throw $ newParsingError "parseWhile" [Word "while"] (Just x) xs

parseWhileDo :: [Token] -> EXPRESSION -> (WHILE, [Token])
parseWhileDo []             _    = throw $ newParsingError "parseWhile" [Word "do"] Nothing []
parseWhileDo (Word "do":xs) expr = let (exprs, rest) = parseExpressions xs in (WHILE expr exprs, rest)
parseWhileDo (x:xs)         _    = throw $ newParsingError "parseWhile" [Word "do"] (Just x) xs

parseExpressions :: [Token] -> (EXPRESSIONS, [Token])
parseExpressions []                    = throw $ newParsingError "parseExpressions" [Word "for", Word "if", Word "while", Word "{expression}"] Nothing []
parseExpressions list@(Word "for":  _) = let (for_expr,   rest)  = parseFor list           in (FOR_EXPR for_expr,         rest )
parseExpressions list@(Word "if":   _) = let (if_expr,    rest)  = parseIf list            in (IF_EXPR if_expr,           rest )
parseExpressions list@(Word "while":_) = let (while_expr, rest)  = parseWhile list         in (WHILE_EXPR while_expr,     rest )
parseExpressions list                  = let (expr,       rest)  = parseExpression list    in
                                         let (arr_expr,   rest') = parseArrExpression rest in (EXPRESSIONS expr arr_expr, rest')

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
parseArrExpression list = let (arrExpr, rest) = parseArrExpression' list in (arrExpr, rest)

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
parseCallExprArg []           = throw $ newParsingError "parseCallExprArg" [] Nothing []
parseCallExprArg tokens@(_:_) = let (first, other , rest) = parseCallExprArg' tokens in (CALL_EXPR_ARGS first other, rest)

parseCallExprArg' :: [Token] -> (EXPRESSION, [EXPRESSION], [Token])
parseCallExprArg' list = let (expr,     rest ) = parseExpression list    in
                         let (exprList, rest') = parseCallExprArg'' rest in
                         (expr, exprList, rest')

parseCallExprArg'' :: [Token] -> ([EXPRESSION], [Token])
parseCallExprArg'' []                     = throw $ newParsingError "parseCallExprArg" [Comma, ClosedParenthesis] Nothing []
parseCallExprArg'' (ClosedParenthesis:xs) = ([], xs)
parseCallExprArg'' (Comma:xs)             = let (expr, rest ) = parseExpression xs      in
                                            let (next, rest') = parseCallExprArg'' rest in
                                            (expr : next, rest')
parseCallExprArg'' (x:xs)                 = throw $ newParsingError "parseCallExprArg" [Comma, ClosedParenthesis] (Just x) xs

parsePrimary :: [Token] -> (PRIMARY, [Token])
parsePrimary []                      = throw $ newParsingError "parsePrimary" [Word "{variable}", Number 0, OpenParenthesis] Nothing []
parsePrimary (Word x : xs)           = (PRIMARY_IDENTIFIER (IDENTIFIER x), xs)
parsePrimary (Number x : xs)         = (PRIMARY_LITERAL $ LITERAL_DECIMAL $ DECIMAL_CONST  x, xs)
parsePrimary (FloatingNumber x : xs) = (PRIMARY_LITERAL $ LITERAL_DOUBLE  $ DOUBLE_CONST   x, xs)
parsePrimary (OpenParenthesis : xs)  = let (exprs, rest) = parseExpressions xs in parsePrimaryExpression (OpenParenthesis : rest) exprs
parsePrimary (x:xs)                  = throw $ newParsingError "parsePrimary" [Word "{variable}", Number 0, OpenParenthesis] (Just x) xs

parsePrimaryExpression :: [Token] -> EXPRESSIONS -> (PRIMARY, [Token])
parsePrimaryExpression []                                     _     = throw $ newParsingError "parsePrimaryExpression" [OpenParenthesis, ClosedParenthesis] Nothing []
parsePrimaryExpression (OpenParenthesis:ClosedParenthesis:xs) exprs = (PRIMARY_EXPRS exprs, xs)
parsePrimaryExpression (x:              ClosedParenthesis:xs) _     = throw $ newParsingError "parsePrimaryExpression" [OpenParenthesis]   (Just x) xs
parsePrimaryExpression (OpenParenthesis:x                :xs) _     = throw $ newParsingError "parsePrimaryExpression" [ClosedParenthesis] (Just x) xs
parsePrimaryExpression list                                   _     = throw $ newParsingError "parsePrimaryExpression" [OpenParenthesis, ClosedParenthesis] Nothing list

parseIdentifier :: [Token] -> (IDENTIFIER, [Token])
parseIdentifier []            = throw $ newParsingError "parseIdentifier" [Word "{any}"] Nothing []
parseIdentifier ((Word w):xs) = (IDENTIFIER w, xs)
parseIdentifier (x:xs)        = throw $ newParsingError "parseIdentifier" [Word "{any}"] (Just x) xs

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

-- isUnaryOp :: Token -> Bool
-- isUnaryOp KL.Plus       = True
-- isUnaryOp KL.Minus      = True
-- isUnaryOp KL.LogicalNot = True
-- isUnaryOp _             = False

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

newParsingError :: String -> [Token] -> Maybe Token -> [Token] -> KoakException
newParsingError at expected actual rest = KoakParserMissingToken at (show expected) (show actual) (show rest)
