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

data FOR            = FOR IDENTIFIER EXPRESSION IDENTIFIER EXPRESSION EXPRESSION EXPRESSION
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

data BIN_OP         = BI_PLUS
                    | BI_MINUS
                    | BI_MULT
                    | BI_DIV
                    | BI_MOD
                    | BI_LT
                    | BI_LTE
                    | BI_GT
                    | BI_GTE
                    | BI_EQ
                    | BI_NEQ
                    | BI_ASSIGN
    deriving (Eq, Show)

data BINARY_OP      = BINARY_OP_UN BIN_OP UNARY
                    | BINARY_OP_EXPR BIN_OP EXPRESSION
    deriving (Eq, Show)

data EXPRESSION     = EXPRESSION UNARY [BINARY_OP]
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
parseKdefs []                  = error "parseKdefs: empty list"
parseKdefs ((Word "def"):xs)
    | (last xs)   == SemiColon = error "parseKdefs: expecting ';'"
    | otherwise                = let (def, rest)   = parseDefs xs          in (KDEFS_DEFS def, rest)
parseKdefs list
    | (last list) == SemiColon = error "parseKdefs: expecting ';'"
    | otherwise                = let (exprs, rest) = parseExpressions list in (KDEFS_EXPR exprs, rest)

parseDefs :: [Token] -> (DEFS, [Token])
parseDefs [] = error "parseDefs: empty list"
parseDefs tokens = let (proto, rest1) = parsePrototype tokens  in
                   let (exprs, rest2) = parseExpressions rest1 in
                   (DEFS proto exprs, rest2)


-- identity :: a -> a
-- toto :: Char -> Int
-- tutu :: Int -> Char

-- wawa :: Int -> Int
-- wawa a = identity identity identity identity identity identity toto identity identity identity identity identity identity tutu identity identity identity identity identity a

parsePrototype :: [Token] -> (PROTOTYPE, [Token])
parsePrototype []                   = error "parseDefs: empty list"
parsePrototype ((Word "unary"):xs)  = parsePrototypeUnary  xs
parsePrototype ((Word "binary"):xs) = parsePrototypeBinary xs
parsePrototype ((Word w):xs)        = parsePrototype'      xs

parsePrototypeUnary :: [Token] -> (PROTOTYPE, [Token])
parsePrototypeUnary [] = error "parsePrototypeUnary: empty list"
parsePrototypeUnary list    = parsePrototypeArgs <<<< parseIdentifier <<<< parsePrecedence <<<< parseUnOp <<<< (PROTOTYPE_UNARY, list)
-- parsePrototypeUnary list    = (PROTOTYPE_UNARY, list) >>>> parseUnOp >>>> parsePrecedence >>>> parseIdentifier >>>> parsePrototypeArgs


-- parsePrototypeUnary list    = parsePrototypeArgs <<<< parseIdentifier <<<< parsePrecedence <<<< parseUnOp <<<< (PROTOTYPE_UNARY, list)
    --  parseUnop (\x y -> PROTOTYPE_UNARY x y )
                                            --   let (unop, rest1) = parseUnOp          xs    in
                                            --   let (prec, rest2) = parsePrecedence    rest1 in
                                            --   let (id,   rest3) = parseIdentifier    rest2 in
                                            --   let (args, rest4) = parsePrototypeArgs rest3 in
                                            --   PROTOTYPE_UNARY unop (Just prec) id args rest4
-- parsePrototypeUnary list                    = let (unop, rest1) = parseUnOp          xs    in
--                                               let (id,   rest2) = parseIdentifier    rest1 in
--                                               let (args, rest3) = parsePrototypeArgs rest2 in
--                                               PROTOTYPE_UNARY unop (getDefaultUnaryPrecedence unop) id args rest3

(<<<<) ::   ([Token] -> (elementParsed, [Token])) ->
            ([Token] , (elementParsed -> partialParsed)) ->
            ([Token], (partialParsed))
(<<<<) fparse (t, f1)  = let (result, rest) = fparse t in (rest, f1 result)


(>>>>) ::   ([Token] , (elementParsed -> partialParsed)) ->
            ([Token] -> (elementParsed, [Token])) ->
            ((partialParsed), [Token])
(>>>>) (t, f1) fparse = let (result, rest) = fparse t in (f1 result, rest)


-- parseUnary' :: (UN_OP -> PRECEDENCE -> IDENTIFIER -> PROTOTYPE_ARGS -> PROTOTYPE) -> [Token] -> (PROTOTYPE, [Token])
-- parseUnary' f1 t = let (unop, rest) = parseUnOp t in (parseUnary'' (f1 unop) rest)

-- parseUnary'' :: (PRECEDENCE -> IDENTIFIER -> PROTOTYPE_ARGS -> PROTOTYPE) -> [Token] -> (PROTOTYPE, [Token])
-- parseUnary'' f1 t = let (prec, rest) = parsePrecedence t in (parseUnary''' (f1 prec) rest)

-- parseUnary''' :: (IDENTIFIER -> PROTOTYPE_ARGS -> PROTOTYPE) -> [Token] -> (PROTOTYPE, [Token])
-- parseUnary''' f1 t = let (id, rest) = parseIdentifier t in (parseUnary''' (f1 id) rest)

-- parseUnary'''' :: (PROTOTYPE_ARGS -> PROTOTYPE) -> [Token] -> (PROTOTYPE, [Token])
-- parseUnary'''' f1 t =

-- parseUnary' :: UN_OP -> [Token] -> (PROTOTYPE, [Token])
-- parseUnary'' :: UN_OP -> PRECEDENCE -> [Token] -> (PROTOTYPE, [Token])
-- parseUnary''' :: UN_OP -> PRECEDENCE -> IDENTIFIER -> [Token] -> (PROTOTYPE, [Token])
-- parseUnary'''' :: UN_OP -> PRECEDENCE -> IDENTIFIER -> PROTOTYPE_ARGS -> [Token] -> (PROTOTYPE, [Token])




-- parseUnary' :: UN_OP -> [Token] -> (PROTOTYPE, [Token])
-- parseUnary' u t = escalier (parseUnary'' u) parsePrecedence t

-- parseUnary'' :: UN_OP -> PRECEDENCE -> [Token] -> (PROTOTYPE, [Token])
-- parseUnary'' u p t = escalier (parseUnary''' u p)  parseIdentifier t

-- parseUnary''' :: UN_OP -> PRECEDENCE -> IDENTIFIER -> [Token] -> (PROTOTYPE, [Token])
-- parseUnary''' u p i t = escalier (parseUnary'''' u p i) parsePrototypeArgs t

-- parseUnary'''' :: UN_OP -> PRECEDENCE -> IDENTIFIER -> PROTOTYPE_ARGS -> [Token] -> (PROTOTYPE, [Token])
-- parseUnary'''' u p i a t = (PROTOTYPE_UNARY u p i a, t)

-- data PROTOTYPE      = PROTOTYPE_UNARY  UN_OP  PRECEDENCE IDENTIFIER PROTOTYPE_ARGS
--                     | PROTOTYPE_BINARY BIN_OP PRECEDENCE IDENTIFIER PROTOTYPE_ARGS
--                     | PROTOTYPE IDENTIFIER PROTOTYPE_ARGS

-- parsePrecedence :: [Token] -> (PRECEDENCE, [Token])
-- parsePrecedence [] = error "parsePrecedence: empty list"
-- parsePrecedence _ = error "Not Implemented"


-- parseUnOpPrototypeUnary :: (UN_OP -> PRECEDENCE -> IDENTIFIER -> PROTOTYPE_ARGS -> PROTOTYPE) -> (PROTOTYPE, [Token])

-- escalier :: (a -> b -> [Token] -> (c, [Token])) -> ([Token] -> (b, [Token])) -> [Token] -> (c, [TOKEN])
-- escalier f1 f2 t = uncurry f1 $ f2 t

-- (==>>) :: (a -> b -> [Token] -> (c, [Token])) -> (Token -> a) -> [Token] -> b -> (c, [Token])
-- (==>>) f i (x:xs) j = f (i x) $ j xs


-- parseUnary''' u p i t = uncurry (parseUnary'''' u p i) $ parsePrototypeArgs t

-- [Token] -> (PROTOTYPE_ARGS, [Token])


-- parseUnOp :: (UN_OP -> a -> [Token] -> b) -> [Token] -> (b, [Token])
-- parseUnOp _ [] = error "parseUnOp: empty list"
-- parseUnOp f (x:xs) = f x xs

-- parseUnary' :: (UN_OP -> a -> [Token] -> b) -> [Token] -> (b, [Token])
-- parseUnary' _ [] = error "parseUnOp: empty list"
-- parseUnary' f (x:xs) = f x xs




-- parsePrecedence :: (PRECEDENCE -> a -> [Token] -> b) -> [Token] -> a -> (b, [Token])
-- parsePrecedence _ [] = error "parsePrecedence: empty list"
-- parsePrecedence f (x:xs) = f x xs

-- f(a, b, c, d) = f(a(b(c(d))))

-- data PROTOTYPE      = PROTOTYPE_UNARY  UN_OP  PRECEDENCE IDENTIFIER PROTOTYPE_ARGS
--                     | PROTOTYPE_BINARY BIN_OP PRECEDENCE IDENTIFIER PROTOTYPE_ARGS
--                     | PROTOTYPE IDENTIFIER PROTOTYPE_ARGS

parsePrototypeBinary :: [Token] -> (PROTOTYPE, [Token])
parsePrototypeBinary [] = error "parsePrototypeBinary: empty list"
parsePrototypeBinary list@((Number n):xs)   = let (binop, rest1) = parseBinOp          xs   in
                                              let (prec,  rest2) = parsePrecedence    rest1 in
                                              let (id,    rest3) = parseIdentifier    rest2 in
                                              let (args,  rest4) = parsePrototypeArgs rest3 in
                                              (PROTOTYPE_BINARY binop prec id args, rest4) 
parsePrototypeBinary list                   = let (binop, rest1) = parseBinOp         list  in
                                              let (id,    rest2) = parseIdentifier    rest1 in
                                              let (args,  rest3) = parsePrototypeArgs rest2 in
                                              (PROTOTYPE_BINARY binop (getDefaultBinaryPrecedence binop) id args, rest3)

parsePrototype' :: [Token] -> (PROTOTYPE, [Token])
parsePrototype' [] = error "parsePrototypeBinary: empty list"
parsePrototype' list    = let (id,    rest1) = parseIdentifier    list  in
                          let (args,  rest2) = parsePrototypeArgs rest1 in
                          (PROTOTYPE id args, rest2) 

parsePrecedence :: [Token] -> (PRECEDENCE, [Token])
parsePrecedence [] = error "parsePrecedence: empty list"
parsePrecedence _ = error "Not Implemented"

parsePrototypeArgs :: [Token] -> (PROTOTYPE_ARGS, [Token])
parsePrototypeArgs [] = error "parsePrototypeArgs: empty list"
parsePrototypeArgs _ = error "Not Implemented"

parsePrototypeId :: [Token] -> (PROTOTYPE_ID, [Token])
parsePrototypeId [] = error "parsePrototypeId: empty list"
parsePrototypeId _ = error "Not Implemented"

parseType :: [Token] -> (TYPE, [Token])
parseType [] = error "parseType: empty list"
parseType _ = error "Not Implemented"

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
parseExpressions _ = error "Not Implemented"

parseBinOp :: [Token] -> (BIN_OP, [Token])
parseBinOp [] = error "parseBinOp: empty list"
parseBinOp _ = error "Not Implemented"

parseBinaryOp :: [Token] -> (BINARY_OP, [Token])
parseBinaryOp [] = error "parseBinaryOp: empty list"
parseBinaryOp _ = error "Not Implemented"

parseExpression :: [Token] -> (EXPRESSION, [Token])
parseExpression [] = error "parseExpression: empty list"
parseExpression _ = error "Not Implemented"

parseUnOp :: [Token] -> (UN_OP, [Token])
parseUnOp [] = error "parseUnOp: empty list"
parseUnOp _ = error "Not Implemented"

parseUnary :: [Token] -> (UNARY, [Token])
parseUnary [] = error "parseUnary: empty list"
parseUnary _ = error "Not Implemented"

parsePostfix :: [Token] -> (POSTFIX, [Token])
parsePostfix [] = error "parsePostfix: empty list"
parsePostfix _ = error "Not Implemented"

parseCallExpr :: [Token] -> (CALL_EXPR, [Token])
parseCallExpr [] = error "parseCallExpr: empty list"
parseCallExpr _ = error "Not Implemented"

parseCallExprArgs :: [Token] -> (CALL_EXPR_ARGS, [Token])
parseCallExprArgs [] = error "parseCallExprArgs: empty list"
parseCallExprArgs _ = error "Not Implemented"

parsePrimary :: [Token] -> (PRIMARY, [Token])
parsePrimary [] = error "parsePrimary: empty list"
parsePrimary _ = error "Not Implemented"

parseIdentifier :: [Token] -> (IDENTIFIER, [Token])
parseIdentifier [] = error "parseIdentifier: empty list"
parseIdentifier ((Word w):xs) = (IDENTIFIER w, xs)
parseIdentifier _  = error "parseIdentifier: expecting ';'"

parseDot :: [Token] -> (DOT, [Token])
parseDot [] = error "parseDot: empty list"
parseDot _ = error "Not Implemented"

parseDecimalConst :: [Token] -> (DECIMAL_CONST, [Token])
parseDecimalConst [] = error "parseDecimalConst: empty list"
parseDecimalConst _ = error "Not Implemented"

parseDoubleConst :: [Token] -> (DOUBLE_CONST, [Token])
parseDoubleConst [] = error "parseDoubleConst: empty list"
parseDoubleConst _ = error "Not Implemented"

parseLitteral :: [Token] -> (LITERAL, [Token])
parseLitteral [] = error "parseLitteral: empty list"
parseLitteral _ = error "Not Implemented"


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