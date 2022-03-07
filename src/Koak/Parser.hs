--
-- EPITECH PROJECT, 2022
-- koak
-- File description:
-- Koak.Parser
--

module Koak.Parser        ( Stmt(..)
                          , Kdefs(..)
                          , Defs(..)
                          , Prototype(..)
                          , PrototypeArgs(..)
                          , PrototypeIdentifier(..)
                          , Type(..)
                          , Boolean(..)
                          , Expressions(..)
                          , For(..)
                          , If(..)
                          , While(..)
                          , Expression(..)
                          , Unary(..)
                          , Postfix(..)
                          , CallExpression(..)
                          , CallExpressionArgs(..)
                          , Primary(..)
                          , Identifier(..)
                          , UnaryOp(..)
                          , BinaryOp(..)
                          , Precedence(..)
                          , DecimalConst(..)
                          , DoubleConst(..)
                          , Literal(..)
                          , parseKoak
                          ) where

import Control.Exception  ( throw )
import Exception          ( KoakException( KoakParserMissingToken ) )

import Koak.Lexer as KL   ( Token(..)
                          , tokenizeKoak
                          )
import Koak.Grammar.Utils ( isSpecialWord )

newtype Stmt   = Stmt [Kdefs]

data Kdefs     = KdefDef        Defs
               | KdefExpression Expressions

data Defs      = Defs Prototype Expressions

data Prototype = PrototypeUnary               UnaryOp    PrototypeArgs
               | PrototypeBinary   Precedence BinaryOp   PrototypeArgs
               | PrototypeFunction            Identifier PrototypeArgs

data PrototypeArgs       = PrototypeArgs [PrototypeIdentifier] Type

data PrototypeIdentifier = PrototypeIdentifier Identifier Type

data Type = Int
          | Double
          | Boolean
          | Void

data Boolean = True
             | False

data Expressions = ExpressionFor   For
                 | ExpressionIf    If
                 | ExpressionWhile While
                 | Expressions     Expression [Expression]

data For         = For   Identifier Expression  Identifier Expression Expression Expressions

data If          = If    Expression Expressions (Maybe Expressions)

data While       = While Expression Expressions

data Expression  = Expression Unary [(BinaryOp, Unary)]

data Unary = Unary        UnaryOp Unary
           | UnaryPostfix Postfix

data Postfix = Postfix Primary (Maybe CallExpression)

newtype CallExpression = CallExpression (Maybe CallExpressionArgs)

data CallExpressionArgs = CallExpressionArgs Expression [Expression]

data Primary = PrimaryIdentifier  Identifier
             | PrimaryLiteral     Literal
             | PrimaryExpressions Expressions

newtype Identifier   = Identifier   String

newtype UnaryOp      = UnaryOp      Identifier

newtype BinaryOp     = BinaryOp     Identifier

newtype Precedence   = Precedence   Int

newtype DecimalConst = DecimalConst Int

newtype DoubleConst  = DoubleConst  Double

data Literal = LiteralDecimal DecimalConst
             | LiteralDouble  DoubleConst

parseKoak :: String -> Stmt
parseKoak = parseTokenizedKoak . tokenizeKoak

parseTokenizedKoak :: [KL.Token] -> Stmt
parseTokenizedKoak = parseStmt

createParsingError :: String -> [KL.Token] -> Maybe KL.Token -> [KL.Token] -> KoakException
createParsingError at expected actual rest = KoakParserMissingToken at (show expected) (show actual) (show rest)

parseStmt :: [KL.Token] -> Stmt
parseStmt []     = Stmt []
parseStmt tokens = let (kdefs, rest) = parseKdefs tokens in Stmt $ kdefs : getKdefsFromStmt (parseStmt rest)

getKdefsFromStmt :: Stmt -> [Kdefs]
getKdefsFromStmt (Stmt kdefs) = kdefs

parseKdefs :: [KL.Token] -> (Kdefs, [KL.Token])
parseKdefs []                 = throw $ createParsingError "parseKdefs" [KL.Word "def"] Nothing []
parseKdefs (KL.Word "def":xs) = let (def , rest) = parseDefs xs            in (KdefDef def, rest)
parseKdefs tokens             = let (expr, rest) = parseExpressions tokens in (KdefExpression expr, parseKdefsCheckSemiColon rest)

parseDefs :: [KL.Token] -> (Defs, [KL.Token])
parseDefs []     = throw $ createParsingError "parseDefs" [] Nothing []
parseDefs tokens = let (prototype  , rest ) = parsePrototype   tokens in
                   let (expressions, rest') = parseExpressions rest   in
                   (Defs prototype expressions, parseKdefsCheckSemiColon rest')

parseKdefsCheckSemiColon :: [KL.Token] -> [KL.Token]
parseKdefsCheckSemiColon []                = throw $ createParsingError "parseKdefs" [KL.SemiColon] Nothing []
parseKdefsCheckSemiColon (KL.SemiColon:xs) = xs
parseKdefsCheckSemiColon (x:xs)            = throw $ createParsingError "parseKdefs" [KL.SemiColon] (Just x) xs

parsePrototype :: [KL.Token] -> (Prototype, [KL.Token])
parsePrototype []                    = throw $ createParsingError "parsePrototype" [KL.Word "unary", KL.Word "binary", KL.Word "{function name}"] Nothing []
parsePrototype (KL.Word "unary":xs)  = parsePrototypeUnary    xs
parsePrototype (KL.Word "binary":xs) = parsePrototypeBinary   xs
parsePrototype tokens@(KL.Word _:_)  = parsePrototypeFunction tokens
parsePrototype (x:xs)                = throw $ createParsingError "parsePrototype" [KL.Word "unary", KL.Word "binary", KL.Word "{function name}"] (Just x) xs

parsePrototypeUnary :: [KL.Token] -> (Prototype, [KL.Token])
parsePrototypeUnary []     = throw $ createParsingError "parsePrototypeUnary" [] Nothing []
parsePrototypeUnary tokens = let (unop         , rest ) = parseUnaryOp         tokens in
                             let (prototypeArgs, rest') = parsePrototypeArgs   rest   in
                             (PrototypeUnary unop prototypeArgs, rest')

parseUnaryOp :: [KL.Token] -> (UnaryOp, [KL.Token])
parseUnaryOp []              = throw $ createParsingError "parseUnaryOp" [KL.Word "{any}"] Nothing []
parseUnaryOp (KL.Word op:xs) = (UnaryOp $ Identifier op, xs)
parseUnaryOp (x:xs)          = throw $ createParsingError "parseUnaryOp" [KL.Word "{any}"] (Just x) xs

parsePrototypeArgs :: [KL.Token] -> (PrototypeArgs, [KL.Token])
parsePrototypeArgs []     = throw $ createParsingError "parsePrototypeArgs" [] Nothing []
parsePrototypeArgs tokens = let rest                           = parsePrototypeArgsCheckOpenedParenthesis tokens in
                            let (prototypeIdentifiers, rest' ) = parsePrototypeArgsList                   rest   in
                            let (returnType          , rest'') = parsePrototypeArgsReturnType             rest'  in
                            let rest'''                        = parsePrototypeArgsCheckClosedParenthesis rest'' in
                            (PrototypeArgs prototypeIdentifiers returnType, rest''')

parsePrototypeArgsCheckOpenedParenthesis :: [KL.Token] -> [KL.Token]
parsePrototypeArgsCheckOpenedParenthesis []                        = throw $ createParsingError "parsePrototypeArgs" [] Nothing []
parsePrototypeArgsCheckOpenedParenthesis (KL.OpenedParenthesis:xs) = xs
parsePrototypeArgsCheckOpenedParenthesis (x:xs)                    = throw $ createParsingError "parsePrototypeArgs" [KL.OpenedParenthesis] (Just x) xs

parsePrototypeArgsList :: [KL.Token] -> ([PrototypeIdentifier], [KL.Token])
parsePrototypeArgsList tokens = parsePrototypeArgsList' tokens []

parsePrototypeArgsList' :: [KL.Token] -> [PrototypeIdentifier] -> ([PrototypeIdentifier], [KL.Token])
parsePrototypeArgsList' tokens@(KL.Word _:_) protoIds = let (protoId, rest) = parsePrototypeIdentifier tokens in parsePrototypeArgsList' rest $ protoId:protoIds
parsePrototypeArgsList' tokens               protoIds = (reverse protoIds, tokens)

parsePrototypeIdentifier :: [KL.Token] -> (PrototypeIdentifier, [KL.Token])
parsePrototypeIdentifier []     = throw $ createParsingError "parsePrototypeIdentifier" [] Nothing []
parsePrototypeIdentifier tokens = let (identifier, rest) = parseIdentifier tokens in parsePrototypeIdentifier' rest identifier

parsePrototypeIdentifier' :: [KL.Token] -> Identifier -> (PrototypeIdentifier, [KL.Token])
parsePrototypeIdentifier' []         _          = throw $ createParsingError "parsePrototypeIdentifier" [Colon] Nothing []
parsePrototypeIdentifier' (Colon:xs) identifier = let (prototypeType, rest) = parseType xs in (PrototypeIdentifier identifier prototypeType, rest)
parsePrototypeIdentifier' (x:xs)     _          = throw $ createParsingError "parsePrototypeIdentifier" [Colon] (Just x) xs

parseIdentifier :: [KL.Token] -> (Identifier, [KL.Token])
parseIdentifier []                      = throw $ createParsingError "parseIdentifier" [KL.Word "{any}"] Nothing []
parseIdentifier (KL.Word identifier:xs) = (Identifier identifier, xs)
parseIdentifier (x:xs)                  = throw $ createParsingError "parseIdentifier" [KL.Word "{any}"] (Just x) xs

parseType :: [KL.Token] -> (Type, [KL.Token])
parseType []                    = throw $ createParsingError "parseType" [KL.Word "int", KL.Word "double", KL.Word "bool", KL.Word "void"] Nothing []
parseType (KL.Word "int":xs)    = (Int    , xs)
parseType (KL.Word "double":xs) = (Double , xs)
parseType (KL.Word "bool":xs)   = (Boolean, xs)
parseType (KL.Word "void":xs)   = (Void   , xs)
parseType (x:xs)                = throw $ createParsingError "parseType" [KL.Word "int", KL.Word "double", KL.Word "bool", KL.Word "void"] (Just x) xs

parsePrototypeArgsReturnType :: [KL.Token] -> (Type, [KL.Token])
parsePrototypeArgsReturnType []            = throw $ createParsingError "parsePrototypeArgsType" [KL.Colon] Nothing []
parsePrototypeArgsReturnType (KL.Colon:xs) = parseType xs
parsePrototypeArgsReturnType (x:xs)        = throw $ createParsingError "parsePrototypeArgsType" [KL.Colon] (Just x) xs

parsePrototypeArgsCheckClosedParenthesis :: [KL.Token] -> [KL.Token]
parsePrototypeArgsCheckClosedParenthesis []                        = throw $ createParsingError "parsePrototypeArgs" [] Nothing []
parsePrototypeArgsCheckClosedParenthesis (KL.ClosedParenthesis:xs) = xs
parsePrototypeArgsCheckClosedParenthesis (x:xs)                    = throw $ createParsingError "parsePrototypeArgs" [KL.ClosedParenthesis] (Just x) xs

parsePrototypeBinary :: [KL.Token] -> (Prototype, [KL.Token])
parsePrototypeBinary []     = throw $ createParsingError "parsePrototypeBinary" [] Nothing []
parsePrototypeBinary tokens = let (precedence   , rest  ) = parseMaybePrecedence tokens in
                              let (binaryOp     , rest' ) = parseBinaryOp        rest   in
                              let (prototypeArgs, rest'') = parsePrototypeArgs   rest'  in
                              parsePrototypeBinary' precedence binaryOp prototypeArgs rest''

parsePrototypeBinary' ::  Maybe Precedence -> BinaryOp -> PrototypeArgs -> [KL.Token] -> (Prototype, [KL.Token])
parsePrototypeBinary' Nothing           binaryOp prototypeArgs tokens = (PrototypeBinary (Precedence 0) binaryOp prototypeArgs, tokens)
parsePrototypeBinary' (Just precedence) binaryOp prototypeArgs tokens = (PrototypeBinary precedence binaryOp prototypeArgs, tokens)

parseMaybePrecedence :: [KL.Token] -> (Maybe Precedence, [KL.Token])
parseMaybePrecedence (IntegerNumber value:xs) = (Just $ Precedence value, xs)
parseMaybePrecedence tokens                   = (Nothing, tokens)

parseBinaryOp :: [KL.Token] -> (BinaryOp, [KL.Token])
parseBinaryOp []              = throw $ createParsingError "parseBinaryOp" [KL.Word "{any}"] Nothing []
parseBinaryOp (KL.Word op:xs) = (BinaryOp $ Identifier op, xs)
parseBinaryOp (x:xs)          = throw $ createParsingError "parseBinaryOp" [KL.Word "{any}"] (Just x) xs

parsePrototypeFunction :: [KL.Token] -> (Prototype, [KL.Token])
parsePrototypeFunction []     = throw $ createParsingError "parsePrototypeFunction" [] Nothing []
parsePrototypeFunction tokens = let (identifier   , rest ) = parseIdentifier    tokens in
                                let (prototypeArgs, rest') = parsePrototypeArgs rest   in
                                (PrototypeFunction identifier prototypeArgs, rest')

parseExpressions :: [KL.Token] -> (Expressions, [KL.Token])
parseExpressions []                         = throw $ createParsingError "parseExpressions" [KL.Word "for", KL.Word "if", KL.Word "while", KL.Word "{expression}"] Nothing []
parseExpressions tokens@(KL.Word "for":_)   = wrapForAroundExpressions   $ parseFor   tokens
parseExpressions tokens@(KL.Word "if":_)    = wrapIfAroundExpressions    $ parseIf    tokens
parseExpressions tokens@(KL.Word "while":_) = wrapWhileAroundExpressions $ parseWhile tokens
parseExpressions tokens                     = let (expression    , rest ) = parseExpression     tokens in
                                              let (expressionList, rest') = parseExpressionList rest   in
                                              (Expressions expression expressionList, rest')

wrapForAroundExpressions :: (For, [KL.Token]) -> (Expressions, [KL.Token])
wrapForAroundExpressions (forExpression, tokens) = (ExpressionFor forExpression, tokens)

wrapIfAroundExpressions :: (If, [KL.Token]) -> (Expressions, [KL.Token])
wrapIfAroundExpressions (ifExpression, tokens) = (ExpressionIf ifExpression, tokens)

wrapWhileAroundExpressions :: (While, [KL.Token]) -> (Expressions, [KL.Token])
wrapWhileAroundExpressions (whileExpression, tokens) = (ExpressionWhile whileExpression, tokens)

parseFor :: [KL.Token] -> (For, [KL.Token])
parseFor []                 = throw $ createParsingError "parseFor" [KL.Word "for"] Nothing []
parseFor (KL.Word "for":xs) = uncurry parseFor' $ parseIdentifier xs
parseFor (x:xs)             = throw $ createParsingError "parseFor" [KL.Word "for"] (Just x) xs

parseFor' :: Identifier -> [KL.Token] -> (For, [KL.Token])
parseFor' _        []               = throw $ createParsingError "parseFor" [KL.Word "="] Nothing []
parseFor' assignId (KL.Word "=":xs) = uncurry (parseFor'' assignId) $ parseExpression xs
parseFor' _        (x:xs)           = throw $ createParsingError "parseFor" [KL.Word "="] (Just x) xs

parseFor'' :: Identifier -> Expression -> [KL.Token] -> (For, [KL.Token])
parseFor'' _        _                []            = throw $ createParsingError "parseFor" [KL.Comma] Nothing []
parseFor'' assignId assignExpression (KL.Comma:xs) = uncurry (parseFor''' assignId assignExpression) (parseIdentifier xs)
parseFor'' _        _                (x:xs)        = throw $ createParsingError "parseFor" [KL.Comma] (Just x) xs

parseFor''' :: Identifier -> Expression -> Identifier -> [KL.Token] -> (For, [KL.Token])
parseFor''' _        _          _     []               = throw $ createParsingError "parseFor" [KL.Word "<"] Nothing []
parseFor''' assignId assignExpr cmpId (KL.Word "<":xs) = uncurry (parseFor'''' assignId assignExpr cmpId) $ parseExpression xs
parseFor''' _        _          _     (x:xs)           = throw $ createParsingError "parseFor" [KL.Word "<"] (Just x) xs

parseFor'''' :: Identifier -> Expression -> Identifier -> Expression -> [KL.Token] -> (For, [KL.Token])
parseFor'''' _        _          _     _       []            = throw $ createParsingError "parseFor" [KL.Comma] Nothing []
parseFor'''' assignId assignExpr cmpId cmpExpr (KL.Comma:xs) = uncurry (parseFor''''' assignId assignExpr cmpId cmpExpr) $ parseExpression xs
parseFor'''' _        _          _     _       (x:xs)        = throw $ createParsingError "parseFor" [KL.Comma] (Just x) xs

parseFor''''' :: Identifier -> Expression -> Identifier -> Expression -> Expression -> [KL.Token] -> (For, [KL.Token])
parseFor''''' _        _          _     _       _       []                = throw $ createParsingError "parseFor" [KL.Word "in"] Nothing []
parseFor''''' assignId assignExpr cmpId cmpExpr incExpr (KL.Word "in":xs) = uncurry (parseFor'''''' assignId assignExpr cmpId cmpExpr incExpr) $ parseExpressions xs
parseFor''''' _        _          _     _       _       (x:xs)            = throw $ createParsingError "parseFor" [KL.Word "in"] (Just x) xs

parseFor'''''' :: Identifier -> Expression -> Identifier -> Expression -> Expression -> Expressions -> [KL.Token] -> (For, [KL.Token])
parseFor'''''' assignId assignExpr cmpId cmpExpr incExpr expressions tokens = (For assignId assignExpr cmpId cmpExpr incExpr expressions, tokens)

parseIf :: [KL.Token] -> (If, [KL.Token])
parseIf []                = throw $ createParsingError "parseIf" [KL.Word "if"] Nothing []
parseIf (KL.Word "if":xs) = uncurry parseIfThen $ parseExpression xs
parseIf (x:xs)            = throw $ createParsingError "parseIf" [KL.Word "if"] (Just x) xs

parseIfThen :: Expression -> [KL.Token] -> (If, [Token])
parseIfThen _      []                  = throw $ createParsingError "parseIf" [KL.Word "then"] Nothing []
parseIfThen ifExpr (KL.Word "then":xs) = uncurry (parseIfThenMaybeElse ifExpr) $ parseExpressions xs
parseIfThen _      (x:xs)              = throw $ createParsingError "parseIf" [KL.Word "then"] (Just x) xs

parseIfThenMaybeElse :: Expression -> Expressions -> [KL.Token] -> (If, [Token])
parseIfThenMaybeElse ifExpr thenExprs (KL.Word "else":xs) = let (elseExprs, rest) = parseExpressions xs in
                                                            (If ifExpr thenExprs (Just elseExprs), rest)
parseIfThenMaybeElse ifExpr thenExprs tokens              = (If ifExpr thenExprs Nothing         , tokens)

parseWhile :: [KL.Token] -> (While, [KL.Token])
parseWhile []                   = throw $ createParsingError "parseWhile" [KL.Word "while"] Nothing []
parseWhile (KL.Word "while":xs) = uncurry parseWhileDo $ parseExpression xs
parseWhile (x:xs)               = throw $ createParsingError "parseWhile" [KL.Word "while"] (Just x) xs

parseWhileDo :: Expression -> [KL.Token] -> (While, [KL.Token])
parseWhileDo _         []                = throw $ createParsingError "parseWhile" [KL.Word "do"] Nothing []
parseWhileDo whileExpr (KL.Word "do":xs) = let (doExprs, rest) = parseExpressions xs in (While whileExpr doExprs, rest)
parseWhileDo _         (x:xs)            = throw $ createParsingError "parseWhile" [KL.Word "do"] (Just x) xs

parseExpression :: [KL.Token] -> (Expression, [KL.Token])
parseExpression []     = throw $ createParsingError "parseExpression" [] Nothing []
parseExpression tokens = let (unary           , rest ) = parseUnary       tokens in
                         let (binaryOpsUnaries, rest') = parseExpression' rest   in
                         (Expression unary binaryOpsUnaries, rest')

parseExpression' :: [KL.Token] -> ([(BinaryOp, Unary)], [KL.Token])
parseExpression' []                 = ([], [])
parseExpression' tokens@(KL.Word w:xs)
    | isSpecialWord w               = let (unary           , rest ) = parseUnary       xs   in
                                      let (binaryOpsUnaries, rest') = parseExpression' rest in
                                      ((BinaryOp $ Identifier w, unary) : binaryOpsUnaries, rest')
    | otherwise                     = ([], tokens)
parseExpression' (x:xs)             = throw $ createParsingError "parseExpression" [Word "{expression}"] (Just x) xs

parseExpressionList :: [KL.Token] -> ([Expression], [KL.Token])
parseExpressionList (Colon:xs) = let (first, rest ) = parseExpression     xs   in
                                 let (next , rest') = parseExpressionList rest in
                                 (first : next, rest')
parseExpressionList tokens     = ([], tokens)

parseUnary :: [KL.Token] -> (Unary, [KL.Token])
parseUnary []                 = throw $ createParsingError "parseUnary" [] Nothing []
parseUnary tokens@(KL.Word w:xs)
  | isSpecialWord w           = let (unOp   , rest) = parseUnary   xs     in (Unary (UnaryOp $ Identifier w) unOp, rest)
  | otherwise                 = let (postfix, rest) = parsePostfix tokens in (UnaryPostfix postfix               , rest)
parseUnary (x:xs)             = throw $ createParsingError "parseUnary" [Word "{unary}"] (Just x) xs

parsePostfix :: [KL.Token] -> (Postfix, [KL.Token])
parsePostfix []     = throw $ createParsingError "parsePostfix" [] Nothing []
parsePostfix tokens = let (primary , rest)  = parsePrimary             tokens in
                      let (callExpr, rest') = parseMaybeCallExpression rest   in
                      (Postfix primary callExpr, rest')

parseMaybeCallExpression :: [KL.Token] -> (Maybe CallExpression, [KL.Token])
parseMaybeCallExpression []                              = (Nothing, [])
parseMaybeCallExpression tokens@(KL.OpenedParenthesis:_) = let (callExpr, rest) = parseCallExpression tokens in (Just callExpr, rest)
parseMaybeCallExpression tokens                          = (Nothing, tokens)

parseCallExpression :: [KL.Token] -> (CallExpression, [KL.Token])
parseCallExpression []                                             = throw $ createParsingError "parseCallExpr" [KL.OpenedParenthesis, KL.ClosedParenthesis] Nothing []
parseCallExpression (KL.OpenedParenthesis:KL.ClosedParenthesis:xs) = (CallExpression Nothing, xs)
parseCallExpression (KL.OpenedParenthesis:xs)                      = let (callExpr, rest) = parseCallExpressionArg xs in (CallExpression (Just callExpr), rest)
parseCallExpression (x:xs)                                         = throw $ createParsingError "parseCallExpression" [KL.OpenedParenthesis, KL.ClosedParenthesis] (Just x) xs

parseCallExpressionArg :: [KL.Token] -> (CallExpressionArgs, [KL.Token])
parseCallExpressionArg []     = throw $ createParsingError "parseCallExpressionArg" [] Nothing []
parseCallExpressionArg tokens = let (first, others, rest) = parseCallExpressionArg' tokens in (CallExpressionArgs first others, rest)

parseCallExpressionArg' :: [KL.Token] -> (Expression, [Expression], [KL.Token])
parseCallExpressionArg' tokens = let (expr    , rest ) = parseExpression          tokens in
                                 let (exprList, rest') = parseCallExpressionArg'' rest   in
                                 (expr, exprList, rest')

parseCallExpressionArg'' :: [KL.Token] -> ([Expression], [KL.Token])
parseCallExpressionArg'' []                        = throw $ createParsingError "parseCallExpressionArg" [KL.Comma, KL.ClosedParenthesis] Nothing []
parseCallExpressionArg'' (KL.ClosedParenthesis:xs) = ([], xs)
parseCallExpressionArg'' (KL.Comma:xs)             = let (expr, rest ) = parseExpression          xs   in
                                                     let (next, rest') = parseCallExpressionArg'' rest in
                                                     (expr : next, rest')
parseCallExpressionArg'' (x:xs)                    = throw $ createParsingError "parseCallExpressionArg" [KL.Comma, KL.ClosedParenthesis] (Just x) xs

parsePrimary :: [KL.Token] -> (Primary, [KL.Token])
parsePrimary []                        = throw $ createParsingError "parsePrimary" [KL.Word "{variable}", KL.IntegerNumber 0, KL.OpenedParenthesis] Nothing []
parsePrimary (KL.Word w:xs)            = (PrimaryIdentifier $ Identifier w, xs)
parsePrimary (KL.IntegerNumber n:xs)   = (PrimaryLiteral $ LiteralDecimal $ DecimalConst n, xs)
parsePrimary (KL.FloatingNumber n:xs)  = (PrimaryLiteral $ LiteralDouble  $ DoubleConst  n, xs)
parsePrimary (KL.OpenedParenthesis:xs) = let (exprs, rest) = parseExpressions xs in parsePrimaryExpressions (KL.OpenedParenthesis : rest) exprs
parsePrimary (x:xs)                    = throw $ createParsingError "parsePrimary" [KL.Word "{variable}", KL.IntegerNumber 0, KL.OpenedParenthesis] (Just x) xs

parsePrimaryExpressions :: [KL.Token] -> Expressions -> (Primary, [KL.Token])
parsePrimaryExpressions []                                             _     = throw $ createParsingError "parsePrimaryExpression" [KL.OpenedParenthesis, KL.ClosedParenthesis] Nothing []
parsePrimaryExpressions (KL.OpenedParenthesis:KL.ClosedParenthesis:xs) exprs = (PrimaryExpressions exprs, xs)
parsePrimaryExpressions (x                   :KL.ClosedParenthesis:xs) _     = throw $ createParsingError "parsePrimaryExpression" [KL.OpenedParenthesis] (Just x) xs
parsePrimaryExpressions (KL.OpenedParenthesis:x                   :xs) _     = throw $ createParsingError "parsePrimaryExpression" [KL.ClosedParenthesis] (Just x) xs
parsePrimaryExpressions tokens                                         _     = throw $ createParsingError "parsePrimaryExpression" [KL.OpenedParenthesis, KL.ClosedParenthesis] Nothing tokens
