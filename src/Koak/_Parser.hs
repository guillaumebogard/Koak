--
-- EPITECH PROJECT, 2022
-- koak
-- File description:
-- Koak.Parser
--

module Koak.Parser       ( {-parseKoak-} ) where

import Control.Exception ( throw )
import Exception         ( KoakException( KoakParserMissingToken ) )

import Koak.Lexer as KL  ( Token(..)
                         , tokenizeKoak
                         )


newtype Stmt   = Stmt [Kdefs]

data Kdefs     = KdefDef        Defs
               | KdefExpression Expressions

data Defs      = Defs Prototype Expressions

data Prototype = PrototypeUnary               Identifier PrototypeArgs
               | PrototypeBinary   Precedence Identifier PrototypeArgs
               | PrototypeFunction            Identifier PrototypeArgs

data PrototypeArgs       = PrototypeArgs [PrototypeIdentifier] Type

data PrototypeIdentifier = PrototypeIdentifier Identifier Type

data Type = Int
          | Double
          | Bool
          | Void

data Bool = True
          | False

data Expressions = ExpressionFor   For
                 | ExpressionIf    If
                 | ExpressionWhile While
                 | Expressions     Expression [Expression]

data For         = For Identifier Expression Identifier Expression Expression Expressions

data If          = If Expression Expressions (Maybe Expressions)

data While       = While Expression Expressions

data Expression  = Expression Unary [(BinaryOp, Unary)]

data Unary = Unary UnaryOp Unary
           | UnaryPostfix Postfix

data Postfix = Postfix Primary (Maybe CallExpression)

newtype CallExpression = CallExpression (Maybe CallExpressionArgs)

data CallExpressionArgs = CallExpressionArgs Expression [Expression]

data Primary = PrimaryIdentifier  Identifier
             | PrimaryLiteral     Literal
             | PrimaryExpressions Expressions

newtype Identifier   = Identifier String

newtype UnaryOp      = UnaryOp Identifier

newtype BinaryOp     = BinaryOp Identifier

newtype Precedence   = Precedence Int

newtype DecimalConst = DecimalConst Int

newtype DoubleConst  = DoubleConst Double

data Literal = LiteralDecimal DecimalConst
             | LiteralDouble  DoubleConst

--parseKoak :: String -> Stmt
--parseKoak = parseTokenizedKoak . tokenizeKoak
--
--parseTokenizedKoak :: [KL.Token] -> Stmt
--parseTokenizedKoak = parseStmt
--
--createParsingError :: String -> [KL.Token] -> Maybe KL.Token -> [KL.Token] -> KoakException
--createParsingError at expected actual rest = KoakParserMissingToken at (show expected) (show actual) (show rest)
--
--parseStmt :: [KL.Token] -> Stmt
--parseStmt []     = Stmt []
--parseStmt tokens = let (kdefs, rest) = parseKdefs tokens in Stmt $ kdefs : getKdefsFromStmt (parseStmt rest)
--
--getKdefsFromStmt :: Stmt -> [Kdefs]
--getKdefsFromStmt (Stmt kdefs) = kdefs
--
--parseKdefs :: [KL.Token] -> (Kdefs, [KL.Token])
--parseKdefs []                 = throw $ createParsingError "parseKdefs" [Word "def"] Nothing []
--parseKdefs (KL.Word "def":xs) = let (def, rest)  = parseDefs xs            in (KdefDef def, rest)
--parseKdefs tokens             = let (expr, rest) = parseExpressions tokens in (KdefExpression expr, parseKdefCheckSemiColon rest)
--
--parseKdefCheckSemiColon :: [KL.Token] -> [KL.Token]
--parseKdefCheckSemiColon []                = throw $ createParsingError "parseKdefs" [SemiColon] Nothing []
--parseKdefCheckSemiColon (KL.SemiColon:xs) = xs
--parseKdefCheckSemiColon (x:xs)            = throw $ createParsingError "parseKdefs" [SemiColon] (Just x) xs
--
--parseDefs :: [KL.Token] -> (Defs, [KL.Token])
--parseDefs []     = throw $ createParsingError "parseDefs" [] Nothing []
--parseDefs tokens = let (prototype  , rest ) = parsePrototype   tokens in
--                   let (expressions, rest') = parseExpressions rest   in
--                   (DEFS prototype expressions, parseKDefsSemiColon rest')
--
--(>=>=>)
