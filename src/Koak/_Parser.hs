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

data Prototype = PrototypeUnary    UnaryOp             Identifier PrototypeArgs
               | PrototypeBinary   BinaryOp Precedence Identifier PrototypeArgs
               | PrototypeFunction                     Identifier PrototypeArgs

data PrototypeArgs       = PrototypeArgs [PrototypeIdentifier] Type

data PrototypeIdentifier = PrototypeIdentifier Identifier Type

data Type = Int
          | Double
          | Void

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

newtype UnaryOp      = UnaryOp String

newtype BinaryOp     = BinaryOp String

newtype Precedence   = Precedence Int

newtype DecimalConst = DecimalConst Int

newtype DoubleConst  = DoubleConst Double

data Literal = LiteralDecimal DecimalConst
             | LiteralDouble  DoubleConst

--parseKoak :: String -> Stmt
--parseKoak = parseTokenizedKoak . tokenizeKoak
--
--parseTokenizedKoak :: [Token] -> Stmt
--parseTokenizedKoak = parseStmt
--
--parsingError :: String -> [Token] -> Maybe Token -> [Token] -> KoakException
--parsingError at expected actual rest = KoakParserMissingToken at (show expected) (show actual) (show rest)
--
--parseStmt :: [Token] -> Stmt
--parseStmt []     = Stmt []
--parseStmt tokens = let (kdefs, rest) = parseKdefs tokens in Stmt $ kdefs : getKdefsFromStmt (parseStmt rest)
--
--getKdefsFromStmt :: Stmt -> [Kdefs]
--getKdefsFromStmt (Stmt kdefs) = kdefs
--
--parseKdefs :: [Token] -> (Kdefs, [Token])
--parseKdefs [] = throw "a"
