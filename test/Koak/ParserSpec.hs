--
-- EPITECH PROJECT, 2022
-- koak
-- File description:
-- Koak.LexerSpec
--

module Koak.ParserSpec  ( spec ) where

import Test.Hspec        ( Spec
                         , it
                         , shouldBe
                         , shouldThrow
                         )

import Koak.Parser as KP

spec :: Spec
spec = do
    it "One simple def: def foo () : int 42;" $
        parseKoak "def foo () : int 42;"
        == KP.Stmt [
            KP.KdefDef
                (KP.Defs
                    (KP.PrototypeFunction
                        (KP.Identifier "foo")
                        (KP.PrototypeArgs [] KP.Int)
                    )
                    (KP.Expressions
                        (KP.Expression
                            (KP.UnaryPostfix
                                (KP.Postfix
                                    (KP.PrimaryLiteral
                                        (KP.LiteralDecimal
                                            (KP.DecimalConst 42)
                                        )
                                    )
                                    Nothing
                                )
                            )
                            []
                        )
                        []
                    )
                )
            ]
    it "One simple def w one arg: def foo (x : int) : int 42;" $
        parseKoak "def foo (x : int) : int 42;"
        == KP.Stmt [
            KP.KdefDef
                (KP.Defs
                    (KP.PrototypeFunction
                        (KP.Identifier "foo")
                        (KP.PrototypeArgs
                            [
                                KP.PrototypeIdentifier
                                    (KP.Identifier "x")
                                    (KP.Int)
                            ]
                            KP.Int)
                    )
                    (KP.Expressions
                        (KP.Expression
                            (KP.UnaryPostfix
                                (KP.Postfix
                                    (KP.PrimaryLiteral
                                        (KP.LiteralDecimal
                                            (KP.DecimalConst 42)
                                        )
                                    )
                                    Nothing
                                )
                            )
                            []
                        )
                        []
                    )
                )
            ]
    it "One simple def w many arg: def foo (x : int y : double z : void) : int 42;" $
        parseKoak "def foo (x : int y : double z : void) : int 42;"
        == KP.Stmt [
            KP.KdefDef
                (KP.Defs
                    (KP.PrototypeFunction
                        (KP.Identifier "foo")
                        (KP.PrototypeArgs
                            [
                                KP.PrototypeIdentifier
                                    (KP.Identifier "x")
                                    KP.Int,
                                KP.PrototypeIdentifier
                                    (KP.Identifier "y")
                                    KP.Double,
                                KP.PrototypeIdentifier
                                    (KP.Identifier "z")
                                    KP.Void
                            ]
                            KP.Int)
                    )
                    (KP.Expressions
                        (KP.Expression
                            (KP.UnaryPostfix
                                (KP.Postfix
                                    (KP.PrimaryLiteral
                                        (KP.LiteralDecimal
                                            (KP.DecimalConst 42)
                                        )
                                    )
                                    Nothing
                                )
                            )
                            []
                        )
                        []
                    )
                )
            ]
    it "Multiple simple def w args: def foo (x : int y : double z : void) : int 42; def bar(a : double) : double a;" $
        parseKoak "def foo (x : int y : double z : void) : int 42; def bar(a : double) : double a;"
        == KP.Stmt [
            KP.KdefDef
                (KP.Defs
                    (KP.PrototypeFunction
                        (KP.Identifier "foo")
                        (KP.PrototypeArgs
                            [
                                KP.PrototypeIdentifier
                                    (KP.Identifier "x")
                                    KP.Int,
                                KP.PrototypeIdentifier
                                    (KP.Identifier "y")
                                    KP.Double,
                                KP.PrototypeIdentifier
                                    (KP.Identifier "z")
                                    KP.Void
                            ]
                            KP.Int)
                    )
                    (KP.Expressions
                        (KP.Expression
                            (KP.UnaryPostfix
                                (KP.Postfix
                                    (KP.PrimaryLiteral
                                        (KP.LiteralDecimal
                                            (KP.DecimalConst 42)
                                        )
                                    )
                                    Nothing
                                )
                            )
                            []
                        )
                        []
                    )
                ),
            KP.KdefDef
                (KP.Defs
                    (KP.PrototypeFunction
                        (KP.Identifier "bar")
                        (KP.PrototypeArgs
                            [
                                KP.PrototypeIdentifier
                                    (KP.Identifier "a")
                                    KP.Double
                            ]
                            KP.Double)
                    )
                    (KP.Expressions
                        (KP.Expression
                            (KP.UnaryPostfix
                                (KP.Postfix
                                    (KP.PrimaryIdentifier
                                        (KP.Identifier "a")
                                    )
                                    Nothing
                                )
                            )
                            []
                        )
                        []
                    )
                )
            ]
    it "Simple function call without args: foo();" $
        parseKoak "foo();"
        == KP.Stmt [
            KP.KdefExpression
                (KP.Expressions
                    (KP.Expression
                        (KP.UnaryPostfix
                            (KP.Postfix
                                (KP.PrimaryIdentifier
                                    (KP.Identifier "foo")
                                )
                                (Just $ KP.CallExpression
                                    Nothing
                                )
                            )
                        )
                        []
                    )
                    []
                )
            ]
    it "Simple function call with one number as argument: foo(1);" $
        parseKoak "foo(1);"
        == KP.Stmt [
            KP.KdefExpression
                (KP.Expressions
                    (KP.Expression
                        (KP.UnaryPostfix
                            (KP.Postfix
                                (KP.PrimaryIdentifier
                                    (KP.Identifier "foo")
                                )
                                (Just $ KP.CallExpression
                                    (Just $ KP.CallExpressionArgs
                                        (KP.Expression
                                            (KP.UnaryPostfix
                                                (KP.Postfix
                                                    (KP.PrimaryLiteral
                                                        (KP.LiteralDecimal
                                                            (KP.DecimalConst 1)
                                                        )
                                                    )
                                                    Nothing
                                                )
                                            )
                                            []
                                        )
                                        []
                                    )
                                )
                            )
                        )
                        []
                    )
                    []
                )
            ]
    it "Simple function call with one variable as argument: foo(my_var);" $
        parseKoak "foo(my_var);"
        == KP.Stmt [
            KP.KdefExpression
                (KP.Expressions
                    (KP.Expression
                        (KP.UnaryPostfix
                            (KP.Postfix
                                (KP.PrimaryIdentifier
                                    (KP.Identifier "foo")
                                )
                                (Just $ KP.CallExpression
                                    (Just $ KP.CallExpressionArgs
                                        (KP.Expression
                                            (KP.UnaryPostfix
                                                (KP.Postfix
                                                    (KP.PrimaryIdentifier
                                                        (KP.Identifier "my_var")
                                                    )
                                                    Nothing
                                                )
                                            )
                                            []
                                        )
                                        []
                                    )
                                )
                            )
                        )
                        []
                    )
                    []
                )
            ]
    it "Simple function call with multiple primary arguments: foo(1, my_var, 3.14);" $
        parseKoak "foo(1, my_var, 3.14);"
        == KP.Stmt [
            KP.KdefExpression
                (KP.Expressions
                    (KP.Expression
                        (KP.UnaryPostfix
                            (KP.Postfix
                                (KP.PrimaryIdentifier
                                    (KP.Identifier "foo")
                                )
                                (Just $ KP.CallExpression
                                    (Just $ KP.CallExpressionArgs
                                        (KP.Expression
                                            (KP.UnaryPostfix
                                                (KP.Postfix
                                                    (KP.PrimaryLiteral
                                                        (KP.LiteralDecimal
                                                            (KP.DecimalConst 1)
                                                        )
                                                    )
                                                    Nothing
                                                )
                                            )
                                            []
                                        )
                                        [
                                            KP.Expression
                                                (KP.UnaryPostfix
                                                    (KP.Postfix
                                                        (KP.PrimaryIdentifier
                                                            (KP.Identifier "my_var")
                                                        )
                                                        Nothing
                                                    )
                                                )
                                                [],
                                            KP.Expression
                                                (KP.UnaryPostfix
                                                    (KP.Postfix
                                                        (KP.PrimaryLiteral
                                                            (KP.LiteralDouble
                                                                (KP.DoubleConst 3.14)
                                                            )
                                                        )
                                                        Nothing
                                                    )
                                                )
                                                []
                                        ]
                                    )
                                )
                            )
                        )
                        []
                    )
                    []
                )
            ]
    it "function call with multiple arguments and expressions: foo(-1 * 3, my_var, bar(1, -2.12), my_var_2 / 2);" $
        parseKoak "foo(-1 * 3, my_var, bar(1, -2.12), my_var_2 / 2);"
        == KP.Stmt [
            KP.KdefExpression
                (KP.Expressions
                    (KP.Expression
                        (KP.UnaryPostfix
                            (KP.Postfix
                                (KP.PrimaryIdentifier
                                    (KP.Identifier "foo")
                                )
                                (Just $ KP.CallExpression
                                    (Just $ KP.CallExpressionArgs
                                        (KP.Expression
                                            (KP.Unary
                                                (KP.UnaryOp
                                                    (KP.Identifier "-")
                                                )
                                                (KP.UnaryPostfix
                                                    (KP.Postfix
                                                        (KP.PrimaryLiteral
                                                            (KP.LiteralDecimal
                                                                (KP.DecimalConst 1)
                                                            )
                                                        )
                                                        Nothing
                                                    )
                                                )
                                            )
                                            [
                                                (
                                                    KP.BinaryOp
                                                        (KP.Identifier "*")
                                                    ,
                                                    KP.UnaryPostfix
                                                        (KP.Postfix
                                                            (KP.PrimaryLiteral
                                                                (KP.LiteralDecimal
                                                                    (KP.DecimalConst 3)
                                                                )
                                                            )
                                                            Nothing
                                                        )
                                                )
                                            ]
                                        )
                                        [
                                            KP.Expression
                                                (KP.UnaryPostfix
                                                    (KP.Postfix
                                                        (KP.PrimaryIdentifier
                                                            (KP.Identifier "my_var")
                                                        )
                                                        Nothing
                                                    )
                                                )
                                                [],
                                            KP.Expression
                                                (KP.UnaryPostfix
                                                    (KP.Postfix
                                                        (KP.PrimaryIdentifier
                                                            (KP.Identifier "bar")
                                                        )
                                                        (Just $ KP.CallExpression
                                                            (Just $ KP.CallExpressionArgs
                                                                (KP.Expression
                                                                    (KP.UnaryPostfix
                                                                        (KP.Postfix
                                                                            (KP.PrimaryLiteral
                                                                                (KP.LiteralDecimal
                                                                                    (KP.DecimalConst 1)
                                                                                )
                                                                            )
                                                                            Nothing
                                                                        )
                                                                    )
                                                                    []
                                                                )
                                                                [
                                                                    KP.Expression
                                                                        (KP.Unary
                                                                            (KP.UnaryOp
                                                                                (KP.Identifier "-")
                                                                            )
                                                                        (KP.UnaryPostfix
                                                                            (KP.Postfix
                                                                                (KP.PrimaryLiteral
                                                                                    (KP.LiteralDouble
                                                                                        (KP.DoubleConst 2.12)
                                                                                    )
                                                                                )
                                                                                Nothing
                                                                            )
                                                                        )
                                                                    )
                                                                    []
                                                                ]
                                                            )
                                                        )
                                                    )
                                                )
                                                [],
                                            KP.Expression
                                                (KP.UnaryPostfix
                                                    (KP.Postfix
                                                        (KP.PrimaryIdentifier
                                                            (KP.Identifier "my_var_2")
                                                        )
                                                        Nothing
                                                    )
                                                )
                                                [
                                                    (KP.BinaryOp
                                                        (KP.Identifier "/")
                                                    ,
                                                    KP.UnaryPostfix
                                                        (KP.Postfix
                                                            (KP.PrimaryLiteral
                                                                (KP.LiteralDecimal
                                                                    (KP.DecimalConst 2)
                                                                )
                                                            )
                                                            Nothing
                                                        )
                                                    )
                                                ]
                                        ]
                                    )
                                )
                            )
                        )
                        []
                    )
                    []
                )
            ]
    it "Chained simple function call: foo(): bar(2): foobar(1, 2, 3.14);" $
        parseKoak "foo(): bar(2): foobar(1, 2, 3.14);"
        == KP.Stmt [
                KP.KdefExpression
                    (KP.Expressions
                        (KP.Expression
                            (KP.UnaryPostfix
                                (KP.Postfix
                                    (KP.PrimaryIdentifier
                                        (KP.Identifier "foo")
                                    )
                                    (Just $ KP.CallExpression Nothing)
                                )
                            )
                            []
                        )
                        [
                            KP.Expression
                                (KP.UnaryPostfix
                                    (KP.Postfix
                                        (KP.PrimaryIdentifier
                                            (KP.Identifier "bar")
                                        )
                                        (Just $ KP.CallExpression
                                            (Just $ KP.CallExpressionArgs
                                                (KP.Expression
                                                    (KP.UnaryPostfix
                                                        (KP.Postfix
                                                            (KP.PrimaryLiteral
                                                                (KP.LiteralDecimal
                                                                    (KP.DecimalConst 2)
                                                                )
                                                            )
                                                            Nothing
                                                        )
                                                    )
                                                    []
                                                )
                                                []
                                            )
                                        )
                                    )
                                )
                                [],
                            KP.Expression
                                (KP.UnaryPostfix
                                    (KP.Postfix
                                        (KP.PrimaryIdentifier
                                            (KP.Identifier "foobar")
                                        )
                                        (Just $ KP.CallExpression
                                            (Just $ KP.CallExpressionArgs
                                                (KP.Expression
                                                    (KP.UnaryPostfix
                                                        (KP.Postfix
                                                            (KP.PrimaryLiteral
                                                                (KP.LiteralDecimal
                                                                    (KP.DecimalConst 1)
                                                                )
                                                            )
                                                            Nothing
                                                        )
                                                    )
                                                    []
                                                )
                                                [
                                                    KP.Expression
                                                        (KP.UnaryPostfix
                                                            (KP.Postfix
                                                                (KP.PrimaryLiteral
                                                                    (KP.LiteralDecimal
                                                                        (KP.DecimalConst 2)
                                                                    )
                                                                )
                                                                Nothing
                                                            )
                                                        )
                                                        [],
                                                    KP.Expression
                                                        (KP.UnaryPostfix
                                                            (KP.Postfix
                                                                (KP.PrimaryLiteral
                                                                    (KP.LiteralDouble
                                                                        (KP.DoubleConst 3.14)
                                                                    )
                                                                )
                                                                Nothing
                                                            )
                                                        )
                                                        []
                                                ]
                                            )
                                        )
                                    )
                                )
                                []
                        ]
                    )
        ]
    it "Simple def and simple call: def test ( x : double ) : double x + 2.0; test (5.0) - 2.0 * 3.0 + 1.0;" $
        parseKoak "def test ( x : double ) : double x + 2.0; test (5.0) - 2.0 * 3.0 + 1.0;"
        == KP.Stmt [
                KP.KdefDef
                    (KP.Defs
                        (KP.PrototypeFunction
                            (KP.Identifier "test")
                            (KP.PrototypeArgs
                                [
                                    KP.PrototypeIdentifier
                                        (KP.Identifier "x")
                                        KP.Double
                                ]
                                KP.Double
                            )
                        )
                        (KP.Expressions
                            (KP.Expression
                                (KP.UnaryPostfix
                                    (KP.Postfix
                                        (KP.PrimaryIdentifier
                                            (KP.Identifier "x")
                                        )
                                        Nothing
                                    )
                                )
                                [
                                    (
                                        KP.BinaryOp
                                            (KP.Identifier "+")
                                        ,
                                        KP.UnaryPostfix
                                            (KP.Postfix
                                                (KP.PrimaryLiteral
                                                    (KP.LiteralDouble
                                                        (KP.DoubleConst 2.0)
                                                    )
                                                )
                                                Nothing
                                            )
                                    )
                                ]
                            )
                            []
                        )
                    ),
                KP.KdefExpression
                    (KP.Expressions
                        (KP.Expression
                            (KP.UnaryPostfix
                                (KP.Postfix
                                    (KP.PrimaryIdentifier
                                        (KP.Identifier "test")
                                    )
                                    (Just $ KP.CallExpression
                                        (Just $ KP.CallExpressionArgs
                                            (KP.Expression
                                                (KP.UnaryPostfix
                                                    (KP.Postfix
                                                        (KP.PrimaryLiteral
                                                            (KP.LiteralDouble
                                                                (KP.DoubleConst 5.0)
                                                            )
                                                        )
                                                        Nothing
                                                    )
                                                )
                                                []
                                            )
                                            []
                                        )
                                    )
                                )
                            )
                            [
                                (
                                    KP.BinaryOp
                                        (KP.Identifier "-")
                                    ,
                                    KP.UnaryPostfix
                                        (KP.Postfix
                                            (KP.PrimaryLiteral
                                                (KP.LiteralDouble
                                                    (KP.DoubleConst 2.0)
                                                )
                                            )
                                            Nothing
                                        )
                                ),
                                (
                                    KP.BinaryOp
                                        (KP.Identifier "*")
                                    ,
                                    KP.UnaryPostfix
                                        (KP.Postfix
                                            (KP.PrimaryLiteral
                                                (KP.LiteralDouble
                                                    (KP.DoubleConst 3.0)
                                                )
                                            )
                                            Nothing
                                        )
                                ),
                                (
                                    KP.BinaryOp
                                        (KP.Identifier "+")
                                    ,
                                    KP.UnaryPostfix
                                        (KP.Postfix
                                            (KP.PrimaryLiteral
                                                (KP.LiteralDouble
                                                    (KP.DoubleConst 1.0)
                                                )
                                            )
                                            Nothing
                                        )
                                )
                            ]
                        )
                        []
                    )
            ]
    it "Simple unary expression: -1;" $
        parseKoak "-1;"
        == KP.Stmt [
            KP.KdefExpression
                (KP.Expressions
                    (KP.Expression
                        (KP.Unary
                            (KP.UnaryOp
                                (KP.Identifier "-")
                            )
                            (KP.UnaryPostfix
                                (KP.Postfix
                                    (KP.PrimaryLiteral
                                        (KP.LiteralDecimal
                                            (KP.DecimalConst 1)
                                        )
                                    )
                                    Nothing
                                )
                            )
                        )
                        []
                    )
                    []
                )
            ]
    it "Deep unary expression: - + + -1;" $
        parseKoak "- + + -1;"
        == KP.Stmt [
                KP.KdefExpression
                    (KP.Expressions
                        (KP.Expression
                            (KP.Unary
                                (KP.UnaryOp
                                    (KP.Identifier "-")
                                )
                                (KP.Unary
                                    (KP.UnaryOp
                                        (KP.Identifier "+")
                                    )
                                    (KP.Unary
                                        (KP.UnaryOp
                                            (KP.Identifier "+")
                                        )
                                        (KP.Unary
                                            (KP.UnaryOp
                                                (KP.Identifier "-")
                                            )
                                            (KP.UnaryPostfix
                                                (KP.Postfix
                                                    (KP.PrimaryLiteral
                                                        (KP.LiteralDecimal
                                                            (KP.DecimalConst 1)
                                                        )
                                                    )
                                                    Nothing
                                                )
                                            )
                                        )
                                    )
                                )
                            )
                            []
                        )
                        []
                    )
            ]
    it "Simple if: if a then b;" $
        parseKoak "if a then b;"
        == KP.Stmt [
                KP.KdefExpression
                    (KP.ExpressionIf
                        (KP.If
                            (KP.Expression
                                (KP.UnaryPostfix
                                    (KP.Postfix
                                        (KP.PrimaryIdentifier
                                            (KP.Identifier "a")
                                        )
                                        Nothing
                                    )
                                )
                                []
                            )
                            (KP.Expressions
                                (KP.Expression
                                    (KP.UnaryPostfix
                                        (KP.Postfix
                                            (KP.PrimaryIdentifier
                                                (KP.Identifier "b")
                                            )
                                            Nothing
                                        )
                                    )
                                    []
                                )
                                []
                            )
                            Nothing
                        )
                    )
            ]
    it "Simple if else: if a then b else c;" $
        parseKoak "if a then b else c;"
        == KP.Stmt [
                KP.KdefExpression
                    (KP.ExpressionIf
                        (KP.If
                            (KP.Expression
                                (KP.UnaryPostfix
                                    (KP.Postfix
                                        (KP.PrimaryIdentifier
                                            (KP.Identifier "a")
                                        )
                                        Nothing
                                    )
                                )
                                []
                            )
                            (KP.Expressions
                                (KP.Expression
                                    (KP.UnaryPostfix
                                        (KP.Postfix
                                            (KP.PrimaryIdentifier
                                                (KP.Identifier "b")
                                            )
                                            Nothing
                                        )
                                    )
                                    []
                                )
                                []
                            )
                            (Just $ KP.Expressions
                                (KP.Expression
                                    (KP.UnaryPostfix
                                        (KP.Postfix
                                            (KP.PrimaryIdentifier
                                                (KP.Identifier "c")
                                            )
                                            Nothing
                                        )
                                    )
                                    []
                                )
                                []
                            )
                        )
                    )
            ]
    it "Simple while: while a do b;" $
        parseKoak "while a do b;"
        == KP.Stmt [
                KP.KdefExpression
                    (KP.ExpressionWhile
                        (KP.While
                            (KP.Expression
                                (KP.UnaryPostfix
                                    (KP.Postfix
                                        (KP.PrimaryIdentifier
                                            (KP.Identifier "a")
                                        )
                                        Nothing
                                    )
                                )
                                []
                            )
                            (KP.Expressions
                                (KP.Expression
                                    (KP.UnaryPostfix
                                        (KP.Postfix
                                            (KP.PrimaryIdentifier
                                                (KP.Identifier "b")
                                            )
                                            Nothing
                                        )
                                    )
                                    []
                                )
                                []
                            )
                        )
                    )
            ]
    it "real world while: len = 99 : i = 0 : (while i < len do print(i) : i = i + 1) : print(i);" $
        parseKoak "len = 99 : i = 0 : (while i < len do print(i) : i = i + 1) : print(i);"
        == KP.Stmt [
                KP.KdefExpression
                    (KP.Expressions
                        (KP.Expression
                            (KP.UnaryPostfix
                                (KP.Postfix
                                    (KP.PrimaryIdentifier
                                        (KP.Identifier "len")
                                    )
                                    Nothing
                                )
                            )
                            [
                                (
                                    KP.BinaryOp
                                        (KP.Identifier "=")
                                    ,
                                    KP.UnaryPostfix
                                        (KP.Postfix
                                            (KP.PrimaryLiteral
                                                (KP.LiteralDecimal
                                                    (KP.DecimalConst 99)
                                                )
                                            )
                                            Nothing
                                        )
                                )
                            ]
                        )
                        [
                            KP.Expression
                                (KP.UnaryPostfix
                                    (KP.Postfix
                                        (KP.PrimaryIdentifier
                                            (KP.Identifier "i")
                                        )
                                        Nothing
                                    )
                                )
                                [
                                    (
                                        KP.BinaryOp
                                            (KP.Identifier "=")
                                        ,
                                        KP.UnaryPostfix
                                            (KP.Postfix
                                                (KP.PrimaryLiteral
                                                    (KP.LiteralDecimal
                                                        (KP.DecimalConst 0)
                                                    )
                                                )
                                                Nothing
                                            )
                                    )
                                ],
                            KP.Expression
                                (KP.UnaryPostfix
                                    (KP.Postfix
                                        (KP.PrimaryExpressions
                                            (KP.ExpressionWhile
                                                (KP.While
                                                    (KP.Expression
                                                        (KP.UnaryPostfix
                                                            (KP.Postfix
                                                                (KP.PrimaryIdentifier
                                                                    (KP.Identifier "i")
                                                                )
                                                                Nothing
                                                            )
                                                        )
                                                        [
                                                            (
                                                                KP.BinaryOp
                                                                    (KP.Identifier "<")
                                                                ,
                                                                KP.UnaryPostfix
                                                                    (KP.Postfix
                                                                        (KP.PrimaryIdentifier
                                                                            (KP.Identifier "len")
                                                                        )
                                                                        Nothing
                                                                    )
                                                            )
                                                        ]
                                                    )
                                                    (KP.Expressions
                                                        (KP.Expression
                                                            (KP.UnaryPostfix
                                                                (KP.Postfix
                                                                    (KP.PrimaryIdentifier
                                                                        (KP.Identifier "print")
                                                                    )
                                                                    (Just $ KP.CallExpression
                                                                        (Just $ KP.CallExpressionArgs
                                                                            (KP.Expression
                                                                                (KP.UnaryPostfix
                                                                                    (KP.Postfix
                                                                                        (KP.PrimaryIdentifier
                                                                                            (KP.Identifier "i")
                                                                                        )
                                                                                        Nothing
                                                                                    )
                                                                                )
                                                                                []
                                                                            )
                                                                            []
                                                                        )
                                                                    )
                                                                )
                                                            )
                                                            []
                                                        )
                                                        [
                                                            KP.Expression
                                                                (KP.UnaryPostfix
                                                                    (KP.Postfix
                                                                        (KP.PrimaryIdentifier
                                                                            (KP.Identifier "i")
                                                                        )
                                                                        Nothing
                                                                    )
                                                                )
                                                                [
                                                                    (
                                                                        KP.BinaryOp
                                                                            (KP.Identifier "=")
                                                                        ,
                                                                        KP.UnaryPostfix
                                                                            (KP.Postfix
                                                                                (KP.PrimaryIdentifier
                                                                                    (KP.Identifier "i")
                                                                                )
                                                                                Nothing
                                                                            )
                                                                    ),
                                                                    (
                                                                        KP.BinaryOp
                                                                            (KP.Identifier "+")
                                                                        ,
                                                                        KP.UnaryPostfix
                                                                            (KP.Postfix
                                                                                (KP.PrimaryLiteral
                                                                                    (KP.LiteralDecimal
                                                                                        (KP.DecimalConst 1)
                                                                                    )
                                                                                )
                                                                                Nothing
                                                                            )
                                                                    )
                                                                ]
                                                        ]
                                                    )
                                                )
                                            )
                                        )
                                        Nothing
                                    )
                                )
                                [],

                            KP.Expression
                                (KP.UnaryPostfix
                                    (KP.Postfix
                                        (KP.PrimaryIdentifier
                                            (KP.Identifier "print")
                                        )
                                        (Just $ KP.CallExpression
                                            (Just $ KP.CallExpressionArgs
                                                (KP.Expression
                                                    (KP.UnaryPostfix
                                                        (KP.Postfix
                                                            (KP.PrimaryIdentifier
                                                                (KP.Identifier "i")
                                                            )
                                                            Nothing
                                                        )
                                                    )
                                                    []
                                                )
                                                []
                                            )
                                        )
                                    )
                                )
                                []
                        ]
                    )
            ]
    it "Simple for: for i = 1, i < n, 1 in print(42);" $
        parseKoak "for i = 1, i < n, 1 in print(42);"
        == KP.Stmt [
                KP.KdefExpression
                    (KP.ExpressionFor
                        (KP.For
                            (KP.Expression
                                (KP.UnaryPostfix
                                    (KP.Postfix
                                        (KP.PrimaryIdentifier (KP.Identifier "i"))
                                        Nothing
                                    )
                                )
                                [
                                    (
                                        (KP.BinaryOp (KP.Identifier "=")),
                                        (KP.UnaryPostfix
                                            (KP.Postfix
                                                (KP.PrimaryLiteral
                                                    (KP.LiteralDecimal
                                                        (KP.DecimalConst 1)
                                                    )
                                                )
                                                Nothing
                                            )
                                        )
                                    )
                                ]
                            )
                            (KP.Expression
                                (KP.UnaryPostfix
                                    (KP.Postfix
                                        (KP.PrimaryIdentifier (KP.Identifier "i"))
                                        Nothing
                                    )
                                )
                                [
                                    (
                                        (KP.BinaryOp (KP.Identifier "<")),
                                        (KP.UnaryPostfix
                                            (KP.Postfix
                                                (KP.PrimaryIdentifier (KP.Identifier "n"))
                                                Nothing
                                            )
                                        )
                                    )

                                ]
                            )
                            (KP.Expression
                                (KP.UnaryPostfix
                                    (KP.Postfix
                                        (KP.PrimaryLiteral
                                            (KP.LiteralDecimal
                                                (KP.DecimalConst 1)
                                            )
                                        )
                                        Nothing
                                    )
                                )
                                []
                            )
                            (KP.Expressions
                                (KP.Expression
                                    (KP.UnaryPostfix
                                        (KP.Postfix
                                            (KP.PrimaryIdentifier
                                                (KP.Identifier "print")
                                            )
                                            (Just $ KP.CallExpression
                                                (Just $ KP.CallExpressionArgs
                                                    (KP.Expression
                                                        (KP.UnaryPostfix
                                                            (KP.Postfix
                                                                (KP.PrimaryLiteral
                                                                    (KP.LiteralDecimal
                                                                        (KP.DecimalConst 42)
                                                                    )
                                                                )
                                                                Nothing
                                                            )
                                                        )
                                                        []
                                                    )
                                                    []
                                                )
                                            )
                                        )
                                    )
                                    []
                                )
                                []
                            )
                        )
                    )
            ]
