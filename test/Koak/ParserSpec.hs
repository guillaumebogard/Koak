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
                         , anyException
                         )

import Koak.Lexer  as KL ( Token(..) )
import Koak.Parser as KP

import qualified Data.Ord

spec :: Spec
spec = do
    it "One simple def: def foo () : int 42;" $
        parseKoak
        [
            KL.Word "def",
            KL.Word "foo",
            KL.OpenParenthesis,
            KL.ClosedParenthesis,
            KL.Colon,
            KL.Word "int",
            KL.IntegerNumber 42,
            KL.SemiColon
        ] == [
                KP.KDEFS_DEFS
                    (KP.DEFS
                        (KP.PROTOTYPE
                            (KP.IDENTIFIER "foo")
                            (KP.PROTOTYPE_ARGS [] KP.INT)
                        )
                        (KP.EXPRESSIONS
                            (KP.EXPRESSION
                                (KP.UNARY_POSTFIX
                                    (KP.POSTFIX
                                        (KP.PRIMARY_LITERAL
                                            (KP.LITERAL_DECIMAL
                                                (KP.DECIMAL_CONST 42)
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
        parseKoak
        [
            KL.Word "def",
            KL.Word "foo",
            KL.OpenParenthesis,
            KL.Word "x",
            KL.Colon,
            KL.Word "int",
            KL.ClosedParenthesis,
            KL.Colon,
            KL.Word "int",
            KL.IntegerNumber 42,
            KL.SemiColon
        ] == [
                KP.KDEFS_DEFS
                    (KP.DEFS
                        (KP.PROTOTYPE
                            (KP.IDENTIFIER "foo")
                            (KP.PROTOTYPE_ARGS
                                [
                                    KP.PROTOTYPE_ID
                                        (KP.IDENTIFIER "x")
                                        (KP.INT)
                                ]
                                KP.INT)
                        )
                        (KP.EXPRESSIONS
                            (KP.EXPRESSION
                                (KP.UNARY_POSTFIX
                                    (KP.POSTFIX
                                        (KP.PRIMARY_LITERAL
                                            (KP.LITERAL_DECIMAL
                                                (KP.DECIMAL_CONST 42)
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
        parseKoak
        [
            KL.Word "def",
            KL.Word "foo",
            KL.OpenParenthesis,
            KL.Word "x",
            KL.Colon,
            KL.Word "int",
            KL.Word "y",
            KL.Colon,
            KL.Word "double",
            KL.Word "z",
            KL.Colon,
            KL.Word "void",
            KL.ClosedParenthesis,
            KL.Colon,
            KL.Word "int",
            KL.IntegerNumber 42,
            KL.SemiColon
        ] == [
                KP.KDEFS_DEFS
                    (KP.DEFS
                        (KP.PROTOTYPE
                            (KP.IDENTIFIER "foo")
                            (KP.PROTOTYPE_ARGS
                                [
                                    KP.PROTOTYPE_ID
                                        (KP.IDENTIFIER "x")
                                        KP.INT,
                                    KP.PROTOTYPE_ID
                                        (KP.IDENTIFIER "y")
                                        KP.DOUBLE,
                                    KP.PROTOTYPE_ID
                                        (KP.IDENTIFIER "z")
                                        KP.VOID
                                ]
                                KP.INT)
                        )
                        (KP.EXPRESSIONS
                            (KP.EXPRESSION
                                (KP.UNARY_POSTFIX
                                    (KP.POSTFIX
                                        (KP.PRIMARY_LITERAL
                                            (KP.LITERAL_DECIMAL
                                                (KP.DECIMAL_CONST 42)
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
        parseKoak
        [
            KL.Word "def",
            KL.Word "foo",
            KL.OpenParenthesis,
            KL.Word "x",
            KL.Colon,
            KL.Word "int",
            KL.Word "y",
            KL.Colon,
            KL.Word "double",
            KL.Word "z",
            KL.Colon,
            KL.Word "void",
            KL.ClosedParenthesis,
            KL.Colon,
            KL.Word "int",
            KL.IntegerNumber 42,
            KL.SemiColon,
            KL.Word "def",            
            KL.Word "bar",
            KL.OpenParenthesis,
            KL.Word "a",
            KL.Colon,
            KL.Word "double",
            KL.ClosedParenthesis,
            KL.Colon,
            KL.Word "double",
            KL.Word "a",
            KL.SemiColon            
        ] == [
                KP.KDEFS_DEFS
                    (KP.DEFS
                        (KP.PROTOTYPE
                            (KP.IDENTIFIER "foo")
                            (KP.PROTOTYPE_ARGS
                                [
                                    KP.PROTOTYPE_ID
                                        (KP.IDENTIFIER "x")
                                        KP.INT,
                                    KP.PROTOTYPE_ID
                                        (KP.IDENTIFIER "y")
                                        KP.DOUBLE,
                                    KP.PROTOTYPE_ID
                                        (KP.IDENTIFIER "z")
                                        KP.VOID
                                ]
                                KP.INT)
                        )
                        (KP.EXPRESSIONS
                            (KP.EXPRESSION
                                (KP.UNARY_POSTFIX
                                    (KP.POSTFIX
                                        (KP.PRIMARY_LITERAL
                                            (KP.LITERAL_DECIMAL
                                                (KP.DECIMAL_CONST 42)
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
                KP.KDEFS_DEFS
                    (KP.DEFS
                        (KP.PROTOTYPE
                            (KP.IDENTIFIER "bar")
                            (KP.PROTOTYPE_ARGS
                                [
                                    KP.PROTOTYPE_ID
                                        (KP.IDENTIFIER "a")
                                        KP.DOUBLE
                                ]
                                KP.DOUBLE)
                        )
                        (KP.EXPRESSIONS
                            (KP.EXPRESSION
                                (KP.UNARY_POSTFIX
                                    (KP.POSTFIX
                                        (KP.PRIMARY_IDENTIFIER
                                            (KP.IDENTIFIER "a")
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
        parseKoak
        [
            KL.Word "foo",
            KL.OpenParenthesis,
            KL.ClosedParenthesis,
            KL.SemiColon
        ] == [
                KP.KDEFS_EXPR
                    (KP.EXPRESSIONS
                        (KP.EXPRESSION
                            (KP.UNARY_POSTFIX
                                (KP.POSTFIX
                                    (KP.PRIMARY_IDENTIFIER
                                        (KP.IDENTIFIER "foo")
                                    )
                                    (Just $ KP.CALL_EXPR
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
        parseKoak
        [
            KL.Word "foo",
            KL.OpenParenthesis,
            KL.IntegerNumber 1,
            KL.ClosedParenthesis,
            KL.SemiColon
        ] == [
                KP.KDEFS_EXPR
                    (KP.EXPRESSIONS
                        (KP.EXPRESSION
                            (KP.UNARY_POSTFIX
                                (KP.POSTFIX
                                    (KP.PRIMARY_IDENTIFIER
                                        (KP.IDENTIFIER "foo")
                                    )
                                    (Just $ KP.CALL_EXPR
                                        (Just $ KP.CALL_EXPR_ARGS
                                            (KP.EXPRESSION
                                                (KP.UNARY_POSTFIX
                                                    (KP.POSTFIX
                                                        (KP.PRIMARY_LITERAL
                                                            (KP.LITERAL_DECIMAL
                                                                (KP.DECIMAL_CONST 1)
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
        parseKoak
        [
            KL.Word "foo",
            KL.OpenParenthesis,
            KL.Word "my_var",
            KL.ClosedParenthesis,
            KL.SemiColon
        ] == [
                KP.KDEFS_EXPR
                    (KP.EXPRESSIONS
                        (KP.EXPRESSION
                            (KP.UNARY_POSTFIX
                                (KP.POSTFIX
                                    (KP.PRIMARY_IDENTIFIER
                                        (KP.IDENTIFIER "foo")
                                    )
                                    (Just $ KP.CALL_EXPR
                                        (Just $ KP.CALL_EXPR_ARGS
                                            (KP.EXPRESSION
                                                (KP.UNARY_POSTFIX
                                                    (KP.POSTFIX
                                                        (KP.PRIMARY_IDENTIFIER
                                                            (KP.IDENTIFIER "my_var")
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
        parseKoak
        [
            KL.Word "foo",
            KL.OpenParenthesis,
            KL.IntegerNumber 1,
            KL.Comma,
            KL.Word "my_var",
            KL.Comma,
            KL.FloatingNumber 3.14,
            KL.ClosedParenthesis,
            KL.SemiColon
        ] == [
                KP.KDEFS_EXPR
                    (KP.EXPRESSIONS
                        (KP.EXPRESSION
                            (KP.UNARY_POSTFIX
                                (KP.POSTFIX
                                    (KP.PRIMARY_IDENTIFIER
                                        (KP.IDENTIFIER "foo")
                                    )
                                    (Just $ KP.CALL_EXPR
                                        (Just $ KP.CALL_EXPR_ARGS
                                            (KP.EXPRESSION
                                                (KP.UNARY_POSTFIX
                                                    (KP.POSTFIX
                                                        (KP.PRIMARY_LITERAL
                                                            (KP.LITERAL_DECIMAL
                                                                (KP.DECIMAL_CONST 1)
                                                            )
                                                        )
                                                        Nothing
                                                    )
                                                )
                                                []
                                            )
                                            [
                                                KP.EXPRESSION
                                                    (KP.UNARY_POSTFIX
                                                        (KP.POSTFIX
                                                            (KP.PRIMARY_IDENTIFIER
                                                                (KP.IDENTIFIER "my_var")
                                                            )
                                                            Nothing
                                                        )
                                                    )
                                                    [],
                                                KP.EXPRESSION
                                                    (KP.UNARY_POSTFIX
                                                        (KP.POSTFIX
                                                            (KP.PRIMARY_LITERAL
                                                                (KP.LITERAL_DOUBLE
                                                                    (KP.DOUBLE_CONST 3.14)
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
        parseKoak
        [
            KL.Word "foo",
            KL.OpenParenthesis,
            KL.Word "-",
            KL.IntegerNumber 1,
            KL.Word "*",
            KL.IntegerNumber 3,
            KL.Comma,
            KL.Word "my_var",
            KL.Comma,
            KL.Word "bar",
            KL.OpenParenthesis,
            KL.IntegerNumber 1,
            KL.Comma,
            KL.Word "-",
            KL.FloatingNumber 2.12,
            KL.ClosedParenthesis,
            KL.Comma,
            KL.Word "my_var_2",
            KL.Word "/",
            KL.IntegerNumber 2,
            KL.ClosedParenthesis,
            KL.SemiColon
        ] == [
                KP.KDEFS_EXPR
                    (KP.EXPRESSIONS
                        (KP.EXPRESSION
                            (KP.UNARY_POSTFIX
                                (KP.POSTFIX
                                    (KP.PRIMARY_IDENTIFIER
                                        (KP.IDENTIFIER "foo")
                                    )
                                    (Just $ KP.CALL_EXPR
                                        (Just $ KP.CALL_EXPR_ARGS
                                            (KP.EXPRESSION
                                                (KP.UNARY_UN
                                                    KP.UN_MINUS
                                                        (KP.UNARY_POSTFIX
                                                            (KP.POSTFIX
                                                                (KP.PRIMARY_LITERAL
                                                                    (KP.LITERAL_DECIMAL
                                                                        (KP.DECIMAL_CONST 1)
                                                                    )
                                                                )
                                                                Nothing
                                                            )
                                                        )
                                                )
                                                [
                                                    (
                                                        KP.BIN_MULT,
                                                        KP.UNARY_POSTFIX
                                                            (KP.POSTFIX
                                                                (KP.PRIMARY_LITERAL
                                                                    (KP.LITERAL_DECIMAL
                                                                        (KP.DECIMAL_CONST 3)
                                                                    )
                                                                )
                                                               Nothing
                                                            )
                                                    )
                                                ]
                                            )
                                            [
                                                KP.EXPRESSION
                                                    (KP.UNARY_POSTFIX
                                                        (KP.POSTFIX
                                                            (KP.PRIMARY_IDENTIFIER
                                                                (KP.IDENTIFIER "my_var")
                                                            )
                                                            Nothing
                                                        )
                                                    )
                                                    [],
                                                KP.EXPRESSION
                                                    (KP.UNARY_POSTFIX
                                                        (KP.POSTFIX
                                                            (KP.PRIMARY_IDENTIFIER
                                                                (KP.IDENTIFIER "bar")
                                                            )
                                                            (Just $ KP.CALL_EXPR
                                                                (Just $ CALL_EXPR_ARGS
                                                                    (KP.EXPRESSION
                                                                        (KP.UNARY_POSTFIX
                                                                            (KP.POSTFIX
                                                                                (KP.PRIMARY_LITERAL
                                                                                    (KP.LITERAL_DECIMAL
                                                                                        (KP.DECIMAL_CONST 1)
                                                                                    )
                                                                                )
                                                                                Nothing
                                                                            )
                                                                        )
                                                                        []
                                                                    )
                                                                    [
                                                                        KP.EXPRESSION
                                                                            (KP.UNARY_UN
                                                                                KP.UN_MINUS
                                                                                (KP.UNARY_POSTFIX
                                                                                    (KP.POSTFIX
                                                                                        (KP.PRIMARY_LITERAL
                                                                                            (KP.LITERAL_DOUBLE
                                                                                                (KP.DOUBLE_CONST 2.12)
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
                                                KP.EXPRESSION
                                                    (KP.UNARY_POSTFIX
                                                        (KP.POSTFIX
                                                            (KP.PRIMARY_IDENTIFIER
                                                                (KP.IDENTIFIER "my_var_2")
                                                            )
                                                            Nothing
                                                        )
                                                    )
                                                    [
                                                        (
                                                            KP.BIN_DIV,
                                                            KP.UNARY_POSTFIX
                                                                (KP.POSTFIX
                                                                    (KP.PRIMARY_LITERAL
                                                                        (KP.LITERAL_DECIMAL
                                                                            (KP.DECIMAL_CONST 2)
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
        parseKoak
        [
            KL.Word "foo",
            KL.OpenParenthesis,
            KL.ClosedParenthesis,
            KL.Colon,
            KL.Word "bar",
            KL.OpenParenthesis,
            KL.IntegerNumber 2,
            KL.ClosedParenthesis,
            KL.Colon,
            KL.Word "foobar",
            KL.OpenParenthesis,
            KL.IntegerNumber 1,
            KL.Comma,
            KL.IntegerNumber 2,
            KL.Comma,
            KL.FloatingNumber 3.14,
            KL.ClosedParenthesis,
            KL.SemiColon
        ] == [
                KP.KDEFS_EXPR
                    (KP.EXPRESSIONS
                        (KP.EXPRESSION
                            (KP.UNARY_POSTFIX
                                (KP.POSTFIX
                                    (KP.PRIMARY_IDENTIFIER
                                        (KP.IDENTIFIER "foo")
                                    )
                                    (Just $ KP.CALL_EXPR Nothing)
                                )
                            )
                            []
                        )
                        [
                            KP.EXPRESSION
                                (KP.UNARY_POSTFIX
                                    (KP.POSTFIX
                                        (KP.PRIMARY_IDENTIFIER
                                            (KP.IDENTIFIER "bar")
                                        )
                                        (Just $ KP.CALL_EXPR
                                            (Just $ KP.CALL_EXPR_ARGS
                                                (KP.EXPRESSION
                                                    (KP.UNARY_POSTFIX
                                                        (KP.POSTFIX
                                                            (KP.PRIMARY_LITERAL
                                                                (KP.LITERAL_DECIMAL
                                                                    (KP.DECIMAL_CONST 2)
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
                            KP.EXPRESSION
                                (KP.UNARY_POSTFIX
                                    (KP.POSTFIX
                                        (KP.PRIMARY_IDENTIFIER
                                            (KP.IDENTIFIER "foobar")
                                        )
                                        (Just $ KP.CALL_EXPR
                                            (Just $ KP.CALL_EXPR_ARGS
                                                (KP.EXPRESSION
                                                    (KP.UNARY_POSTFIX
                                                        (KP.POSTFIX
                                                            (KP.PRIMARY_LITERAL
                                                                (KP.LITERAL_DECIMAL
                                                                    (KP.DECIMAL_CONST 1)
                                                                )
                                                            )
                                                            Nothing
                                                        )
                                                    )
                                                    []
                                                )
                                                [
                                                    KP.EXPRESSION
                                                        (KP.UNARY_POSTFIX
                                                            (KP.POSTFIX
                                                                (KP.PRIMARY_LITERAL
                                                                    (KP.LITERAL_DECIMAL
                                                                        (KP.DECIMAL_CONST 2)
                                                                    )
                                                                )
                                                                Nothing
                                                            )
                                                        )
                                                        [],
                                                    KP.EXPRESSION
                                                        (KP.UNARY_POSTFIX
                                                            (KP.POSTFIX
                                                                (KP.PRIMARY_LITERAL
                                                                    (KP.LITERAL_DOUBLE
                                                                        (KP.DOUBLE_CONST 3.14)
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
        parseKoak
        [
            KL.Word "def",
            KL.Word "test",
            KL.OpenParenthesis,
            KL.Word "x",
            KL.Colon,
            KL.Word "double",
            KL.ClosedParenthesis,
            KL.Colon,
            KL.Word "double",
            KL.Word "x",
            KL.Word "+",
            KL.FloatingNumber 2.0,
            KL.SemiColon,
            KL.Word "test",
            KL.OpenParenthesis,
            KL.FloatingNumber 5.0,
            KL.ClosedParenthesis,
            KL.Word "-",
            KL.FloatingNumber 2.0,
            KL.Word "*",
            KL.FloatingNumber 3.0,
            KL.Word "+",
            KL.FloatingNumber 1.0,
            KL.SemiColon
        ] == [
                KP.KDEFS_DEFS
                    (KP.DEFS
                        (KP.PROTOTYPE
                            (KP.IDENTIFIER "test")
                            (KP.PROTOTYPE_ARGS
                                [
                                    KP.PROTOTYPE_ID
                                        (KP.IDENTIFIER "x")
                                        KP.DOUBLE
                                ]
                                KP.DOUBLE
                            )
                        )
                        (KP.EXPRESSIONS
                            (KP.EXPRESSION
                                (KP.UNARY_POSTFIX
                                    (KP.POSTFIX
                                        (KP.PRIMARY_IDENTIFIER
                                            (KP.IDENTIFIER "x")
                                        )
                                        Nothing
                                    )
                                )
                                [
                                    (
                                        KP.BIN_PLUS,
                                        KP.UNARY_POSTFIX
                                            (KP.POSTFIX
                                                (KP.PRIMARY_LITERAL
                                                    (KP.LITERAL_DOUBLE
                                                        (KP.DOUBLE_CONST 2.0)
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
                KP.KDEFS_EXPR
                    (KP.EXPRESSIONS
                        (KP.EXPRESSION
                            (KP.UNARY_POSTFIX
                                (KP.POSTFIX
                                    (KP.PRIMARY_IDENTIFIER
                                        (KP.IDENTIFIER "test")
                                    )
                                    (Just $ KP.CALL_EXPR
                                        (Just $ KP.CALL_EXPR_ARGS
                                            (KP.EXPRESSION
                                                (KP.UNARY_POSTFIX
                                                    (KP.POSTFIX
                                                        (KP.PRIMARY_LITERAL
                                                            (KP.LITERAL_DOUBLE
                                                                (KP.DOUBLE_CONST 5.0)
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
                                    KP.BIN_MINUS,
                                    KP.UNARY_POSTFIX
                                        (KP.POSTFIX
                                            (KP.PRIMARY_LITERAL
                                                (KP.LITERAL_DOUBLE
                                                    (KP.DOUBLE_CONST 2.0)
                                                )
                                            )
                                            Nothing
                                        )
                                ),
                                (
                                    KP.BIN_MULT,
                                    KP.UNARY_POSTFIX
                                        (KP.POSTFIX
                                            (KP.PRIMARY_LITERAL
                                                (KP.LITERAL_DOUBLE
                                                    (KP.DOUBLE_CONST 3.0)
                                                )
                                            )
                                            Nothing
                                        )
                                ),
                                (
                                    KP.BIN_PLUS,
                                    KP.UNARY_POSTFIX
                                        (KP.POSTFIX
                                            (KP.PRIMARY_LITERAL
                                                (KP.LITERAL_DOUBLE
                                                    (KP.DOUBLE_CONST 1.0)
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
        parseKoak
        [
            KL.Word "-",
            KL.IntegerNumber 1,
            KL.SemiColon
        ] == [
                KP.KDEFS_EXPR
                    (KP.EXPRESSIONS
                        (KP.EXPRESSION
                            (KP.UNARY_UN
                                KP.UN_MINUS
                                (KP.UNARY_POSTFIX
                                    (KP.POSTFIX
                                        (KP.PRIMARY_LITERAL
                                            (KP.LITERAL_DECIMAL
                                                (KP.DECIMAL_CONST 1)
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
    it "Deep unary expression: -++-1;" $
        parseKoak
        [
            KL.Word "-",
            KL.Word "+",
            KL.Word "+",
            KL.Word "-",
            KL.IntegerNumber 1,
            KL.SemiColon
        ] == [
                KP.KDEFS_EXPR
                    (KP.EXPRESSIONS
                        (KP.EXPRESSION
                            (KP.UNARY_UN
                                KP.UN_MINUS
                                (KP.UNARY_UN
                                    KP.UN_PLUS
                                    (KP.UNARY_UN
                                        KP.UN_PLUS
                                        (KP.UNARY_UN
                                            KP.UN_MINUS
                                            (KP.UNARY_POSTFIX
                                                (KP.POSTFIX
                                                    (KP.PRIMARY_LITERAL
                                                        (KP.LITERAL_DECIMAL
                                                            (KP.DECIMAL_CONST 1)
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
        parseKoak
        [
            KL.Word "if",
            KL.Word "a",
            KL.Word "then",
            KL.Word "b",
            KL.SemiColon
        ] == [
                KP.KDEFS_EXPR
                    (KP.IF_EXPR
                        (KP.IF
                            (KP.EXPRESSION
                                (KP.UNARY_POSTFIX
                                    (KP.POSTFIX
                                        (KP.PRIMARY_IDENTIFIER
                                            (KP.IDENTIFIER "a")
                                        )
                                        Nothing
                                    )
                                )
                                []
                            )
                            (KP.EXPRESSIONS
                                (KP.EXPRESSION
                                    (KP.UNARY_POSTFIX
                                        (KP.POSTFIX
                                            (KP.PRIMARY_IDENTIFIER
                                                (KP.IDENTIFIER "b")
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
        parseKoak
        [
            KL.Word "if",
            KL.Word "a",
            KL.Word "then",
            KL.Word "b",
            KL.Word "else",
            KL.Word "c",
            KL.SemiColon
        ] == [
                KP.KDEFS_EXPR
                    (KP.IF_EXPR
                        (KP.IF
                            (KP.EXPRESSION
                                (KP.UNARY_POSTFIX
                                    (KP.POSTFIX
                                        (KP.PRIMARY_IDENTIFIER
                                            (KP.IDENTIFIER "a")
                                        )
                                        Nothing
                                    )
                                )
                                []
                            )
                            (KP.EXPRESSIONS
                                (KP.EXPRESSION
                                    (KP.UNARY_POSTFIX
                                        (KP.POSTFIX
                                            (KP.PRIMARY_IDENTIFIER
                                                (KP.IDENTIFIER "b")
                                            )
                                            Nothing
                                        )
                                    )
                                    []
                                )
                                []
                            )
                            (Just $ KP.EXPRESSIONS
                                (KP.EXPRESSION
                                    (KP.UNARY_POSTFIX
                                        (KP.POSTFIX
                                            (KP.PRIMARY_IDENTIFIER
                                                (KP.IDENTIFIER "c")
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
        parseKoak
        [
            KL.Word "while",
            KL.Word "a",
            KL.Word "do",
            KL.Word "b",
            KL.SemiColon
        ] == [
                KP.KDEFS_EXPR
                    (KP.WHILE_EXPR
                        (KP.WHILE
                            (KP.EXPRESSION
                                (KP.UNARY_POSTFIX
                                    (KP.POSTFIX
                                        (KP.PRIMARY_IDENTIFIER
                                            (KP.IDENTIFIER "a")
                                        )
                                        Nothing
                                    )
                                )
                                []
                            )
                            (KP.EXPRESSIONS
                                (KP.EXPRESSION
                                    (KP.UNARY_POSTFIX
                                        (KP.POSTFIX
                                            (KP.PRIMARY_IDENTIFIER
                                                (KP.IDENTIFIER "b")
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
        parseKoak
        [
            KL.Word "len",
            KL.Word "=",
            KL.IntegerNumber 99,
            KL.Colon,
            KL.Word "i",
            KL.Word "=",
            KL.IntegerNumber 0,
            KL.Colon,
            KL.OpenParenthesis,
            KL.Word "while",
            KL.Word "i",
            KL.Word "<",
            KL.Word "len",
            KL.Word "do",
            KL.Word "print",
            KL.OpenParenthesis,
            KL.Word "i",
            KL.ClosedParenthesis,
            KL.Colon,
            KL.Word "i",
            KL.Word "=",
            KL.Word "i",
            KL.Word "+",
            KL.IntegerNumber 1,
            KL.ClosedParenthesis,
            KL.Colon,
            KL.Word "print",
            KL.OpenParenthesis,
            KL.Word "i",
            KL.ClosedParenthesis,
            KL.SemiColon
        ] == [
                KP.KDEFS_EXPR
                    (KP.EXPRESSIONS
                        (KP.EXPRESSION
                            (KP.UNARY_POSTFIX
                                (KP.POSTFIX
                                    (KP.PRIMARY_IDENTIFIER
                                        (KP.IDENTIFIER "len")
                                    )
                                    Nothing
                                )
                            )
                            [
                                (
                                    KP.BIN_ASSIGN,
                                    KP.UNARY_POSTFIX
                                        (KP.POSTFIX
                                            (KP.PRIMARY_LITERAL
                                                (KP.LITERAL_DECIMAL
                                                    (KP.DECIMAL_CONST 99)
                                                )
                                            )
                                            Nothing
                                        )
                                )
                            ]
                        )
                        [
                            KP.EXPRESSION
                                (KP.UNARY_POSTFIX
                                    (KP.POSTFIX
                                        (KP.PRIMARY_IDENTIFIER
                                            (KP.IDENTIFIER "i")
                                        )
                                        Nothing
                                    )
                                )
                                [
                                    (
                                        KP.BIN_ASSIGN,
                                        KP.UNARY_POSTFIX
                                            (KP.POSTFIX
                                                (KP.PRIMARY_LITERAL
                                                    (KP.LITERAL_DECIMAL
                                                        (KP.DECIMAL_CONST 0)
                                                    )
                                                )
                                                Nothing
                                            )
                                    )
                                ],
                            KP.EXPRESSION
                                (KP.UNARY_POSTFIX
                                    (KP.POSTFIX
                                        (KP.PRIMARY_EXPRS
                                            (KP.WHILE_EXPR
                                                (KP.WHILE
                                                    (KP.EXPRESSION
                                                        (KP.UNARY_POSTFIX
                                                            (KP.POSTFIX
                                                                (KP.PRIMARY_IDENTIFIER
                                                                    (KP.IDENTIFIER "i")
                                                                )
                                                                Nothing
                                                            )
                                                        )
                                                        [
                                                            (
                                                                KP.BIN_LT,
                                                                KP.UNARY_POSTFIX
                                                                    (KP.POSTFIX
                                                                        (KP.PRIMARY_IDENTIFIER
                                                                            (KP.IDENTIFIER "len")
                                                                        )
                                                                        Nothing
                                                                    )
                                                            )
                                                        ]
                                                    )
                                                    (KP.EXPRESSIONS
                                                        (KP.EXPRESSION
                                                            (KP.UNARY_POSTFIX
                                                                (KP.POSTFIX
                                                                    (KP.PRIMARY_IDENTIFIER
                                                                        (KP.IDENTIFIER "print")
                                                                    )
                                                                    (Just $ KP.CALL_EXPR
                                                                        (Just $ KP.CALL_EXPR_ARGS
                                                                            (KP.EXPRESSION
                                                                                (KP.UNARY_POSTFIX
                                                                                    (KP.POSTFIX
                                                                                        (KP.PRIMARY_IDENTIFIER
                                                                                            (KP.IDENTIFIER "i")
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
                                                            KP.EXPRESSION
                                                                (KP.UNARY_POSTFIX
                                                                    (KP.POSTFIX
                                                                        (KP.PRIMARY_IDENTIFIER
                                                                            (KP.IDENTIFIER "i")
                                                                        )
                                                                        Nothing
                                                                    )
                                                                )
                                                                [
                                                                    (
                                                                        KP.BIN_ASSIGN,
                                                                        KP.UNARY_POSTFIX
                                                                            (KP.POSTFIX
                                                                                (KP.PRIMARY_IDENTIFIER
                                                                                    (KP.IDENTIFIER "i")
                                                                                )
                                                                                Nothing
                                                                            )
                                                                    ),
                                                                    (
                                                                        KP.BIN_PLUS,
                                                                        KP.UNARY_POSTFIX
                                                                            (KP.POSTFIX
                                                                                (KP.PRIMARY_LITERAL
                                                                                    (KP.LITERAL_DECIMAL
                                                                                        (KP.DECIMAL_CONST 1)
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

                            KP.EXPRESSION
                                (KP.UNARY_POSTFIX
                                    (KP.POSTFIX
                                        (KP.PRIMARY_IDENTIFIER
                                            (KP.IDENTIFIER "print")
                                        )
                                        (Just $ KP.CALL_EXPR
                                            (Just $ KP.CALL_EXPR_ARGS
                                                (KP.EXPRESSION
                                                    (KP.UNARY_POSTFIX
                                                        (KP.POSTFIX
                                                            (KP.PRIMARY_IDENTIFIER
                                                                (KP.IDENTIFIER "i")
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
        parseKoak
        [
            KL.Word "for",
            KL.Word "i",
            KL.Word "=",
            KL.IntegerNumber 1,
            KL.Comma ,
            KL.Word "i",
            KL.Word "<",
            KL.Word "n",
            KL.Comma,
            KL.IntegerNumber 1,
            KL.Word "in",
            KL.Word "print",
            KL.OpenParenthesis,
            KL.IntegerNumber 42,
            KL.ClosedParenthesis,
            KL.SemiColon
        ] == [
                KP.KDEFS_EXPR
                    (KP.FOR_EXPR
                        (KP.FOR
                            (KP.IDENTIFIER "i")
                            (KP.EXPRESSION
                                (KP.UNARY_POSTFIX
                                    (KP.POSTFIX
                                        (KP.PRIMARY_LITERAL
                                            (KP.LITERAL_DECIMAL
                                                (KP.DECIMAL_CONST 1)
                                            )
                                        )
                                        Nothing
                                    )
                                )
                                []
                            )
                            (KP.IDENTIFIER "i")
                            (KP.EXPRESSION
                                (KP.UNARY_POSTFIX
                                    (KP.POSTFIX
                                        (KP.PRIMARY_IDENTIFIER
                                            (KP.IDENTIFIER "n")
                                        )
                                        Nothing
                                    )
                                )
                                []
                            )
                            (KP.EXPRESSION
                                (KP.UNARY_POSTFIX
                                    (KP.POSTFIX
                                        (KP.PRIMARY_LITERAL
                                            (KP.LITERAL_DECIMAL
                                                (KP.DECIMAL_CONST 1)
                                            )
                                        )
                                        Nothing
                                    )
                                )
                                []
                            )
                            (KP.EXPRESSIONS
                                (KP.EXPRESSION
                                    (KP.UNARY_POSTFIX
                                        (KP.POSTFIX
                                            (KP.PRIMARY_IDENTIFIER
                                                (KP.IDENTIFIER "print")
                                            )
                                            (Just $ KP.CALL_EXPR
                                                (Just $ KP.CALL_EXPR_ARGS
                                                    (KP.EXPRESSION
                                                        (KP.UNARY_POSTFIX
                                                            (KP.POSTFIX
                                                                (KP.PRIMARY_LITERAL
                                                                    (KP.LITERAL_DECIMAL
                                                                        (KP.DECIMAL_CONST 42)
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
