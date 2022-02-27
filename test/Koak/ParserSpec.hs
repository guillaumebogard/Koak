--
-- EPITECH PROJECT, 2022
-- koak
-- File description:
-- Koak.LexerSpec
--

module Koak.ParserSpec  ( spec ) where

import Test.Hspec       ( Spec
                        , it
                        , shouldBe
                        , shouldThrow
                        , anyException
                        )

import Koak.Lexer as KL ( Token(..) )
import Koak.Parser as KP

import qualified Data.Ord
import qualified GHC.IO.Handle.Internals

spec :: Spec
spec = do
    it "One simple def: def foo () : int 42;" $ do
        parseKoak
            [
                KL.Word "def",
                KL.Word "foo",
                KL.OpenParenthesis,
                KL.ClosedParenthesis,
                KL.Colon,
                KL.Word "int",
                KL.Number 42,
                KL.SemiColon
            ] == [
                    (KP.KDEFS_DEFS
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
                                            (Nothing)
                                        )
                                    )
                                    []
                                )
                                []
                            )
                        )
                    )
                ]
    it "Simple function call without args: foo();" $ do
        parseKoak
            [
                KL.Word "foo",
                KL.OpenParenthesis,
                KL.ClosedParenthesis,
                KL.SemiColon
            ] == [
                    (KP.KDEFS_EXPR
                        (KP.EXPRESSIONS
                            (KP.EXPRESSION
                                (KP.UNARY_POSTFIX
                                    (KP.POSTFIX
                                        (KP.PRIMARY_IDENTIFIER
                                            (KP.IDENTIFIER "foo")
                                        )
                                        (Just $ KP.CALL_EXPR
                                            (Nothing)
                                        )
                                    )
                                )
                                []
                            )
                            []
                        )
                    )
                ]
    it "Simple function call with one number as argument: foo(1);" $ do
        parseKoak
            [
                KL.Word "foo",
                KL.OpenParenthesis,
                KL.Number 1,
                KL.ClosedParenthesis,
                KL.SemiColon
            ] == [
                    (KP.KDEFS_EXPR
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
                    )
                ]
    it "Simple function call with one variable as argument: foo(my_var);" $ do
        parseKoak
            [
                KL.Word "foo",
                KL.OpenParenthesis,
                KL.Word "my_var",
                KL.ClosedParenthesis,
                KL.SemiColon
            ] == [
                    (KP.KDEFS_EXPR
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
                    )
                ]
    it "Simple function call with multiple primary arguments: foo(1, my_var, 3.14);" $ do
        parseKoak
            [
                KL.Word "foo",
                KL.OpenParenthesis,
                KL.Number 1,
                KL.Comma,
                KL.Word "my_var",
                KL.Comma,
                KL.FloatingNumber 3.14,
                KL.ClosedParenthesis,
                KL.SemiColon
            ] == [
                    (KP.KDEFS_EXPR
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
                                                    ),
                                                    (KP.EXPRESSION
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
                                                    )    
                                                ]
                                            )
                                        )
                                    )
                                )
                                []
                            )
                            []
                        )
                    )
                ]
    it "function call with multiple arguments and expressions: foo(-1 * 3, my_var, bar(1, -2.12), my_var_2 / 2);" $ do
        parseKoak
            [
                KL.Word "foo",
                KL.OpenParenthesis,
                KL.Minus,
                KL.Number 1,
                KL.Multiply,
                KL.Number 3,
                KL.Comma,
                KL.Word "my_var",
                KL.Comma,
                KL.Word "bar",
                KL.OpenParenthesis,
                KL.Number 1,
                KL.Comma,
                KL.Minus,
                KL.FloatingNumber 2.12,
                KL.ClosedParenthesis,
                KL.Comma,
                KL.Word "my_var_2",
                KL.Divide,
                KL.Number 2,
                KL.ClosedParenthesis,
                KL.SemiColon
            ] == [
                    (KP.KDEFS_EXPR
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
                                                        (KP.UN_MINUS)
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
                                                            (KP.BIN_MULT),
                                                            (KP.UNARY_POSTFIX
                                                                (KP.POSTFIX
                                                                    (KP.PRIMARY_LITERAL
                                                                        (KP.LITERAL_DECIMAL
                                                                            (KP.DECIMAL_CONST 3)   
                                                                        )
                                                                    )
                                                                   Nothing
                                                                )
                                                            )
                                                        )
                                                    ]
                                                )
                                                [
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
                                                    ),
                                                    (KP.EXPRESSION
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
                                                                            (KP.EXPRESSION
                                                                                (KP.UNARY_UN
                                                                                    (KP.UN_MINUS)
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
                                                                            )   
                                                                        ]
                                                                    )
                                                                )
                                                            )
                                                        )
                                                        []
                                                    ),
                                                    (KP.EXPRESSION
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
                                                                (KP.BIN_DIV),
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
                                                            )
                                                        ]
                                                    )    
                                                ]
                                            )
                                        )
                                    )
                                )
                                []
                            )
                            []
                        )
                    )
                ]
    it "Simple unary expression: -1;" $ do
        parseKoak
            [
                KL.Minus,
                KL.Number 1,
                KL.SemiColon
            ] == [
                    (KP.KDEFS_EXPR
                        (KP.EXPRESSIONS
                            (KP.EXPRESSION
                                (KP.UNARY_UN
                                    (KP.UN_MINUS)
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
                    )                ]
    it "Deep unary expression: -++-1;" $ do
        parseKoak
            [
                KL.Minus,
                KL.Plus,
                KL.Plus,
                KL.Minus,
                KL.Number 1,
                KL.SemiColon
            ] == [
                    (KP.KDEFS_EXPR
                        (KP.EXPRESSIONS
                            (KP.EXPRESSION
                                (KP.UNARY_UN
                                    (KP.UN_MINUS)
                                    (KP.UNARY_UN
                                        (KP.UN_PLUS)
                                        (KP.UNARY_UN
                                            (KP.UN_PLUS)
                                            (KP.UNARY_UN
                                                (KP.UN_MINUS)
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
                    )
                ]

-- foo(): bar(2): foobar(1, 2, 3)

-- def foo () : int 42;
-- test (5.0) - 2 * 3 + 1;

-- def test ( x : double ) : double x + 2.0;
-- test (5.0) - 2 * 3 + 1;
-- ready> ;
-- ...

