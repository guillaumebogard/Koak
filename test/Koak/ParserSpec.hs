--
-- EPITECH PROJECT, 2022
-- koak
-- File description:
-- Koak.LexerSpec
--

module Koak.ParserSpec  ( spec ) where

import Test.Hspec       ( Spec
                        , it, shouldBe, shouldThrow, anyException
                        )

import Koak.Lexer as KL ( Token(..)
                        )
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
                                        (Just (KP.CALL_EXPR
                                            (Nothing)
                                        ))
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
                                        (Just (KP.CALL_EXPR
                                            (Just (KP.CALL_EXPR_ARGS
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
                                            ))
                                        ))
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
                                        (Just (KP.CALL_EXPR
                                            (Just (KP.CALL_EXPR_ARGS
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
                                            ))
                                        ))
                                    )
                                )
                                []
                            )
                            []
                        )
                    )
                ]
    it "Simple function call with multiple primary arguments: foo(1, my_var, 3);" $ do
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
                                        (Just (KP.CALL_EXPR
                                            (Just (KP.CALL_EXPR_ARGS
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
                                                                    (KP.LITERAL_DECIMAL
                                                                        (KP.DECIMAL_CONST 3)
                                                                    )
                                                                )
                                                                Nothing
                                                            )
                                                        )
                                                        []
                                                    )    
                                                ]
                                            ))
                                        ))
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
                                        (Just (KP.CALL_EXPR
                                            (Nothing)
                                        ))
                                    )
                                )
                                []
                            )
                            []
                        )
                    )
                ]
    it "Deep unary expression: -++-1;" $ do
        parseKoak
            [
                KL.Minus,
                KL.Plus,
                KL.Plus,
                KL.Minus,
                KL.Number 1
            ] == [
                    (KP.KDEFS_EXPR
                        (KP.EXPRESSIONS
                            (KP.EXPRESSION
                                (KP.UNARY_UN
                                    (KP.U_MINUS)
                                    (KP.UNARY_UN
                                        (KP.U_PLUS)
                                        (KP.UNARY_UN
                                            (KP.U_PLUS)
                                            (KP.UNARY_UN
                                                (KP.U_MINUS)
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

