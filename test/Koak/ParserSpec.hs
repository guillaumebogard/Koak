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
        parseKoak [
            KL.Word "def",
            KL.Word "foo",
            KL.OpenParenthesis,
            KL.ClosedParenthesis,
            KL.Colon,
            KL.Word "int",
            KL.Number 42,
            KL.SemiColon]
            == [
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
                                                (DECIMAL_CONST 42)
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
                ]
    it "Simple function call without args: foo();" $ do
        parseKoak [
            KL.Word "foo",
            KL.OpenParenthesis,
            KL.ClosedParenthesis,
            KL.SemiColon]
            == [
                KP.KDEFS_EXPR
                    (EXPRESSIONS
                        (EXPRESSION
                            (UNARY_POSTFIX
                                (POSTFIX
                                    (PRIMARY_IDENTIFIER
                                        (IDENTIFIER "foo")
                                    )
                                    (Just (CALL_EXPR
                                        (Nothing)
                                    ))
                                )
                            )
                            []
                        )
                        []
                    )
                ]
    it "Simple function call with one number as argument: foo(1);" $ do
        parseKoak [
            KL.Word "foo",
            KL.OpenParenthesis,
            KL.ClosedParenthesis,
            KL.SemiColon]
            == [
                KP.KDEFS_EXPR
                    (EXPRESSIONS
                        (EXPRESSION
                            (UNARY_POSTFIX
                                (POSTFIX
                                    (PRIMARY_IDENTIFIER
                                        (IDENTIFIER "foo")
                                    )
                                    (Just (CALL_EXPR
                                        (Just (CALL_EXPR_ARGS
                                            (EXPRESSION
                                                (UNARY_POSTFIX
                                                    (POSTFIX
                                                        (PRIMARY_LITERAL
                                                            (LITERAL_DECIMAL
                                                                (DECIMAL_CONST 1)
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
                ]
    it "Simple function call with one variable as argument: foo(my_var);" $ do
        parseKoak [
            KL.Word "foo",
            KL.OpenParenthesis,
            KL.ClosedParenthesis,
            KL.SemiColon]
            == [
                KP.KDEFS_EXPR
                    (EXPRESSIONS
                        (EXPRESSION
                            (UNARY_POSTFIX
                                (POSTFIX
                                    (PRIMARY_IDENTIFIER
                                        (IDENTIFIER "foo")
                                    )
                                    (Just (CALL_EXPR
                                        (Just (CALL_EXPR_ARGS
                                            (EXPRESSION
                                                (UNARY_POSTFIX
                                                    (POSTFIX
                                                        (PRIMARY_IDENTIFIER
                                                            (IDENTIFIER "my_var")
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
                ]
    it "Simple function call with multiple primary arguments: foo(1, my_var, 3);" $ do
        parseKoak [
            KL.Word "foo",
            KL.OpenParenthesis,
            KL.ClosedParenthesis,
            KL.SemiColon]
            == [
                KP.KDEFS_EXPR
                    (EXPRESSIONS
                        (EXPRESSION
                            (UNARY_POSTFIX
                                (POSTFIX
                                    (PRIMARY_IDENTIFIER
                                        (IDENTIFIER "foo")
                                    )
                                    (Just (CALL_EXPR
                                        (Just (CALL_EXPR_ARGS
                                            (EXPRESSION
                                                (UNARY_POSTFIX
                                                    (POSTFIX
                                                        (PRIMARY_LITERAL
                                                            (LITERAL_DECIMAL
                                                                (DECIMAL_CONST 1)
                                                            )
                                                        )
                                                        Nothing
                                                    )
                                                )
                                                []
                                            )
                                            [
                                                (EXPRESSION
                                                    (UNARY_POSTFIX
                                                        (POSTFIX
                                                            (PRIMARY_IDENTIFIER
                                                                (IDENTIFIER "my_var")
                                                            )
                                                            Nothing
                                                        )
                                                    )
                                                    []
                                                ),
                                                (EXPRESSION
                                                    (UNARY_POSTFIX
                                                        (POSTFIX
                                                            (PRIMARY_LITERAL
                                                                (LITERAL_DECIMAL
                                                                    (DECIMAL_CONST 3)
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
                ]

-- foo(): bar(2): foobar(1, 2, 3)

-- def foo () : int 42;
-- test (5.0) - 2 * 3 + 1;

-- def test ( x : double ) : double x + 2.0;
-- test (5.0) - 2 * 3 + 1;
-- ready> ;
-- ...

