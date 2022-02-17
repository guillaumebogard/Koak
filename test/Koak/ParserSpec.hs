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

-- def foo () : int 42;
-- test (5.0) - 2 * 3 + 1;

-- def test ( x : double ) : double x + 2.0;
-- test (5.0) - 2 * 3 + 1;
-- ready> ;
-- ...

