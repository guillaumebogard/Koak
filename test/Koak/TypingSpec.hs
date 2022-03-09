--
-- EPITECH PROJECT, 2022
-- koak
-- File description:
-- Koak.TypingSpec
--

module Koak.TypingSpec                          ( spec ) where

import Control.Exception                        ( evaluate )

import Test.Hspec                               ( Spec
                                                , it
                                                , shouldBe
                                                , shouldThrow
                                                , anyException
                                                )

import qualified Koak.Parser as KP
import qualified Koak.Typing as KT
import qualified Koak.Typing.Exception as KTE   ( KoakTypingException(..) )

import Data.HashMap.Strict  as HM               ( HashMap
                                                , fromList
                                                , empty
                                                , member
                                                , insert
                                                )

spec :: Spec
spec = do
    it "Basic assign" $
        KT.checkKoakTyping (KP.getKdefsFromStmt $ KP.parseKoak "i = 1;")
            ==
            ()
    it "Complex assign" $
        KT.checkKoakTyping (KP.getKdefsFromStmt $ KP.parseKoak "i = 8 * 3 + 4.7; i = 2.0;")
            ==
            ()

    it "Bad assign, wrong type." $
        evaluate(KT.checkKoakTyping $ KP.getKdefsFromStmt $ KP.parseKoak "i = 1: i = 42.42;")
        `shouldThrow`
        (== KTE.AssignmentOfDifferentType (KP.Identifier "i") KP.Double KP.Int)
    it "Basic expression" $
        KT.checkKoakTyping (KP.getKdefsFromStmt $ KP.parseKoak "---1 + 3 * 4 / 1 + 3.0;")
            ==
            ()
    it "Basic function call" $
        KT.checkKoakTyping (KP.getKdefsFromStmt $ KP.parseKoak "def foo(): int 42; foo();")
            ==
            ()
    it "Basic function call with arguments" $
        KT.checkKoakTyping (KP.getKdefsFromStmt $ KP.parseKoak
            (
                "def foo(a:int b:double): int 42;" ++
                "foo(6, 4.2);"
            ))
            ==
            ()
    it "Complex function call with arguments" $
        KT.checkKoakTyping (KP.getKdefsFromStmt $ KP.parseKoak
            (
                "def foo(a:int b:double): int 42;" ++
                "foo(foo(1, 1 * 7.8 + 3 - -1), 4.2);"
            ))
            ==
            ()
    it "Bad function call. Bad argument number 1." $ do
        evaluate (KT.checkKoakTyping (KP.getKdefsFromStmt $ KP.parseKoak
            (
                "def foo(a:int b:double): int 42;" ++
                "foo(2.2);"
            )))
            `shouldThrow`
            (== KTE.MismatchedArgumentNumber (KP.Identifier "foo") 1)
    it "Bad function call. Bad argument number 1." $ do
        evaluate (KT.checkKoakTyping (KP.getKdefsFromStmt $ KP.parseKoak
            (
                "def foo(a:int b:double): int 42;" ++
                "foo();"
            )))
            `shouldThrow`
            (== KTE.MismatchedArgumentNumber (KP.Identifier "foo") 0)
    it "Bad function call. Bad argument number 2." $ do
        evaluate (KT.checkKoakTyping (KP.getKdefsFromStmt $ KP.parseKoak
            (
                "def foo(a:int b:double): int 42;" ++
                "foo(2.2, 1, 3, 5);"
            )))
            `shouldThrow`
            (== KTE.MismatchedArgumentNumber (KP.Identifier "foo") 4)
    it "Bad function call. Bad argument type." $ do
        evaluate (KT.checkKoakTyping (KP.getKdefsFromStmt $ KP.parseKoak
            (
                "def foo(a:int b:double): int 42;" ++
                "foo(2.2, 4.2);"
            )))
            `shouldThrow`
            (== KTE.MismatchedArgumentType (KP.Identifier "foo") [KP.Double, KP.Double])
    it "Complex function def" $
        KT.checkKoakTyping (KP.getKdefsFromStmt $ KP.parseKoak
            (
                "def foo(): int" ++
                "i = 0:" ++
                "while i < 42 do" ++
                "   i = i + 1:" ++
                "   i;"
            ))
            ==
            ()
