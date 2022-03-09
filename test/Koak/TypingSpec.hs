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
import qualified Koak.TypingContext as KTC
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
        KT.checkKoakTyping KTC.getDefaultKContext (KP.getKdefsFromStmt $ KP.parseKoak "i = 1;")
            ==
            KTC.kContextPushVar
                (
                    KP.VarAssignment
                        (KP.Identifier "i")
                        KP.Int
                )
                KTC.getDefaultKContext
    it "Complex assign" $
        KT.checkKoakTyping
            KTC.getDefaultKContext
            (KP.getKdefsFromStmt $ KP.parseKoak "i = 8 * 3 + 4.7; i = 2.0;")
            ==
            KTC.kContextPushVar
                (
                    KP.VarAssignment
                        (KP.Identifier "i")
                        KP.Double
                )
                KTC.getDefaultKContext
    it "Bad assign, wrong type." $
        evaluate
            (
                KT.checkKoakTyping
                    KTC.getDefaultKContext
                    (KP.getKdefsFromStmt $ KP.parseKoak "i = 1: i = 42.42;")
            )
        `shouldThrow`
        (== KTE.ShadowedVariableByVariable
            (KP.Identifier "i")
            (KP.VarAssignment
                (KP.Identifier "i")
                KP.Int
            )
        )
    it "Basic expression" $
        KT.checkKoakTyping
            KTC.getDefaultKContext
            (KP.getKdefsFromStmt $ KP.parseKoak "---1 + 3 * 4 / 1 + 3.0;")
            ==
            KTC.getDefaultKContext
    it "Basic function call" $
        KT.checkKoakTyping
            KTC.getDefaultKContext
            (KP.getKdefsFromStmt $ KP.parseKoak "def foo(): int 42; foo();")
            ==
            KTC.kContextPushFunction
                (
                    KP.PrototypeFunction
                        (KP.Identifier "foo")
                        (KP.PrototypeArgs [] KP.Int)
                )
                KTC.getDefaultKContext
    it "Basic function call with arguments" $
        KT.checkKoakTyping
            KTC.getDefaultKContext
            (KP.getKdefsFromStmt $ KP.parseKoak
                (
                    "def foo(a:int b:double): int 42;" ++
                    "foo(6, 4.2);"
                )
            )
            ==
            KTC.kContextPushFunction
                (
                    KP.PrototypeFunction
                        (KP.Identifier "foo")
                        (KP.PrototypeArgs
                            [
                                KP.PrototypeIdentifier (KP.Identifier "a") KP.Int,
                                KP.PrototypeIdentifier (KP.Identifier "b") KP.Double
                            ]
                            KP.Int
                        )
                )
                KTC.getDefaultKContext
    it "Complex function call with arguments" $
        KT.checkKoakTyping
            KTC.getDefaultKContext
            (KP.getKdefsFromStmt $ KP.parseKoak
                (
                    "def foo(a:int b:double): int 42;"      ++
                    "foo(foo(1, 1 * 7.8 + 3 - -1), 4.2);"
                )
            )
            ==
            KTC.kContextPushFunction
                (
                    KP.PrototypeFunction
                        (KP.Identifier "foo")
                        (KP.PrototypeArgs
                            [
                                KP.PrototypeIdentifier (KP.Identifier "a") KP.Int,
                                KP.PrototypeIdentifier (KP.Identifier "b") KP.Double
                            ]
                            KP.Int
                        )
                )
                KTC.getDefaultKContext
    it "Bad function call. Bad argument number 1." $
        evaluate
            (
                KT.checkKoakTyping
                    KTC.getDefaultKContext
                    (KP.getKdefsFromStmt $ KP.parseKoak
                        (
                            "def foo(a:int b:double): int 42;" ++
                            "foo(2.2);"
                        )
                    )
            )
        `shouldThrow`
        (== KTE.MismatchedArgumentNumber (KP.Identifier "foo") 1)
    it "Bad function call. Bad argument number 1." $
        evaluate
            (
                KT.checkKoakTyping
                    KTC.getDefaultKContext
                    (KP.getKdefsFromStmt $ KP.parseKoak
                        (
                            "def foo(a:int b:double): int 42;" ++
                            "foo();"
                        )
                    )
            )
        `shouldThrow`
        (== KTE.MismatchedArgumentNumber (KP.Identifier "foo") 0)
    it "Bad function call. Bad argument number 2." $
        evaluate
            (
                KT.checkKoakTyping
                    KTC.getDefaultKContext
                    (KP.getKdefsFromStmt $ KP.parseKoak
                        (
                            "def foo(a:int b:double): int 42;" ++
                            "foo(2.2, 1, 3, 5);"
                        )
                    )
            )
        `shouldThrow`
        (== KTE.MismatchedArgumentNumber (KP.Identifier "foo") 4)
    it "Bad function call. Bad argument type." $
        evaluate
            (
                KT.checkKoakTyping
                    KTC.getDefaultKContext
                    (KP.getKdefsFromStmt $ KP.parseKoak
                        (
                            "def foo(a:int b:double): int 42;" ++
                            "foo(2.2, 4.2);"
                        )
                    )
            )
        `shouldThrow`
        (== KTE.MismatchedArgumentType (KP.Identifier "foo") [KP.Double, KP.Double])
    it "Basic function with a while" $
        KT.checkKoakTyping
            KTC.getDefaultKContext
            (KP.getKdefsFromStmt $ KP.parseKoak
                (
                    "def foo(): int "           ++
                    "   i = 0:"                 ++
                    "   ("                      ++
                    "       while i < 42 do"    ++
                    "           i = i + 1"      ++
                    "   )"                      ++
                    ";"
                )
            )
            ==
            KTC.kContextPushFunction
                (
                    KP.PrototypeFunction
                        (KP.Identifier "foo")
                        (KP.PrototypeArgs [] KP.Int)
                )
                KTC.getDefaultKContext
    it "Basic function with a for" $
        KT.checkKoakTyping
            KTC.getDefaultKContext
            (KP.getKdefsFromStmt $ KP.parseKoak
                (
                    "def foo(): double"                 ++
                    "   for i = 0.0, i < 42.1, 2.0 in"  ++
                    "       i"                          ++
                    ";"
                )
            )
            ==
            KTC.kContextPushFunction
                (
                    KP.PrototypeFunction
                        (KP.Identifier "foo")
                        (KP.PrototypeArgs [] KP.Double)
                )
                KTC.getDefaultKContext
    it "Shadowing global by a local variable 1" $
        KT.checkKoakTyping
            KTC.getDefaultKContext
            (KP.getKdefsFromStmt $ KP.parseKoak
                (
                    "i = 0;"            ++
                    "def foo(): double" ++
                    "   i = 2.1"        ++
                    ";"
                )
            )
            ==
            KTC.kContextPushVar
                (KP.VarAssignment
                    (KP.Identifier "i")
                    KP.Int
                )
                (
                    KTC.kContextPushFunction
                        (
                            KP.PrototypeFunction
                                (KP.Identifier "foo")
                                (KP.PrototypeArgs [] KP.Double)
                        )
                        KTC.getDefaultKContext
                )
    it "Shadowing global by a local variable 2" $
        KT.checkKoakTyping
            KTC.getDefaultKContext
            (KP.getKdefsFromStmt $ KP.parseKoak
                (
                    "i = 0;"                    ++
                    "def foo(i:double): double" ++
                    "   i + 1"                  ++
                    ";"
                )
            )
            ==
            KTC.kContextPushVar
                (KP.VarAssignment
                    (KP.Identifier "i")
                    KP.Int
                )
                (
                    KTC.kContextPushFunction
                        (
                            KP.PrototypeFunction
                                (KP.Identifier "foo")
                                (KP.PrototypeArgs [] KP.Double)
                        )
                        KTC.getDefaultKContext
                )
    it "Shadowing definition by a definition 1" $
        evaluate
            (
                KT.checkKoakTyping
                    KTC.getDefaultKContext
                    (KP.getKdefsFromStmt $ KP.parseKoak
                        (
                            "def foo(a:int b:double): int 42;" ++
                            "def foo(a:int b:double): int 42;"
                        )
                    )
            )
        `shouldThrow`
        (== KTE.ShadowedDefinitionByDefinition
            (KP.Identifier "foo")
            (KP.PrototypeFunction
                (KP.Identifier "foo")
                (KP.PrototypeArgs
                    [
                        KP.PrototypeIdentifier (KP.Identifier "a") KP.Int,
                        KP.PrototypeIdentifier (KP.Identifier "b") KP.Double
                    ] KP.Int)
            )
        )
    it "Shadowing definition by a definition 2" $
        evaluate
            (
                KT.checkKoakTyping
                    KTC.getDefaultKContext
                    (KP.getKdefsFromStmt $ KP.parseKoak
                        (
                            "def foo(a:int b:double): double 42;" ++
                            "def foo(): int 42;"
                        )
                    )
            )
        `shouldThrow`
        (== KTE.ShadowedDefinitionByDefinition
            (KP.Identifier "foo")
            (KP.PrototypeFunction
                (KP.Identifier "foo")
                (KP.PrototypeArgs [] KP.Int)
            )
        )
    it "Shadowing definition by a var 1" $
        evaluate
            (
                KT.checkKoakTyping
                    KTC.getDefaultKContext
                    (KP.getKdefsFromStmt $ KP.parseKoak
                        (
                            "def foo(a:int b:double): double 42;" ++
                            "foo = 1;"
                        )
                    )
            )
        `shouldThrow`
        (== KTE.NotAVar (KP.Identifier "foo"))
    it "Shadowing definition by a var 2" $
        evaluate
            (
                KT.checkKoakTyping
                    KTC.getDefaultKContext
                    (KP.getKdefsFromStmt $ KP.parseKoak
                        (
                            "def foo(a:int b:double): double 42;"   ++
                            "def bar(): double"                     ++
                            "   foo = 1:"                           ++
                            "   2.1"                                ++
                            ";"
                        )
                    )
            )
        `shouldThrow`
        (== KTE.ShadowedDefinitionByVariable
            (KP.Identifier "foo")
            (KP.VarAssignment
                (KP.Identifier "foo")
                KP.Int
            )
        )
    it "Shadowing var by a var 1" $
        evaluate
            (
                KT.checkKoakTyping
                    KTC.getDefaultKContext
                    (KP.getKdefsFromStmt $ KP.parseKoak
                        (
                            "a = 1;"    ++
                            "a = 2.5;"
                        )
                    )
            )
        `shouldThrow`
        (== KTE.ShadowedVariableByVariable
            (KP.Identifier "a")
            (KP.VarAssignment
                (KP.Identifier "a")
                KP.Int
            )
        )
    it "Unknown var" $
        evaluate
            (
                KT.checkKoakTyping
                    KTC.getDefaultKContext
                    (KP.getKdefsFromStmt $ KP.parseKoak
                        "a = a;"
                    )
            )
        `shouldThrow`
        (== KTE.UnknownDefinition (KP.Identifier "a"))
    it "Unknown function" $
        evaluate
            (
                KT.checkKoakTyping
                    KTC.getDefaultKContext
                    (KP.getKdefsFromStmt $ KP.parseKoak
                        "foo(1);"
                    )
            )
        `shouldThrow`
        (== KTE.UnknownDefinition (KP.Identifier "foo") )
    it "Unknown binary function" $
        evaluate
            (
                KT.checkKoakTyping
                    KTC.getDefaultKContext
                    (KP.getKdefsFromStmt $ KP.parseKoak
                        "1 <-> 2;"
                    )
            )
        `shouldThrow`
        (== KTE.UnknownDefinition (KP.Identifier "<-->") )
    it "Unknown unary function" $
        evaluate
            (
                KT.checkKoakTyping
                    KTC.getDefaultKContext
                    (KP.getKdefsFromStmt $ KP.parseKoak
                        ">>> 2;"
                    )
            )
        `shouldThrow`
        (== KTE.UnknownDefinition (KP.Identifier ">>>") )
    it "Real world example: while 1" $
        KT.checkKoakTyping
            KTC.getDefaultKContext
            (KP.getKdefsFromStmt $ KP.parseKoak
                (
                    "def print(i:int): int i;"          ++
                    "len = 99 :"                        ++
                    "i = 0 :"                           ++
                    "("                                 ++
                    "   while i < len do"               ++
                    "      print(i) :"                  ++
                    "      i = i + 1"                   ++
                    "):"                                ++
                    "print(i);"
                )
            )
            ==
            KTC.kContextPushVar
                (KP.VarAssignment (KP.Identifier "len") KP.Int)
                (
                    KTC.kContextPushVar
                        (KP.VarAssignment (KP.Identifier "i") KP.Int)
                        (
                            KTC.kContextPushFunction
                                (
                                    KP.PrototypeFunction
                                        (KP.Identifier "print")
                                        (KP.PrototypeArgs
                                            [
                                                KP.PrototypeIdentifier (KP.Identifier "i") KP.Int
                                            ]
                                            KP.Int
                                        )
                                )
                                KTC.getDefaultKContext
                        )
                )
    it "Assignment of a RValue 1" $
        evaluate
            (
                KT.checkKoakTyping
                    KTC.getDefaultKContext
                    (KP.getKdefsFromStmt $ KP.parseKoak
                        "1 = 1;"
                    )
            )
        `shouldThrow`
        (== KTE.AssignmentToRValue)
    it "Assignment of a RValue 2" $
        evaluate
            (
                KT.checkKoakTyping
                    KTC.getDefaultKContext
                    (KP.getKdefsFromStmt $ KP.parseKoak
                        "-a = 1;"
                    )
            )
        `shouldThrow`
        (== KTE.AssignmentToRValue)
    it "Assignment of a RValue 3" $
        evaluate
            (
                KT.checkKoakTyping
                    KTC.getDefaultKContext
                    (KP.getKdefsFromStmt $ KP.parseKoak
                        "2 * a = 1;"
                    )
            )
        `shouldThrow`
        (== KTE.AssignmentToRValue)
    it "Assignment of a RValue 3" $
        evaluate
            (
                KT.checkKoakTyping
                    KTC.getDefaultKContext
                    (KP.getKdefsFromStmt $ KP.parseKoak
                        (
                            "def foo() : int 1;" ++
                            "foo() = 1;"
                        )
                    )
            )
        `shouldThrow`
        (== KTE.AssignmentToRValue)
