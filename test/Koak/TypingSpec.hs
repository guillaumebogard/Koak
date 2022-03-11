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
        KT.checkKoakTyping (KP.parseKoak "i = 1;") KTC.getDefaultKContext
            ==
            Right (
                KTC.kContextPushVar
                    (
                        KP.VarAssignment
                            (KP.Identifier "i")
                            KP.Int
                    )
                    KTC.getDefaultKContext
            )
    it "Complex assign" $
        KT.checkKoakTyping
            (KP.parseKoak "i = 8 * 3 + 4.7; i = 2.0;")
            KTC.getDefaultKContext
            ==
            Right (
                KTC.kContextPushVar
                    (
                        KP.VarAssignment
                            (KP.Identifier "i")
                            KP.Double
                    )
                    KTC.getDefaultKContext
            )
    it "Bad assign, wrong type." $
        KT.checkKoakTyping
            (KP.parseKoak "i = 1: i = 42.42;")
            KTC.getDefaultKContext
        ==
        Left (
            KTE.KoakTypingShadowedVariableByVariable
            (KP.Identifier "i")
            (KP.VarAssignment
                (KP.Identifier "i")
                KP.Int
            )
        )
    it "Basic expression" $
        KT.checkKoakTyping
            (KP.parseKoak "---1 + 3 * 4 / 1 + 3.0;")
            KTC.getDefaultKContext
            ==
            Right KTC.getDefaultKContext
    it "Basic function call" $
        KT.checkKoakTyping
            (KP.parseKoak "def foo(): int 42; foo();")
            KTC.getDefaultKContext
            ==
            Right (
                KTC.kContextPushFunction
                    (
                        KP.PrototypeFunction
                            (KP.Identifier "foo")
                            (KP.PrototypeArgs [] KP.Int)
                    )
                    KTC.getDefaultKContext
            )
    it "Basic function call with arguments" $
        KT.checkKoakTyping
            (KP.parseKoak
                (
                    "def foo(a:int b:double): int 42;" ++
                    "foo(6, 4.2);"
                )
            )
            KTC.getDefaultKContext
            ==
            Right (
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
            )
    it "Complex function call with arguments" $
        KT.checkKoakTyping
            (KP.parseKoak
                (
                    "def foo(a:int b:double): int 42;"      ++
                    "foo(foo(1, 1 * 7.8 + 3 - -1), 4.2);"
                )
            )
            KTC.getDefaultKContext
            ==
            Right (
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
            )
    it "Bad function call. Bad argument number 1." $
        KT.checkKoakTyping
            (KP.parseKoak
                (
                    "def foo(a:int b:double): int 42;" ++
                    "foo(2.2);"
                )
            )
            KTC.getDefaultKContext
            == 
            Left (
                KTE.KoakTypingMismatchedArgumentNumber (KP.Identifier "foo") 1
            )
    it "Bad function call. Bad argument number 2." $
        KT.checkKoakTyping
            (KP.parseKoak
                (
                    "def foo(a:int b:double): int 42;" ++
                    "foo();"
                )
            )
            KTC.getDefaultKContext
            ==
            Left (
                KTE.KoakTypingMismatchedArgumentNumber (KP.Identifier "foo") 0
            )
    it "Bad function call. Bad argument number 3." $
        KT.checkKoakTyping
            (KP.parseKoak
                (
                    "def foo(a:int b:double): int 42;" ++
                    "foo(2.2, 1, 3, 5);"
                )
            )
            KTC.getDefaultKContext
        ==
        Left (
            KTE.KoakTypingMismatchedArgumentNumber (KP.Identifier "foo") 4
        )
    it "Bad function call. Bad argument type." $
        KT.checkKoakTyping
            (KP.parseKoak
                (
                    "def foo(a:int b:double): int 42;" ++
                    "foo(2.2, 4.2);"
                )
            )
            KTC.getDefaultKContext
        ==
        Left (
            KTE.KoakTypingMismatchedArgumentType (KP.Identifier "foo") [KP.Double, KP.Double]
        )
    it "Basic function with a while" $
        KT.checkKoakTyping
            (KP.parseKoak
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
            KTC.getDefaultKContext
            ==
            Right (
                KTC.kContextPushFunction
                (
                    KP.PrototypeFunction
                        (KP.Identifier "foo")
                        (KP.PrototypeArgs [] KP.Int)
                )
                KTC.getDefaultKContext
            )
    it "Basic function with a for" $
        KT.checkKoakTyping
            (KP.parseKoak
                (
                    "def foo(): double"                 ++
                    "   for i = 0.0, i < 42.1, 2.0 in"  ++
                    "       i"                          ++
                    ";"
                )
            )
            KTC.getDefaultKContext
            ==
            Right (
                KTC.kContextPushFunction
                    (
                        KP.PrototypeFunction
                            (KP.Identifier "foo")
                            (KP.PrototypeArgs [] KP.Double)
                    )
                    KTC.getDefaultKContext
            )
    it "Shadowing global by a local variable 1" $
        KT.checkKoakTyping
            (KP.parseKoak
                (
                    "i = 0;"            ++
                    "def foo(): double" ++
                    "   i = 2.1"        ++
                    ";"
                )
            )
            KTC.getDefaultKContext
            ==
            Right (
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
            )
    it "Shadowing global by a local variable 2" $
        KT.checkKoakTyping
            (KP.parseKoak
                (
                    "i = 0;"                    ++
                    "def foo(i:double): double" ++
                    "   i + 1"                  ++
                    ";"
                )
            )
            KTC.getDefaultKContext
            ==
            Right (
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
                                    (KP.PrototypeArgs [KP.PrototypeIdentifier (KP.Identifier "i") KP.Double] KP.Double)
                            )
                            KTC.getDefaultKContext
                    )
            )
    it "Shadowing definition by a definition 1" $
        KT.checkKoakTyping
            (KP.parseKoak
                (
                    "def foo(a:int b:double): int 42;" ++
                    "def foo(a:int b:double): int 42;"
                )
            )
            KTC.getDefaultKContext
        ==
        Left (
            KTE.KoakTypingShadowedDefinitionByDefinition
            (KP.Identifier "foo")
            (KP.PrototypeFunction
                (KP.Identifier "foo")
                (KP.PrototypeArgs
                    [
                        KP.PrototypeIdentifier (KP.Identifier "a") KP.Int,
                        KP.PrototypeIdentifier (KP.Identifier "b") KP.Double
                    ] KP.Int
                )
            )
        )
    it "Shadowing definition by a definition 2" $
        KT.checkKoakTyping
            (KP.parseKoak
                (
                    "def foo(a:int b:double): double 42.2;" ++
                    "def foo(): int 42;"
                )
            )
            KTC.getDefaultKContext
        ==
        Left (
            KTE.KoakTypingShadowedDefinitionByDefinition
            (KP.Identifier "foo")
            (KP.PrototypeFunction
                (KP.Identifier "foo")
                (KP.PrototypeArgs
                    [
                        KP.PrototypeIdentifier (KP.Identifier "a") KP.Int,
                        KP.PrototypeIdentifier (KP.Identifier "b") KP.Double
                    ] KP.Double
                )
            )
        )
    it "Shadowing definition by a var 1" $
        KT.checkKoakTyping
            (KP.parseKoak
                (
                    "def foo(a:int b:double): double 42.2;" ++
                    "foo = 1;"
                )
            )
            KTC.getDefaultKContext
        ==
        Left (
            KTE.KoakTypingShadowedVariableByDefinition
            (KP.Identifier "foo")
            (KP.PrototypeFunction
                (KP.Identifier "foo")
                (KP.PrototypeArgs
                    [
                        KP.PrototypeIdentifier (KP.Identifier "a") KP.Int,
                        KP.PrototypeIdentifier (KP.Identifier "b") KP.Double
                    ] KP.Double
                )
            )
        )
    it "Shadowing definition by a var 2" $
        KT.checkKoakTyping
            (KP.parseKoak
                (
                    "def foo(a:int b:double): double 42.2;"   ++
                    "def bar(): double"                       ++
                    "   foo = 1:"                             ++
                    "   2.1"                                  ++
                    ";"
                )
            )
            KTC.getDefaultKContext
        ==
        Left (
            KTE.KoakTypingShadowedVariableByDefinition
            (KP.Identifier "foo")
            (KP.PrototypeFunction
                (KP.Identifier "foo")
                (KP.PrototypeArgs
                    [
                        KP.PrototypeIdentifier (KP.Identifier "a") KP.Int,
                        KP.PrototypeIdentifier (KP.Identifier "b") KP.Double
                    ] KP.Double
                )
            )
        )
    it "Shadowing var by a var 1" $
        KT.checkKoakTyping
            (KP.parseKoak
                (
                    "a = 1;"    ++
                    "a = 2.5;"
                )
            )
            KTC.getDefaultKContext
        ==
        Left (
            KTE.KoakTypingShadowedVariableByVariable
            (KP.Identifier "a")
            (KP.VarAssignment
                (KP.Identifier "a")
                KP.Double
            )
        )
    it "Unknown var" $
        KT.checkKoakTyping
            (KP.parseKoak
                "a = a;"
            )
            KTC.getDefaultKContext
        ==
        Left (
            KTE.KoakTypingUnknownDefinition (KP.Identifier "a")
        )
    it "Unknown function" $
        KT.checkKoakTyping
            (KP.parseKoak
                "foo(1);"
            )
            KTC.getDefaultKContext
        ==
        Left (KTE.KoakTypingUnknownDefinition (KP.Identifier "foo") )
    it "Unknown binary function" $
        KT.checkKoakTyping
            (KP.parseKoak
                "1 <--> 2;"
            )
            KTC.getDefaultKContext
        ==
        Left (KTE.KoakTypingUnknownDefinition (KP.Identifier "<-->") )
    it "Unknown unary function" $
        KT.checkKoakTyping
            (KP.parseKoak
                ">>>> 2;"
            )
            KTC.getDefaultKContext
        ==
        Left (KTE.KoakTypingUnknownDefinition (KP.Identifier ">>>>") )
    it "Real world example: while 1" $
        KT.checkKoakTyping
            (KP.parseKoak
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
            KTC.getDefaultKContext
            ==
            Right (
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
            )
    it "Assignment of a RValue 1" $
        KT.checkKoakTyping
            (KP.parseKoak
                "1 = 1;"
            )
            KTC.getDefaultKContext
        ==
        Left (KTE.KoakTypingAssignmentToRValue)
    it "Assignment of a RValue 2" $
                KT.checkKoakTyping
                    (KP.parseKoak
                        "-a = 1;"
                    )
                    KTC.getDefaultKContext
        ==
        Left (KTE.KoakTypingAssignmentToRValue)
    it "Assignment of a RValue 3" $
        KT.checkKoakTyping
            (KP.parseKoak
                "2 * a = 1;"
            )
            KTC.getDefaultKContext
        ==
        Left (KTE.KoakTypingAssignmentToRValue)
    it "Assignment of a RValue 3" $
        KT.checkKoakTyping
            (KP.parseKoak
                (
                    "def foo() : int 1;" ++
                    "foo() = 1;"
                )
            )
            KTC.getDefaultKContext
        ==
        Left (KTE.KoakTypingAssignmentToRValue)
    it "No matching binary function" $
        (KT.checkKoakTyping
            (KP.parseKoak
                    "1 + 2;"
            )
            (KTC.Kcontext
                    (
                        KTC.DefContext $
                            HM.fromList [
                                    (KP.Identifier "+", KTC.PrimitiveFunction  [KTC.BinaryFunctionTyping (KP.Precedence 11) KTC.Double KTC.Double KTC.Double])
                            ]
                    )
                    (KTC.VarContext HM.empty)
            ))
            ==
            Left (KTE.KoakTypingMismatchedArgumentType (KP.Identifier "+") [])
    it "Assign hell 1" $
        KT.checkKoakTyping
            (KP.parseKoak "a = b = 3;")
            KTC.getDefaultKContext
        ==
        Left (KTE.KoakTypingAssignmentToRValue)
    it "Assign hell 2" $
        KT.checkKoakTyping
            (KP.parseKoak
                (
                    "def foo() : int 1;" ++
                    "a = b = foo() * 2 + 3;"
                )
            )
            KTC.getDefaultKContext
        ==
        Left (KTE.KoakTypingAssignmentToRValue)
    -- it "Assign hell 2" $
    --     evaluate
    --         (
    --             KT.checkKoakTyping
    --                 (KP.parseKoak
    --                         "a = b = c = d = e = f = g = a = 1 + 3;"
    --                 )
    --                 KTC.getDefaultKContext
    --         )
    --     `shouldThrow`
    --     (== KTE.KoakTypingAssignmentToRValue)
