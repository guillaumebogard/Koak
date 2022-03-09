--
-- EPITECH PROJECT, 2022
-- koak
-- File description:
-- Koak.TypingContextSpec
--

module Koak.TypingContextSpec       ( spec ) where

import Control.Exception            ( evaluate )

import Test.Hspec                   ( Spec
                                    , it
                                    , shouldBe
                                    , shouldThrow
                                    , anyException
                                    )

import Koak.TypingContext           ( Kcontext(..)
                                    , GlobalContext(..)
                                    , DefContext(..)
                                    , LocalContext(..)
                                    , BaseType(..)
                                    , FunctionTyping(..)
                                    , TypeSignature(..)
                                    , getEmptyKContext
                                    , kContextPushDef
                                    , kContextPushVar
                                    , kContextFind
                                    , kContextEnterLocalContext
                                    )
import qualified Koak.Parser as KP

import Data.HashMap.Strict  as HM   ( HashMap
                                    , fromList
                                    , empty
                                    , member
                                    , insert
                                    )

import Koak.Typing.Exception        ( KoakTypingException(..) )
import Data.Maybe (isNothing)

spec :: Spec
spec = do
    it "getEmptyKContext basic" $
        getEmptyKContext
            ==
            Kcontext (GlobalContext HM.empty) (DefContext HM.empty) Nothing
    it "kContextPushDef: One def push, simple 1, success" $
        kContextPushDef
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
            getEmptyKContext
            ==
            Kcontext
                (GlobalContext $ HM.fromList [])
                (DefContext    $ HM.fromList [
                    (KP.Identifier "foo", Function $ FunctionTyping [] Int)
                ])
                Nothing
    it "kContextPushDef: One def push, simple 2, success" $
        kContextPushDef
            (KP.Defs
                (KP.PrototypeFunction
                    (KP.Identifier "bar")
                    (KP.PrototypeArgs [
                        KP.PrototypeIdentifier (KP.Identifier "x") KP.Int
                    ] KP.Int)
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
            getEmptyKContext
            ==
            Kcontext
                (GlobalContext $ HM.fromList [])
                (DefContext    $ HM.fromList [
                    (KP.Identifier "bar", Function $ FunctionTyping [Int] Int)
                ])
                Nothing
    it "kContextPushDef: One def push, simple 3, success" $
        kContextPushDef
            (KP.Defs
                (KP.PrototypeFunction
                    (KP.Identifier "foobar")
                    (KP.PrototypeArgs [
                        KP.PrototypeIdentifier (KP.Identifier "x") KP.Double
                    ] KP.Void)
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
            getEmptyKContext
            ==
            Kcontext
                (GlobalContext $ HM.fromList [])
                (DefContext    $ HM.fromList [
                    (KP.Identifier "foobar", Function $ FunctionTyping [Double] Nil)
                ])
                Nothing
    it "kContextPushDef: One def push, complex 1, success" $
        kContextPushDef
            (KP.Defs
                (KP.PrototypeFunction
                    (KP.Identifier "foobar")
                    (KP.PrototypeArgs [
                        KP.PrototypeIdentifier (KP.Identifier "a") KP.Int,
                        KP.PrototypeIdentifier (KP.Identifier "b") KP.Double,
                        KP.PrototypeIdentifier (KP.Identifier "c") KP.Boolean,
                        KP.PrototypeIdentifier (KP.Identifier "d") KP.Void
                    ] KP.Double)
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
            getEmptyKContext
            ==
            Kcontext
                (GlobalContext $ HM.fromList [])
                (DefContext    $ HM.fromList [
                    (KP.Identifier "foobar", Function $ FunctionTyping [Int, Double, Boolean, Nil] Double)
                ])
                Nothing
    it "kContextPushDef: Multiple def push, complex 1, success" $
            KP.Defs
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
            `kContextPushDef`
            (
                KP.Defs
                    (KP.PrototypeFunction
                        (KP.Identifier "bar")
                        (KP.PrototypeArgs [
                            KP.PrototypeIdentifier (KP.Identifier "a") KP.Int,
                            KP.PrototypeIdentifier (KP.Identifier "b") KP.Double,
                            KP.PrototypeIdentifier (KP.Identifier "c") KP.Boolean,
                            KP.PrototypeIdentifier (KP.Identifier "d") KP.Void
                        ] KP.Double)
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
                `kContextPushDef`
                (
                    KP.Defs
                        (KP.PrototypeFunction
                            (KP.Identifier "foobar")
                            (KP.PrototypeArgs [
                                KP.PrototypeIdentifier (KP.Identifier "a") KP.Int,
                                KP.PrototypeIdentifier (KP.Identifier "b") KP.Int,
                                KP.PrototypeIdentifier (KP.Identifier "c") KP.Boolean,
                                KP.PrototypeIdentifier (KP.Identifier "d") KP.Int
                            ] KP.Boolean)
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
                    `kContextPushDef`
                    getEmptyKContext
                )
            )
            ==
            Kcontext
                (GlobalContext $ HM.fromList [])
                (DefContext    $ HM.fromList [
                    (KP.Identifier "foo",    Function $ FunctionTyping [] Int),
                    (KP.Identifier "bar",    Function $ FunctionTyping [Int, Double, Boolean, Nil] Double),
                    (KP.Identifier "foobar", Function $ FunctionTyping [Int, Int, Boolean, Int] Boolean)
                ])
                Nothing
    it "kContextPushDef: Two same def push, simple 1, failure" $ do
        evaluate (
                KP.Defs
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
                `kContextPushDef`
                (
                    KP.Defs
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
                    `kContextPushDef`
                    getEmptyKContext
                )
            )
            `shouldThrow`
            (== ShadowedDefinitionByDefinition
                (KP.Identifier "foo")
                (KP.PrototypeFunction
                    (KP.Identifier "foo")
                    (KP.PrototypeArgs [] KP.Int)
                )
            )
    it "kContextPushDef: Two same def push, complex 1, failure" $ do
        evaluate (
                KP.Defs
                    (KP.PrototypeFunction
                        (KP.Identifier "foo")
                        (KP.PrototypeArgs [
                                    KP.PrototypeIdentifier (KP.Identifier "a") KP.Boolean,
                                    KP.PrototypeIdentifier (KP.Identifier "b") KP.Boolean
                            ] KP.Boolean
                        )
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
                `kContextPushDef`
                (
                    KP.Defs
                        (KP.PrototypeFunction
                            (KP.Identifier "foo")
                            (KP.PrototypeArgs [
                                KP.PrototypeIdentifier (KP.Identifier "a") KP.Int,
                                KP.PrototypeIdentifier (KP.Identifier "b") KP.Int,
                                KP.PrototypeIdentifier (KP.Identifier "c") KP.Boolean,
                                KP.PrototypeIdentifier (KP.Identifier "d") KP.Int

                            ] KP.Void)
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
                    `kContextPushDef`
                    getEmptyKContext
                )
            )
            `shouldThrow`
            (== ShadowedDefinitionByDefinition
                (KP.Identifier "foo")
                (KP.PrototypeFunction
                    (KP.Identifier "foo")
                    (KP.PrototypeArgs [
                            KP.PrototypeIdentifier (KP.Identifier "a") KP.Boolean,
                            KP.PrototypeIdentifier (KP.Identifier "b") KP.Boolean
                    ] KP.Boolean)
                )
            )
    it "kContextPushVar: One global var push, simple 1, success." $ do
            kContextPushVar
                (
                    KP.VarAssignment
                        (KP.Identifier "var1")
                        KP.Int
                )
                getEmptyKContext
            ==
            Kcontext
                (GlobalContext $ HM.fromList [
                    (KP.Identifier "var1", Var Int)
                ])
                (DefContext    $ HM.fromList [])
                Nothing
    it "kContextPushVar: Multiple global var push, complex 1, success." $ do
            kContextPushVar
                (
                    KP.VarAssignment
                        (KP.Identifier "var4")
                        KP.Boolean
                )
                (
                    kContextPushVar
                    (
                        KP.VarAssignment
                            (KP.Identifier "var3")
                            KP.Int
                    )
                    (
                        kContextPushVar
                        (
                            KP.VarAssignment
                                (KP.Identifier "var2")
                                KP.Double
                        )
                        (
                            kContextPushVar
                            (
                                KP.VarAssignment
                                    (KP.Identifier "var1")
                                    KP.Int
                            )
                            getEmptyKContext
                        )
                    )
                )
            ==
            Kcontext
                (GlobalContext $ HM.fromList [
                    (KP.Identifier "var1", Var Int),
                    (KP.Identifier "var2", Var Double),
                    (KP.Identifier "var3", Var Int),
                    (KP.Identifier "var4", Var Boolean)
                ])
                (DefContext    $ HM.fromList [])
                Nothing
    it "kContextPushVar: Multiple global var push & Multiple local var push, complex 1, success." $ do
            kContextPushVar
                (
                    KP.VarAssignment
                        (KP.Identifier "var4")
                        KP.Boolean
                )
                (
                    kContextPushVar
                    (
                        KP.VarAssignment
                            (KP.Identifier "var3")
                            KP.Int
                    )
                    (
                        kContextEnterLocalContext
                        (
                            kContextPushVar
                            (
                                KP.VarAssignment
                                    (KP.Identifier "var2")
                                    KP.Double
                            )
                            (
                                kContextPushVar
                                (
                                    KP.VarAssignment
                                        (KP.Identifier "var1")
                                        KP.Int
                                )
                                getEmptyKContext
                            )
                        )
                    )
                )
            ==
            Kcontext
                (GlobalContext $ HM.fromList [
                    (KP.Identifier "var1", Var Int),
                    (KP.Identifier "var2", Var Double)
                ])
                (DefContext    $ HM.fromList [])
                (Just $ LocalContext $ HM.fromList [
                    (KP.Identifier "var3", Var Int),
                    (KP.Identifier "var4", Var Boolean)
                ])
    it "kContextPushVar: Multiple global var push & Multiple local var with the same name, complex 1, failure." $ do
            kContextPushVar
                (
                    KP.VarAssignment
                        (KP.Identifier "var2")
                        KP.Boolean
                )
                (
                    kContextPushVar
                    (
                        KP.VarAssignment
                            (KP.Identifier "var1")
                            KP.Int
                    )
                    (
                        kContextEnterLocalContext
                        (
                            kContextPushVar
                            (
                                KP.VarAssignment
                                    (KP.Identifier "var2")
                                    KP.Double
                            )
                            (
                                kContextPushVar
                                (
                                    KP.VarAssignment
                                        (KP.Identifier "var1")
                                        KP.Int
                                )
                                getEmptyKContext
                            )
                        )
                    )
                )
            ==
            Kcontext
                (GlobalContext $ HM.fromList [
                    (KP.Identifier "var1", Var Int),
                    (KP.Identifier "var2", Var Double)
                ])
                (DefContext    $ HM.fromList [])
                (Just $ LocalContext $ HM.fromList [
                    (KP.Identifier "var1", Var Int),
                    (KP.Identifier "var2", Var Boolean)
                ])
    it "kContextPushVar: Two same global var push, simple 1, failure." $ do
        evaluate (
            kContextPushVar
                (
                    KP.VarAssignment
                        (KP.Identifier "var")
                        KP.Int
                )
                (
                    kContextPushVar
                        (
                            KP.VarAssignment
                                (KP.Identifier "var")
                                KP.Int
                        )
                        getEmptyKContext
                )
            )
            `shouldThrow`
            (== ShadowedVariableByVariable
                (KP.Identifier "var")
                (KP.VarAssignment
                    (KP.Identifier "var")
                    KP.Int
                )
            )
    it "kContextPushVar: Two same local var push, simple 1, failure." $ do
        evaluate (
            kContextPushVar
                (
                    KP.VarAssignment
                        (KP.Identifier "var")
                        KP.Int
                )
                (
                    kContextPushVar
                        (
                            KP.VarAssignment
                                (KP.Identifier "var")
                                KP.Int
                        )
                        (kContextEnterLocalContext getEmptyKContext)
                )
            )
            `shouldThrow`
            (== ShadowedVariableByVariable
                (KP.Identifier "var")
                (KP.VarAssignment
                    (KP.Identifier "var")
                    KP.Int
                )
            )
    it "kContextPushVar: One global var & One def push with same name, simple 1, failure." $ do
        evaluate (
            kContextPushDef
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
                (
                    kContextPushVar
                    (
                        KP.VarAssignment
                            (KP.Identifier "foo")
                            KP.Int
                    )
                    getEmptyKContext
                )
            )
            `shouldThrow`
            (== ShadowedVariableByDefinition
                (KP.Identifier "foo")
                (KP.PrototypeFunction
                    (KP.Identifier "foo")
                    (KP.PrototypeArgs [] KP.Int)
                )
            )
    it "kContextPushVar: One def push & One global var push with same name, simple 1, failure." $ do
        evaluate (
            kContextPushVar
                (
                    KP.VarAssignment
                        (KP.Identifier "foo")
                        KP.Int
                )
                (
                    kContextPushDef
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
                        getEmptyKContext
                )
            )
            `shouldThrow`
            (== ShadowedDefinitionByVariable
                (KP.Identifier "foo")
                (KP.VarAssignment
                    (KP.Identifier "foo")
                    KP.Int
                )
            )
    it "kContextPushVar: One def push & One local var push with same name, simple 1, failure." $ do
        evaluate (
            kContextPushVar
                (
                    KP.VarAssignment
                        (KP.Identifier "foo")
                        KP.Int
                )
                (
                    kContextPushDef
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
                        (kContextEnterLocalContext getEmptyKContext)
                )
            )
            `shouldThrow`
            (== ShadowedDefinitionByVariable
                (KP.Identifier "foo")
                (KP.VarAssignment
                    (KP.Identifier "foo")
                    KP.Int
                )
            )
    it "kContextFind: Empty context, failure." $
        isNothing $
        kContextFind
            getEmptyKContext
            (KP.Identifier "foo")
    it "kContextFind: Only def context, failure." $
        isNothing $
        kContextFind
            (
                Kcontext
                    (GlobalContext $ HM.fromList [] )
                    (DefContext    $ HM.fromList
                        [
                            (KP.Identifier "func1", Function $ FunctionTyping [Boolean, Boolean] Boolean),
                            (KP.Identifier "func2", Function $ FunctionTyping [] Int)
                        ]
                    )
                Nothing
            )
            (KP.Identifier "foo")
    it "kContextFind: Only global var context, failure." $
        isNothing $
        kContextFind
            (
                Kcontext
                    (GlobalContext $ HM.fromList
                        [
                            (KP.Identifier "var1", Var Boolean),
                            (KP.Identifier "var2", Var Boolean),
                            (KP.Identifier "var3", Var Boolean)
                        ]
                    )
                    (DefContext HM.empty)
                Nothing
            )
            (KP.Identifier "foo")
    it "kContextFind: Only local var context, failure." $
        isNothing $
        kContextFind
            (
                Kcontext
                    (GlobalContext HM.empty)
                    (DefContext    HM.empty)
                    (Just $ LocalContext $ HM.fromList
                        [
                            (KP.Identifier "var1", Var Boolean),
                            (KP.Identifier "var2", Var Boolean),
                            (KP.Identifier "var3", Var Boolean)
                        ]
                    )
            )
            (KP.Identifier "foo")
    it "kContextFind: Only def context, success." $
        kContextFind
            (
                Kcontext
                    (GlobalContext $ HM.fromList [] )
                    (DefContext    $ HM.fromList
                        [
                            (KP.Identifier "func1", Function $ FunctionTyping [Boolean, Boolean] Boolean),
                            (KP.Identifier "func2", Function $ FunctionTyping [] Int)
                        ]
                    )
                Nothing
            )
            (KP.Identifier "func2")
            ==
            Just  (Function $ FunctionTyping [] Int)
    it "kContextFind: Only global var context, success." $
        kContextFind
            (
                Kcontext
                    (GlobalContext $ HM.fromList
                        [
                            (KP.Identifier "var1", Var Boolean),
                            (KP.Identifier "var2", Var Boolean),
                            (KP.Identifier "var3", Var Boolean)
                        ]
                    )
                    (DefContext HM.empty)
                Nothing
            )
            (KP.Identifier "var2")
            ==
            Just (Var Boolean)
    it "kContextFind: Only local var context, success." $
        kContextFind
            (
                Kcontext
                    (GlobalContext HM.empty)
                    (DefContext    HM.empty)
                    (Just $ LocalContext $ HM.fromList
                        [
                            (KP.Identifier "var1", Var Boolean),
                            (KP.Identifier "var2", Var Boolean),
                            (KP.Identifier "var3", Var Boolean)
                        ]
                    )
            )
            (KP.Identifier "var2")
            ==
            Just (Var Boolean)
    it "kContextFind: Shadowed global var by local var with same type, success." $
        kContextFind
            (
                Kcontext
                    (GlobalContext       $ HM.fromList
                        [
                            (KP.Identifier "var1",         Var Int),
                            (KP.Identifier "shadowed_var", Var Double),
                            (KP.Identifier "var2",         Var Int)
                        ]
                    )
                    (DefContext HM.empty)
                    (Just $ LocalContext $ HM.fromList
                        [
                            (KP.Identifier "var3",         Var Int),
                            (KP.Identifier "shadowed_var", Var Double),
                            (KP.Identifier "var4",         Var Int)
                        ]
                    )
            )
            (KP.Identifier "shadowed_var")
            ==
            Just (Var Double)
    it "kContextFind: Shadowed global var by local var with different type, success." $
        kContextFind
            (
                Kcontext
                    (GlobalContext       $ HM.fromList
                        [
                            (KP.Identifier "var1",         Var Int),
                            (KP.Identifier "shadowed_var", Var Double),
                            (KP.Identifier "var2",         Var Int)
                        ]
                    )
                    (DefContext HM.empty)
                    (Just $ LocalContext $ HM.fromList
                        [
                            (KP.Identifier "var3",         Var Int),
                            (KP.Identifier "shadowed_var", Var Boolean),
                            (KP.Identifier "var4",         Var Int)
                        ]
                    )
            )
            (KP.Identifier "shadowed_var")
            ==
            Just (Var Boolean)
