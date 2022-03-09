--
-- EPITECH PROJECT, 2022
-- koak
-- File description:
-- Koak.TypingContextSpec
--

module Koak.TypingContextSpec                   ( spec ) where

import Control.Exception                        ( evaluate )

import Test.Hspec                               ( Spec
                                                , it
                                                , shouldBe
                                                , shouldThrow
                                                , anyException
                                                )

import qualified Koak.Parser as KP
import qualified Koak.TypingContext as KTC
import qualified Koak.Typing.Exception as KTE   ( KoakTypingException(..) )

import Data.HashMap.Strict  as HM               ( HashMap
                                                , fromList
                                                , empty
                                                , member
                                                , insert
                                                )

import Data.Maybe (isNothing)

spec :: Spec
spec = do
    it "getEmptyKContext basic" $
        KTC.getEmptyKContext
            ==
            KTC.Kcontext (KTC.GlobalContext HM.empty) (KTC.DefContext HM.empty) Nothing
    it "kContextPushFunction: One def push, simple 1, success" $
        KTC.kContextPushFunction
            (KP.PrototypeFunction
                (KP.Identifier "foo")
                (KP.PrototypeArgs [] KP.Int)
            )
            KTC.getEmptyKContext
            ==
            KTC.Kcontext
                (KTC.GlobalContext $ HM.fromList [])
                (KTC.DefContext    $ HM.fromList [
                    (KP.Identifier "foo", KTC.Function $ KTC.FunctionTyping [] KTC.Int)
                ])
                Nothing
    it "kContextPushFunction: One def push, simple 2, success" $
        KTC.kContextPushFunction
            (KP.PrototypeFunction
                (KP.Identifier "bar")
                (KP.PrototypeArgs [
                    KP.PrototypeIdentifier (KP.Identifier "x") KP.Int
                ] KP.Int)
            )
            KTC.getEmptyKContext
            ==
            KTC.Kcontext
                (KTC.GlobalContext $ HM.fromList [])
                (KTC.DefContext    $ HM.fromList [
                    (KP.Identifier "bar", KTC.Function $ KTC.FunctionTyping [KTC.Int] KTC.Int)
                ])
                Nothing
    it "kContextPushFunction: One def push, simple 3, success" $
        KTC.kContextPushFunction
            (KP.PrototypeFunction
                (KP.Identifier "foobar")
                (KP.PrototypeArgs [
                    KP.PrototypeIdentifier (KP.Identifier "x") KP.Double
                ] KP.Void)
            )
            KTC.getEmptyKContext
            ==
            KTC.Kcontext
                (KTC.GlobalContext $ HM.fromList [])
                (KTC.DefContext    $ HM.fromList [
                    (KP.Identifier "foobar", KTC.Function $ KTC.FunctionTyping [KTC.Double] KTC.Nil)
                ])
                Nothing
    it "kContextPushFunction: One def push, complex 1, success" $
        KTC.kContextPushFunction
            (KP.PrototypeFunction
                (KP.Identifier "foobar")
                (KP.PrototypeArgs [
                    KP.PrototypeIdentifier (KP.Identifier "a") KP.Int,
                    KP.PrototypeIdentifier (KP.Identifier "b") KP.Double,
                    KP.PrototypeIdentifier (KP.Identifier "c") KP.Boolean,
                    KP.PrototypeIdentifier (KP.Identifier "d") KP.Void
                ] KP.Double)
            )
            KTC.getEmptyKContext
            ==
            KTC.Kcontext
                (KTC.GlobalContext $ HM.fromList [])
                (KTC.DefContext    $ HM.fromList [
                    (KP.Identifier "foobar", KTC.Function $ KTC.FunctionTyping [KTC.Int, KTC.Double, KTC.Boolean, KTC.Nil] KTC.Double)
                ])
                Nothing
    it "kContextPushFunction: Multiple def push, complex 1, success" $
                KP.PrototypeFunction
                    (KP.Identifier "foo")
                    (KP.PrototypeArgs [] KP.Int)
            `KTC.kContextPushFunction`
            (
                    KP.PrototypeFunction
                        (KP.Identifier "bar")
                        (KP.PrototypeArgs [
                            KP.PrototypeIdentifier (KP.Identifier "a") KP.Int,
                            KP.PrototypeIdentifier (KP.Identifier "b") KP.Double,
                            KP.PrototypeIdentifier (KP.Identifier "c") KP.Boolean,
                            KP.PrototypeIdentifier (KP.Identifier "d") KP.Void
                        ] KP.Double)
                `KTC.kContextPushFunction`
                (
                        KP.PrototypeFunction
                            (KP.Identifier "foobar")
                            (KP.PrototypeArgs [
                                KP.PrototypeIdentifier (KP.Identifier "a") KP.Int,
                                KP.PrototypeIdentifier (KP.Identifier "b") KP.Int,
                                KP.PrototypeIdentifier (KP.Identifier "c") KP.Boolean,
                                KP.PrototypeIdentifier (KP.Identifier "d") KP.Int
                            ] KP.Boolean)
                    `KTC.kContextPushFunction`
                    KTC.getEmptyKContext
                )
            )
            ==
            KTC.Kcontext
                (KTC.GlobalContext $ HM.fromList [])
                (KTC.DefContext    $ HM.fromList [
                    (KP.Identifier "foo",    KTC.Function $ KTC.FunctionTyping [] KTC.Int),
                    (KP.Identifier "bar",    KTC.Function $ KTC.FunctionTyping [KTC.Int, KTC.Double, KTC.Boolean, KTC.Nil] KTC.Double),
                    (KP.Identifier "foobar", KTC.Function $ KTC.FunctionTyping [KTC.Int, KTC.Int, KTC.Boolean, KTC.Int] KTC.Boolean)
                ])
                Nothing
    it "kContextPushFunction: Two same def push, simple 1, failure" $
        evaluate (
                KP.PrototypeFunction
                    (KP.Identifier "foo")
                    (KP.PrototypeArgs [] KP.Int)
            `KTC.kContextPushFunction`
            (
                    KP.PrototypeFunction
                        (KP.Identifier "foo")
                        (KP.PrototypeArgs [] KP.Int)
                `KTC.kContextPushFunction`
                KTC.getEmptyKContext
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
    it "kContextPushFunction: Two same def push, complex 1, failure" $
        evaluate (
                KP.PrototypeFunction
                    (KP.Identifier "foo")
                    (KP.PrototypeArgs [
                                KP.PrototypeIdentifier (KP.Identifier "a") KP.Boolean,
                                KP.PrototypeIdentifier (KP.Identifier "b") KP.Boolean
                        ] KP.Boolean
                    )
            `KTC.kContextPushFunction`
            (
                    KP.PrototypeFunction
                        (KP.Identifier "foo")
                        (KP.PrototypeArgs [
                            KP.PrototypeIdentifier (KP.Identifier "a") KP.Int,
                            KP.PrototypeIdentifier (KP.Identifier "b") KP.Int,
                            KP.PrototypeIdentifier (KP.Identifier "c") KP.Boolean,
                            KP.PrototypeIdentifier (KP.Identifier "d") KP.Int

                        ] KP.Void)
                `KTC.kContextPushFunction`
                KTC.getEmptyKContext
            )
        )
        `shouldThrow`
        (== KTE.ShadowedDefinitionByDefinition
            (KP.Identifier "foo")
            (KP.PrototypeFunction
                (KP.Identifier "foo")
                (KP.PrototypeArgs [
                        KP.PrototypeIdentifier (KP.Identifier "a") KP.Boolean,
                        KP.PrototypeIdentifier (KP.Identifier "b") KP.Boolean
                ] KP.Boolean)
            )
        )
    it "kContextPushVar: One global var push, simple 1, success." $
            KTC.kContextPushVar
        (
            KP.VarAssignment
                (KP.Identifier "var1")
                KP.Int
        )
        KTC.getEmptyKContext
            ==
            KTC.Kcontext
                (KTC.GlobalContext $ HM.fromList [
                    (KP.Identifier "var1", KTC.Var KTC.Int)
                ])
                (KTC.DefContext    $ HM.fromList [])
                Nothing
    it "kContextPushVar: Multiple global var push, complex 1, success." $
            KTC.kContextPushVar
        (
            KP.VarAssignment
                (KP.Identifier "var4")
                KP.Boolean
        )
        (
            KTC.kContextPushVar
            (
                KP.VarAssignment
                    (KP.Identifier "var3")
                    KP.Int
            )
            (
                KTC.kContextPushVar
                (
                    KP.VarAssignment
                        (KP.Identifier "var2")
                        KP.Double
                )
                (
                    KTC.kContextPushVar
                    (
                        KP.VarAssignment
                            (KP.Identifier "var1")
                            KP.Int
                    )
                    KTC.getEmptyKContext
                )
            )
        )
            ==
            KTC.Kcontext
                (KTC.GlobalContext $ HM.fromList [
                    (KP.Identifier "var1", KTC.Var KTC.Int),
                    (KP.Identifier "var2", KTC.Var KTC.Double),
                    (KP.Identifier "var3", KTC.Var KTC.Int),
                    (KP.Identifier "var4", KTC.Var KTC.Boolean)
                ])
                (KTC.DefContext    $ HM.fromList [])
                Nothing
    it "kContextPushVar: Multiple global var push & Multiple local var push, complex 1, success." $
            KTC.kContextPushVar
        (
            KP.VarAssignment
                (KP.Identifier "var4")
                KP.Boolean
        )
        (
            KTC.kContextPushVar
            (
                KP.VarAssignment
                    (KP.Identifier "var3")
                    KP.Int
            )
            (
                KTC.kContextEnterLocalContext
                (
                    KTC.kContextPushVar
                    (
                        KP.VarAssignment
                            (KP.Identifier "var2")
                            KP.Double
                    )
                    (
                        KTC.kContextPushVar
                        (
                            KP.VarAssignment
                                (KP.Identifier "var1")
                                KP.Int
                        )
                        KTC.getEmptyKContext
                    )
                )
            )
        )
            ==
            KTC.Kcontext
                (KTC.GlobalContext $ HM.fromList [
                    (KP.Identifier "var1", KTC.Var KTC.Int),
                    (KP.Identifier "var2", KTC.Var KTC.Double)
                ])
                (KTC.DefContext    $ HM.fromList [])
                (Just $ KTC.LocalContext $ HM.fromList [
                    (KP.Identifier "var3", KTC.Var KTC.Int),
                    (KP.Identifier "var4", KTC.Var KTC.Boolean)
                ])
    it "kContextPushVar: Multiple global var push & Multiple local var with the same name, complex 1, failure." $
            KTC.kContextPushVar
        (
            KP.VarAssignment
                (KP.Identifier "var2")
                KP.Boolean
        )
        (
            KTC.kContextPushVar
            (
                KP.VarAssignment
                    (KP.Identifier "var1")
                    KP.Int
            )
            (
                KTC.kContextEnterLocalContext
                (
                    KTC.kContextPushVar
                    (
                        KP.VarAssignment
                            (KP.Identifier "var2")
                            KP.Double
                    )
                    (
                        KTC.kContextPushVar
                        (
                            KP.VarAssignment
                                (KP.Identifier "var1")
                                KP.Int
                        )
                        KTC.getEmptyKContext
                    )
                )
            )
        )
            ==
            KTC.Kcontext
                (KTC.GlobalContext $ HM.fromList [
                    (KP.Identifier "var1", KTC.Var KTC.Int),
                    (KP.Identifier "var2", KTC.Var KTC.Double)
                ])
                (KTC.DefContext    $ HM.fromList [])
                (Just $ KTC.LocalContext $ HM.fromList [
                    (KP.Identifier "var1", KTC.Var KTC.Int),
                    (KP.Identifier "var2", KTC.Var KTC.Boolean)
                ])
    it "kContextPushVar: Two same global var push, simple 1, failure." $
        evaluate (
        KTC.kContextPushVar
            (
                KP.VarAssignment
                    (KP.Identifier "var")
                    KP.Int
            )
            (
                KTC.kContextPushVar
                    (
                        KP.VarAssignment
                            (KP.Identifier "var")
                            KP.Int
                    )
                    KTC.getEmptyKContext
            )
        )
        `shouldThrow`
        (== KTE.ShadowedVariableByVariable
            (KP.Identifier "var")
            (KP.VarAssignment
                (KP.Identifier "var")
                KP.Int
            )
        )
    it "kContextPushVar: Two same local var push, simple 1, failure." $
        evaluate (
        KTC.kContextPushVar
            (
                KP.VarAssignment
                    (KP.Identifier "var")
                    KP.Int
            )
            (
                KTC.kContextPushVar
                    (
                        KP.VarAssignment
                            (KP.Identifier "var")
                            KP.Int
                    )
                    (KTC.kContextEnterLocalContext KTC.getEmptyKContext)
            )
        )
        `shouldThrow`
        (== KTE.ShadowedVariableByVariable
            (KP.Identifier "var")
            (KP.VarAssignment
                (KP.Identifier "var")
                KP.Int
            )
        )
    it "kContextPushVar: One global var & One def push with same name, simple 1, failure." $
        evaluate (
        KTC.kContextPushFunction
            (
                KP.PrototypeFunction
                    (KP.Identifier "foo")
                    (KP.PrototypeArgs [] KP.Int)
            )
            (
                KTC.kContextPushVar
                (
                    KP.VarAssignment
                        (KP.Identifier "foo")
                        KP.Int
                )
                KTC.getEmptyKContext
            )
        )
        `shouldThrow`
        (== KTE.ShadowedVariableByDefinition
            (KP.Identifier "foo")
            (KP.PrototypeFunction
                (KP.Identifier "foo")
                (KP.PrototypeArgs [] KP.Int)
            )
        )
    it "kContextPushVar: One def push & One global var push with same name, simple 1, failure." $
        evaluate (
        KTC.kContextPushVar
            (
                KP.VarAssignment
                    (KP.Identifier "foo")
                    KP.Int
            )
            (
                KTC.kContextPushFunction
                    (KP.PrototypeFunction
                        (KP.Identifier "foo")
                        (KP.PrototypeArgs [] KP.Int)
                    )
                    KTC.getEmptyKContext
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
    it "kContextPushVar: One def push & One local var push with same name, simple 1, failure." $
        evaluate (
        KTC.kContextPushVar
            (
                KP.VarAssignment
                    (KP.Identifier "foo")
                    KP.Int
            )
            (
                KTC.kContextPushFunction
                    (KP.PrototypeFunction
                        (KP.Identifier "foo")
                        (KP.PrototypeArgs [] KP.Int)
                    )
                    (KTC.kContextEnterLocalContext KTC.getEmptyKContext)
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
    it "kContextFind: Empty context, failure." $
        isNothing $
        KTC.kContextFind
            KTC.getEmptyKContext
            (KP.Identifier "foo")
    it "kContextFind: Only def context, failure." $
        isNothing $
        KTC.kContextFind
            (
                KTC.Kcontext
                    (KTC.GlobalContext $ HM.fromList [] )
                    (KTC.DefContext    $ HM.fromList
                        [
                            (KP.Identifier "func1", KTC.Function $ KTC.FunctionTyping [KTC.Boolean, KTC.Boolean] KTC.Boolean),
                            (KP.Identifier "func2", KTC.Function $ KTC.FunctionTyping [] KTC.Int)
                        ]
                    )
                Nothing
            )
            (KP.Identifier "foo")
    it "kContextFind: Only global var context, failure." $
        isNothing $
        KTC.kContextFind
            (
                KTC.Kcontext
                    (KTC.GlobalContext $ HM.fromList
                        [
                            (KP.Identifier "var1", KTC.Var KTC.Boolean),
                            (KP.Identifier "var2", KTC.Var KTC.Boolean),
                            (KP.Identifier "var3", KTC.Var KTC.Boolean)
                        ]
                    )
                    (KTC.DefContext HM.empty)
                Nothing
            )
            (KP.Identifier "foo")
    it "kContextFind: Only local var context, failure." $
        isNothing $
        KTC.kContextFind
            (
                KTC.Kcontext
                    (KTC.GlobalContext HM.empty)
                    (KTC.DefContext    HM.empty)
                    (Just $ KTC.LocalContext $ HM.fromList
                        [
                            (KP.Identifier "var1", KTC.Var KTC.Boolean),
                            (KP.Identifier "var2", KTC.Var KTC.Boolean),
                            (KP.Identifier "var3", KTC.Var KTC.Boolean)
                        ]
                    )
            )
            (KP.Identifier "foo")
    it "kContextFind: Only def context, success." $
        KTC.kContextFind
            (
                KTC.Kcontext
                    (KTC.GlobalContext $ HM.fromList [] )
                    (KTC.DefContext    $ HM.fromList
                        [
                            (KP.Identifier "func1", KTC.Function $ KTC.FunctionTyping [KTC.Boolean, KTC.Boolean] KTC.Boolean),
                            (KP.Identifier "func2", KTC.Function $ KTC.FunctionTyping [] KTC.Int)
                        ]
                    )
                Nothing
            )
            (KP.Identifier "func2")
            ==
            Just  (KTC.Function $ KTC.FunctionTyping [] KTC.Int)
    it "kContextFind: Only global var context, success." $
        KTC.kContextFind
            (
                KTC.Kcontext
                    (KTC.GlobalContext $ HM.fromList
                        [
                            (KP.Identifier "var1", KTC.Var KTC.Boolean),
                            (KP.Identifier "var2", KTC.Var KTC.Boolean),
                            (KP.Identifier "var3", KTC.Var KTC.Boolean)
                        ]
                    )
                    (KTC.DefContext HM.empty)
                Nothing
            )
            (KP.Identifier "var2")
            ==
            Just (KTC.Var KTC.Boolean)
    it "kContextFind: Only local var context, success." $
        KTC.kContextFind
            (
                KTC.Kcontext
                    (KTC.GlobalContext HM.empty)
                    (KTC.DefContext    HM.empty)
                    (Just $ KTC.LocalContext $ HM.fromList
                        [
                            (KP.Identifier "var1", KTC.Var KTC.Boolean),
                            (KP.Identifier "var2", KTC.Var KTC.Boolean),
                            (KP.Identifier "var3", KTC.Var KTC.Boolean)
                        ]
                    )
            )
            (KP.Identifier "var2")
            ==
            Just (KTC.Var KTC.Boolean)
    it "kContextFind: Shadowed global var by local var with same type, success." $
        KTC.kContextFind
            (
                KTC.Kcontext
                    (KTC.GlobalContext       $ HM.fromList
                        [
                            (KP.Identifier "var1",         KTC.Var KTC.Int),
                            (KP.Identifier "shadowed_var", KTC.Var KTC.Double),
                            (KP.Identifier "var2",         KTC.Var KTC.Int)
                        ]
                    )
                    (KTC.DefContext HM.empty)
                    (Just $ KTC.LocalContext $ HM.fromList
                        [
                            (KP.Identifier "var3",         KTC.Var KTC.Int),
                            (KP.Identifier "shadowed_var", KTC.Var KTC.Double),
                            (KP.Identifier "var4",         KTC.Var KTC.Int)
                        ]
                    )
            )
            (KP.Identifier "shadowed_var")
            ==
            Just (KTC.Var KTC.Double)
    it "kContextFind: Shadowed global var by local var with different type, success." $
        KTC.kContextFind
            (
                KTC.Kcontext
                    (KTC.GlobalContext       $ HM.fromList
                        [
                            (KP.Identifier "var1",         KTC.Var KTC.Int),
                            (KP.Identifier "shadowed_var", KTC.Var KTC.Double),
                            (KP.Identifier "var2",         KTC.Var KTC.Int)
                        ]
                    )
                    (KTC.DefContext HM.empty)
                    (Just $ KTC.LocalContext $ HM.fromList
                        [
                            (KP.Identifier "var3",         KTC.Var KTC.Int),
                            (KP.Identifier "shadowed_var", KTC.Var KTC.Boolean),
                            (KP.Identifier "var4",         KTC.Var KTC.Int)
                        ]
                    )
            )
            (KP.Identifier "shadowed_var")
            ==
            Just (KTC.Var KTC.Boolean)