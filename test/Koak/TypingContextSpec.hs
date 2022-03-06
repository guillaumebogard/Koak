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

import Koak.TypingContext           ( KCONTEXT(..)
                                    , GLOBAL_CONTEXT(..)
                                    , DEF_CONTEXT(..)
                                    , LOCAL_CONTEXT(..)
                                    , BASE_TYPE(..)
                                    , FUNCTION_TYPING(..)
                                    , TYPE_SIGNATURE(..)
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
            KCONTEXT (GLOBAL_CONTEXT HM.empty) (DEF_CONTEXT HM.empty) Nothing
    it "kContextPushDef: One def push, simple 1, success" $
        kContextPushDef
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
            getEmptyKContext
            ==
            KCONTEXT
                (GLOBAL_CONTEXT $ HM.fromList [])
                (DEF_CONTEXT    $ HM.fromList [
                    (KP.IDENTIFIER "foo", FUNCTION $ FUNCTION_TYPING [] INT)
                ])
                Nothing
    it "kContextPushDef: One def push, simple 2, success" $
        kContextPushDef
            (KP.DEFS
                (KP.PROTOTYPE
                    (KP.IDENTIFIER "bar")
                    (KP.PROTOTYPE_ARGS [
                        KP.PROTOTYPE_ID (KP.IDENTIFIER "x") KP.INT
                    ] KP.INT)
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
            getEmptyKContext
            ==
            KCONTEXT
                (GLOBAL_CONTEXT $ HM.fromList [])
                (DEF_CONTEXT    $ HM.fromList [
                    (KP.IDENTIFIER "bar", FUNCTION $ FUNCTION_TYPING [INT] INT)
                ])
                Nothing
    it "kContextPushDef: One def push, simple 3, success" $
        kContextPushDef
            (KP.DEFS
                (KP.PROTOTYPE
                    (KP.IDENTIFIER "foobar")
                    (KP.PROTOTYPE_ARGS [
                        KP.PROTOTYPE_ID (KP.IDENTIFIER "x") KP.DOUBLE
                    ] KP.VOID)
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
            getEmptyKContext
            ==
            KCONTEXT
                (GLOBAL_CONTEXT $ HM.fromList [])
                (DEF_CONTEXT    $ HM.fromList [
                    (KP.IDENTIFIER "foobar", FUNCTION $ FUNCTION_TYPING [DOUBLE] NIL)
                ])
                Nothing
    it "kContextPushDef: One def push, complex 1, success" $
        kContextPushDef
            (KP.DEFS
                (KP.PROTOTYPE
                    (KP.IDENTIFIER "foobar")
                    (KP.PROTOTYPE_ARGS [
                        KP.PROTOTYPE_ID (KP.IDENTIFIER "a") KP.INT,
                        KP.PROTOTYPE_ID (KP.IDENTIFIER "b") KP.DOUBLE,
                        KP.PROTOTYPE_ID (KP.IDENTIFIER "c") KP.BOOLEAN,
                        KP.PROTOTYPE_ID (KP.IDENTIFIER "d") KP.VOID
                    ] KP.DOUBLE)
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
            getEmptyKContext
            ==
            KCONTEXT
                (GLOBAL_CONTEXT $ HM.fromList [])
                (DEF_CONTEXT    $ HM.fromList [
                    (KP.IDENTIFIER "foobar", FUNCTION $ FUNCTION_TYPING [INT, DOUBLE, BOOLEAN, NIL] DOUBLE)
                ])
                Nothing
    it "kContextPushDef: Multiple def push, complex 1, success" $
            KP.DEFS
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
            `kContextPushDef`
            (
                KP.DEFS
                    (KP.PROTOTYPE
                        (KP.IDENTIFIER "bar")
                        (KP.PROTOTYPE_ARGS [
                            KP.PROTOTYPE_ID (KP.IDENTIFIER "a") KP.INT,
                            KP.PROTOTYPE_ID (KP.IDENTIFIER "b") KP.DOUBLE,
                            KP.PROTOTYPE_ID (KP.IDENTIFIER "c") KP.BOOLEAN,
                            KP.PROTOTYPE_ID (KP.IDENTIFIER "d") KP.VOID
                        ] KP.DOUBLE)
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
                `kContextPushDef`
                (
                    KP.DEFS
                        (KP.PROTOTYPE
                            (KP.IDENTIFIER "foobar")
                            (KP.PROTOTYPE_ARGS [
                                KP.PROTOTYPE_ID (KP.IDENTIFIER "a") KP.INT,
                                KP.PROTOTYPE_ID (KP.IDENTIFIER "b") KP.INT,
                                KP.PROTOTYPE_ID (KP.IDENTIFIER "c") KP.BOOLEAN,
                                KP.PROTOTYPE_ID (KP.IDENTIFIER "d") KP.INT
                            ] KP.BOOLEAN)
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
                    `kContextPushDef`
                    getEmptyKContext
                )
            )
            ==
            KCONTEXT
                (GLOBAL_CONTEXT $ HM.fromList [])
                (DEF_CONTEXT    $ HM.fromList [
                    (KP.IDENTIFIER "foo",    FUNCTION $ FUNCTION_TYPING [] INT),
                    (KP.IDENTIFIER "bar",    FUNCTION $ FUNCTION_TYPING [INT, DOUBLE, BOOLEAN, NIL] DOUBLE),
                    (KP.IDENTIFIER "foobar", FUNCTION $ FUNCTION_TYPING [INT, INT, BOOLEAN, INT] BOOLEAN)
                ])
                Nothing
    it "kContextPushDef: Two same def push, simple 1, failure" $ do
        evaluate (
                KP.DEFS
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
                `kContextPushDef`
                (
                    KP.DEFS
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
                    `kContextPushDef`
                    getEmptyKContext
                )
            )
            `shouldThrow`
            (== ShadowedDefinitionByDefinition
                (KP.IDENTIFIER "foo")
                (KP.PROTOTYPE
                    (KP.IDENTIFIER "foo")
                    (KP.PROTOTYPE_ARGS [] KP.INT)
                )
            )
    it "kContextPushDef: Two same def push, complex 1, failure" $ do
        evaluate (
                KP.DEFS
                    (KP.PROTOTYPE
                        (KP.IDENTIFIER "foo")
                        (KP.PROTOTYPE_ARGS [
                                    KP.PROTOTYPE_ID (KP.IDENTIFIER "a") KP.BOOLEAN,
                                    KP.PROTOTYPE_ID (KP.IDENTIFIER "b") KP.BOOLEAN
                            ] KP.BOOLEAN
                        )
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
                `kContextPushDef`
                (
                    KP.DEFS
                        (KP.PROTOTYPE
                            (KP.IDENTIFIER "foo")
                            (KP.PROTOTYPE_ARGS [
                                KP.PROTOTYPE_ID (KP.IDENTIFIER "a") KP.INT,
                                KP.PROTOTYPE_ID (KP.IDENTIFIER "b") KP.INT,
                                KP.PROTOTYPE_ID (KP.IDENTIFIER "c") KP.BOOLEAN,
                                KP.PROTOTYPE_ID (KP.IDENTIFIER "d") KP.INT

                            ] KP.VOID)
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
                    `kContextPushDef`
                    getEmptyKContext
                )
            )
            `shouldThrow`
            (== ShadowedDefinitionByDefinition
                (KP.IDENTIFIER "foo")
                (KP.PROTOTYPE
                    (KP.IDENTIFIER "foo")
                    (KP.PROTOTYPE_ARGS [
                            KP.PROTOTYPE_ID (KP.IDENTIFIER "a") KP.BOOLEAN,
                            KP.PROTOTYPE_ID (KP.IDENTIFIER "b") KP.BOOLEAN
                    ] KP.BOOLEAN)
                )
            )
    it "kContextPushDef: One global var push, simple 1, success." $ do
            kContextPushVar
                (
                    KP.VAR_ASSIGNMENT
                        (KP.IDENTIFIER "var1")
                        KP.INT
                )
                getEmptyKContext
            ==
            KCONTEXT
                (GLOBAL_CONTEXT $ HM.fromList [
                    (KP.IDENTIFIER "var1", VAR INT)
                ])
                (DEF_CONTEXT    $ HM.fromList [])
                Nothing
    it "kContextPushDef: Multiple global var push, complex 1, success." $ do
            kContextPushVar
                (
                    KP.VAR_ASSIGNMENT
                        (KP.IDENTIFIER "var4")
                        KP.BOOLEAN
                )
                (
                    kContextPushVar
                    (
                        KP.VAR_ASSIGNMENT
                            (KP.IDENTIFIER "var3")
                            KP.INT
                    )
                    (
                        kContextPushVar
                        (
                            KP.VAR_ASSIGNMENT
                                (KP.IDENTIFIER "var2")
                                KP.DOUBLE
                        )
                        (
                            kContextPushVar
                            (
                                KP.VAR_ASSIGNMENT
                                    (KP.IDENTIFIER "var1")
                                    KP.INT
                            )
                            getEmptyKContext
                        )
                    )
                )
            ==
            KCONTEXT
                (GLOBAL_CONTEXT $ HM.fromList [
                    (KP.IDENTIFIER "var1", VAR INT),
                    (KP.IDENTIFIER "var2", VAR DOUBLE),
                    (KP.IDENTIFIER "var3", VAR INT),
                    (KP.IDENTIFIER "var4", VAR BOOLEAN)
                ])
                (DEF_CONTEXT    $ HM.fromList [])
                Nothing
    it "kContextPushDef: Multiple global var push & Multiple local var push, complex 1, success." $ do
            kContextPushVar
                (
                    KP.VAR_ASSIGNMENT
                        (KP.IDENTIFIER "var4")
                        KP.BOOLEAN
                )
                (
                    kContextPushVar
                    (
                        KP.VAR_ASSIGNMENT
                            (KP.IDENTIFIER "var3")
                            KP.INT
                    )
                    (
                        kContextEnterLocalContext
                        (
                            kContextPushVar
                            (
                                KP.VAR_ASSIGNMENT
                                    (KP.IDENTIFIER "var2")
                                    KP.DOUBLE
                            )
                            (
                                kContextPushVar
                                (
                                    KP.VAR_ASSIGNMENT
                                        (KP.IDENTIFIER "var1")
                                        KP.INT
                                )
                                getEmptyKContext
                            )
                        )
                    )
                )
            ==
            KCONTEXT
                (GLOBAL_CONTEXT $ HM.fromList [
                    (KP.IDENTIFIER "var1", VAR INT),
                    (KP.IDENTIFIER "var2", VAR DOUBLE)
                ])
                (DEF_CONTEXT    $ HM.fromList [])
                (Just $ LOCAL_CONTEXT $ HM.fromList [
                    (KP.IDENTIFIER "var3", VAR INT),
                    (KP.IDENTIFIER "var4", VAR BOOLEAN)
                ])
    it "kContextPushDef: Multiple global var push & Multiple local var with the same name, complex 1, failure." $ do
            kContextPushVar
                (
                    KP.VAR_ASSIGNMENT
                        (KP.IDENTIFIER "var2")
                        KP.BOOLEAN
                )
                (
                    kContextPushVar
                    (
                        KP.VAR_ASSIGNMENT
                            (KP.IDENTIFIER "var1")
                            KP.INT
                    )
                    (
                        kContextEnterLocalContext
                        (
                            kContextPushVar
                            (
                                KP.VAR_ASSIGNMENT
                                    (KP.IDENTIFIER "var2")
                                    KP.DOUBLE
                            )
                            (
                                kContextPushVar
                                (
                                    KP.VAR_ASSIGNMENT
                                        (KP.IDENTIFIER "var1")
                                        KP.INT
                                )
                                getEmptyKContext
                            )
                        )
                    )
                )
            ==
            KCONTEXT
                (GLOBAL_CONTEXT $ HM.fromList [
                    (KP.IDENTIFIER "var1", VAR INT),
                    (KP.IDENTIFIER "var2", VAR DOUBLE)
                ])
                (DEF_CONTEXT    $ HM.fromList [])
                (Just $ LOCAL_CONTEXT $ HM.fromList [
                    (KP.IDENTIFIER "var1", VAR INT),
                    (KP.IDENTIFIER "var2", VAR BOOLEAN)
                ])
    it "kContextPushDef: Two same global var push, simple 1, failure." $ do
        evaluate (
            kContextPushVar
                (
                    KP.VAR_ASSIGNMENT
                        (KP.IDENTIFIER "var")
                        KP.INT
                )
                (
                    kContextPushVar
                        (
                            KP.VAR_ASSIGNMENT
                                (KP.IDENTIFIER "var")
                                KP.INT
                        )
                        getEmptyKContext
                )
            )
            `shouldThrow`
            (== ShadowedVariableByVariable
                (KP.IDENTIFIER "var")
                (KP.VAR_ASSIGNMENT
                    (KP.IDENTIFIER "var")
                    KP.INT
                )
            )
    it "kContextPushDef: Two same local var push, simple 1, failure." $ do
        evaluate (
            kContextPushVar
                (
                    KP.VAR_ASSIGNMENT
                        (KP.IDENTIFIER "var")
                        KP.INT
                )
                (
                    kContextPushVar
                        (
                            KP.VAR_ASSIGNMENT
                                (KP.IDENTIFIER "var")
                                KP.INT
                        )
                        (kContextEnterLocalContext getEmptyKContext)
                )
            )
            `shouldThrow`
            (== ShadowedVariableByVariable
                (KP.IDENTIFIER "var")
                (KP.VAR_ASSIGNMENT
                    (KP.IDENTIFIER "var")
                    KP.INT
                )
            )
    it "kContextPushDef: One global var & One def push with same name, simple 1, failure." $ do
        evaluate (
            kContextPushDef
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
                (
                    kContextPushVar
                    (
                        KP.VAR_ASSIGNMENT
                            (KP.IDENTIFIER "foo")
                            KP.INT
                    )
                    getEmptyKContext
                )
            )
            `shouldThrow`
            (== ShadowedVariableByDefinition
                (KP.IDENTIFIER "foo")
                (KP.PROTOTYPE
                    (KP.IDENTIFIER "foo")
                    (KP.PROTOTYPE_ARGS [] KP.INT)
                )
            )
    it "kContextPushDef: One def push & One global var push with same name, simple 1, failure." $ do
        evaluate (
            kContextPushVar
                (
                    KP.VAR_ASSIGNMENT
                        (KP.IDENTIFIER "foo")
                        KP.INT
                )
                (
                    kContextPushDef
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
                        getEmptyKContext
                )
            )
            `shouldThrow`
            (== ShadowedDefinitionByVariable
                (KP.IDENTIFIER "foo")
                (KP.VAR_ASSIGNMENT
                    (KP.IDENTIFIER "foo")
                    KP.INT
                )
            )
    it "kContextPushDef: One def push & One local var push with same name, simple 1, failure." $ do
        evaluate (
            kContextPushVar
                (
                    KP.VAR_ASSIGNMENT
                        (KP.IDENTIFIER "foo")
                        KP.INT
                )
                (
                    kContextPushDef
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
                        (kContextEnterLocalContext getEmptyKContext)
                )
            )
            `shouldThrow`
            (== ShadowedDefinitionByVariable
                (KP.IDENTIFIER "foo")
                (KP.VAR_ASSIGNMENT
                    (KP.IDENTIFIER "foo")
                    KP.INT
                )
            )
    it "kContextFind: Empty context, failure." $
        isNothing $
        kContextFind
            getEmptyKContext
            (KP.IDENTIFIER "foo")
    it "kContextFind: Only def context, failure." $
        isNothing $
        kContextFind
            (
                KCONTEXT
                    (GLOBAL_CONTEXT $ HM.fromList [] )
                    (DEF_CONTEXT    $ HM.fromList
                        [
                            (KP.IDENTIFIER "func1", FUNCTION $ FUNCTION_TYPING [BOOLEAN, BOOLEAN] BOOLEAN),
                            (KP.IDENTIFIER "func2", FUNCTION $ FUNCTION_TYPING [] INT)
                        ]
                    )
                Nothing
            )
            (KP.IDENTIFIER "foo")
    it "kContextFind: Only global var context, failure." $
        isNothing $
        kContextFind
            (
                KCONTEXT
                    (GLOBAL_CONTEXT $ HM.fromList
                        [
                            (KP.IDENTIFIER "var1", VAR BOOLEAN),
                            (KP.IDENTIFIER "var2", VAR BOOLEAN),
                            (KP.IDENTIFIER "var3", VAR BOOLEAN)
                        ]
                    )
                    (DEF_CONTEXT HM.empty)
                Nothing
            )
            (KP.IDENTIFIER "foo")
    it "kContextFind: Only local var context, failure." $
        isNothing $
        kContextFind
            (
                KCONTEXT
                    (GLOBAL_CONTEXT HM.empty)
                    (DEF_CONTEXT    HM.empty)
                    (Just $ LOCAL_CONTEXT $ HM.fromList
                        [
                            (KP.IDENTIFIER "var1", VAR BOOLEAN),
                            (KP.IDENTIFIER "var2", VAR BOOLEAN),
                            (KP.IDENTIFIER "var3", VAR BOOLEAN)
                        ]
                    )
            )
            (KP.IDENTIFIER "foo")
    it "kContextFind: Only def context, success." $
        kContextFind
            (
                KCONTEXT
                    (GLOBAL_CONTEXT $ HM.fromList [] )
                    (DEF_CONTEXT    $ HM.fromList
                        [
                            (KP.IDENTIFIER "func1", FUNCTION $ FUNCTION_TYPING [BOOLEAN, BOOLEAN] BOOLEAN),
                            (KP.IDENTIFIER "func2", FUNCTION $ FUNCTION_TYPING [] INT)
                        ]
                    )
                Nothing
            )
            (KP.IDENTIFIER "func2")
            ==
            Just  (FUNCTION $ FUNCTION_TYPING [] INT)
    it "kContextFind: Only global var context, success." $
        kContextFind
            (
                KCONTEXT
                    (GLOBAL_CONTEXT $ HM.fromList
                        [
                            (KP.IDENTIFIER "var1", VAR BOOLEAN),
                            (KP.IDENTIFIER "var2", VAR BOOLEAN),
                            (KP.IDENTIFIER "var3", VAR BOOLEAN)
                        ]
                    )
                    (DEF_CONTEXT HM.empty)
                Nothing
            )
            (KP.IDENTIFIER "var2")
            ==
            Just (VAR BOOLEAN)
    it "kContextFind: Only local var context, success." $
        kContextFind
            (
                KCONTEXT
                    (GLOBAL_CONTEXT HM.empty)
                    (DEF_CONTEXT    HM.empty)
                    (Just $ LOCAL_CONTEXT $ HM.fromList
                        [
                            (KP.IDENTIFIER "var1", VAR BOOLEAN),
                            (KP.IDENTIFIER "var2", VAR BOOLEAN),
                            (KP.IDENTIFIER "var3", VAR BOOLEAN)
                        ]
                    )
            )
            (KP.IDENTIFIER "var2")
            ==
            Just (VAR BOOLEAN)
    it "kContextFind: Shadowed global var by local var with same type, success." $
        kContextFind
            (
                KCONTEXT
                    (GLOBAL_CONTEXT       $ HM.fromList
                        [
                            (KP.IDENTIFIER "var1",         VAR INT),
                            (KP.IDENTIFIER "shadowed_var", VAR DOUBLE),
                            (KP.IDENTIFIER "var2",         VAR INT)
                        ]
                    )
                    (DEF_CONTEXT HM.empty)
                    (Just $ LOCAL_CONTEXT $ HM.fromList
                        [
                            (KP.IDENTIFIER "var3",         VAR INT),
                            (KP.IDENTIFIER "shadowed_var", VAR DOUBLE),
                            (KP.IDENTIFIER "var4",         VAR INT)
                        ]
                    )
            )
            (KP.IDENTIFIER "shadowed_var")
            ==
            Just (VAR DOUBLE)
    it "kContextFind: Shadowed global var by local var with different type, success." $
        kContextFind
            (
                KCONTEXT
                    (GLOBAL_CONTEXT       $ HM.fromList
                        [
                            (KP.IDENTIFIER "var1",         VAR INT),
                            (KP.IDENTIFIER "shadowed_var", VAR DOUBLE),
                            (KP.IDENTIFIER "var2",         VAR INT)
                        ]
                    )
                    (DEF_CONTEXT HM.empty)
                    (Just $ LOCAL_CONTEXT $ HM.fromList
                        [
                            (KP.IDENTIFIER "var3",         VAR INT),
                            (KP.IDENTIFIER "shadowed_var", VAR BOOLEAN),
                            (KP.IDENTIFIER "var4",         VAR INT)
                        ]
                    )
            )
            (KP.IDENTIFIER "shadowed_var")
            ==
            Just (VAR BOOLEAN)
