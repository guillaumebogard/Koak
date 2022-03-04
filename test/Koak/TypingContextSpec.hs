--
-- EPITECH PROJECT, 2022
-- koak
-- File description:
-- Koak.TypingContextSpec
--

module Koak.TypingContextSpec       ( spec ) where

import Control.Exception (evaluate)

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
                                    )
import qualified Koak.Parser as KP

import Data.HashMap.Strict  as HM   ( HashMap
                                    , fromList
                                    , empty
                                    , member
                                    , insert
                                    )

import Koak.Typing.Exception        ( KoakTypingException(..) )

spec :: Spec
spec = do
    it "getEmptyKContext basic" $
        getEmptyKContext
            ==
            KCONTEXT (GLOBAL_CONTEXT HM.empty) (DEF_CONTEXT HM.empty) (LOCAL_CONTEXT HM.empty)
    it "kContextPushDef: One push, empty context, simple signature 1" $
        kContextPushDef
            getEmptyKContext
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
            ==
            KCONTEXT
                (GLOBAL_CONTEXT $ HM.fromList [])
                (DEF_CONTEXT    $ HM.fromList [
                    (KP.IDENTIFIER "foo", FUNCTION $ FUNCTION_TYPING [] INT)
                ])
                (LOCAL_CONTEXT  $ HM.fromList [])
    it "kContextPushDef: One push, empty context, simple signature 2" $
        kContextPushDef
            getEmptyKContext
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
            ==
            KCONTEXT
                (GLOBAL_CONTEXT $ HM.fromList [])
                (DEF_CONTEXT    $ HM.fromList [
                    (KP.IDENTIFIER "bar", FUNCTION $ FUNCTION_TYPING [INT] INT)
                ])
                (LOCAL_CONTEXT  $ HM.fromList [])
    it "kContextPushDef: One push, empty context, simple signature 3" $
        kContextPushDef
            getEmptyKContext
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
            ==
            KCONTEXT
                (GLOBAL_CONTEXT $ HM.fromList [])
                (DEF_CONTEXT    $ HM.fromList [
                    (KP.IDENTIFIER "foobar", FUNCTION $ FUNCTION_TYPING [DOUBLE] NIL)
                ])
                (LOCAL_CONTEXT  $ HM.fromList [])
    it "kContextPushDef: One push, empty context, complex signature 1" $
        kContextPushDef
            getEmptyKContext
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
            ==
            KCONTEXT
                (GLOBAL_CONTEXT $ HM.fromList [])
                (DEF_CONTEXT    $ HM.fromList [
                    (KP.IDENTIFIER "foobar", FUNCTION $ FUNCTION_TYPING [INT, DOUBLE, BOOLEAN, NIL] DOUBLE)
                ])
                (LOCAL_CONTEXT  $ HM.fromList [])
    it "kContextPushDef: Multiple push, empty context, complex signature" $
            getEmptyKContext
            `kContextPushDef`
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
            ==
            KCONTEXT
                (GLOBAL_CONTEXT $ HM.fromList [])
                (DEF_CONTEXT    $ HM.fromList [
                    (KP.IDENTIFIER "foo",    FUNCTION $ FUNCTION_TYPING [] INT),
                    (KP.IDENTIFIER "bar",    FUNCTION $ FUNCTION_TYPING [INT, DOUBLE, BOOLEAN, NIL] DOUBLE),
                    (KP.IDENTIFIER "foobar", FUNCTION $ FUNCTION_TYPING [INT, INT, BOOLEAN, INT] BOOLEAN)
                ])
                (LOCAL_CONTEXT  $ HM.fromList [])
    it "kContextPushDef: Pushing twice same function" $ do
        evaluate (
                getEmptyKContext
                `kContextPushDef`
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
            )
            `shouldThrow`
            (== ShadowedDefinitionByDefinition 
                (KP.PROTOTYPE 
                    (KP.IDENTIFIER "foo")
                    (KP.PROTOTYPE_ARGS [] KP.INT)
                )
            )
    it "kContextPushDef: Pushing two functions with same name" $ do
        evaluate (
                getEmptyKContext
                `kContextPushDef`
                KP.DEFS
                    (KP.PROTOTYPE
                        (KP.IDENTIFIER "foo")
                        (KP.PROTOTYPE_ARGS [
                                KP.PROTOTYPE_ID (KP.IDENTIFIER "a") KP.INT,
                                KP.PROTOTYPE_ID (KP.IDENTIFIER "b") KP.INT,
                                KP.PROTOTYPE_ID (KP.IDENTIFIER "c") KP.BOOLEAN,
                                KP.PROTOTYPE_ID (KP.IDENTIFIER "d") KP.INT
                            ] KP.VOID
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
                KP.DEFS
                    (KP.PROTOTYPE
                        (KP.IDENTIFIER "foo")
                        (KP.PROTOTYPE_ARGS [
                                KP.PROTOTYPE_ID (KP.IDENTIFIER "a") KP.BOOLEAN,
                                KP.PROTOTYPE_ID (KP.IDENTIFIER "b") KP.BOOLEAN
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
            )
            `shouldThrow`
            (== ShadowedDefinitionByDefinition 
                (KP.PROTOTYPE
                    (KP.IDENTIFIER "foo")
                    (KP.PROTOTYPE_ARGS [
                            KP.PROTOTYPE_ID (KP.IDENTIFIER "a") KP.BOOLEAN,
                            KP.PROTOTYPE_ID (KP.IDENTIFIER "b") KP.BOOLEAN
                    ] KP.BOOLEAN)
                )
            )
