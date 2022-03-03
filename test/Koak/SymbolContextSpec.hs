--
-- EPITECH PROJECT, 2022
-- koak
-- File description:
-- Koak.SymbolContextSpec
--

module Koak.SymbolContextSpec  ( spec ) where

import Test.Hspec           ( Spec
                            , it
                            , shouldBe
                            , shouldThrow
                            , anyException
                            )

import Koak.SymbolContext   ( sContextPushNewFrame
                            , sContextPushVar
                            , sContextPushVars
                            , sContextPushPrototype
                            , SYMBOL_CONTEXT(..)
                            , VAR_FRAME_STACK(..)
                            , VAR_FRAME(..)
                            )

import Koak.Parser as KP    ( IDENTIFIER(..)
                            , TYPE(..)
                            , VAR_SIGNATURE(..)
                            )


import qualified Data.Ord

spec :: Spec
spec = do
    it "sContextPushNewFrame empty context" $
        sContextPushNewFrame (
            SYMBOL_CONTEXT
                []
                []
        )
            ==
            SYMBOL_CONTEXT
                []
                [
                    VAR_FRAME []
                ]
    it "sContextPushNewFrame basic context" $
        sContextPushNewFrame (
            SYMBOL_CONTEXT
                []
                [
                    VAR_FRAME
                        [
                            VAR_SIGNATURE (IDENTIFIER "var1") INT,
                            VAR_SIGNATURE (IDENTIFIER "var2") INT,
                            VAR_SIGNATURE (IDENTIFIER "var3") DOUBLE
                        ],
                    VAR_FRAME
                        [
                            VAR_SIGNATURE (IDENTIFIER "var4") DOUBLE
                        ]
                ]
        )
            ==
            SYMBOL_CONTEXT
                []
                [
                    VAR_FRAME
                        [],
                    VAR_FRAME
                        [
                            VAR_SIGNATURE (IDENTIFIER "var1") INT,
                            VAR_SIGNATURE (IDENTIFIER "var2") INT,
                            VAR_SIGNATURE (IDENTIFIER "var3") DOUBLE
                        ],
                    VAR_FRAME
                        [
                            VAR_SIGNATURE (IDENTIFIER "var4") DOUBLE
                        ]
                ]
    it "sContextPushVar empty context, no effect" $
        sContextPushVar (
            SYMBOL_CONTEXT
                []
                []
        ) (VAR_SIGNATURE (IDENTIFIER "var") INT)
            ==
            SYMBOL_CONTEXT
                []
                []
    it "sContextPushVar only one empty frame" $
        sContextPushVar (
            SYMBOL_CONTEXT
                []
                [
                    VAR_FRAME []
                ]
        ) (VAR_SIGNATURE (IDENTIFIER "var") INT)
            ==
            SYMBOL_CONTEXT
                []
                [
                    VAR_FRAME
                        [
                            VAR_SIGNATURE (IDENTIFIER "var") INT
                        ]
                ]
    it "sContextPushVar one frame with many vars" $
        sContextPushVar (
            SYMBOL_CONTEXT
                []
                [
                    VAR_FRAME
                        [
                            VAR_SIGNATURE (IDENTIFIER "var1") INT,
                            VAR_SIGNATURE (IDENTIFIER "var2") DOUBLE,
                            VAR_SIGNATURE (IDENTIFIER "var3") INT

                        ]
                ]
        ) (VAR_SIGNATURE (IDENTIFIER "var4") DOUBLE)
            ==
            SYMBOL_CONTEXT
                []
                [
                    VAR_FRAME
                        [
                            VAR_SIGNATURE (IDENTIFIER "var4") DOUBLE,
                            VAR_SIGNATURE (IDENTIFIER "var1") INT,
                            VAR_SIGNATURE (IDENTIFIER "var2") DOUBLE,
                            VAR_SIGNATURE (IDENTIFIER "var3") INT
                        ]
                ]
    it "sContextPushVar many frame with many vars" $
        sContextPushVar (
            SYMBOL_CONTEXT
                []
                [
                    VAR_FRAME
                        [
                            VAR_SIGNATURE (IDENTIFIER "var1") INT,
                            VAR_SIGNATURE (IDENTIFIER "var2") DOUBLE,
                            VAR_SIGNATURE (IDENTIFIER "var3") INT

                        ],
                    VAR_FRAME
                        [
                            VAR_SIGNATURE (IDENTIFIER "var4") DOUBLE,
                            VAR_SIGNATURE (IDENTIFIER "var5") DOUBLE

                        ],
                    VAR_FRAME [],
                    VAR_FRAME
                        [
                            VAR_SIGNATURE (IDENTIFIER "var6") DOUBLE,
                            VAR_SIGNATURE (IDENTIFIER "var7") INT,
                            VAR_SIGNATURE (IDENTIFIER "var8") INT,
                            VAR_SIGNATURE (IDENTIFIER "var9") INT

                        ]
                ]
        ) (VAR_SIGNATURE (IDENTIFIER "var10") DOUBLE)
            ==
            SYMBOL_CONTEXT
                []
                [
                    VAR_FRAME
                        [
                            VAR_SIGNATURE (IDENTIFIER "var10") DOUBLE,
                            VAR_SIGNATURE (IDENTIFIER "var1") INT,
                            VAR_SIGNATURE (IDENTIFIER "var2") DOUBLE,
                            VAR_SIGNATURE (IDENTIFIER "var3") INT

                        ],
                    VAR_FRAME
                        [
                            VAR_SIGNATURE (IDENTIFIER "var4") DOUBLE,
                            VAR_SIGNATURE (IDENTIFIER "var5") DOUBLE

                        ],
                    VAR_FRAME [],
                    VAR_FRAME
                        [
                            VAR_SIGNATURE (IDENTIFIER "var6") DOUBLE,
                            VAR_SIGNATURE (IDENTIFIER "var7") INT,
                            VAR_SIGNATURE (IDENTIFIER "var8") INT,
                            VAR_SIGNATURE (IDENTIFIER "var9") INT

                        ]
                ]
