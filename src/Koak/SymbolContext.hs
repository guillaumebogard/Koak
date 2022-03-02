--
-- EPITECH PROJECT, 2022
-- koak
-- File description:
-- sContext
--

module Koak.SymbolContext   ( sContextPushNewFrame
                            , sContextPushVar
                            , sContextPushVars
                            , sContextPushPrototype
                            , SYMBOL_CONTEXT(..)
                            , VAR_FRAME_STACK
                            , VAR_FRAME(..)
                            , VAR_SIGNATURE(..)
                            ) where

import Koak.Parser          ( KDEFS(..)
                            , PROTOTYPE(..)
                            , PROTOTYPE_ARGS(..)
                            , PROTOTYPE_ID(..)
                            , IDENTIFIER(..)
                            , TYPE(..)

                            )

data VAR_SIGNATURE  = VAR_SIGNATURE IDENTIFIER TYPE
    deriving (Show, Eq)

newtype VAR_FRAME  = VAR_FRAME [VAR_SIGNATURE]
    deriving (Show, Eq)

type VAR_FRAME_STACK = [VAR_FRAME]

data SYMBOL_CONTEXT = SYMBOL_CONTEXT [KDEFS] VAR_FRAME_STACK
    deriving (Show, Eq)

sContextPushNewFrame :: SYMBOL_CONTEXT -> SYMBOL_CONTEXT
sContextPushNewFrame (SYMBOL_CONTEXT kdefs stack) = SYMBOL_CONTEXT kdefs (VAR_FRAME [] :stack)

sContextPushVar :: SYMBOL_CONTEXT -> VAR_SIGNATURE -> SYMBOL_CONTEXT
sContextPushVar context@(SYMBOL_CONTEXT _     []                    ) _        = context
sContextPushVar (SYMBOL_CONTEXT         kdefs ((VAR_FRAME signs):xs)) new_sign = SYMBOL_CONTEXT kdefs (VAR_FRAME ( new_sign : signs) : xs)

sContextPushVars :: SYMBOL_CONTEXT -> [VAR_SIGNATURE] -> SYMBOL_CONTEXT
sContextPushVars context@(SYMBOL_CONTEXT _     []                    ) _         = context
sContextPushVars (SYMBOL_CONTEXT         kdefs ((VAR_FRAME signs):xs)) new_signs = SYMBOL_CONTEXT kdefs (VAR_FRAME ( new_signs ++ signs) : xs)

sContextPushPrototype :: SYMBOL_CONTEXT -> PROTOTYPE -> SYMBOL_CONTEXT
sContextPushPrototype context (PROTOTYPE_UNARY  _ _ _ prototype_args) = sContextPushPrototype' context prototype_args
sContextPushPrototype context (PROTOTYPE_BINARY _ _ _ prototype_args) = sContextPushPrototype' context prototype_args
sContextPushPrototype context (PROTOTYPE        _     prototype_args) = sContextPushPrototype' context prototype_args

sContextPushPrototype' :: SYMBOL_CONTEXT -> PROTOTYPE_ARGS -> SYMBOL_CONTEXT
sContextPushPrototype' context (PROTOTYPE_ARGS prototype_ids _) = sContextPushVars context (map prototypeIdToVarSignature prototype_ids)

prototypeIdToVarSignature :: PROTOTYPE_ID -> VAR_SIGNATURE
prototypeIdToVarSignature (PROTOTYPE_ID var_name var_type) = VAR_SIGNATURE var_name var_type
