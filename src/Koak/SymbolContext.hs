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
                            ) where

import Koak.Parser          ( KDEFS(..)
                            , PROTOTYPE(..)
                            , PROTOTYPE_ARGS(..)
                            , VAR_SIGNATURE(..), DEFS
                            )

import Koak.Typing.Exception ( KoakTypingException(..) )

import Control.Exception (throw)

newtype VAR_FRAME  = VAR_FRAME [VAR_SIGNATURE]
    deriving (Show, Eq)

type VAR_FRAME_STACK = [VAR_FRAME]

data SYMBOL_CONTEXT = SYMBOL_CONTEXT [KDEFS] VAR_FRAME_STACK
    deriving (Show, Eq)

sContextPushNewFrame :: SYMBOL_CONTEXT -> SYMBOL_CONTEXT
sContextPushNewFrame (SYMBOL_CONTEXT kdefs stack) = SYMBOL_CONTEXT kdefs (VAR_FRAME [] :stack)

sContextPushVar :: SYMBOL_CONTEXT -> VAR_SIGNATURE -> SYMBOL_CONTEXT
sContextPushVar context@(SYMBOL_CONTEXT _     []        ) _        = context
sContextPushVar (SYMBOL_CONTEXT         kdefs (frame:xs)) new_var = SYMBOL_CONTEXT kdefs (varFramePushVar frame new_var:xs)

checkPushVarShadowsDefinition :: SYMBOL_CONTEXT -> VAR_SIGNATURE -> SYMBOL_CONTEXT
checkPushVarShadowsDefinition context@(SYMBOL_CONTEXT []     _        ) _ = context
checkPushVarShadowsDefinition context@(SYMBOL_CONTEXT (x:xs) _        ) _ = context

checkPushVarShadowsDefinition' :: KDEFS -> VAR_SIGNATURE -> KDEFS
checkPushVarShadowsDefinition'  kdef@(KDEFS_DEFS _) _ = kdef
checkPushVarShadowsDefinition'  kdef@(KDEFS_EXPR _) _ = kdef

-- checkPushVarShadowsDefinition'' :: DEFS -> VAR_SIGNATURE -> DEFS

varFramePushVar :: VAR_FRAME -> VAR_SIGNATURE -> VAR_FRAME
varFramePushVar (VAR_FRAME vars) var = VAR_FRAME $ varFramePushVar' vars var

varFramePushVar' :: [VAR_SIGNATURE] -> VAR_SIGNATURE -> [VAR_SIGNATURE]
varFramePushVar' [] var = [var]
varFramePushVar' vars@(x@(VAR_SIGNATURE xi xt):xs) var@(VAR_SIGNATURE vi vt)
    | vi == xi  = throw $ ShadowedVariableByVariable x var
    | vi <= xi  = var : vars
    | otherwise = x   : varFramePushVar' xs var

sContextPushVars :: SYMBOL_CONTEXT -> [VAR_SIGNATURE] -> SYMBOL_CONTEXT
sContextPushVars = foldl sContextPushVar

sContextPushPrototype :: SYMBOL_CONTEXT -> PROTOTYPE -> SYMBOL_CONTEXT
sContextPushPrototype context (PROTOTYPE_UNARY  _ _ _ prototype_args) = sContextPushPrototype' context prototype_args
sContextPushPrototype context (PROTOTYPE_BINARY _ _ _ prototype_args) = sContextPushPrototype' context prototype_args
sContextPushPrototype context (PROTOTYPE        _     prototype_args) = sContextPushPrototype' context prototype_args

sContextPushPrototype' :: SYMBOL_CONTEXT -> PROTOTYPE_ARGS -> SYMBOL_CONTEXT
sContextPushPrototype' context (PROTOTYPE_ARGS vars _) = sContextPushVars context (map prototypeIdToVarSignature vars)

prototypeIdToVarSignature :: VAR_SIGNATURE -> VAR_SIGNATURE
prototypeIdToVarSignature (VAR_SIGNATURE var_name var_type) = VAR_SIGNATURE var_name var_type
