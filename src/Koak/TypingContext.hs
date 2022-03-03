--
-- EPITECH PROJECT, 2022
-- koak
-- File description:
-- TypingContext
--

module Koak.TypingContext           ( CONTEXT
                                    , getDefaultContext
                                    ) where

import Koak.Parser                  ( KDEFS(..)
                                    , PROTOTYPE(..)
                                    , PROTOTYPE_ARGS(..)
                                    , PROTOTYPE_ID(..)
                                    , DEFS (..)
                                    , IDENTIFIER(..),
                                    )

import Koak.Typing.Exception        ( KoakTypingException(..) )

import Control.Exception            ( throw )

import Data.HashMap.Strict  as HM   ( HashMap
                                    , empty
                                    , insert
                                    )

data BASE_TYPE  = INT
                | DOUBLE
                | BOOL
                | NIL

data FUNCTION_TYPING    = FUNCTION_TYPING [BASE_TYPE] BASE_TYPE

data TYPINGS    = PRIMITIVE_FUNCTION [FUNCTION_TYPING]
                | FUNCTION           FUNCTION_TYPING
                | VAR                BASE_TYPE

type CONTEXT = HashMap IDENTIFIER TYPINGS

-- evaluateFunctionCall :: CONTEXT -> (CONTEXT, RESULT)
-- evaluateFunctionCall c = let (_, r) = evaluateOther c in (c, r)


-- evaluateOther :: CONTEXT -> (CONTEXT, RESULT)
-- evaluateOther c = evaluateOther c

getDefaultContext :: CONTEXT
getDefaultContext = HM.insert
                        (IDENTIFIER "+")
                        (PRIMITIVE_FUNCTION [FUNCTION_TYPING [INT, INT] INT, FUNCTION_TYPING [DOUBLE, DOUBLE] DOUBLE, FUNCTION_TYPING [DOUBLE, INT] DOUBLE, FUNCTION_TYPING [INT, DOUBLE] DOUBLE])
                        HM.empty
                    -- 
                    -- 

-- def unary --- 200 (int) : int
-- def toto(double) : int

-- foo()

-- --- 3 * 1.0 // -> double
-- toto(4.0) + 1 * 2
-- 1 * toto(4.0) + 2

-- loadKdefToContext :: CONTEXT -> KDEFS -> CONTEXT

-- sContextPushNewFrame :: SYMBOL_CONTEXT -> SYMBOL_CONTEXT
-- sContextPushNewFrame (SYMBOL_CONTEXT kdefs stack) = SYMBOL_CONTEXT kdefs (VAR_FRAME [] :stack)

-- sContextPushVar :: SYMBOL_CONTEXT -> VAR_SIGNATURE -> SYMBOL_CONTEXT
-- sContextPushVar context@(SYMBOL_CONTEXT _     []        ) _       = context
-- sContextPushVar (SYMBOL_CONTEXT         kdefs (frame:xs)) new_var = SYMBOL_CONTEXT kdefs (varFramePushVar frame new_var:xs)

-- checkPushVarShadowsDefinition :: SYMBOL_CONTEXT -> VAR_SIGNATURE -> SYMBOL_CONTEXT
-- checkPushVarShadowsDefinition context@(SYMBOL_CONTEXT []     _        ) _ = context
-- checkPushVarShadowsDefinition context@(SYMBOL_CONTEXT (x:xs) _        ) _ = context

-- checkPushVarShadowsDefinition' :: KDEFS -> VAR_SIGNATURE -> KDEFS
-- checkPushVarShadowsDefinition'  (KDEFS_DEFS (DEFS prototype e)) _  var = KDEFS_DEFS (DEFS prototype (checkPushVarShadowsDefinition'' def var) e)
-- checkPushVarShadowsDefinition'  kdef@(KDEFS_EXPR _)             _      = kdef

-- checkPushVarShadowsDefinition'' :: PROTOTYPE -> VAR_SIGNATURE -> PROTOTYPE
-- checkPushVarShadowsDefinition'' p@(PROTOTYPE_UNARY  _ _ identifier _) var = checkPushVarShadowsDefinition''' p identifier var
-- checkPushVarShadowsDefinition'' p@(PROTOTYPE_BINARY _ _ identifier _) var = checkPushVarShadowsDefinition''' p identifier var
-- checkPushVarShadowsDefinition'' p@(PROTOTYPE identifier _)            var = checkPushVarShadowsDefinition''' p identifier var 

-- checkPushVarShadowsDefinition''' :: PROTOTYPE -> IDENTIFIER -> VAR_SIGNATURE -> PROTOTYPE
-- checkPushVarShadowsDefinition''' p identifier v@(VAR_SIGNATURE vi _)
--     | identifier == vi = throw $ ShadowedDefinitionByVariable p v
--     | otherwise        = p

-- varFramePushVar :: VAR_FRAME -> VAR_SIGNATURE -> VAR_FRAME
-- varFramePushVar (VAR_FRAME vars) var = VAR_FRAME $ varFramePushVar' vars var

-- varFramePushVar' :: [VAR_SIGNATURE] -> VAR_SIGNATURE -> [VAR_SIGNATURE]
-- varFramePushVar' [] var = [var]
-- varFramePushVar' vars@(x@(VAR_SIGNATURE xi xt):xs) var@(VAR_SIGNATURE vi vt)
--     | vi == xi  = throw $ ShadowedVariableByVariable x var
--     | vi <= xi  = var : vars
--     | otherwise = x   : varFramePushVar' xs var

-- sContextPushVars :: SYMBOL_CONTEXT -> [VAR_SIGNATURE] -> SYMBOL_CONTEXT
-- sContextPushVars = foldl sContextPushVar

-- sContextPushPrototype :: SYMBOL_CONTEXT -> PROTOTYPE -> SYMBOL_CONTEXT
-- sContextPushPrototype context (PROTOTYPE_UNARY  _ _ _ prototype_args) = sContextPushPrototype' context prototype_args
-- sContextPushPrototype context (PROTOTYPE_BINARY _ _ _ prototype_args) = sContextPushPrototype' context prototype_args
-- sContextPushPrototype context (PROTOTYPE        _     prototype_args) = sContextPushPrototype' context prototype_args

-- sContextPushPrototype' :: SYMBOL_CONTEXT -> PROTOTYPE_ARGS -> SYMBOL_CONTEXT
-- sContextPushPrototype' context (PROTOTYPE_ARGS vars _) = sContextPushVars context (map prototypeIdToVarSignature vars)
