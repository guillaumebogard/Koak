--
-- EPITECH PROJECT, 2022
-- koak
-- File description:
-- TypingContext
--

module Koak.TypingContext           ( KCONTEXT(..)
                                    , GLOBAL_CONTEXT(..)
                                    , DEF_CONTEXT(..)
                                    , LOCAL_CONTEXT(..)
                                    , BASE_TYPE(..)
                                    , FUNCTION_TYPING(..)
                                    , TYPE_SIGNATURE(..)
                                    , getDefaultKContext
                                    , getEmptyKContext
                                    , kContextPushDef
                                    ) where


import qualified Koak.Parser as KP  ( KDEFS(..)
                                    , PROTOTYPE(..)
                                    , PROTOTYPE_ARGS(..)
                                    , PROTOTYPE_ID(..)
                                    , DEFS (..)
                                    , IDENTIFIER(..)
                                    , TYPE(..)
                                    )
                    

import Koak.Typing.Exception        ( KoakTypingException(..) )

import Control.Exception            ( throw )

import Data.HashMap.Strict  as HM   ( HashMap
                                    , fromList
                                    , empty
                                    , member
                                    , insert
                                    )

class Identify a where
    toIdentifier :: a -> KP.IDENTIFIER

data BASE_TYPE  = INT
                | DOUBLE
                | BOOLEAN
                | NIL
    deriving (Eq, Show)

data FUNCTION_TYPING    = UNARY_FUNCTION_TYPING  BASE_TYPE BASE_TYPE
                        | BINARY_FUNCTION_TYPING BASE_TYPE BASE_TYPE BASE_TYPE
                        | FUNCTION_TYPING [BASE_TYPE] BASE_TYPE
    deriving (Eq, Show)



data TYPE_SIGNATURE = PRIMITIVE_FUNCTION [FUNCTION_TYPING]
                    | FUNCTION           FUNCTION_TYPING
                    | VAR                BASE_TYPE
    deriving (Eq, Show)

class Type a where
    toTypeSignature :: a -> TYPE_SIGNATURE

type CONTEXT    = HashMap KP.IDENTIFIER TYPE_SIGNATURE

newtype GLOBAL_CONTEXT  = GLOBAL_CONTEXT CONTEXT
    deriving (Eq, Show)

newtype DEF_CONTEXT     = DEF_CONTEXT    CONTEXT
    deriving (Eq, Show)

newtype LOCAL_CONTEXT   = LOCAL_CONTEXT  CONTEXT
    deriving (Eq, Show)

data KCONTEXT   = KCONTEXT GLOBAL_CONTEXT DEF_CONTEXT LOCAL_CONTEXT
    deriving (Eq, Show)

-- evaluateFunctionCall :: CONTEXT -> (CONTEXT, RESULT)
-- evaluateFunctionCall c = let (_, r) = evaluateOther c in (c, r)


-- evaluateOther :: CONTEXT -> (CONTEXT, RESULT)
-- evaluateOther c = evaluateOther c

getDefaultKContext :: KCONTEXT
getDefaultKContext = KCONTEXT
                        (
                            GLOBAL_CONTEXT
                            HM.empty
                        )
                        (
                            DEF_CONTEXT $
                                HM.fromList [
                                    (
                                        KP.IDENTIFIER "Solène lefeu",
                                        FUNCTION $ FUNCTION_TYPING [] NIL
                                    ),
                                    (
                                        KP.IDENTIFIER "+",
                                        PRIMITIVE_FUNCTION [FUNCTION_TYPING [INT, INT] INT, FUNCTION_TYPING [DOUBLE, DOUBLE] DOUBLE, FUNCTION_TYPING [DOUBLE, INT] DOUBLE, FUNCTION_TYPING [INT, DOUBLE] DOUBLE]
                                    ),
                                    (
                                        KP.IDENTIFIER "-",
                                        PRIMITIVE_FUNCTION [FUNCTION_TYPING [INT, INT] INT, FUNCTION_TYPING [DOUBLE, DOUBLE] DOUBLE, FUNCTION_TYPING [DOUBLE, INT] DOUBLE, FUNCTION_TYPING [INT, DOUBLE] DOUBLE]
                                    ),
                                    (
                                        KP.IDENTIFIER "*",
                                        PRIMITIVE_FUNCTION [FUNCTION_TYPING [INT, INT] INT, FUNCTION_TYPING [DOUBLE, DOUBLE] DOUBLE, FUNCTION_TYPING [DOUBLE, INT] DOUBLE, FUNCTION_TYPING [INT, DOUBLE] DOUBLE]
                                    ),
                                    (
                                        KP.IDENTIFIER "/",
                                        PRIMITIVE_FUNCTION [FUNCTION_TYPING [INT, INT] INT, FUNCTION_TYPING [DOUBLE, DOUBLE] DOUBLE, FUNCTION_TYPING [DOUBLE, INT] DOUBLE, FUNCTION_TYPING [INT, DOUBLE] DOUBLE]
                                    ),
                                    (
                                        KP.IDENTIFIER "%",
                                        PRIMITIVE_FUNCTION [FUNCTION_TYPING [INT, INT] INT, FUNCTION_TYPING [DOUBLE, DOUBLE] DOUBLE, FUNCTION_TYPING [DOUBLE, INT] DOUBLE, FUNCTION_TYPING [INT, DOUBLE] DOUBLE]
                                    )
                                ]
                        )
                        (
                            LOCAL_CONTEXT
                                HM.empty
                        )

getEmptyKContext :: KCONTEXT
getEmptyKContext = KCONTEXT (GLOBAL_CONTEXT HM.empty) (DEF_CONTEXT HM.empty) (LOCAL_CONTEXT HM.empty)

kContextPushDef :: KCONTEXT -> KP.DEFS -> KCONTEXT
kContextPushDef kcontext def@(KP.DEFS p _) = kContextPushDef' kcontext p (toIdentifier def) (toTypeSignature def)

kContextPushDef' :: KCONTEXT -> KP.PROTOTYPE  -> KP.IDENTIFIER -> TYPE_SIGNATURE -> KCONTEXT
kContextPushDef' (KCONTEXT (GLOBAL_CONTEXT gc) (DEF_CONTEXT dc) lc) p i ts
    | HM.member i gc = throw $ ShadowedDefinitionByVariable   p
    | HM.member i dc = throw $ ShadowedDefinitionByDefinition p
    | otherwise      = KCONTEXT (GLOBAL_CONTEXT gc) (DEF_CONTEXT $ contextPushItem dc i ts) lc

contextPushItem :: CONTEXT -> KP.IDENTIFIER -> TYPE_SIGNATURE -> CONTEXT
contextPushItem c i ts = insert i ts c

instance Identify KP.DEFS where
    toIdentifier (KP.DEFS (KP.PROTOTYPE_UNARY  _  _ i _) _) = i
    toIdentifier (KP.DEFS (KP.PROTOTYPE_BINARY _  _ i _) _) = i
    toIdentifier (KP.DEFS (KP.PROTOTYPE i      _)        _) = i

instance Type KP.DEFS where
    toTypeSignature (KP.DEFS   (KP.PROTOTYPE_UNARY  _  _ _ (KP.PROTOTYPE_ARGS [x]        return_type)) _) = FUNCTION $ UNARY_FUNCTION_TYPING (prototypeIdToBaseType x) (typeToBaseType return_type)
    toTypeSignature (KP.DEFS p@(KP.PROTOTYPE_UNARY  _  _ _ (KP.PROTOTYPE_ARGS args       _          )) _) = throw $ UnaryFunctionInvalidArgumentNumber p (length args) 
    toTypeSignature (KP.DEFS   (KP.PROTOTYPE_BINARY _  _ _ (KP.PROTOTYPE_ARGS [x,y]      return_type)) _) = FUNCTION $ BINARY_FUNCTION_TYPING (prototypeIdToBaseType x) (prototypeIdToBaseType y) (typeToBaseType return_type)
    toTypeSignature (KP.DEFS p@(KP.PROTOTYPE_BINARY _  _ _ (KP.PROTOTYPE_ARGS args       _          )) _) = throw $ BinaryFunctionInvalidArgumentNumber p (length args)
    toTypeSignature (KP.DEFS   (KP.PROTOTYPE _             (KP.PROTOTYPE_ARGS args       return_type)) _) = FUNCTION $ FUNCTION_TYPING (map prototypeIdToBaseType args) (typeToBaseType return_type)

prototypeIdToBaseType :: KP.PROTOTYPE_ID -> BASE_TYPE
prototypeIdToBaseType (KP.PROTOTYPE_ID _ t) = typeToBaseType t

typeToBaseType :: KP.TYPE -> BASE_TYPE
typeToBaseType KP.INT       = INT
typeToBaseType KP.DOUBLE    = DOUBLE
typeToBaseType KP.BOOLEAN   = BOOLEAN
typeToBaseType KP.VOID      = NIL

baseTypeToType :: BASE_TYPE -> KP.TYPE
baseTypeToType INT      = KP.INT
baseTypeToType DOUBLE   = KP.DOUBLE
baseTypeToType BOOLEAN  = KP.BOOLEAN
baseTypeToType NIL      = KP.VOID


    -- hashWithSalt salt (IDENTIFIER string)   = salt `hashWithSalt` string

-- data DEFS           = DEFS PROTOTYPE EXPRESSIONS
--     deriving (Eq, Show)

-- newtype PRECEDENCE  = PRECEDENCE Int
--     deriving (Eq, Show)

-- data PROTOTYPE      = PROTOTYPE_UNARY  UN_OP  PRECEDENCE IDENTIFIER PROTOTYPE_ARGS
--                     | PROTOTYPE_BINARY BIN_OP PRECEDENCE IDENTIFIER PROTOTYPE_ARGS
--                     | PROTOTYPE IDENTIFIER PROTOTYPE_ARGS
--     deriving (Eq, Show)

-- data PROTOTYPE_ARGS = PROTOTYPE_ARGS [PROTOTYPE_ID] TYPE
--     deriving (Eq, Show)

-- data PROTOTYPE_ID   = PROTOTYPE_ID IDENTIFIER TYPE
--     deriving (Eq, Show)


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
