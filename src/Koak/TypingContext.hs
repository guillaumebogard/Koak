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
                                    , CONTEXT_TYPE(..)
                                    , getDefaultKContext
                                    , getEmptyKContext
                                    , kContextPushDef
                                    , kContextPushVar
                                    , kContextFind
                                    , globalContextFind
                                    , defContextFind
                                    , localContextFind
                                    , kContextEnterLocalContext
                                    , kContextEnterFunctionCall
                                    ) where


import qualified Koak.Parser as KP  ( KDEFS(..)
                                    , PROTOTYPE(..)
                                    , PROTOTYPE_ARGS(..)
                                    , PROTOTYPE_ID(..)
                                    , DEFS (..)
                                    , IDENTIFIER(..)
                                    , TYPE(..)
                                    , PRECEDENCE(..)
                                    , VAR_ASSIGNMENT(..)
                                    , BIN_OP(..)
                                    )

import Koak.Typing.Exception        ( KoakTypingException(..) )

import Control.Exception            ( throw )

import Data.HashMap.Strict  as HM   ( HashMap
                                    , fromList
                                    , empty
                                    , member
                                    , insert
                                    , lookup
                                    )

import Data.Maybe                   ( isNothing
                                    , isJust
                                    )

class Identify a where
    toIdentifier :: a -> KP.IDENTIFIER

data BASE_TYPE  = INT
                | DOUBLE
                | BOOLEAN
                | NIL
    deriving (Eq, Show)

data FUNCTION_TYPING    = UNARY_FUNCTION_TYPING BASE_TYPE BASE_TYPE
                        | BINARY_FUNCTION_TYPING KP.PRECEDENCE BASE_TYPE BASE_TYPE BASE_TYPE
                        | FUNCTION_TYPING [BASE_TYPE] BASE_TYPE
    deriving (Eq, Show)

data TYPE_SIGNATURE = PRIMITIVE_FUNCTION [FUNCTION_TYPING]
                    | FUNCTION           FUNCTION_TYPING
                    | VAR                BASE_TYPE
    deriving (Eq, Show)

class Type a where
    toTypeSignature :: a -> TYPE_SIGNATURE

type CONTEXT            = HashMap KP.IDENTIFIER TYPE_SIGNATURE

newtype GLOBAL_CONTEXT  = GLOBAL_CONTEXT CONTEXT
    deriving (Eq, Show)

newtype DEF_CONTEXT     = DEF_CONTEXT    CONTEXT
    deriving (Eq, Show)

newtype LOCAL_CONTEXT   = LOCAL_CONTEXT  CONTEXT
    deriving (Eq, Show)

data CONTEXT_TYPE       = CONTEXT_TYPE_GLOBAL | CONTEXT_TYPE_DEF | CONTEXT_TYPE_LOCAL

data KCONTEXT           = KCONTEXT GLOBAL_CONTEXT DEF_CONTEXT (Maybe LOCAL_CONTEXT)
    deriving (Eq, Show)

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
                        Nothing

getEmptyKContext :: KCONTEXT
getEmptyKContext = KCONTEXT (GLOBAL_CONTEXT HM.empty) (DEF_CONTEXT HM.empty) Nothing

kContextPushDef :: KP.DEFS -> KCONTEXT -> KCONTEXT
kContextPushDef def@(KP.DEFS p _) = kContextPushDef' p (toIdentifier def) (toTypeSignature def)

kContextPushDef' :: KP.PROTOTYPE  -> KP.IDENTIFIER -> TYPE_SIGNATURE -> KCONTEXT -> KCONTEXT
kContextPushDef' p i ts (KCONTEXT (GLOBAL_CONTEXT gc) (DEF_CONTEXT dc) lc)
    | HM.member i gc = throw $ ShadowedVariableByDefinition   i p
    | HM.member i dc = throw $ ShadowedDefinitionByDefinition i p
    | otherwise      = KCONTEXT (GLOBAL_CONTEXT gc) (DEF_CONTEXT $ contextPushItem i ts dc) lc

kContextEnterFunctionCall :: KP.PROTOTYPE -> KCONTEXT -> KCONTEXT
kContextEnterFunctionCall (KP.PROTOTYPE_UNARY  _ _ _ (KP.PROTOTYPE_ARGS args _)) k = kContextEnterFunctionCall' args $ kContextEnterLocalContext k
kContextEnterFunctionCall (KP.PROTOTYPE_BINARY _ _ _ (KP.PROTOTYPE_ARGS args _)) k = kContextEnterFunctionCall' args $ kContextEnterLocalContext k
kContextEnterFunctionCall (KP.PROTOTYPE _            (KP.PROTOTYPE_ARGS args _)) k = kContextEnterFunctionCall' args $ kContextEnterLocalContext k

kContextEnterFunctionCall' :: [KP.PROTOTYPE_ID] -> KCONTEXT -> KCONTEXT
kContextEnterFunctionCall' args k = foldr (kContextPushVar . prototypeIdToVarAssignment) k args

kContextEnterLocalContext :: KCONTEXT -> KCONTEXT
kContextEnterLocalContext (KCONTEXT (GLOBAL_CONTEXT gc) (DEF_CONTEXT dc) _) = KCONTEXT (GLOBAL_CONTEXT gc) (DEF_CONTEXT dc) (Just $ LOCAL_CONTEXT HM.empty)

kContextPushVar :: KP.VAR_ASSIGNMENT -> KCONTEXT -> KCONTEXT
kContextPushVar v@(KP.VAR_ASSIGNMENT i t) (KCONTEXT (GLOBAL_CONTEXT gc) (DEF_CONTEXT dc) Nothing)
    | HM.member i dc    = throw $ ShadowedDefinitionByVariable i v
    | HM.member i gc    = throw $ ShadowedVariableByVariable   i v
    | otherwise         = KCONTEXT (GLOBAL_CONTEXT $ contextPushItem i (toTypeSignature v) gc) (DEF_CONTEXT dc) Nothing
kContextPushVar v@(KP.VAR_ASSIGNMENT i t) (KCONTEXT (GLOBAL_CONTEXT gc) (DEF_CONTEXT dc) (Just (LOCAL_CONTEXT lc)))
    | HM.member i dc    = throw $ ShadowedDefinitionByVariable i v
    -- | HM.member i gc    = throw $ ShadowedVariableByVariable   v -- Ignore global vars
    | HM.member i lc    = throw $ ShadowedVariableByVariable   i v
    | otherwise         = KCONTEXT (GLOBAL_CONTEXT gc) (DEF_CONTEXT dc) (Just $ LOCAL_CONTEXT $ contextPushItem i (toTypeSignature v) lc)

kContextFind :: KCONTEXT -> KP.IDENTIFIER -> Maybe (TYPE_SIGNATURE, CONTEXT_TYPE)
kContextFind k@(KCONTEXT _ _ Nothing)   i = kContextFind' k i
kContextFind k@(KCONTEXT _ _ (Just lc)) i = let found = localContextFind lc i
    in case found of
        (Just found') -> Just (found', CONTEXT_TYPE_LOCAL)
        Nothing       -> kContextFind' k i

kContextFind' :: KCONTEXT -> KP.IDENTIFIER -> Maybe (TYPE_SIGNATURE, CONTEXT_TYPE)
kContextFind' k@(KCONTEXT gc _ _) i = let found = globalContextFind gc i
    in case found of
        (Just found') -> Just (found', CONTEXT_TYPE_GLOBAL)
        Nothing       -> kContextFind' k i

kContextFind'' :: KCONTEXT -> KP.IDENTIFIER -> Maybe (TYPE_SIGNATURE, CONTEXT_TYPE)
kContextFind'' k@(KCONTEXT _ dc _) i = let found = defContextFind dc i
    in case found of
        (Just found') -> Just (found', CONTEXT_TYPE_DEF)
        Nothing       -> kContextFind' k i

globalContextFind :: GLOBAL_CONTEXT -> KP.IDENTIFIER -> Maybe TYPE_SIGNATURE
globalContextFind (GLOBAL_CONTEXT c) i = HM.lookup i c

defContextFind :: DEF_CONTEXT -> KP.IDENTIFIER -> Maybe TYPE_SIGNATURE
defContextFind (DEF_CONTEXT c) i = HM.lookup i c

localContextFind :: LOCAL_CONTEXT -> KP.IDENTIFIER -> Maybe TYPE_SIGNATURE
localContextFind (LOCAL_CONTEXT c) i = HM.lookup i c

contextPushItem :: KP.IDENTIFIER -> TYPE_SIGNATURE -> CONTEXT -> CONTEXT
contextPushItem = insert

instance Identify KP.DEFS where
    toIdentifier (KP.DEFS (KP.PROTOTYPE_UNARY  _  _ i _) _) = i
    toIdentifier (KP.DEFS (KP.PROTOTYPE_BINARY _  _ i _) _) = i
    toIdentifier (KP.DEFS (KP.PROTOTYPE i      _)        _) = i

instance Type KP.DEFS where
    toTypeSignature (KP.DEFS   (KP.PROTOTYPE_UNARY  _ _   _ (KP.PROTOTYPE_ARGS [x]        return_type)) _) = FUNCTION $ UNARY_FUNCTION_TYPING (prototypeIdToBaseType x) (typeToBaseType return_type)
    toTypeSignature (KP.DEFS p@(KP.PROTOTYPE_UNARY  _ _   _ (KP.PROTOTYPE_ARGS args       _          )) _) = throw    $ UnaryFunctionInvalidArgumentNumber p  (length args)
    toTypeSignature (KP.DEFS   (KP.PROTOTYPE_BINARY _ pre _ (KP.PROTOTYPE_ARGS [x,y]      return_type)) _) = FUNCTION $ BINARY_FUNCTION_TYPING pre (prototypeIdToBaseType x) (prototypeIdToBaseType y) (typeToBaseType return_type)
    toTypeSignature (KP.DEFS p@(KP.PROTOTYPE_BINARY _ _   _ (KP.PROTOTYPE_ARGS args       _          )) _) = throw    $ BinaryFunctionInvalidArgumentNumber p (length args)
    toTypeSignature (KP.DEFS   (KP.PROTOTYPE _              (KP.PROTOTYPE_ARGS args       return_type)) _) = FUNCTION $ FUNCTION_TYPING (map prototypeIdToBaseType args) (typeToBaseType return_type)

instance Identify KP.VAR_ASSIGNMENT where
    toIdentifier (KP.VAR_ASSIGNMENT i _) = i

instance Type KP.VAR_ASSIGNMENT where
    toTypeSignature (KP.VAR_ASSIGNMENT _ var_type) = VAR $ typeToBaseType var_type

prototypeIdToBaseType :: KP.PROTOTYPE_ID -> BASE_TYPE
prototypeIdToBaseType (KP.PROTOTYPE_ID _ t) = typeToBaseType t

prototypeIdToVarAssignment :: KP.PROTOTYPE_ID -> KP.VAR_ASSIGNMENT
prototypeIdToVarAssignment (KP.PROTOTYPE_ID i t) = KP.VAR_ASSIGNMENT i t

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
