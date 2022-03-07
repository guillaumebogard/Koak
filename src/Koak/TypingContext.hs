--
-- EPITECH PROJECT, 2022
-- koak
-- File description:
-- TypingContext
--

module Koak.TypingContext           ( Kcontext(..)
                                    , GlobalContext(..)
                                    , DefContext(..)
                                    , LocalContext(..)
                                    , BaseType(..)
                                    , FunctionTyping(..)
                                    , TypeSignature(..)
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
                                    , isUnaryFunctionParamMatchingFunction
                                    , isBinaryFunctionParamMatchingFunction
                                    , isFunctionParamMatchingFunction
                                    , getFunctionReturnType
                                    , prototypeIdToBaseType
                                    , prototypeIdToVarAssignment
                                    , typeToBaseType
                                    , baseTypeToType
                                    , toIdentifier
                                    , toTypeSignature
                                    ) where


import qualified Koak.Parser as KP

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
    toIdentifier :: a -> KP.Identifier

data BaseType   = Int
                | Double
                | Boolean
                | Nil
    deriving (Eq, Show)

data FunctionTyping     = UnaryFunctionTyping                 BaseType           BaseType
                        | BinaryFunctionTyping KP.Precedence  BaseType  BaseType BaseType
                        | FunctionTyping                     [BaseType]          BaseType
    deriving (Eq, Show)

data TypeSignature  = PrimitiveFunction [FunctionTyping]
                    | Function           FunctionTyping
                    | Var                BaseType
    deriving (Eq, Show)

class Type a where
    toTypeSignature :: a -> TypeSignature

type Context            = HashMap KP.Identifier TypeSignature

newtype GlobalContext   = GlobalContext Context
    deriving (Eq, Show)

newtype DefContext      = DefContext    Context
    deriving (Eq, Show)

newtype LocalContext    = LocalContext  Context
    deriving (Eq, Show)

data Kcontext           = Kcontext GlobalContext DefContext (Maybe LocalContext)
    deriving (Eq, Show)

getDefaultKContext :: Kcontext
getDefaultKContext = Kcontext
                        (
                            GlobalContext
                            HM.empty
                        )
                        (
                            DefContext $
                                HM.fromList [
                                    (
                                        KP.Identifier "+",
                                        PrimitiveFunction [FunctionTyping [Int, Int] Int, FunctionTyping [Double, Double] Double, FunctionTyping [Double, Int] Double, FunctionTyping [Int, Double] Double]
                                    ),
                                    (
                                        KP.Identifier "-",
                                        PrimitiveFunction [FunctionTyping [Int, Int] Int, FunctionTyping [Double, Double] Double, FunctionTyping [Double, Int] Double, FunctionTyping [Int, Double] Double]
                                    ),
                                    (
                                        KP.Identifier "*",
                                        PrimitiveFunction [FunctionTyping [Int, Int] Int, FunctionTyping [Double, Double] Double, FunctionTyping [Double, Int] Double, FunctionTyping [Int, Double] Double]
                                    ),
                                    (
                                        KP.Identifier "/",
                                        PrimitiveFunction [FunctionTyping [Int, Int] Int, FunctionTyping [Double, Double] Double, FunctionTyping [Double, Int] Double, FunctionTyping [Int, Double] Double]
                                    ),
                                    (
                                        KP.Identifier "%",
                                        PrimitiveFunction [FunctionTyping [Int, Int] Int, FunctionTyping [Double, Double] Double, FunctionTyping [Double, Int] Double, FunctionTyping [Int, Double] Double]
                                    )
                                ]
                        )
                        Nothing

getEmptyKContext :: Kcontext
getEmptyKContext = Kcontext (GlobalContext HM.empty) (DefContext HM.empty) Nothing

kContextPushDef :: KP.Defs -> Kcontext -> Kcontext
kContextPushDef def@(KP.Defs p _) = kContextPushDef' p (toIdentifier def) (toTypeSignature def)

kContextPushDef' :: KP.Prototype  -> KP.Identifier -> TypeSignature -> Kcontext -> Kcontext
kContextPushDef' p i ts (Kcontext (GlobalContext gc) (DefContext dc) lc)
    | HM.member i gc = throw $ ShadowedVariableByDefinition   i p
    | HM.member i dc = throw $ ShadowedDefinitionByDefinition i p
    | otherwise      = Kcontext (GlobalContext gc) (DefContext $ contextPushItem i ts dc) lc

kContextEnterFunctionCall :: KP.Prototype -> Kcontext -> Kcontext
kContextEnterFunctionCall (KP.PrototypeUnary    _     (KP.PrototypeArgs args _)) k = kContextEnterFunctionCall' args $ kContextEnterLocalContext k
kContextEnterFunctionCall (KP.PrototypeBinary   _ _   (KP.PrototypeArgs args _)) k = kContextEnterFunctionCall' args $ kContextEnterLocalContext k
kContextEnterFunctionCall (KP.PrototypeFunction _     (KP.PrototypeArgs args _)) k = kContextEnterFunctionCall' args $ kContextEnterLocalContext k

kContextEnterFunctionCall' :: [KP.PrototypeIdentifier] -> Kcontext -> Kcontext
kContextEnterFunctionCall' args k = foldr (kContextPushVar . prototypeIdToVarAssignment) k args

kContextEnterLocalContext :: Kcontext -> Kcontext
kContextEnterLocalContext (Kcontext (GlobalContext gc) (DefContext dc) _) = Kcontext (GlobalContext gc) (DefContext dc) (Just $ LocalContext HM.empty)

kContextPushVar :: KP.VarAssignment -> Kcontext -> Kcontext
kContextPushVar v@(KP.VarAssignment i t) (Kcontext (GlobalContext gc) (DefContext dc) Nothing)
    | HM.member i dc    = throw $ ShadowedDefinitionByVariable i v
    | HM.member i gc    = throw $ ShadowedVariableByVariable   i v
    | otherwise         = Kcontext (GlobalContext $ contextPushItem i (toTypeSignature v) gc) (DefContext dc) Nothing
kContextPushVar v@(KP.VarAssignment i t) (Kcontext (GlobalContext gc) (DefContext dc) (Just (LocalContext lc)))
    | HM.member i dc    = throw $ ShadowedDefinitionByVariable i v
    -- | HM.member i gc    = throw $ ShadowedVariableByVariable   v -- Ignore global vars
    | HM.member i lc    = throw $ ShadowedVariableByVariable   i v
    | otherwise         = Kcontext (GlobalContext gc) (DefContext dc) (Just $ LocalContext $ contextPushItem i (toTypeSignature v) lc)

kContextFind :: Kcontext -> KP.Identifier -> Maybe TypeSignature
kContextFind k@(Kcontext gc dc Nothing  ) i
    | let dc_res = defContextFind    dc i, isJust dc_res = dc_res
    | let gc_res = globalContextFind gc i, isJust gc_res = gc_res
    | otherwise                                          = Nothing
kContextFind k@(Kcontext gc dc (Just lc)) i
    | let lc_res = localContextFind  lc i, isJust lc_res = lc_res
    | let dc_res = defContextFind    dc i, isJust dc_res = dc_res
    | let gc_res = globalContextFind gc i, isJust gc_res = gc_res
    | otherwise                                          = Nothing

globalContextFind :: GlobalContext -> KP.Identifier -> Maybe TypeSignature
globalContextFind (GlobalContext c) i = HM.lookup i c

defContextFind :: DefContext -> KP.Identifier -> Maybe TypeSignature
defContextFind (DefContext c) i = HM.lookup i c

localContextFind :: LocalContext -> KP.Identifier -> Maybe TypeSignature
localContextFind (LocalContext c) i = HM.lookup i c

contextPushItem :: KP.Identifier -> TypeSignature -> Context -> Context
contextPushItem = insert

instance Identify KP.Defs where
    toIdentifier (KP.Defs (KP.PrototypeUnary      (KP.UnaryOp  i) _) _) = i
    toIdentifier (KP.Defs (KP.PrototypeBinary   _ (KP.BinaryOp i) _) _) = i
    toIdentifier (KP.Defs (KP.PrototypeFunction   i               _) _) = i

instance Type KP.Defs where
    toTypeSignature (KP.Defs   (KP.PrototypeUnary        _ (KP.PrototypeArgs [x]        return_type)) _) = Function $ UnaryFunctionTyping (prototypeIdToBaseType x) (typeToBaseType return_type)
    toTypeSignature (KP.Defs p@(KP.PrototypeUnary        _ (KP.PrototypeArgs args       _          )) _) = throw    $ UnaryFunctionInvalidArgumentNumber p  (length args)
    toTypeSignature (KP.Defs   (KP.PrototypeBinary   pre _ (KP.PrototypeArgs [x,y]      return_type)) _) = Function $ BinaryFunctionTyping pre (prototypeIdToBaseType x) (prototypeIdToBaseType y) (typeToBaseType return_type)
    toTypeSignature (KP.Defs p@(KP.PrototypeBinary   _   _ (KP.PrototypeArgs args       _          )) _) = throw    $ BinaryFunctionInvalidArgumentNumber p (length args)
    toTypeSignature (KP.Defs   (KP.PrototypeFunction _     (KP.PrototypeArgs args       return_type)) _) = Function $ FunctionTyping (map prototypeIdToBaseType args) (typeToBaseType return_type)

instance Identify KP.UnaryOp where
    toIdentifier (KP.UnaryOp i) = i

instance Identify KP.BinaryOp where
    toIdentifier (KP.BinaryOp i) = i

instance Identify KP.VarAssignment where
    toIdentifier (KP.VarAssignment i _) = i

instance Type KP.VarAssignment where
    toTypeSignature (KP.VarAssignment _ var_type) = Var $ typeToBaseType var_type

isUnaryFunctionParamMatchingFunction :: FunctionTyping -> BaseType -> Bool
isUnaryFunctionParamMatchingFunction (UnaryFunctionTyping arg1 return_type) arg1'
                                         = arg1 == arg1'
isUnaryFunctionParamMatchingFunction _ _ = False 

isBinaryFunctionParamMatchingFunction :: FunctionTyping -> BaseType -> BaseType -> Bool
isBinaryFunctionParamMatchingFunction (BinaryFunctionTyping _ arg1 arg2 return_type) arg1' arg2'
                                            = arg1 == arg1' && arg2 == arg2'
isBinaryFunctionParamMatchingFunction _ _ _ = False

isFunctionParamMatchingFunction :: FunctionTyping -> [BaseType] -> Bool
isFunctionParamMatchingFunction (FunctionTyping args return_type) args'
                                    = args == args'
isFunctionParamMatchingFunction _ _ = False

getFunctionReturnType :: FunctionTyping -> BaseType
getFunctionReturnType (UnaryFunctionTyping  _      return_type) = return_type 
getFunctionReturnType (BinaryFunctionTyping _ _ _  return_type) = return_type
getFunctionReturnType (FunctionTyping       _      return_type) = return_type

prototypeIdToBaseType :: KP.PrototypeIdentifier -> BaseType
prototypeIdToBaseType (KP.PrototypeIdentifier _ t) = typeToBaseType t

prototypeIdToVarAssignment :: KP.PrototypeIdentifier -> KP.VarAssignment
prototypeIdToVarAssignment (KP.PrototypeIdentifier i t) = KP.VarAssignment i t

typeToBaseType :: KP.Type -> BaseType
typeToBaseType KP.Int       = Int
typeToBaseType KP.Double    = Double
typeToBaseType KP.Boolean   = Boolean
typeToBaseType KP.Void      = Nil

baseTypeToType :: BaseType -> KP.Type
baseTypeToType Int      = KP.Int
baseTypeToType Double   = KP.Double
baseTypeToType Boolean  = KP.Boolean
baseTypeToType Nil      = KP.Void

-- sContextPushNewFrame :: SYMBOL_CONTEXT -> SYMBOL_CONTEXT
-- sContextPushNewFrame (SYMBOL_CONTEXT kdefs stack) = SYMBOL_CONTEXT kdefs (VAR_FRAME [] :stack)

-- sContextPushVar :: SYMBOL_CONTEXT -> VAR_SIGNATURE -> SYMBOL_CONTEXT
-- sContextPushVar context@(SYMBOL_CONTEXT _     []        ) _       = context
-- sContextPushVar (SYMBOL_CONTEXT         kdefs (frame:xs)) new_var = SYMBOL_CONTEXT kdefs (varFramePushVar frame new_var:xs)

-- checkPushVarShadowsDefinition :: SYMBOL_CONTEXT -> VAR_SIGNATURE -> SYMBOL_CONTEXT
-- checkPushVarShadowsDefinition context@(SYMBOL_CONTEXT []     _        ) _ = context
-- checkPushVarShadowsDefinition context@(SYMBOL_CONTEXT (x:xs) _        ) _ = context

-- checkPushVarShadowsDefinition' :: Kdefs -> VAR_SIGNATURE -> Kdefs
-- checkPushVarShadowsDefinition'  (Kdefs_Defs (Defs prototype e)) _  var = Kdefs_Defs (Defs prototype (checkPushVarShadowsDefinition'' def var) e)
-- checkPushVarShadowsDefinition'  kdef@(Kdefs_EXPR _)             _      = kdef

-- checkPushVarShadowsDefinition'' :: Prototype -> VAR_SIGNATURE -> Prototype
-- checkPushVarShadowsDefinition'' p@(Prototype_UNARY  _ _ identifier _) var = checkPushVarShadowsDefinition''' p identifier var
-- checkPushVarShadowsDefinition'' p@(Prototype_BINARY _ _ identifier _) var = checkPushVarShadowsDefinition''' p identifier var
-- checkPushVarShadowsDefinition'' p@(Prototype identifier _)            var = checkPushVarShadowsDefinition''' p identifier var 

-- checkPushVarShadowsDefinition''' :: Prototype -> Identifier -> VAR_SIGNATURE -> Prototype
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

-- sContextPushPrototype :: SYMBOL_CONTEXT -> Prototype -> SYMBOL_CONTEXT
-- sContextPushPrototype context (Prototype_UNARY  _ _ _ prototype_args) = sContextPushPrototype' context prototype_args
-- sContextPushPrototype context (Prototype_BINARY _ _ _ prototype_args) = sContextPushPrototype' context prototype_args
-- sContextPushPrototype context (Prototype        _     prototype_args) = sContextPushPrototype' context prototype_args

-- sContextPushPrototype' :: SYMBOL_CONTEXT -> PrototypeArgs -> SYMBOL_CONTEXT
-- sContextPushPrototype' context (PrototypeArgs vars _) = sContextPushVars context (map prototypeIdToVarSignature vars)
