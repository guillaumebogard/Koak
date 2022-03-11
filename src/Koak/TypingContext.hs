--
-- EPITECH PROJECT, 2022
-- koak
-- File description:
-- TypingContext
--

module Koak.TypingContext              ( Kcontext(..)
                                       , DefContext(..)
                                       , VarContext(..)
                                       , BaseType(..)
                                       , FunctionTyping(..)
                                       , TypeSignature(..)
                                       , getDefaultKContext
                                       , getEmptyKContext
                                       , kContextPushFunction
                                       , kContextPushVar
                                       , kContextFind
                                       , kContextEnterLocalContext
                                       , kContextEnterFunctionCall
                                       , isUnaryFunctionParamMatchingFunction
                                       , isBinaryFunctionParamMatchingFunction
                                       , isFunctionParamMatchingFunction
                                       , getFunctionReturnType
                                       , prototypeToBaseType
                                       , prototypeIdToBaseType
                                       , prototypeIdToVarAssignment
                                       , typeToBaseType
                                       , baseTypeToType
                                       , toIdentifier
                                       , toTypeSignature
                                       ) where

import Control.Exception               ( throw )

import Data.HashMap.Strict  as HM      ( HashMap
                                       , fromList
                                       , empty
                                       , member
                                       , insert
                                       , lookup
                                       )

import Data.Maybe                      ( isJust )

import qualified Koak.Typing.Exception as KTE ( KoakTypingException(..) )
import qualified Koak.Parser           as KP

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

newtype VarContext      = VarContext Context
    deriving (Eq, Show)

newtype DefContext      = DefContext Context
    deriving (Eq, Show)

data Kcontext           = Kcontext DefContext VarContext
    deriving (Eq, Show)


instance Identify KP.Defs where
    toIdentifier (KP.Defs p _) = toIdentifier p

instance Type KP.Defs where
    toTypeSignature (KP.Defs p _) = toTypeSignature p

instance Identify KP.Prototype where
    toIdentifier (KP.PrototypeUnary      (KP.UnaryOp  i) _) = i
    toIdentifier (KP.PrototypeBinary   _ (KP.BinaryOp i) _) = i
    toIdentifier (KP.PrototypeFunction   i               _) = i

instance Type KP.Prototype where
    toTypeSignature (KP.PrototypeUnary        _ (KP.PrototypeArgs [x]        return_type)) = Function $ UnaryFunctionTyping (prototypeIdToBaseType x) (typeToBaseType return_type)
    toTypeSignature (KP.PrototypeUnary        u (KP.PrototypeArgs args       _          )) = throw    $ KTE.KoakTypingMismatchedArgumentNumber (toIdentifier u)  (length args)
    toTypeSignature (KP.PrototypeBinary   pre _ (KP.PrototypeArgs [x,y]      return_type)) = Function $ BinaryFunctionTyping pre (prototypeIdToBaseType x) (prototypeIdToBaseType y) (typeToBaseType return_type)
    toTypeSignature (KP.PrototypeBinary   _   b (KP.PrototypeArgs args       _          )) = throw    $ KTE.KoakTypingMismatchedArgumentNumber (toIdentifier b) (length args)
    toTypeSignature (KP.PrototypeFunction _     (KP.PrototypeArgs args       return_type)) = Function $ FunctionTyping (map prototypeIdToBaseType args) (typeToBaseType return_type)

instance Identify KP.UnaryOp where
    toIdentifier (KP.UnaryOp i) = i

instance Identify KP.BinaryOp where
    toIdentifier (KP.BinaryOp i) = i

instance Identify KP.VarAssignment where
    toIdentifier (KP.VarAssignment i _) = i

instance Type KP.VarAssignment where
    toTypeSignature (KP.VarAssignment _ var_type) = Var $ typeToBaseType var_type

getDefaultKContext :: Kcontext
getDefaultKContext = Kcontext
                        (
                            DefContext $
                                HM.fromList [
                                        (KP.Identifier "=", PrimitiveFunction  [BinaryFunctionTyping (KP.Precedence 1)  Int Int Int, BinaryFunctionTyping (KP.Precedence 1)  Double Double Double, BinaryFunctionTyping (KP.Precedence 1)  Double Int Double, BinaryFunctionTyping (KP.Precedence 1)  Int Double Double]),
                                        (KP.Identifier "==", PrimitiveFunction [BinaryFunctionTyping (KP.Precedence 8)  Int Int Int, BinaryFunctionTyping (KP.Precedence 8)  Double Double Double, BinaryFunctionTyping (KP.Precedence 8)  Double Int Double, BinaryFunctionTyping (KP.Precedence 8)  Int Double Double]),
                                        (KP.Identifier "!=", PrimitiveFunction [BinaryFunctionTyping (KP.Precedence 8)  Int Int Int, BinaryFunctionTyping (KP.Precedence 8)  Double Double Double, BinaryFunctionTyping (KP.Precedence 8)  Double Int Double, BinaryFunctionTyping (KP.Precedence 8)  Int Double Double]),
                                        (KP.Identifier ">", PrimitiveFunction  [BinaryFunctionTyping (KP.Precedence 9)  Int Int Int, BinaryFunctionTyping (KP.Precedence 9)  Double Double Double, BinaryFunctionTyping (KP.Precedence 9)  Double Int Double, BinaryFunctionTyping (KP.Precedence 9)  Int Double Double]),
                                        (KP.Identifier "<", PrimitiveFunction  [BinaryFunctionTyping (KP.Precedence 9)  Int Int Int, BinaryFunctionTyping (KP.Precedence 9)  Double Double Double, BinaryFunctionTyping (KP.Precedence 9)  Double Int Double, BinaryFunctionTyping (KP.Precedence 9)  Int Double Double]),
                                        (KP.Identifier ">=", PrimitiveFunction [BinaryFunctionTyping (KP.Precedence 9)  Int Int Int, BinaryFunctionTyping (KP.Precedence 9)  Double Double Double, BinaryFunctionTyping (KP.Precedence 9)  Double Int Double, BinaryFunctionTyping (KP.Precedence 9)  Int Double Double]),
                                        (KP.Identifier "<=", PrimitiveFunction [BinaryFunctionTyping (KP.Precedence 9)  Int Int Int, BinaryFunctionTyping (KP.Precedence 9)  Double Double Double, BinaryFunctionTyping (KP.Precedence 9)  Double Int Double, BinaryFunctionTyping (KP.Precedence 9)  Int Double Double]),
                                        (KP.Identifier "+", PrimitiveFunction  [BinaryFunctionTyping (KP.Precedence 11) Int Int Int, BinaryFunctionTyping (KP.Precedence 11) Double Double Double]),
                                        (KP.Identifier "-", PrimitiveFunction  [BinaryFunctionTyping (KP.Precedence 11) Int Int Int, BinaryFunctionTyping (KP.Precedence 11) Double Double Double]),
                                        (KP.Identifier "--", PrimitiveFunction  [UnaryFunctionTyping  Int Int, UnaryFunctionTyping Double Double]),
                                        (KP.Identifier "*", PrimitiveFunction  [BinaryFunctionTyping (KP.Precedence 12) Int Int Int, BinaryFunctionTyping (KP.Precedence 12) Double Double Double]),
                                        (KP.Identifier "/", PrimitiveFunction  [BinaryFunctionTyping (KP.Precedence 12) Int Int Int, BinaryFunctionTyping (KP.Precedence 12) Double Double Double]),
                                        (KP.Identifier "%", PrimitiveFunction  [BinaryFunctionTyping (KP.Precedence 12) Int Int Int, BinaryFunctionTyping (KP.Precedence 12) Double Double Double]),
                                        (KP.Identifier "!", PrimitiveFunction  [UnaryFunctionTyping  Int Boolean, UnaryFunctionTyping Double Boolean, UnaryFunctionTyping Boolean Boolean]),
                                        (KP.Identifier "toInt",    PrimitiveFunction  [FunctionTyping [Int] Int,    FunctionTyping [Double] Int,    FunctionTyping [Boolean] Int   ]),
                                        (KP.Identifier "show",    PrimitiveFunction  [FunctionTyping [Int] Nil,    FunctionTyping [Double] Nil,    FunctionTyping [Boolean] Nil   ]),
                                        (KP.Identifier "toDouble", PrimitiveFunction  [FunctionTyping [Int] Double, FunctionTyping [Double] Double, FunctionTyping [Boolean] Double])
                                ]
                        )
                        (VarContext HM.empty)

getEmptyKContext :: Kcontext
getEmptyKContext = Kcontext (DefContext HM.empty) (VarContext HM.empty)

kContextPushFunction :: KP.Prototype -> Kcontext -> Kcontext
kContextPushFunction p = kContextPushFunction' p (toIdentifier p) (toTypeSignature p)

kContextPushFunction' :: KP.Prototype  -> KP.Identifier -> TypeSignature -> Kcontext -> Kcontext
kContextPushFunction' p i ts (Kcontext (DefContext dc) v@(VarContext vc))
    | HM.member i vc = throw $ KTE.KoakTypingShadowedVariableByDefinition   i p
    | HM.member i dc = throw $ KTE.KoakTypingShadowedDefinitionByDefinition i p
    | otherwise      = Kcontext (DefContext $ contextPushItem i ts dc) v

kContextEnterFunctionCall :: KP.Prototype -> Kcontext -> Kcontext
kContextEnterFunctionCall (KP.PrototypeUnary    _   (KP.PrototypeArgs args _)) k = kContextEnterFunctionCall' args $ kContextEnterLocalContext k
kContextEnterFunctionCall (KP.PrototypeBinary   _ _ (KP.PrototypeArgs args _)) k = kContextEnterFunctionCall' args $ kContextEnterLocalContext k
kContextEnterFunctionCall (KP.PrototypeFunction _   (KP.PrototypeArgs args _)) k = kContextEnterFunctionCall' args $ kContextEnterLocalContext k

kContextEnterFunctionCall' :: [KP.PrototypeIdentifier] -> Kcontext -> Kcontext
kContextEnterFunctionCall' args k = foldr (kContextPushVar . prototypeIdToVarAssignment) k args

kContextEnterLocalContext :: Kcontext -> Kcontext
kContextEnterLocalContext (Kcontext (DefContext dc) _) = Kcontext (DefContext dc) (VarContext HM.empty)

kContextPushVar :: KP.VarAssignment -> Kcontext -> Kcontext
kContextPushVar v@(KP.VarAssignment i _) (Kcontext (DefContext dc) (VarContext vc))
    | HM.member i dc    = throw $ KTE.KoakTypingShadowedDefinitionByVariable i v
    | HM.member i vc    = throw $ KTE.KoakTypingShadowedVariableByVariable   i v
    | otherwise         = Kcontext (DefContext dc) (VarContext $ contextPushItem i (toTypeSignature v) vc)

kContextFind :: Kcontext -> KP.Identifier -> Maybe TypeSignature
kContextFind (Kcontext dc vc) i
    | let vc_res = varContextFind vc i, isJust vc_res = vc_res
    | let dc_res = defContextFind dc i, isJust dc_res = dc_res
    | otherwise                                       = Nothing

defContextFind :: DefContext -> KP.Identifier -> Maybe TypeSignature
defContextFind (DefContext c) i = HM.lookup i c

varContextFind :: VarContext -> KP.Identifier -> Maybe TypeSignature
varContextFind (VarContext c) i = HM.lookup i c

contextPushItem :: KP.Identifier -> TypeSignature -> Context -> Context
contextPushItem = insert

isUnaryFunctionParamMatchingFunction :: BaseType -> FunctionTyping -> Bool
isUnaryFunctionParamMatchingFunction arg1 (UnaryFunctionTyping func_arg1 _)
                                         = arg1 == func_arg1
isUnaryFunctionParamMatchingFunction _ _ = False 

isBinaryFunctionParamMatchingFunction :: BaseType -> BaseType -> FunctionTyping -> Bool
isBinaryFunctionParamMatchingFunction arg1 arg2 (BinaryFunctionTyping _ func_arg1 func_arg2 _)
                                            = arg1 == func_arg1 && arg2 == func_arg2
isBinaryFunctionParamMatchingFunction _ _ _ = False

isFunctionParamMatchingFunction :: [BaseType] -> FunctionTyping -> Bool
isFunctionParamMatchingFunction args (FunctionTyping func_args _)
                                    = args == func_args
isFunctionParamMatchingFunction _ _ = False

getFunctionReturnType :: FunctionTyping -> BaseType
getFunctionReturnType (UnaryFunctionTyping  _      return_type) = return_type 
getFunctionReturnType (BinaryFunctionTyping _ _ _  return_type) = return_type
getFunctionReturnType (FunctionTyping       _      return_type) = return_type

prototypeToBaseType :: KP.Prototype -> BaseType
prototypeToBaseType (KP.PrototypeUnary      _ (KP.PrototypeArgs _ return_type))  = typeToBaseType return_type
prototypeToBaseType (KP.PrototypeBinary   _ _ (KP.PrototypeArgs _ return_type) ) = typeToBaseType return_type
prototypeToBaseType (KP.PrototypeFunction   _ (KP.PrototypeArgs _ return_type))  = typeToBaseType return_type

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
