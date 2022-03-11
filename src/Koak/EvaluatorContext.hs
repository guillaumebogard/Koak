--
-- EPITECH PROJECT, 2022
-- koak
-- File description:
-- EvaluatorContext
--

module Koak.EvaluatorContext        (
                                      Value(..)
                                    , Function(..)
                                    , PrimFunction(..)
                                    , Signature(..)
                                    , Variables(..)
                                    , Kcontext(..)
                                    , getDefaultKContext
                                    , kContextEnterLocalContext
                                    , kContextPushFunction
                                    , kContextPushVariable
                                    , kContextHasVariable
                                    , kContextFindVariable
                                    , kContextFindFunction
                                    ) where


import Control.Exception            ( throw )

import Data.Hashable                ( Hashable
                                    , hashWithSalt
                                    )

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

import qualified Koak.Lexer         as KL
import qualified Koak.Parser        as KP
import qualified Koak.Typing        as KT
import qualified Koak.TypingContext as KTC

data Value      = IntVal Int
                | DoubleVal Double
                | BooleanVal Bool
                | NilVal
    deriving (Eq, Show)

data Function   = UnaryFunction                  KP.PrototypeArgs KP.Expressions
                | BinaryFunction KP.Precedence   KP.PrototypeArgs KP.Expressions
                | Function                       KP.PrototypeArgs KP.Expressions
    deriving (Eq, Show)

data PrimFunction   = PrimUnaryFunction                  (Value   -> Value)
                    | PrimBinaryFunction KP.Precedence   (Value   -> Value -> Value)
                    | PrimFunction                       ([Value] -> Value)

instance Eq PrimFunction where
    (PrimUnaryFunction      _) == (PrimUnaryFunction       _) = True
    (PrimBinaryFunction pre _) == (PrimBinaryFunction pre' _) = pre == pre'
    (PrimFunction           _) == (PrimFunction            _) = True
    _                          == _                           = False

instance Show PrimFunction where
    show (PrimUnaryFunction      _) = "PrimUnaryFunction"
    show (PrimBinaryFunction pre _) = "PrimBinaryFunction " ++ show pre
    show (PrimFunction           _) = "PrimFunction"

data Signature  = PrimitiveFunction PrimFunction
                | RefinedFunction   Function
    deriving (Eq, Show)

newtype Definitions         = Definitions (HashMap KP.Identifier Signature)
    deriving (Eq, Show)

newtype Variables           = Variables (HashMap KP.Identifier Value)
    deriving (Eq, Show)

data Kcontext               = Kcontext Definitions Variables
    deriving (Eq, Show)

getDefaultKContext :: Kcontext
getDefaultKContext = Kcontext
                        (
                            Definitions $
                                HM.fromList [
                                        (KP.Identifier "=" , PrimitiveFunction $ PrimBinaryFunction (KP.Precedence 1) primitiveBinaryPlus),
                                        (KP.Identifier "+" , PrimitiveFunction $ PrimBinaryFunction (KP.Precedence 1) primitiveBinaryPlus)
                                ]
                        )
                        (Variables HM.empty)

primitiveBinaryPlus :: Value -> Value -> Value
primitiveBinaryPlus (IntVal    lv) (IntVal    rv) = IntVal (lv + rv)
primitiveBinaryPlus _              _              = error "primitiveBinaryPlus"

getEmptyKContext :: Kcontext
getEmptyKContext = Kcontext (Definitions HM.empty) (Variables HM.empty)

kContextEnterLocalContext :: Kcontext -> KP.PrototypeArgs -> [Value] -> Kcontext
kContextEnterLocalContext (Kcontext definitions _) (KP.PrototypeArgs [] _) [] = Kcontext definitions (Variables HM.empty)
kContextEnterLocalContext (Kcontext definitions _) (KP.PrototypeArgs args _) values =
                            kContextEnterLocalContext' (Kcontext definitions (Variables HM.empty)) args values

kContextEnterLocalContext' :: Kcontext -> [KP.PrototypeIdentifier] -> [Value] -> Kcontext
kContextEnterLocalContext' context [] [] = context
kContextEnterLocalContext' context ((KP.PrototypeIdentifier id _):args) (val:vals) =
    kContextEnterLocalContext' (kContextPushVariable id val context) args vals
kContextEnterLocalContext' _ _ _ = error "Invalid number of arguments"

kContextPushVariable :: KP.Identifier -> Value -> Kcontext -> Kcontext
kContextPushVariable identifier val (Kcontext ds (Variables vs)) = Kcontext ds (Variables $ insert identifier val vs)

kContextPushFunction :: KP.Defs -> Kcontext -> Kcontext
kContextPushFunction (KP.Defs (KP.PrototypeUnary        unop       args) expr) (Kcontext (Definitions ds) vs) = Kcontext (Definitions $ insert (KTC.toIdentifier unop)  (RefinedFunction $ UnaryFunction      args expr) ds) vs 
kContextPushFunction (KP.Defs (KP.PrototypeBinary   pre binop      args) expr) (Kcontext (Definitions ds) vs) = Kcontext (Definitions $ insert (KTC.toIdentifier binop) (RefinedFunction $ BinaryFunction pre args expr) ds) vs
kContextPushFunction (KP.Defs (KP.PrototypeFunction     identifier args) expr) (Kcontext (Definitions ds) vs) = Kcontext (Definitions $ insert identifier           (RefinedFunction $ Function           args expr) ds) vs

kContextHasVariable :: KP.Identifier -> Kcontext -> Bool
kContextHasVariable identifier (Kcontext _ (Variables vs)) = HM.member identifier vs

kContextFindVariable :: KP.Identifier -> Kcontext -> Value
kContextFindVariable identifier (Kcontext _ (Variables vs)) = let found = HM.lookup identifier vs
                                                              in case found of
                                                                Nothing  -> error "kContextFindVar"
                                                                (Just v) -> v

kContextFindFunction :: KP.Identifier -> Kcontext -> Signature
kContextFindFunction identifier (Kcontext (Definitions ds) _) = let found = HM.lookup identifier ds
                                                                in case found of
                                                                    Nothing  -> error "kContextFindFunction"
                                                                    (Just s) -> s
