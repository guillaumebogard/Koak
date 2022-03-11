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

import qualified Koak.Lexer  as KL
import qualified Koak.Parser as KP
import Koak.TypingContext (toIdentifier)

data Value      = IntVal Int
                | DoubleVal Double
                | BooleanVal Bool
                | NilVal
    deriving (Eq, Show)

data Function   = UnaryFunction                  KP.PrototypeArgs KP.Expressions
                | BinaryFunction KP.Precedence   KP.PrototypeArgs KP.Expressions
                | Function                       KP.PrototypeArgs KP.Expressions
    deriving (Eq, Show)

data PrimFunction   = PrimUnaryFunction                  KP.PrototypeArgs -- (Value -> Value)
                    | PrimBinaryFunction KP.Precedence   KP.PrototypeArgs -- (Value -> Value -> Value)
                    | PrimFunction                       KP.PrototypeArgs
    deriving (Eq, Show)

data Signature  = PrimitiveFunction [PrimFunction]
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
                                        (KP.Identifier "+", PrimitiveFunction [
                                            PrimBinaryFunction (KP.Precedence 1)  (KP.PrototypeArgs [
                                                    KP.PrototypeIdentifier (KP.Identifier "a") KP.Int,
                                                    KP.PrototypeIdentifier (KP.Identifier "b") KP.Int
                                                ]
                                                KP.Int),
                                            PrimBinaryFunction (KP.Precedence 1)  (KP.PrototypeArgs [
                                                    KP.PrototypeIdentifier (KP.Identifier "a") KP.Int,
                                                    KP.PrototypeIdentifier (KP.Identifier "b") KP.Double
                                                ]
                                                KP.Double),
                                            PrimBinaryFunction (KP.Precedence 1)  (KP.PrototypeArgs [
                                                    KP.PrototypeIdentifier (KP.Identifier "a") KP.Double,
                                                    KP.PrototypeIdentifier (KP.Identifier "b") KP.Int
                                                ]
                                                KP.Double),
                                            PrimBinaryFunction (KP.Precedence 1)  (KP.PrototypeArgs [
                                                    KP.PrototypeIdentifier (KP.Identifier "a") KP.Double,
                                                    KP.PrototypeIdentifier (KP.Identifier "b") KP.Double
                                                ]
                                                KP.Double)
                                            ]
                                        )
                                ]
                        )
                        (Variables HM.empty)

getEmptyKContext :: Kcontext
getEmptyKContext = Kcontext (Definitions HM.empty) (Variables HM.empty)

kContextEnterLocalContext :: Kcontext -> Kcontext
kContextEnterLocalContext (Kcontext definitions _) = Kcontext definitions (Variables HM.empty)

kContextPushFunction :: KP.Defs -> Kcontext -> Kcontext
kContextPushFunction (KP.Defs (KP.PrototypeUnary        unop       args) expr) (Kcontext (Definitions ds) vs) = Kcontext (Definitions $ insert (toIdentifier unop)  (RefinedFunction $ UnaryFunction      args expr) ds) vs 
kContextPushFunction (KP.Defs (KP.PrototypeBinary   pre binop      args) expr) (Kcontext (Definitions ds) vs) = Kcontext (Definitions $ insert (toIdentifier binop) (RefinedFunction $ BinaryFunction pre args expr) ds) vs
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
