--
-- EPITECH PROJECT, 2022
-- koak
-- File description:
-- EvaluatorContext
--

module Koak.EvaluatorContext        (
                                      Value(..)
                                    , BaseType(..)
                                    , Function(..)
                                    , PrimFunction(..)
                                    , Signature(..)
                                    , Context(..)
                                    , DefContext(..)
                                    , VarContext(..)
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

import qualified Koak.Parser as KP

data Value      = IntVal Int
                | DoubleVal Double
                | BooleanVal Bool
                | NilVal
    deriving (Eq, Show)

data BaseType   = Int
                | Double
                | Boolean
                | Nil
    deriving (Eq, Show)

data Function   = UnaryFunction                  KP.PrototypeArgs KP.Expressions
                | BinaryFunction KP.Precedence   KP.PrototypeArgs KP.Expressions
                | Function                       KP.PrototypeArgs KP.Expressions
    deriving (Eq, Show)

data PrimFunction   = PrimUnaryFunction                  [KP.PrototypeArgs]
                    | PrimBinaryFunction KP.Precedence   [KP.PrototypeArgs]
                    | PrimFunction                       [KP.PrototypeArgs]
    deriving (Eq, Show)

data Signature  = PrimitiveFunction PrimFunction
                | RefinedFunction   Function
    deriving (Eq, Show)

newtype Definitions            = Definitions (HashMap KP.Identifier Signature)
    deriving (Eq, Show)

newtype Variables                   = Variables (HashMap KP.Identifier Value)
    deriving (Eq, Show)

data Kcontext           = Kcontext Definitions Variables
    deriving (Eq, Show)

class Identify a where
    toIdentifier :: a -> KP.Identifier

instance Identify KP.Defs where
    toIdentifier (KP.Defs p _) = toIdentifier p

instance Identify KP.Prototype where
    toIdentifier (KP.PrototypeUnary      (KP.UnaryOp  i) _) = i
    toIdentifier (KP.PrototypeBinary   _ (KP.BinaryOp i) _) = i
    toIdentifier (KP.PrototypeFunction   i               _) = i

instance Identify KP.UnaryOp where
    toIdentifier (KP.UnaryOp i) = i

instance Identify KP.BinaryOp where
    toIdentifier (KP.BinaryOp i) = i

instance Identify KP.VarAssignment where
    toIdentifier (KP.VarAssignment i _) = i


instance Hashable KP.Identifier where
    hashWithSalt salt (KP.Identifier string)   = salt `hashWithSalt` string

getDefaultKContext :: Kcontext
getDefaultKContext = Kcontext
                        (
                            DefContext $
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
                        (VarContext HM.empty)
