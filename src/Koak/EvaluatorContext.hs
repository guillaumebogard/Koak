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
                                    , valueToType
                                    ) where

import Data.HashMap.Strict  as HM   ( HashMap
                                    , fromList
                                    , empty
                                    , member
                                    , insert
                                    , lookup
                                    )

import qualified Koak.Parser        as KP
import qualified Koak.TypingContext as KTC
import qualified Koak.Evaluator.Exception as KEE
import Control.Exception (throw)

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
                                        (KP.Identifier "=" , PrimitiveFunction $ PrimBinaryFunction (KP.Precedence 1)  primitiveBinaryAssign),
                                        (KP.Identifier "==", PrimitiveFunction $ PrimBinaryFunction (KP.Precedence 8)  primitiveBinaryEqual),
                                        (KP.Identifier "!=", PrimitiveFunction $ PrimBinaryFunction (KP.Precedence 8)  primitiveBinaryNotEqual),
                                        (KP.Identifier ">" , PrimitiveFunction $ PrimBinaryFunction (KP.Precedence 9)  primitiveBinaryMoreThan),
                                        (KP.Identifier "<" , PrimitiveFunction $ PrimBinaryFunction (KP.Precedence 9)  primitiveBinaryLessThan),
                                        (KP.Identifier ">=", PrimitiveFunction $ PrimBinaryFunction (KP.Precedence 9)  primitiveBinaryMoreEqThan),
                                        (KP.Identifier "<=", PrimitiveFunction $ PrimBinaryFunction (KP.Precedence 9)  primitiveBinaryLessEqThan),
                                        (KP.Identifier "+" , PrimitiveFunction $ PrimBinaryFunction (KP.Precedence 11) primitiveBinaryPlus),
                                        (KP.Identifier "-" , PrimitiveFunction $ PrimBinaryFunction (KP.Precedence 11) primitiveBinaryMinus),
                                        (KP.Identifier "*" , PrimitiveFunction $ PrimBinaryFunction (KP.Precedence 12) primitiveBinaryMultiply),
                                        (KP.Identifier "/" , PrimitiveFunction $ PrimBinaryFunction (KP.Precedence 12) primitiveBinaryDivide),
                                        (KP.Identifier "%" , PrimitiveFunction $ PrimBinaryFunction (KP.Precedence 12) primitiveBinaryModulo),
                                        (KP.Identifier "--" , PrimitiveFunction $ PrimUnaryFunction                    primitiveUnaryMinus),
                                        (KP.Identifier "!" ,  PrimitiveFunction $ PrimUnaryFunction                    primitiveUnaryNot),
                                        (KP.Identifier "show",    PrimitiveFunction $ PrimFunction primitiveShow)
                                ]
                        )
                        (Variables HM.empty)

primitiveBinaryAssign :: Value -> Value -> Value
primitiveBinaryAssign _ _ = NilVal

primitiveBinaryEqual :: Value -> Value -> Value
primitiveBinaryEqual (IntVal    lv) (IntVal    rv) = BooleanVal (lv == rv)
primitiveBinaryEqual (DoubleVal lv) (IntVal    rv) = BooleanVal (floor lv == toInteger rv)
primitiveBinaryEqual (IntVal    lv) (DoubleVal rv) = BooleanVal (toInteger lv == floor rv)
primitiveBinaryEqual (DoubleVal lv) (DoubleVal rv) = BooleanVal (lv == rv)
primitiveBinaryEqual _              _              = error "primitiveBinaryPlus"

primitiveBinaryNotEqual :: Value -> Value -> Value
primitiveBinaryNotEqual (IntVal    lv) (IntVal    rv) = BooleanVal $ lv /= rv
primitiveBinaryNotEqual (DoubleVal lv) (IntVal    rv) = BooleanVal (floor lv /= toInteger rv)
primitiveBinaryNotEqual (IntVal    lv) (DoubleVal rv) = BooleanVal (toInteger lv /= floor rv)
primitiveBinaryNotEqual (DoubleVal lv) (DoubleVal rv) = BooleanVal $ lv /= rv
primitiveBinaryNotEqual _              _              = error "primitiveBinaryPlus"

primitiveBinaryLessThan :: Value -> Value -> Value
primitiveBinaryLessThan (IntVal    lv) (IntVal    rv) = BooleanVal $ lv < rv
primitiveBinaryLessThan (DoubleVal lv) (IntVal    rv) = BooleanVal (floor lv < toInteger rv)
primitiveBinaryLessThan (IntVal    lv) (DoubleVal rv) = BooleanVal (toInteger lv < floor rv)
primitiveBinaryLessThan (DoubleVal lv) (DoubleVal rv) = BooleanVal $ lv < rv
primitiveBinaryLessThan _              _              = error "primitiveBinaryPlus"

primitiveBinaryMoreThan :: Value -> Value -> Value
primitiveBinaryMoreThan (IntVal    lv) (IntVal    rv) = BooleanVal $ lv > rv
primitiveBinaryMoreThan (DoubleVal lv) (IntVal    rv) = BooleanVal (floor lv > toInteger rv)
primitiveBinaryMoreThan (IntVal    lv) (DoubleVal rv) = BooleanVal (toInteger lv > floor rv)
primitiveBinaryMoreThan (DoubleVal lv) (DoubleVal rv) = BooleanVal $ lv > rv
primitiveBinaryMoreThan _              _              = error "primitiveBinaryPlus"


primitiveBinaryLessEqThan :: Value -> Value -> Value
primitiveBinaryLessEqThan (IntVal    lv) (IntVal    rv) = BooleanVal $ lv <= rv
primitiveBinaryLessEqThan (DoubleVal lv) (IntVal    rv) = BooleanVal (floor lv <= toInteger rv)
primitiveBinaryLessEqThan (IntVal    lv) (DoubleVal rv) = BooleanVal (toInteger lv <= floor rv)
primitiveBinaryLessEqThan (DoubleVal lv) (DoubleVal rv) = BooleanVal $ lv <= rv
primitiveBinaryLessEqThan _              _              = error "primitiveBinaryPlus"

primitiveBinaryMoreEqThan :: Value -> Value -> Value
primitiveBinaryMoreEqThan (IntVal    lv) (IntVal    rv) = BooleanVal $ lv >= rv
primitiveBinaryMoreEqThan (DoubleVal lv) (IntVal    rv) = BooleanVal (floor lv >= toInteger rv)
primitiveBinaryMoreEqThan (IntVal    lv) (DoubleVal rv) = BooleanVal (toInteger lv >= floor rv)
primitiveBinaryMoreEqThan (DoubleVal lv) (DoubleVal rv) = BooleanVal $ lv >= rv
primitiveBinaryMoreEqThan _              _              = error "primitiveBinaryPlus"

primitiveBinaryPlus :: Value -> Value -> Value
primitiveBinaryPlus (IntVal    lv) (IntVal    rv) = IntVal (lv + rv)
primitiveBinaryPlus (DoubleVal lv) (DoubleVal rv) = DoubleVal (lv + rv)
primitiveBinaryPlus _              _              = error "primitiveBinaryPlus"

primitiveBinaryMinus :: Value -> Value -> Value
primitiveBinaryMinus (IntVal    lv) (IntVal    rv) = IntVal (lv - rv)
primitiveBinaryMinus (DoubleVal lv) (DoubleVal rv) = DoubleVal (lv - rv)
primitiveBinaryMinus _              _              = error "primitiveBinaryMinus"

primitiveBinaryMultiply :: Value -> Value -> Value
primitiveBinaryMultiply (IntVal    lv) (IntVal    rv) = IntVal (lv * rv)
primitiveBinaryMultiply (DoubleVal lv) (DoubleVal rv) = DoubleVal (lv * rv)
primitiveBinaryMultiply _              _              = error "primitiveBinaryPMultiply"

primitiveBinaryDivide :: Value -> Value -> Value
primitiveBinaryDivide (IntVal    _ ) (IntVal    0)   = throw $ KEE.KoakEvaluatorException "Int Division by zero"
primitiveBinaryDivide (IntVal    lv) (IntVal    rv)  = IntVal (lv `div` rv)
primitiveBinaryDivide (DoubleVal lv) (DoubleVal 0.0) = throw $ KEE.KoakEvaluatorException "Double Division by zero"
primitiveBinaryDivide (DoubleVal lv) (DoubleVal rv)  = DoubleVal (lv / rv)
primitiveBinaryDivide _              _               = error "primitiveBinaryDivide"

primitiveBinaryModulo :: Value -> Value -> Value
primitiveBinaryModulo (IntVal    _ ) (IntVal    0 ) = throw $ KEE.KoakEvaluatorException "Int Modulo by zero"
primitiveBinaryModulo (IntVal    lv) (IntVal    rv) = IntVal (lv `mod` rv)
primitiveBinaryModulo _              _              = error "primitiveBinaryModulo"

primitiveUnaryNot :: Value -> Value
primitiveUnaryNot (IntVal    0)      = IntVal 1
primitiveUnaryNot (IntVal    1)      = IntVal 0
primitiveUnaryNot (DoubleVal 0.0)    = DoubleVal 1.0
primitiveUnaryNot (DoubleVal 1.0)    = DoubleVal 0.0
primitiveUnaryNot (BooleanVal True)  = BooleanVal False
primitiveUnaryNot (BooleanVal False) = BooleanVal True
primitiveUnaryNot NilVal             = NilVal
primitiveUnaryNot  _                 = error "primitiveUnaryNot"

primitiveUnaryMinus :: Value -> Value
primitiveUnaryMinus (IntVal    x)      = IntVal (-x)
primitiveUnaryMinus (DoubleVal x)      = DoubleVal (-x)
primitiveUnaryMinus (BooleanVal True)  = BooleanVal False
primitiveUnaryMinus (BooleanVal False) = BooleanVal True
primitiveUnaryMinus NilVal             = NilVal

primitiveShow :: [Value] -> Value
primitiveShow = foldr ((-->) . show) NilVal

(-->) :: b -> a -> a
(-->) _ a = a

kContextEnterLocalContext :: Kcontext -> KP.PrototypeArgs -> [Value] -> Kcontext
kContextEnterLocalContext (Kcontext definitions _) (KP.PrototypeArgs [] _) [] = Kcontext definitions (Variables HM.empty)
kContextEnterLocalContext (Kcontext definitions _) (KP.PrototypeArgs args _) values =
                            kContextEnterLocalContext' (Kcontext definitions (Variables HM.empty)) args values

kContextEnterLocalContext' :: Kcontext -> [KP.PrototypeIdentifier] -> [Value] -> Kcontext
kContextEnterLocalContext' context [] [] = context
kContextEnterLocalContext' context ((KP.PrototypeIdentifier identifier _):args) (val:vals) =
    kContextEnterLocalContext' (kContextPushVariable identifier val context) args vals
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

valueToType :: Value -> KP.Type
valueToType (IntVal _) = KP.Int
valueToType (DoubleVal _) = KP.Double
valueToType (BooleanVal _) = KP.Boolean
valueToType NilVal = KP.Void

