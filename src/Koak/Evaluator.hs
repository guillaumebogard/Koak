--
-- EPITECH PROJECT, 2022
-- koak
-- File description:
-- Koak.Evaluator
--

module Koak.Evaluator                  ( evaluateKoak
                                       ) where


import Data.Foldable                ( find )
import Control.Exception            ( throw )

import qualified Koak.Parser as KP

import Koak.EvaluatorContext           ( Value(..)
                                       , BaseType(..)
                                       , Function(..)
                                       , PrimFunction(..)
                                       , Signature(..)
                                       , Context(..)
                                       , DefContext(..)
                                       , VarContext(..)
                                       , Kcontext(..)
                                       , getDefaultKContext
                                       )

data EvaluationResult = EvaluationResult Kcontext Value

evaluateKoak :: Kcontext -> [KP.Kdefs] -> Kcontext
evaluateKoak = foldl evaluateKdef

evaluateKdef :: Kcontext -> KP.Kdefs -> Kcontext
evaluateKdef context (KP.KdefDef        _    ) = context -- il faut ajouter la fonction dans le contexte
evaluateKdef context (KP.KdefExpression exprs) = getEvaluatedKcontext $ evaluateExpressions context exprs

evaluateExpressions :: Kcontext -> [KP.Expressions] -> EvaluationResult
evaluateExpressions context (KP.ExpressionFor    for_expr  ) = evaluateFor            context for_expr
evaluateExpressions context (KP.ExpressionIf     if_expr   ) = evaluateIf             context if_expr
evaluateExpressions context (KP.ExpressionWhile  while_expr) = evaluateWhile          context while_expr
evaluateExpressions context (KP.Expressions      expr exprs) = evaluateExpressionList context (expr:exprs)

(<->) :: () -> () -> ()
(<->) _ _ = ()

(<--) :: a -> b -> a
(<--) a _ = a

(-->) :: b -> a -> a
(-->) _ a = a

getEvaluatedValue :: EvaluationResult -> Value
getEvaluatedValue (EvaluationResult _ value) = value

getEvaluatedKcontext :: EvaluationResult -> Kcontext
getEvaluatedKcontext (EvaluationResult context _) = context
