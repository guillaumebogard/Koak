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
                                       , Function(..)
                                       , PrimFunction(..)
                                       , Signature(..)
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

evaluateFor :: Kcontext -> KP.ForExpression -> EvaluationResult
evaluateFor context (KP.ForExpression assign_expr cond_expr inc_expr exprs) = let assign_result = evaluateExpression context assign_expr
                                                                              in evaluateFor' (EvaluationResult (getEvaluatedContext assign_result) Nil) cond_expr inc_expr exprs

evaluateFor' :: EvaluationResult -> KP.Expression -> KP.Expression -> KP.Expressions -> EvaluationResult
evaluateFor' result@(EvaluationResult context _) cond_expr inc_expr exprs = let cond_result = evaluateExpression context cond_expr in
                                                if isConditionTrue cond_result then (let inc_result  = evaluateExpression (getEvaluatedKcontext cond_result) inc_expr
                                                                                         new_context = getEvaluatedContext inc_result
                                                                                         expr_result = evaluateExpressions new_context exprs
                                                                                     in evaluateFor' expr_result cond_expr inc_expr exprs)
                                                                                else result

evaluateIf :: Kcontext -> KP.IfExpression -> EvaluationResult
evaluateIf context (KP.IfExpression cond_expr exprs Nothing)           = let cond_result = evaluateExpression context cond_expr
                                                      in if isConditionTrue cond_result then evaluateExpressions (getEvaluatedKcontext cond_result) exprs
                                                                                        else EvaluationResult context Nil
evaluateIf context (KP.IfExpression cond_expr exprs (Just else_exprs)) = let cond_result = evaluateExpression context cond_expr
                                                      in if isConditionTrue cond_result then evaluateExpressions (getEvaluatedKcontext cond_result) exprs
                                                                                        else evaluateExpressions (getEvaluatedKcontext cond_result) else_exprs

evaluateWhile :: Kcontext -> KP.WhileExpression -> EvaluationResult
evaluateWhile context = evaluateWhile' (EvaluationResult context Nil)

evaluateWhile' :: EvaluationResult -> KP.WhileExpression -> EvaluationResult
evaluateWhile' result@(EvaluationResult context _) while_expr@(KP.WhileExpression cond_expr exprs) = let cond_result = evaluateExpression context cond_expr in
                                                                                            if isConditionTrue cond_result
                                                                                            then evaluateWhile' (EvaluationResult (getEvaluatedKcontext cond_result) Nil) while_expr
                                                                                            else result

evaluateExpressionList :: Kcontext -> [KP.Expressions] -> EvaluationResult
evaluateExpressionList context [] = error "this should never happend :)"
evaluateExpressionList context [expr] = evaluateExpression context expr
evaluateExpressionList context (x:xs) = let expr_result = evaluateExpression context x
                                        in evaluateExpressionList (getEvaluatedKcontext expr_result) xs

evaluateExpression :: Kcontext -> KP.Expression -> EvaluationResult
evaluateExpression context expression = evaluateExpression' context $ buildExpressionTree context expression

----------------evalexpr------------------

data BinaryTreeExpression = ExprNode KP.BinaryOp BinaryTreeExpression BinaryTreeExpression | ExprLeaf KP.Unary
    deriving (Show, Eq)

data UnitExpression = Un KP.Unary | Bin KP.BinaryOp
    deriving (Show, Eq)


buildExpressionTree :: Kcontext -> KP.Expression -> BinaryTreeExpression
buildExpressionTree context expression@(KP.Expression unary _) = buildExpressionTree' context (convertExpressionToList context expression) $ ExprLeaf unary

buildExpressionTree' :: Kcontext -> [UnitExpression] -> BinaryTreeExpression -> BinaryTreeExpression
buildExpressionTree' context []               tree              = tree
buildExpressionTree' context (x:xs)           node@(ExprLeaf _) = callbackCreateBinaryNode context x xs node
buildExpressionTree' context (bin:(Un un):xs) tree              = buildExpressionTree' context xs $ placeTokenInTree context tree bin $ ExprLeaf un
buildExpressionTree' c       l                b                 = error "In buildExpressionTree"

convertExpressionToList :: Kcontext -> KP.Expression -> [UnitExpression]
convertExpressionToList context (KP.Expression _ [])                     = []
convertExpressionToList context (KP.Expression _ ((binop, second) : xs)) = Bin binop : Un second : convertExpressionToList context (KP.Expression second xs)

placeTokenInTree :: Kcontext -> BinaryTreeExpression -> UnitExpression -> BinaryTreeExpression -> BinaryTreeExpression
placeTokenInTree context node@(ExprLeaf _) (Bin newOp) tree = ExprNode newOp node tree
placeTokenInTree context base@(ExprNode op left right) (Bin newOp) tree
    | isLessPrio context newOp op                           = ExprNode newOp base tree
    | otherwise                                             = ExprNode op left $ placeTokenInTree context right (Bin newOp) tree
placeTokenInTree _ _ _ _                                    = error "In placeTokenInTree"

callbackCreateBinaryNode :: Kcontext -> UnitExpression -> [UnitExpression] -> BinaryTreeExpression -> BinaryTreeExpression
callbackCreateBinaryNode context (Bin binop) ((Un unRight):xs) unLeft = buildExpressionTree' context xs $ ExprNode binop unLeft (ExprLeaf unRight)
callbackCreateBinaryNode _ _ _ _ = error "In callbackCreateBinaryNode"

isLessPrio :: Kcontext -> KP.BinaryOp -> KP.BinaryOp -> Bool
isLessPrio context left right
    | getBinOpPrecedence context left <= getBinOpPrecedence context right   = True
    | otherwise                                             = False

getBinOpPrecedence :: Kcontext -> KP.BinaryOp -> Int
getBinOpPrecedence context binop = let signature = kContextFind context (toIdentifier binop)
    in case signature of
        Nothing                                                                        -> throw $ UnknownDefinition $ toIdentifier binop
        Just (PrimitiveFunction ((BinaryFunctionTyping (KP.Precedence  pre) _ _ _):_)) -> pre
        Just (Function (BinaryFunctionTyping (KP.Precedence  pre) _ _ _))              -> pre
        _                                                                              -> error "Not a binOp"


------------------------------------------

evaluateExpression' :: Kcontext -> BinaryTreeExpression -> EvaluationResult
evaluateExpression' context (ExprLeaf unary)                            = evaluateUnary context unary
evaluateExpression' context (ExprNode binary@(KP.BinaryOp (KP.Identifier "=")) left@(ExprLeaf (KP.UnaryPostFix (KP.Postfix (PrimaryIdentifier identifier) Nothing) )) right)
                                                                        = let right_result = evaluateUnary context right in right_result -- add identifier and right result value to context
evaluateExpression' context (ExprNode (KP.BinaryOp binary) left right)  = let leftResult  = evaluateExpression' context left                                   in
                                                                          let rightResult = evaluateExpression' (getEvaluatedKcontext leftResult)  right       in
                                                                              evaluateBinOperation (getEvaluatedKcontext rightResult) binary (getEvaluatedType leftResult) (getEvaluatedType rightResult)


isConditionTrue :: EvaluationResult -> Bool
isConditionTrue (EvaluationResult _ NilVal)            = False
isConditionTrue (EvaluationResult _ BooleanVal False ) = False
isConditionTrue (EvaluationResult _ IntVal 0)          = False
isConditionTrue (EvaluationResult _ BoolVal 0.0)       = False
isConditionTrue _                                      = True

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
