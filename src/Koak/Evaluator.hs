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

import qualified Koak.Parser        as KP
import qualified Koak.TypingContext as KTC
import Koak.EvaluatorContext        as KEC


data EvaluationResult = EvaluationResult Kcontext Value

evaluateKoak :: Kcontext -> [KP.Kdefs] -> Kcontext
evaluateKoak = foldl evaluateKdef

evaluateKdef :: Kcontext -> KP.Kdefs -> Kcontext
evaluateKdef context (KP.KdefDef        _    ) = context -- il faut ajouter la fonction dans le contextes
evaluateKdef context (KP.KdefExpression exprs) = getEvaluatedKcontext $ evaluateExpressions context exprs

evaluateExpressions :: Kcontext -> KP.Expressions -> EvaluationResult
evaluateExpressions context (KP.ExpressionFor    for_expr  ) = evaluateFor            context for_expr
evaluateExpressions context (KP.ExpressionIf     if_expr   ) = evaluateIf             context if_expr
evaluateExpressions context (KP.ExpressionWhile  while_expr) = evaluateWhile          context while_expr
evaluateExpressions context (KP.Expressions      expr exprs) = evaluateExpressionList context (expr:exprs)

evaluateFor :: Kcontext -> KP.For -> EvaluationResult
evaluateFor context (KP.For assign_expr cond_expr inc_expr exprs) = let assign_result = evaluateExpression context assign_expr
                                                                    in evaluateFor' (EvaluationResult (getEvaluatedKcontext assign_result) NilVal) cond_expr inc_expr exprs

evaluateFor' :: EvaluationResult -> KP.Expression -> KP.Expression -> KP.Expressions -> EvaluationResult
evaluateFor' result@(EvaluationResult context _) cond_expr inc_expr exprs = let cond_result = evaluateExpression context cond_expr in
                                                if isConditionTrue cond_result then (let inc_result  = evaluateExpression   (getEvaluatedKcontext cond_result) inc_expr
                                                                                         new_context = getEvaluatedKcontext inc_result
                                                                                         expr_result = evaluateExpressions  new_context exprs
                                                                                     in evaluateFor' expr_result cond_expr inc_expr exprs)
                                                                                else result

evaluateIf :: Kcontext -> KP.If -> EvaluationResult
evaluateIf context (KP.If cond_expr exprs Nothing)           = let cond_result = evaluateExpression context cond_expr in
                                                                    if isConditionTrue cond_result
                                                                        then evaluateExpressions (getEvaluatedKcontext cond_result) exprs
                                                                        else EvaluationResult context NilVal 
evaluateIf context (KP.If cond_expr exprs (Just else_exprs)) = let cond_result = evaluateExpression context cond_expr in
                                                                    if isConditionTrue cond_result
                                                                        then evaluateExpressions (getEvaluatedKcontext cond_result) exprs
                                                                        else evaluateExpressions (getEvaluatedKcontext cond_result) else_exprs

evaluateWhile :: Kcontext -> KP.While -> EvaluationResult
evaluateWhile context = evaluateWhile' (EvaluationResult context NilVal)

evaluateWhile' :: EvaluationResult -> KP.While -> EvaluationResult
evaluateWhile' result@(EvaluationResult context _) while_expr@(KP.While cond_expr exprs) = let cond_result = evaluateExpression context cond_expr in
                                                                                            if isConditionTrue cond_result
                                                                                                then evaluateWhile' (EvaluationResult (getEvaluatedKcontext cond_result) NilVal) while_expr
                                                                                                else result

evaluateExpressionList :: Kcontext -> [KP.Expression] -> EvaluationResult
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
    | getBinOpPrecedence context left <= getBinOpPrecedence context right = True
    | otherwise                                                           = False

getBinOpPrecedence :: Kcontext -> KP.BinaryOp -> Int
getBinOpPrecedence context binop = let signature = kContextFindFunction (KTC.toIdentifier binop) context
    in case signature of
        PrimitiveFunction ((PrimBinaryFunction (KP.Precedence  pre) _ _):_) -> pre
        RefinedFunction   (BinaryFunction      (KP.Precedence  pre) _ _   ) -> pre
        _                                                                   -> error "getBinOpPrecedence"


------------------------------------------

evaluateExpression' :: Kcontext -> BinaryTreeExpression -> EvaluationResult
evaluateExpression' context (ExprLeaf unary)                            = evaluateUnary context unary
evaluateExpression' context (ExprNode (KP.BinaryOp (KP.Identifier "=")) left@(ExprLeaf (KP.UnaryPostfix (KP.Postfix (KP.PrimaryIdentifier identifier) Nothing) )) right)
                                                                        = let right_result = evaluateExpression' context right in
                                                                          evaluateAssignment identifier (getEvaluatedValue right_result) (getEvaluatedKcontext right_result)
evaluateExpression' context (ExprNode (KP.BinaryOp binary) left right)  = let left_result  = evaluateExpression' context left                                   in
                                                                          let right_result = evaluateExpression' (getEvaluatedKcontext left_result)  right       in
                                                                          evaluateBinaryOperation (getEvaluatedKcontext right_result) binary (getEvaluatedValue left_result) (getEvaluatedValue right_result)
evaluateAssignment ::  KP.Identifier -> Value -> Kcontext -> EvaluationResult
evaluateAssignment identifier value context = EvaluationResult (kContextPushVariable identifier value context) value

evaluateUnary :: Kcontext -> KP.Unary -> EvaluationResult
evaluateUnary context (KP.Unary (KP.UnaryOp identifier) unary) =
    let next  = evaluateUnary context unary   in
    evaluateUnaryOperation (getEvaluatedKcontext next) identifier (getEvaluatedValue next)
evaluateUnary context (KP.UnaryPostfix postfix) = evaluatePostfix context postfix

evaluateUnaryOperation :: Kcontext -> KP.Identifier -> Value -> EvaluationResult
evaluateUnaryOperation context func_name arg = evaluateUnaryOperation' context arg (KEC.kContextFindFunction func_name context )

evaluateUnaryOperation' :: Kcontext -> Value -> Signature -> EvaluationResult
evaluateUnaryOperation' context arg (PrimitiveFunction func_list) = evaluatePrimitiveFunctionCall context [arg] func_list
evaluateUnaryOperation' context arg (RefinedFunction func)        = evaluateRefinedFunctionCall   context [arg] func

evaluateBinaryOperation :: Kcontext -> KP.Identifier -> Value -> Value -> EvaluationResult
evaluateBinaryOperation context func_name arg1 arg2 = evaluateBinaryOperation' context arg1 arg2 (KEC.kContextFindFunction func_name context )

evaluateBinaryOperation' :: Kcontext -> Value -> Value -> Signature -> EvaluationResult
evaluateBinaryOperation' context arg1 arg2 (PrimitiveFunction func_list) = evaluatePrimitiveFunctionCall context [arg1, arg2] func_list
evaluateBinaryOperation' context arg1 arg2 (RefinedFunction func)        = evaluateRefinedFunctionCall   context [arg1, arg2] func

evaluatePostfix :: Kcontext -> KP.Postfix -> EvaluationResult
evaluatePostfix context (KP.Postfix (KP.PrimaryLiteral    (KP.LiteralDecimal (KP.DecimalConst v))) Nothing) = EvaluationResult context (IntVal v)
evaluatePostfix context (KP.Postfix (KP.PrimaryLiteral    (KP.LiteralDouble  (KP.DoubleConst  v))) Nothing) = EvaluationResult context (DoubleVal v)
evaluatePostfix context (KP.Postfix (KP.PrimaryIdentifier identifier           ) Nothing)                   = evaluatePostfixVar      context identifier
evaluatePostfix context (KP.Postfix (KP.PrimaryIdentifier identifier           ) (Just call_expr))          = evaluateFunctionCall    context identifier call_expr
evaluatePostfix context (KP.Postfix (KP.PrimaryExpressions expressions         ) Nothing)                   = evaluateExpressions     context expressions
evaluatePostfix context _ = error ""

evaluatePostfixVar :: Kcontext -> KP.Identifier -> EvaluationResult
evaluatePostfixVar context identifier = EvaluationResult context (kContextFindVariable identifier context)

evaluateFunctionCall :: Kcontext -> KP.Identifier -> KP.CallExpression -> EvaluationResult
evaluateFunctionCall context identifier (KP.CallExpression Nothing)                                 = evaluateFunctionCall' context identifier []
evaluateFunctionCall context identifier (KP.CallExpression (Just (KP.CallExpressionArgs arg args))) = evaluateFunctionCall' context identifier (map (getEvaluatedValue . evaluateExpression context) (arg:args))

evaluateFunctionCall' :: Kcontext -> KP.Identifier -> [Value] -> EvaluationResult
evaluateFunctionCall' context identifier args = evaluateFunctionCall'' context args (kContextFindFunction identifier context)

evaluateFunctionCall'' :: Kcontext -> [Value] -> Signature -> EvaluationResult
evaluateFunctionCall'' context args (RefinedFunction   function)  = evaluateRefinedFunctionCall   context args function
evaluateFunctionCall'' context args (PrimitiveFunction functions) = evaluatePrimitiveFunctionCall context args functions

evaluateRefinedFunctionCall :: Kcontext -> [Value] -> Function -> EvaluationResult
evaluateRefinedFunctionCall context args (UnaryFunction    pargs exprs) = evaluateExpressions (kContextEnterLocalContext context pargs args) exprs
evaluateRefinedFunctionCall context args (BinaryFunction _ pargs exprs) = evaluateExpressions (kContextEnterLocalContext context pargs args) exprs
evaluateRefinedFunctionCall context args (Function         pargs exprs) = evaluateExpressions (kContextEnterLocalContext context pargs args) exprs

evaluatePrimitiveFunctionCall :: Kcontext -> [Value] -> [PrimFunction] -> EvaluationResult
evaluatePrimitiveFunctionCall = 

isConditionTrue :: EvaluationResult -> Bool
isConditionTrue (EvaluationResult _ NilVal           )  = False
isConditionTrue (EvaluationResult _ (BooleanVal False)) = False
isConditionTrue (EvaluationResult _ (IntVal     0    )) = False
isConditionTrue (EvaluationResult _ (DoubleVal  0.0  )) = False
isConditionTrue _                                       = True

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
