--
-- EPITECH PROJECT, 2022
-- koak
-- File description:
-- Koak.Evaluator
--
module Koak.Evaluator                  ( KoakEvaluation(..)
                                       , evaluateKoak
                                       ) where

import qualified Koak.Parser          as KP
import qualified Koak.TypingContext   as KTC
import qualified Koak.EvaluatorContext as KEC

data EvaluationResult = EvaluationResult KEC.Kcontext KEC.Value

data KoakEvaluation = KoakEvaluation [KEC.Value] KEC.Kcontext
    deriving (Eq, Show)

evaluateKoak :: [KP.Kdefs] -> KoakEvaluation
evaluateKoak = foldl evaluateKdef (KoakEvaluation [] KEC.getDefaultKContext)

evaluateKdef :: KoakEvaluation -> KP.Kdefs -> KoakEvaluation
evaluateKdef (KoakEvaluation evaluations context) (KP.KdefDef        def  ) = KoakEvaluation evaluations $ KEC.kContextPushFunction def context
evaluateKdef (KoakEvaluation evaluations context) (KP.KdefExpression exprs) = let evaluation = evaluateExpressions context exprs in
                                                                              KoakEvaluation
                                                                                (getEvaluatedValue    evaluation:evaluations)
                                                                                (getEvaluatedKcontext evaluation)

evaluateExpressions :: KEC.Kcontext -> KP.Expressions -> EvaluationResult
evaluateExpressions context (KP.ExpressionFor    for_expr  ) = evaluateFor            context for_expr
evaluateExpressions context (KP.ExpressionIf     if_expr   ) = evaluateIf             context if_expr
evaluateExpressions context (KP.ExpressionWhile  while_expr) = evaluateWhile          context while_expr
evaluateExpressions context (KP.Expressions      expr exprs) = evaluateExpressionList context (expr:exprs)

evaluateFor :: KEC.Kcontext -> KP.For -> EvaluationResult
evaluateFor context (KP.For assign_expr cond_expr inc_expr exprs) = let assign_result = evaluateExpression context assign_expr
                                                                    in evaluateFor' (EvaluationResult (getEvaluatedKcontext assign_result) KEC.NilVal) cond_expr inc_expr exprs

evaluateFor' :: EvaluationResult -> KP.Expression -> KP.Expression -> KP.Expressions -> EvaluationResult
evaluateFor' result@(EvaluationResult context _) cond_expr inc_expr exprs = let cond_result = evaluateExpression context cond_expr in
                                                if isConditionTrue cond_result then (let inc_result  = evaluateExpression   (getEvaluatedKcontext cond_result) inc_expr
                                                                                         new_context = getEvaluatedKcontext inc_result
                                                                                         expr_result = evaluateExpressions  new_context exprs
                                                                                     in evaluateFor' expr_result cond_expr inc_expr exprs)
                                                                                else result

evaluateIf :: KEC.Kcontext -> KP.If -> EvaluationResult
evaluateIf context (KP.If cond_expr exprs Nothing)           = let cond_result = evaluateExpression context cond_expr in
                                                                    if isConditionTrue cond_result
                                                                        then evaluateExpressions (getEvaluatedKcontext cond_result) exprs
                                                                        else EvaluationResult context KEC.NilVal 
evaluateIf context (KP.If cond_expr exprs (Just else_exprs)) = let cond_result = evaluateExpression context cond_expr in
                                                                    if isConditionTrue cond_result
                                                                        then evaluateExpressions (getEvaluatedKcontext cond_result) exprs
                                                                        else evaluateExpressions (getEvaluatedKcontext cond_result) else_exprs

evaluateWhile :: KEC.Kcontext -> KP.While -> EvaluationResult
evaluateWhile context = evaluateWhile' (EvaluationResult context KEC.NilVal)

evaluateWhile' :: EvaluationResult -> KP.While -> EvaluationResult
evaluateWhile' result@(EvaluationResult context _) while_expr@(KP.While cond_expr exprs) = let cond_result = evaluateExpression context cond_expr
                                                                                               exprs_result = evaluateExpressions (getEvaluatedKcontext cond_result) exprs
                                                                                           in
                                                                                           if isConditionTrue cond_result
                                                                                              then evaluateWhile' (EvaluationResult (getEvaluatedKcontext exprs_result) KEC.NilVal) while_expr
                                                                                              else result

evaluateExpressionList :: KEC.Kcontext -> [KP.Expression] -> EvaluationResult
evaluateExpressionList _ []           = error "this should never happend :)"
evaluateExpressionList context [expr] = evaluateExpression context expr
evaluateExpressionList context (x:xs) = let expr_result = evaluateExpression context x
                                        in evaluateExpressionList (getEvaluatedKcontext expr_result) xs

evaluateExpression :: KEC.Kcontext -> KP.Expression -> EvaluationResult
evaluateExpression context expression = evaluateExpression' context $ buildExpressionTree context expression

----------------evalexpr------------------

data BinaryTreeExpression = ExprNode KP.BinaryOp BinaryTreeExpression BinaryTreeExpression | ExprLeaf KP.Unary
    deriving (Show, Eq)

data UnitExpression = Un KP.Unary | Bin KP.BinaryOp
    deriving (Show, Eq)


buildExpressionTree :: KEC.Kcontext -> KP.Expression -> BinaryTreeExpression
buildExpressionTree context expression@(KP.Expression unary _) = buildExpressionTree' context (convertExpressionToList context expression) $ ExprLeaf unary

buildExpressionTree' :: KEC.Kcontext -> [UnitExpression] -> BinaryTreeExpression -> BinaryTreeExpression
buildExpressionTree' _       []               tree              = tree
buildExpressionTree' context (x:xs)           node@(ExprLeaf _) = callbackCreateBinaryNode context x xs node
buildExpressionTree' context (bin:(Un un):xs) tree              = buildExpressionTree' context xs $ placeTokenInTree context tree bin $ ExprLeaf un
buildExpressionTree' _       _                _                 = error "In buildExpressionTree"

convertExpressionToList :: KEC.Kcontext -> KP.Expression -> [UnitExpression]
convertExpressionToList _       (KP.Expression _ [])                     = []
convertExpressionToList context (KP.Expression _ ((binop, second) : xs)) = Bin binop : Un second : convertExpressionToList context (KP.Expression second xs)

placeTokenInTree :: KEC.Kcontext -> BinaryTreeExpression -> UnitExpression -> BinaryTreeExpression -> BinaryTreeExpression
placeTokenInTree _       node@(ExprLeaf _) (Bin newOp) tree = ExprNode newOp node tree
placeTokenInTree context base@(ExprNode op left right) (Bin newOp) tree
    | isLessPrio context newOp op                           = ExprNode newOp base tree
    | otherwise                                             = ExprNode op left $ placeTokenInTree context right (Bin newOp) tree
placeTokenInTree _ _ _ _                                    = error "In placeTokenInTree"

callbackCreateBinaryNode :: KEC.Kcontext -> UnitExpression -> [UnitExpression] -> BinaryTreeExpression -> BinaryTreeExpression
callbackCreateBinaryNode context (Bin binop) ((Un unRight):xs) unLeft = buildExpressionTree' context xs $ ExprNode binop unLeft (ExprLeaf unRight)
callbackCreateBinaryNode _ _ _ _ = error "In callbackCreateBinaryNode"

isLessPrio :: KEC.Kcontext -> KP.BinaryOp -> KP.BinaryOp -> Bool
isLessPrio context left right
    | getBinOpPrecedence context left <= getBinOpPrecedence context right = True
    | otherwise                                                           = False

getBinOpPrecedence :: KEC.Kcontext -> KP.BinaryOp -> Int
getBinOpPrecedence context binop = let signature = KEC.kContextFindFunction (KTC.toIdentifier binop) context 
    in case signature of
        KEC.PrimitiveFunction (KEC.PrimBinaryFunction (KP.Precedence  pre) _  ) -> pre
        KEC.RefinedFunction   (KEC.BinaryFunction     (KP.Precedence  pre) _ _) -> pre
        _                                                                       -> error "getBinOpPrecedence"

------------------------------------------

evaluateExpression' :: KEC.Kcontext -> BinaryTreeExpression -> EvaluationResult
evaluateExpression' context (ExprLeaf unary)                            = evaluateUnary context unary
evaluateExpression' context (ExprNode (KP.BinaryOp (KP.Identifier "=")) (ExprLeaf (KP.UnaryPostfix (KP.Postfix (KP.PrimaryIdentifier identifier) Nothing) )) right)
                                                                        = let right_result = evaluateExpression' context right                             in
                                                                          evaluateAssignment identifier (getEvaluatedValue right_result) (getEvaluatedKcontext right_result)
evaluateExpression' context (ExprNode (KP.BinaryOp binary) left right)  = let left_result  = evaluateExpression' context left                              in
                                                                          let right_result = evaluateExpression' (getEvaluatedKcontext left_result)  right in
                                                                          evaluateBinaryOperation (getEvaluatedKcontext right_result) binary (getEvaluatedValue left_result) (getEvaluatedValue right_result)

evaluateAssignment ::  KP.Identifier -> KEC.Value -> KEC.Kcontext -> EvaluationResult
evaluateAssignment identifier value context = EvaluationResult (KEC.kContextPushVariable identifier value context) value

evaluateUnary :: KEC.Kcontext -> KP.Unary -> EvaluationResult
evaluateUnary context (KP.Unary (KP.UnaryOp identifier) unary) =
    let next  = evaluateUnary context unary   in
    evaluateUnaryOperation (getEvaluatedKcontext next) identifier (getEvaluatedValue next)
evaluateUnary context (KP.UnaryPostfix postfix) = evaluatePostfix context postfix

evaluateUnaryOperation :: KEC.Kcontext -> KP.Identifier -> KEC.Value -> EvaluationResult
evaluateUnaryOperation context func_name arg = evaluateUnaryOperation' context arg (KEC.kContextFindFunction func_name context )

evaluateUnaryOperation' :: KEC.Kcontext -> KEC.Value -> KEC.Signature -> EvaluationResult
evaluateUnaryOperation' context arg (KEC.PrimitiveFunction func_list) = evaluatePrimitiveFunctionCall context [arg] func_list
evaluateUnaryOperation' context arg (KEC.RefinedFunction func)        = evaluateRefinedFunctionCall   context [arg] func

evaluateBinaryOperation :: KEC.Kcontext -> KP.Identifier -> KEC.Value -> KEC.Value -> EvaluationResult
evaluateBinaryOperation context func_name arg1 arg2 = evaluateBinaryOperation' context arg1 arg2 (KEC.kContextFindFunction func_name context )

evaluateBinaryOperation' :: KEC.Kcontext -> KEC.Value -> KEC.Value -> KEC.Signature -> EvaluationResult
evaluateBinaryOperation' context arg1 arg2 (KEC.PrimitiveFunction func_list) = evaluatePrimitiveFunctionCall context [arg1, arg2] func_list
evaluateBinaryOperation' context arg1 arg2 (KEC.RefinedFunction func)        = evaluateRefinedFunctionCall   context [arg1, arg2] func

evaluatePostfix :: KEC.Kcontext -> KP.Postfix -> EvaluationResult
evaluatePostfix context (KP.Postfix (KP.PrimaryLiteral    (KP.LiteralDecimal (KP.DecimalConst v))) Nothing) = EvaluationResult context (KEC.IntVal v)
evaluatePostfix context (KP.Postfix (KP.PrimaryLiteral    (KP.LiteralDouble  (KP.DoubleConst  v))) Nothing) = EvaluationResult context (KEC.DoubleVal v)
evaluatePostfix context (KP.Postfix (KP.PrimaryIdentifier identifier           ) Nothing)                   = evaluatePostfixVar      context identifier
evaluatePostfix context (KP.Postfix (KP.PrimaryIdentifier identifier           ) (Just call_expr))          = evaluateFunctionCall    context identifier call_expr
evaluatePostfix context (KP.Postfix (KP.PrimaryExpressions expressions         ) Nothing)                   = evaluateExpressions     context expressions
evaluatePostfix _ _                                                                                         = error ""

evaluatePostfixVar :: KEC.Kcontext -> KP.Identifier -> EvaluationResult
evaluatePostfixVar context identifier = EvaluationResult context (KEC.kContextFindVariable identifier context)

evaluateFunctionCall :: KEC.Kcontext -> KP.Identifier -> KP.CallExpression -> EvaluationResult
evaluateFunctionCall context identifier (KP.CallExpression Nothing)                                 = evaluateFunctionCall' context identifier []
evaluateFunctionCall context identifier (KP.CallExpression (Just (KP.CallExpressionArgs arg args))) = evaluateFunctionCall' context identifier (map (getEvaluatedValue . evaluateExpression context) (arg:args))

evaluateFunctionCall' :: KEC.Kcontext -> KP.Identifier -> [KEC.Value] -> EvaluationResult
evaluateFunctionCall' context identifier args = evaluateFunctionCall'' context args (KEC.kContextFindFunction identifier context)

evaluateFunctionCall'' :: KEC.Kcontext -> [KEC.Value] -> KEC.Signature -> EvaluationResult
evaluateFunctionCall'' context args (KEC.RefinedFunction   function)  = evaluateRefinedFunctionCall   context args function
evaluateFunctionCall'' context args (KEC.PrimitiveFunction functions) = evaluatePrimitiveFunctionCall context args functions

evaluateRefinedFunctionCall :: KEC.Kcontext -> [KEC.Value] -> KEC.Function -> EvaluationResult
evaluateRefinedFunctionCall context args (KEC.UnaryFunction    pargs exprs) = evaluateExpressions (KEC.kContextEnterLocalContext context pargs args) exprs
evaluateRefinedFunctionCall context args (KEC.BinaryFunction _ pargs exprs) = evaluateExpressions (KEC.kContextEnterLocalContext context pargs args) exprs
evaluateRefinedFunctionCall context args (KEC.Function         pargs exprs) = evaluateExpressions (KEC.kContextEnterLocalContext context pargs args) exprs

evaluatePrimitiveFunctionCall :: KEC.Kcontext -> [KEC.Value] -> KEC.PrimFunction -> EvaluationResult
evaluatePrimitiveFunctionCall context (x:_)    (KEC.PrimUnaryFunction    f) = EvaluationResult context $ f x
evaluatePrimitiveFunctionCall context (x:x':_) (KEC.PrimBinaryFunction _ f) = EvaluationResult context $ f x x'
evaluatePrimitiveFunctionCall context args     (KEC.PrimFunction         f) = EvaluationResult context $ f args
evaluatePrimitiveFunctionCall _       _        _                        = error "evaluatePrimitiveFunctionCall"

isConditionTrue :: EvaluationResult -> Bool
isConditionTrue (EvaluationResult _ KEC.NilVal           )  = False
isConditionTrue (EvaluationResult _ (KEC.BooleanVal False)) = False
isConditionTrue (EvaluationResult _ (KEC.IntVal     0    )) = False
isConditionTrue (EvaluationResult _ (KEC.DoubleVal  0.0  )) = False
isConditionTrue _                                       = True

getEvaluatedValue :: EvaluationResult -> KEC.Value
getEvaluatedValue (EvaluationResult _ value) = value

getEvaluatedKcontext :: EvaluationResult -> KEC.Kcontext
getEvaluatedKcontext (EvaluationResult context _) = context
