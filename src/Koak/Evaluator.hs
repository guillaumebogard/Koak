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
        PrimitiveFunction ((PrimBinaryFunction (KP.Precedence  pre) _  ):_) -> pre
        RefinedFunction   (BinaryFunction      (KP.Precedence  pre) _ _)    -> pre
        _                                                                   -> error "getBinOpPrecedence"


------------------------------------------

evaluateExpression' :: Kcontext -> BinaryTreeExpression -> EvaluationResult
evaluateExpression' context (ExprLeaf unary)                            = evaluateUnary context unary
evaluateExpression' context (ExprNode binary@(KP.BinaryOp (KP.Identifier "=")) left@(ExprLeaf (KP.UnaryPostfix (KP.Postfix (KP.PrimaryIdentifier identifier) Nothing) )) right)
                                                                        = let right_result = evaluateUnary context right in right_result -- add identifier and right result value to context
evaluateExpression' context (ExprNode (KP.BinaryOp binary) left right)  = let leftResult  = evaluateExpression' context left                                   in
                                                                          let rightResult = evaluateExpression' (getEvaluatedKcontext leftResult)  right       in
                                                                              evaluateBinOperation (getEvaluatedKcontext rightResult) binary (getEvaluatedType leftResult) (getEvaluatedType rightResult)

evaluateUnary :: Kcontext -> KP.Unary -> EvaluationResult
evaluateUnary context (KP.Unary (KP.UnaryOp identifier) unary) =
    let found = kContextFind context identifier     in
    let next  = evaluateUnaryTyping context unary   in
    EvaluationResult
        (getFunctionReturnType $
            findUnaryMatchingFunction
                (getEvaluatedKcontext next)
                identifier
                (getEvaluatedType     next)
        )
        (getEvaluatedKcontext next)
evaluateUnaryTyping context (KP.UnaryPostfix postfix) = evaluatePostfixTyping context postfix

evaluateUnaryOperation :: Kcontext -> KP.Identifier -> Value -> EvaluationResult
evaluateUnaryOperation context func_name arg = evaluateUnaryOperation' context arg (KEC.kContextFindFunction context func_name)

evaluateUnaryOperation' :: Kcontext -> Value -> Signature -> EvaluationResult
evaluateUnaryOperation' context arg (PrimitiveFunction func_list) =
    let found = find (isUnaryFunctionParamMatchingFunction arg) func_list
    in case found of
    Nothing              -> error "evaluateUnaryOperation"
    (Just matching_func) -> matching_func
evaluateUnaryOperation' context arg (RefinedFunction func)        = evaluateUnaryOperationCall context arg func

evaluateUnaryOperationCall :: Kcontext -> Value -> Function -> Value
evaluateUnaryOperationCall context arg (KEC.UnaryFunction pargs exprs) = 

evaluateUnaryOperationPrimCall :: Kcontext -> Value -> PrimFunction -> Value


evaluatePostfix :: Kcontext -> KP.Postfix -> EvaluationResult
evaluatePostfix context (KP.Postfix (KP.PrimaryLiteral    (KP.LiteralDecimal _)) Nothing)          = EvaluationResult Int    context
evaluatePostfix context (KP.Postfix (KP.PrimaryLiteral    (KP.LiteralDouble  _)) Nothing)          = EvaluationResult Double context
evaluatePostfix context (KP.Postfix (KP.PrimaryIdentifier identifier           ) Nothing)          = evaluatePostfixTypingVar   context identifier
evaluatePostfix context (KP.Postfix (KP.PrimaryIdentifier identifier           ) (Just call_expr)) = evaluateFunctionCallTyping context identifier call_expr
evaluatePostfix context (KP.Postfix (KP.PrimaryExpressions expressions         ) Nothing)          = evaluateExpressionsTyping  context expressions
evaluatePostfix context _ = error ""

evaluatePostfixVar :: Kcontext -> KP.Identifier -> EvaluationResult
evaluatePostfixVar context identifier = evaluatePostfixTypingVar' context identifier $ kContextFind context identifier

evaluatePostfixVar' :: Kcontext -> KP.Identifier -> Maybe TypeSignature -> EvaluationResult
evaluatePostfixVar' context identifier Nothing               = throw $ UnknownDefinition identifier
evaluatePostfixVar' context identifier (Just (Var var_type)) = EvaluationResult var_type context
evaluatePostfixVar' context identifier (Just _             ) = throw $ NotAVar identifier

evaluateFunctionCall :: Kcontext -> KP.Identifier -> KP.CallExpression -> EvaluationResult
evaluateFunctionCall context identifier (KP.CallExpression Nothing)                                 = evaluateFunctionCallTyping' context identifier []
evaluateFunctionCall context identifier (KP.CallExpression (Just (KP.CallExpressionArgs arg args))) = evaluateFunctionCallTyping' context identifier []

evaluateFunctionCall' :: Kcontext -> KP.Identifier -> [BaseType] -> EvaluationResult
evaluateFunctionCall' context identifier args = evaluateFunctionCallTyping'' context identifier args (kContextFind context identifier)

evaluateFunctionCall'' :: Kcontext -> KP.Identifier -> [BaseType] -> Maybe TypeSignature -> EvaluationResult
evaluateFunctionCall'' context identifier args Nothing             = throw $ UnknownDefinition identifier
evaluateFunctionCall'' context identifier args (Just (Function _)) = error ""
evaluateFunctionCall'' context identifier args (Just _)            = error ""


isConditionTrue :: EvaluationResult -> Bool
isConditionTrue (EvaluationResult _ NilVal           )  = False
isConditionTrue (EvaluationResult _ (BooleanVal False)) = False
isConditionTrue (EvaluationResult _ (IntVal     0    )) = False
isConditionTrue (EvaluationResult _ (BooleanVal 0.0  )) = False
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
