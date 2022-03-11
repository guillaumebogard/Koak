--
-- EPITECH PROJECT, 2022
-- koak
-- File description:
-- Koak.Typing
--

module Koak.Typing                            ( checkKoakTyping ) where

import Control.Exception                      ( throw )
import Data.Foldable                          ( find )

import qualified Koak.Typing.Exception as KTE ( KoakTypingException(..) )

import qualified Koak.TypingContext    as KTC ( Kcontext(..)
                                              , BaseType(..)
                                              , FunctionTyping(..)
                                              , TypeSignature(..)
                                              , kContextEnterFunctionCall
                                              , baseTypeToType
                                              , kContextFind
                                              , kContextPushFunction
                                              , kContextPushVar
                                              , isUnaryFunctionParamMatchingFunction
                                              , isBinaryFunctionParamMatchingFunction
                                              , getFunctionReturnType
                                              , toIdentifier
                                              , prototypeToBaseType
                                              , isFunctionParamMatchingFunction
                                              )
import qualified Koak.Parser           as KP

data EvaluationResult = EvaluationResult KTC.BaseType KTC.Kcontext

checkKoakTyping :: KP.Stmt -> KTC.Kcontext -> Either KTE.KoakTypingException KTC.Kcontext
checkKoakTyping stmt = checkKoakTyping' $ KP.getKdefsFromStmt stmt

checkKoakTyping' :: [KP.Kdefs] -> KTC.Kcontext -> Either KTE.KoakTypingException KTC.Kcontext
checkKoakTyping' kdefs context = foldrOnRight checkKdefTyping (Right context) kdefs

foldrOnRight :: (a -> b -> Either c b) -> Either c b -> [a] -> Either c b
foldrOnRight _  left@(Left _)       _     = left
foldrOnRight f right@(Right value) (x:xs) = foldrOnRight f (f x value) xs
foldrOnRight f       value         _      = value

checkKdefTyping :: KP.Kdefs -> KTC.Kcontext -> Either KTE.KoakTypingException KTC.Kcontext
checkKdefTyping (KP.KdefDef        defs ) context = checkDefsTyping defs context
checkKdefTyping (KP.KdefExpression exprs) context = getEvaluatedKcontext <$> evaluateExpressionsTyping exprs context

checkDefsTyping :: KP.Defs -> KTC.Kcontext -> Either KTE.KoakTypingException KTC.Kcontext
checkDefsTyping def@(KP.Defs proto _) context = checkDefsTyping' def (KTC.kContextPushFunction proto context)

checkDefsTyping' :: KP.Defs -> KTC.Kcontext -> Either KTE.KoakTypingException KTC.Kcontext
checkDefsTyping' (KP.Defs proto exprs) context = checkDefsTypingSub (checkDefsTyping'' (KTC.toIdentifier proto) (KTC.prototypeToBaseType proto)) (evaluateExpressionsTyping exprs (KTC.kContextEnterFunctionCall proto context)) context

checkDefsTypingSub :: (KTC.BaseType -> KTC.Kcontext -> Either KTE.KoakTypingException KTC.Kcontext) -> Either KTE.KoakTypingException EvaluationResult -> KTC.Kcontext -> Either KTE.KoakTypingException KTC.Kcontext
checkDefsTypingSub f (Left  value) context = Left value
checkDefsTypingSub f (Right value) context = f (getEvaluatedType value) context

checkDefsTyping'' :: KP.Identifier -> KTC.BaseType -> KTC.BaseType -> KTC.Kcontext -> Either KTE.KoakTypingException KTC.Kcontext
checkDefsTyping'' funcName expectedType evaluatedType context
    | expectedType == evaluatedType = Right context
    | otherwise                     = throw $ KTE.KoakTypingMismatchedReturnType funcName (KTC.baseTypeToType expectedType) $ KTC.baseTypeToType evaluatedType

evaluateExpressionsTyping :: KP.Expressions -> KTC.Kcontext -> Either KTE.KoakTypingException EvaluationResult
evaluateExpressionsTyping (KP.ExpressionFor   forExpr        ) = evaluateForTyping            forExpr
evaluateExpressionsTyping (KP.ExpressionIf    ifExpr         ) = evaluateIfTyping             ifExpr
evaluateExpressionsTyping (KP.ExpressionWhile whileExpr      ) = evaluateWhileTyping          whileExpr
evaluateExpressionsTyping (KP.Expressions     expr      exprs) = evaluateExpressionListTyping (expr : exprs)

evaluateExpressionListTyping :: [KP.Expression] -> KTC.Kcontext -> Either KTE.KoakTypingException EvaluationResult
evaluateExpressionListTyping []     _       = error "This should never happen :)"
evaluateExpressionListTyping [x]    context = evaluateExpressionTyping x context
evaluateExpressionListTyping (x:xs) context = evaluateExpressionListTypingSub (evaluateExpressionTyping x context) (evaluateExpressionListTyping xs)

evaluateExpressionListTypingSub :: Either KTE.KoakTypingException EvaluationResult -> (KTC.Kcontext -> Either KTE.KoakTypingException EvaluationResult) -> Either KTE.KoakTypingException EvaluationResult
evaluateExpressionListTypingSub (Left value ) f = Left value
evaluateExpressionListTypingSub (Right value) f = f $ getEvaluatedKcontext value

evaluateForTyping :: KP.For -> KTC.Kcontext -> Either KTE.KoakTypingException EvaluationResult
evaluateForTyping    forExpr@(KP.For assignExpr _ _ _) context = evaluateTypingSub (evaluateForTyping'   forExpr . getEvaluatedKcontext) (evaluateExpressionTyping assignExpr context)

evaluateTypingSub :: (EvaluationResult -> Either KTE.KoakTypingException EvaluationResult) -> Either KTE.KoakTypingException EvaluationResult -> Either KTE.KoakTypingException EvaluationResult
evaluateTypingSub _ (Left value ) = Left value
evaluateTypingSub f (Right value) = f value

evaluateForTyping' :: KP.For -> KTC.Kcontext -> Either KTE.KoakTypingException EvaluationResult
evaluateForTyping'   forExpr@(KP.For _ condExpr _ _)   context = evaluateTypingSub (evaluateForTyping''  forExpr . getEvaluatedKcontext) (evaluateExpressionTyping condExpr context)

evaluateForTyping'' :: KP.For -> KTC.Kcontext -> Either KTE.KoakTypingException EvaluationResult
evaluateForTyping''  forExpr@(KP.For _ _ incExpr _)    context = evaluateTypingSub (evaluateForTyping''' forExpr . getEvaluatedKcontext) (evaluateExpressionTyping incExpr context)

evaluateForTyping''' :: KP.For -> KTC.Kcontext -> Either KTE.KoakTypingException EvaluationResult
evaluateForTyping''' (KP.For _ _ _ exprs) = evaluateExpressionsTyping exprs

evaluateIfTyping :: KP.If -> KTC.Kcontext -> Either KTE.KoakTypingException EvaluationResult
evaluateIfTyping  ifExpr@(KP.If condExpr _ _)  context = evaluateTypingSub (evaluateIfTyping'  ifExpr . getEvaluatedKcontext) (evaluateExpressionTyping condExpr context)

evaluateIfTyping' :: KP.If -> KTC.Kcontext -> Either KTE.KoakTypingException EvaluationResult
evaluateIfTyping' ifExpr@(KP.If _ thenExprs _) context = evaluateTypingSub (evaluateIfTyping'' ifExpr) (evaluateExpressionsTyping thenExprs context)

evaluateIfTyping'' :: KP.If -> EvaluationResult -> Either KTE.KoakTypingException EvaluationResult
evaluateIfTyping''       (KP.If _ _ Nothing     ) thenResult                              = Right thenResult
evaluateIfTyping''       (KP.If _ _ (Just exprs)) (EvaluationResult thenType thenContext) = evaluateTypingSub (evaluateIfTyping''' thenType) (evaluateExpressionsTyping exprs thenContext)

evaluateIfTyping''' :: KTC.BaseType -> EvaluationResult -> Either KTE.KoakTypingException EvaluationResult
evaluateIfTyping''' thenType elseResult
    | thenType == getEvaluatedType elseResult = Right elseResult
    | otherwise                               = Left $ KTE.KoakTypingMismatchedThenElseType (KTC.baseTypeToType thenType) $ KTC.baseTypeToType $ getEvaluatedType elseResult

evaluateWhileTyping :: KP.While -> KTC.Kcontext -> Either KTE.KoakTypingException EvaluationResult
evaluateWhileTyping whileExpr@(KP.While condExpr _) context = evaluateTypingSub' (evaluateWhileTyping' whileExpr . getEvaluatedKcontext) (evaluateExpressionTyping condExpr context) context

evaluateTypingSub' :: (EvaluationResult -> Either KTE.KoakTypingException EvaluationResult) -> Either KTE.KoakTypingException EvaluationResult -> KTC.Kcontext -> Either KTE.KoakTypingException EvaluationResult
evaluateTypingSub' f (Left  value) context = Left value
evaluateTypingSub' f (Right value) context = f value

evaluateWhileTyping' :: KP.While -> KTC.Kcontext -> Either KTE.KoakTypingException EvaluationResult
evaluateWhileTyping'          (KP.While _ exprs)    = evaluateExpressionsTyping exprs

evaluateExpressionTyping :: KP.Expression -> KTC.Kcontext -> Either KTE.KoakTypingException EvaluationResult
evaluateExpressionTyping expression context =  evaluateExpressionTypingSub (evaluateExpressionTyping' context) (buildExpressionTree context expression)

evaluateExpressionTypingSub :: (BinaryTreeExpression -> Either KTE.KoakTypingException EvaluationResult) -> Either KTE.KoakTypingException BinaryTreeExpression -> Either KTE.KoakTypingException EvaluationResult
evaluateExpressionTypingSub f (Left  value) = Left value
evaluateExpressionTypingSub f (Right value) = f value

-------------------evalexpr------------------

data BinaryTreeExpression = ExprNode KP.BinaryOp BinaryTreeExpression BinaryTreeExpression | ExprLeaf KP.Unary
    deriving (Show, Eq)

data UnitExpression = Un KP.Unary | Bin KP.BinaryOp
    deriving (Show, Eq)

buildExpressionTree :: KTC.Kcontext -> KP.Expression -> Either KTE.KoakTypingException BinaryTreeExpression
buildExpressionTree context expression@(KP.Expression unary _) = buildExpressionTree' context (convertExpressionToList context expression) $ ExprLeaf unary

buildExpressionTree' :: KTC.Kcontext -> [UnitExpression] -> BinaryTreeExpression -> Either KTE.KoakTypingException BinaryTreeExpression
buildExpressionTree' _       []               tree              = Right tree
buildExpressionTree' context (x:xs)           node@(ExprLeaf _) = callbackCreateBinaryNode context x xs node
buildExpressionTree' context (bin:(Un un):xs) tree              = buildExpressionTreeSub (buildExpressionTree' context xs) (placeTokenInTree context tree bin $ ExprLeaf un)
buildExpressionTree' _       _                _                 = error "In buildExpressionTree"

buildExpressionTreeSub :: (BinaryTreeExpression -> Either KTE.KoakTypingException BinaryTreeExpression) -> Either KTE.KoakTypingException BinaryTreeExpression -> Either KTE.KoakTypingException BinaryTreeExpression
buildExpressionTreeSub f (Left value) = Left value
buildExpressionTreeSub f (Right value) = f value

convertExpressionToList :: KTC.Kcontext -> KP.Expression -> [UnitExpression]
convertExpressionToList _       (KP.Expression _ [])                     = []
convertExpressionToList context (KP.Expression _ ((binop, second) : xs)) = Bin binop : Un second : convertExpressionToList context (KP.Expression second xs)

placeTokenInTree :: KTC.Kcontext -> BinaryTreeExpression -> UnitExpression -> BinaryTreeExpression -> Either KTE.KoakTypingException BinaryTreeExpression
placeTokenInTree _       node@(ExprLeaf _) (Bin newOp) tree = Right $ ExprNode newOp node tree
placeTokenInTree context base@(ExprNode op left right) (Bin newOp) tree
    | placeTokenInTreeSub (isLessPrio context newOp op)     = Right $ ExprNode newOp base tree
    | otherwise                                             = placeTokenInTreeSub' (ExprNode op left) (placeTokenInTree context right (Bin newOp) tree)
placeTokenInTree _ _ _ _                                    = error "In placeTokenInTree"

placeTokenInTreeSub :: Either KTE.KoakTypingException Bool -> Bool
placeTokenInTreeSub (Left  _) = False
placeTokenInTreeSub (Right _) = True

placeTokenInTreeSub' :: (BinaryTreeExpression -> BinaryTreeExpression) -> Either KTE.KoakTypingException BinaryTreeExpression -> Either KTE.KoakTypingException BinaryTreeExpression
placeTokenInTreeSub' f (Left value) = Left value
placeTokenInTreeSub' f (Right value) = Right $ f value

callbackCreateBinaryNode :: KTC.Kcontext -> UnitExpression -> [UnitExpression] -> BinaryTreeExpression -> Either KTE.KoakTypingException BinaryTreeExpression
callbackCreateBinaryNode context (Bin binop) ((Un unRight):xs) unLeft = buildExpressionTree' context xs $ ExprNode binop unLeft (ExprLeaf unRight)
callbackCreateBinaryNode _ _ _ _ = error "In callbackCreateBinaryNode"

-- Un (UnaryPostfix (Postfix (PrimaryIdentifier (Identifier "i")) Nothing)) 
-- [Bin (BinaryOp (Identifier "=")),Un (UnaryPostfix (Postfix (PrimaryLiteral (LiteralDecimal (DecimalConst 1))) Nothing))] 
-- ExprLeaf (UnaryPostfix (Postfix (PrimaryIdentifier (Identifier "i")) Nothing)) 0

isLessPrio :: KTC.Kcontext -> KP.BinaryOp -> KP.BinaryOp -> Either KTE.KoakTypingException Bool
isLessPrio context left right
    | isLessPrioSub (getBinOpPrecedence context left) (getBinOpPrecedence context right) == Right True = Right True
    | otherwise                                                                                        = Right False

isLessPrioSub :: Either KTE.KoakTypingException Int -> Either KTE.KoakTypingException Int -> Either KTE.KoakTypingException Bool
isLessPrioSub (Right left) (Right right) = Right $ left <= right
isLessPrioSub (Left value) _ = Left value
isLessPrioSub _ (Left value) = Left value

getBinOpPrecedence :: KTC.Kcontext -> KP.BinaryOp -> Either KTE.KoakTypingException Int
getBinOpPrecedence context binop = let signature = KTC.kContextFind context (KTC.toIdentifier binop)
    in case signature of
        Nothing                                                                                -> Left $ KTE.KoakTypingUnknownDefinition $ KTC.toIdentifier binop
        Just (KTC.PrimitiveFunction ((KTC.BinaryFunctionTyping (KP.Precedence  pre) _ _ _):_)) -> Right pre
        Just (KTC.Function (KTC.BinaryFunctionTyping (KP.Precedence  pre) _ _ _))              -> Right pre
        _                                                                                      -> Left $ KTE.KoakTypingNotABinaryFunction $ KTC.toIdentifier binop

-- getBinOpType = 
-------------------------------------------
evaluateExpressionTyping' :: KTC.Kcontext -> BinaryTreeExpression -> Either KTE.KoakTypingException EvaluationResult
evaluateExpressionTyping' context (ExprLeaf unary)                                                    = evaluateUnaryTyping context unary
evaluateExpressionTyping' context (ExprNode (KP.BinaryOp (KP.Identifier "=")) (ExprLeaf unary) right) = evaluateExpressionTypingSub' (evaluateExpressionTyping' context right) unary
evaluateExpressionTyping' _       (ExprNode (KP.BinaryOp (KP.Identifier "=")) _                _    ) = Left KTE.KoakTypingAssignmentToRValue
evaluateExpressionTyping' context (ExprNode (KP.BinaryOp binary) left right)                          = evaluateExpressionTypingSub''
                                                                                                            (evaluateExpressionTyping' context left)
                                                                                                            (\left -> evaluateExpressionTyping' (getEvaluatedKcontext left) right)
                                                                                                            (\left right -> evaluateBinOperation (getEvaluatedKcontext right) binary (getEvaluatedType left) (getEvaluatedType right))

evaluateExpressionTypingSub' :: Either KTE.KoakTypingException EvaluationResult -> KP.Unary -> Either KTE.KoakTypingException EvaluationResult
evaluateExpressionTypingSub' (Left value) unary = Left value
evaluateExpressionTypingSub' (Right value) unary = evaluateAssignment (getEvaluatedKcontext value) unary (getEvaluatedType value)

evaluateExpressionTypingSub'' :: Either KTE.KoakTypingException EvaluationResult -> (EvaluationResult -> Either KTE.KoakTypingException EvaluationResult) -> (EvaluationResult -> EvaluationResult -> Either KTE.KoakTypingException EvaluationResult) -> Either KTE.KoakTypingException EvaluationResult
evaluateExpressionTypingSub'' (Left value) f1 f2 = Left value
evaluateExpressionTypingSub'' (Right value) f1 f2 = evaluateExpressionTypingSub''' value (f1 value) f2

evaluateExpressionTypingSub''' :: EvaluationResult -> Either KTE.KoakTypingException EvaluationResult -> (EvaluationResult -> EvaluationResult -> Either KTE.KoakTypingException EvaluationResult) -> Either KTE.KoakTypingException EvaluationResult
evaluateExpressionTypingSub''' left (Left value) f2 = Left value
evaluateExpressionTypingSub''' left (Right value) f2 = f2 left value

evaluateAssignment :: KTC.Kcontext -> KP.Unary -> KTC.BaseType -> Either KTE.KoakTypingException EvaluationResult
evaluateAssignment context (KP.UnaryPostfix (KP.Postfix (KP.PrimaryIdentifier identifier) Nothing)) base_type = evaluateAssignment' context identifier base_type (KTC.kContextFind context identifier)
evaluateAssignment _ _ _                                                                                      = Left KTE.KoakTypingAssignmentToRValue

evaluateAssignment' :: KTC.Kcontext -> KP.Identifier -> KTC.BaseType -> Maybe KTC.TypeSignature -> Either KTE.KoakTypingException EvaluationResult
evaluateAssignment' context identifier base_type Nothing               = Right $ EvaluationResult
                                                                                    base_type
                                                                                    (KTC.kContextPushVar (KP.VarAssignment identifier (KTC.baseTypeToType base_type)) context)
evaluateAssignment' context identifier base_type (Just (KTC.Var var_type))
    | var_type /= base_type                                            = Left $ KTE.KoakTypingShadowedVariableByVariable identifier (KP.VarAssignment identifier (KTC.baseTypeToType var_type))
    | otherwise                                                        = Right $ EvaluationResult base_type context
evaluateAssignment' _       identifier _          _                    = Left $ KTE.KoakTypingNotAVar identifier

evaluateBinOperation :: KTC.Kcontext -> KP.Identifier -> KTC.BaseType -> KTC.BaseType -> Either KTE.KoakTypingException EvaluationResult
evaluateBinOperation context binary left_type right_type = evaluateBinOperationSub (\x -> EvaluationResult (KTC.getFunctionReturnType x) context) (findBinaryMatchingFunction context binary left_type right_type)

evaluateBinOperationSub :: (KTC.FunctionTyping -> EvaluationResult) -> Either KTE.KoakTypingException KTC.FunctionTyping -> Either KTE.KoakTypingException EvaluationResult
evaluateBinOperationSub _ (Left value) = Left value
evaluateBinOperationSub f (Right value) = Right $ f value

findBinaryMatchingFunction :: KTC.Kcontext -> KP.Identifier -> KTC.BaseType -> KTC.BaseType -> Either KTE.KoakTypingException KTC.FunctionTyping
findBinaryMatchingFunction context func_name left_type right_type = findBinaryMatchingFunction' func_name left_type right_type (KTC.kContextFind context func_name)

findBinaryMatchingFunction' :: KP.Identifier -> KTC.BaseType -> KTC.BaseType -> Maybe KTC.TypeSignature -> Either KTE.KoakTypingException KTC.FunctionTyping
findBinaryMatchingFunction' func_name _         _          Nothing                                  = Left $ KTE.KoakTypingUnknownDefinition func_name
findBinaryMatchingFunction' func_name left_type right_type (Just (KTC.PrimitiveFunction func_list)) =
    let found = find (KTC.isBinaryFunctionParamMatchingFunction left_type right_type) func_list
    in case found of
    Nothing              -> throw $ KTE.KoakTypingMismatchedArgumentType func_name [KTC.baseTypeToType left_type, KTC.baseTypeToType right_type]
    (Just matching_func) -> Right matching_func
findBinaryMatchingFunction' func_name left_type right_type (Just (KTC.Function func))
    | KTC.isBinaryFunctionParamMatchingFunction left_type right_type func                           = Right func
    | otherwise                                                                                     = Left $ KTE.KoakTypingMismatchedArgumentType func_name [KTC.baseTypeToType left_type, KTC.baseTypeToType right_type]
findBinaryMatchingFunction' func_name _        _           (Just _)                                 = Left $ KTE.KoakTypingNotABinaryFunction func_name

evaluateUnaryTyping :: KTC.Kcontext -> KP.Unary -> Either KTE.KoakTypingException EvaluationResult
evaluateUnaryTyping context (KP.Unary (KP.UnaryOp identifier) unary) =
    let next = evaluateUnaryTyping context unary   in
    evaluateUnaryTypingSub identifier next
evaluateUnaryTyping context (KP.UnaryPostfix postfix) = evaluatePostfixTyping context postfix

evaluateUnaryTypingSub :: KP.Identifier -> Either KTE.KoakTypingException EvaluationResult -> Either KTE.KoakTypingException EvaluationResult
evaluateUnaryTypingSub _ (Left value) = Left value
evaluateUnaryTypingSub identifier (Right value) = evaluateUnaryTypingSub' identifier value

evaluateUnaryTypingSub' :: KP.Identifier -> EvaluationResult -> Either KTE.KoakTypingException EvaluationResult
evaluateUnaryTypingSub' identifier next = evaluateUnaryTypingSub'' (findUnaryMatchingFunction (getEvaluatedKcontext next) identifier (getEvaluatedType next)) (\x -> EvaluationResult (KTC.getFunctionReturnType x) (getEvaluatedKcontext next))

evaluateUnaryTypingSub'' :: Either KTE.KoakTypingException KTC.FunctionTyping -> (KTC.FunctionTyping -> EvaluationResult) -> Either KTE.KoakTypingException EvaluationResult
evaluateUnaryTypingSub'' (Left value) f = Left value
evaluateUnaryTypingSub'' (Right value) f = Right $ f value

composeEither :: (b -> Either a c) -> Either a b -> Either a c
composeEither f (Right value) = f value
composeEither f (Left  value) = Left value

findUnaryMatchingFunction :: KTC.Kcontext -> KP.Identifier -> KTC.BaseType -> Either KTE.KoakTypingException KTC.FunctionTyping
findUnaryMatchingFunction context func_name arg_type = findUnaryMatchingFunction' func_name arg_type (KTC.kContextFind context func_name)

findUnaryMatchingFunction' :: KP.Identifier -> KTC.BaseType -> Maybe KTC.TypeSignature -> Either KTE.KoakTypingException KTC.FunctionTyping
findUnaryMatchingFunction' func_name _        Nothing                                  = Left $ KTE.KoakTypingUnknownDefinition func_name
findUnaryMatchingFunction' func_name arg_type (Just (KTC.PrimitiveFunction func_list)) =
    let found = find (KTC.isUnaryFunctionParamMatchingFunction arg_type) func_list
    in case found of
    Nothing              -> Left $ KTE.KoakTypingMismatchedArgumentType func_name [KTC.baseTypeToType arg_type]
    (Just matching_func) -> Right matching_func
findUnaryMatchingFunction' func_name arg_type (Just (KTC.Function func))
    | KTC.isUnaryFunctionParamMatchingFunction arg_type func                           = Right func
    | otherwise                                                                        = Left $ KTE.KoakTypingMismatchedArgumentType func_name [KTC.baseTypeToType arg_type]
findUnaryMatchingFunction' func_name _        (Just _)                                 = Left $ KTE.KoakTypingNotAUnaryFunction func_name

evaluatePostfixTyping :: KTC.Kcontext -> KP.Postfix -> Either KTE.KoakTypingException EvaluationResult
evaluatePostfixTyping context (KP.Postfix (KP.PrimaryLiteral    (KP.LiteralDecimal _)) Nothing)          = Right $ EvaluationResult KTC.Int    context
evaluatePostfixTyping context (KP.Postfix (KP.PrimaryLiteral    (KP.LiteralDouble  _)) Nothing)          = Right $ EvaluationResult KTC.Double context
evaluatePostfixTyping context (KP.Postfix (KP.PrimaryIdentifier identifier           ) Nothing)          = evaluatePostfixTypingVar   context identifier
evaluatePostfixTyping context (KP.Postfix (KP.PrimaryIdentifier identifier           ) (Just call_expr)) = evaluateFunctionCallTyping context identifier call_expr
evaluatePostfixTyping context (KP.Postfix (KP.PrimaryExpressions expressions         ) Nothing)          = evaluateExpressionsTyping  expressions context
evaluatePostfixTyping _       _                                                                          = Left $ KTE.KoakTypingNotAFunction $ KP.Identifier "Undefined"

evaluatePostfixTypingVar :: KTC.Kcontext -> KP.Identifier -> Either KTE.KoakTypingException EvaluationResult
evaluatePostfixTypingVar context identifier = evaluatePostfixTypingVar' context identifier $ KTC.kContextFind context identifier

evaluatePostfixTypingVar' :: KTC.Kcontext -> KP.Identifier -> Maybe KTC.TypeSignature -> Either KTE.KoakTypingException EvaluationResult
evaluatePostfixTypingVar' _       identifier Nothing                   = Left  $ KTE.KoakTypingUnknownDefinition identifier
evaluatePostfixTypingVar' context _          (Just (KTC.Var var_type)) = Right $ EvaluationResult var_type context
evaluatePostfixTypingVar' _       identifier (Just _                 ) = Left  $ KTE.KoakTypingNotAVar identifier

evaluateFunctionCallTyping :: KTC.Kcontext -> KP.Identifier -> KP.CallExpression -> Either KTE.KoakTypingException EvaluationResult
evaluateFunctionCallTyping context identifier (KP.CallExpression Nothing)                                 = evaluateFunctionCallTyping' context identifier []
evaluateFunctionCallTyping context identifier (KP.CallExpression (Just (KP.CallExpressionArgs arg args))) = evaluateFunctionCallArgsTyping context identifier (arg:args)

evaluateFunctionCallArgsTyping :: KTC.Kcontext -> KP.Identifier -> [KP.Expression] -> Either KTE.KoakTypingException EvaluationResult
evaluateFunctionCallArgsTyping context identifier exprs = evaluateFunctionCallArgsTypingSub (evaluateFunctionCallArgs context exprs) identifier

evaluateFunctionCallArgsTypingSub :: Either KTE.KoakTypingException (KTC.Kcontext, [KTC.BaseType]) -> KP.Identifier -> Either KTE.KoakTypingException EvaluationResult
evaluateFunctionCallArgsTypingSub (Right value) identifier = evaluateFunctionCallArgsTyping' value identifier
evaluateFunctionCallArgsTypingSub (Left value)  _          = Left value

evaluateFunctionCallArgsTyping' :: (KTC.Kcontext, [KTC.BaseType]) -> KP.Identifier -> Either KTE.KoakTypingException EvaluationResult
evaluateFunctionCallArgsTyping' (context, args) identifier = evaluateFunctionCallTyping' context identifier args

evaluateFunctionCallArgs :: KTC.Kcontext -> [KP.Expression] -> Either KTE.KoakTypingException (KTC.Kcontext, [KTC.BaseType])
evaluateFunctionCallArgs context exprs = evaluateFunctionCallArgsSub $ evaluateFunctionCallArgs' [] context exprs

evaluateFunctionCallArgsSub :: Either KTE.KoakTypingException (KTC.Kcontext, [KTC.BaseType]) -> Either KTE.KoakTypingException (KTC.Kcontext, [KTC.BaseType])
evaluateFunctionCallArgsSub (Right value) = Right $ evaluateFunctionCallArgsFlipArgs value
evaluateFunctionCallArgsSub left          = left

evaluateFunctionCallArgsFlipArgs :: (KTC.Kcontext, [KTC.BaseType]) -> (KTC.Kcontext, [KTC.BaseType])
evaluateFunctionCallArgsFlipArgs (context, args) = (context, reverse args)

evaluateFunctionCallArgs' :: [KTC.BaseType] -> KTC.Kcontext -> [KP.Expression] -> Either KTE.KoakTypingException (KTC.Kcontext, [KTC.BaseType])
evaluateFunctionCallArgs' evaluated_types context []     = Right (context, evaluated_types)
evaluateFunctionCallArgs' evaluated_types context (x:xs) = let evaluated_arg = evaluateExpressionTyping x context in
                                                            case evaluated_arg of
                                                                (Left error) -> Left error
                                                                (Right value) -> evaluateFunctionCallArgs' (getEvaluatedType value:evaluated_types) (getEvaluatedKcontext value) xs

evaluateFunctionCallTyping' :: KTC.Kcontext -> KP.Identifier -> [KTC.BaseType] -> Either KTE.KoakTypingException EvaluationResult
evaluateFunctionCallTyping' context identifier args = (\x -> EvaluationResult (KTC.getFunctionReturnType x) context) <$> findMatchingFunction context identifier args

findMatchingFunction :: KTC.Kcontext -> KP.Identifier -> [KTC.BaseType] -> Either KTE.KoakTypingException KTC.FunctionTyping
findMatchingFunction context func_name args = findMatchingFunction' func_name args (KTC.kContextFind context func_name)

findMatchingFunction' :: KP.Identifier -> [KTC.BaseType] -> Maybe KTC.TypeSignature -> Either KTE.KoakTypingException KTC.FunctionTyping
findMatchingFunction' func_name _    Nothing                                  = Left $ KTE.KoakTypingUnknownDefinition func_name
findMatchingFunction' func_name args (Just (KTC.PrimitiveFunction func_list)) =
    let found = find (KTC.isFunctionParamMatchingFunction args) func_list
    in case found of
    Nothing              -> Left $ KTE.KoakTypingMismatchedArgumentType func_name (map KTC.baseTypeToType args)
    (Just matching_func) -> Right matching_func
findMatchingFunction' func_name args (Just (KTC.Function func))
    | KTC.isFunctionParamMatchingFunction args func                           = Right func
    | otherwise                                                               = Left $ KTE.KoakTypingMismatchedArgumentType func_name (map KTC.baseTypeToType args)
findMatchingFunction' func_name _        (Just _)                             = Left $ KTE.KoakTypingNotAFunction func_name

getEvaluatedType :: EvaluationResult -> KTC.BaseType
getEvaluatedType (EvaluationResult base_type _) = base_type

getEvaluatedKcontext :: EvaluationResult -> KTC.Kcontext
getEvaluatedKcontext (EvaluationResult _ context) = context
