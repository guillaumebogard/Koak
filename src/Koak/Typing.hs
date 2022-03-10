--
-- EPITECH PROJECT, 2022
-- koak
-- File description:
-- Koak.Typing
--

module Koak.Typing                            ( checkKoakTyping ) where

import Control.Exception                      ( throw )
--import Data.Foldable                          ( find )

import qualified Koak.Typing.Exception as KTE ( KoakTypingException(..) )

import qualified Koak.TypingContext    as KTC ( Kcontext(..)
                                              , BaseType(..)
                                              , FunctionTyping(..)
                                              , TypeSignature(..)
                                              , kContextEnterFunctionCall
                                              , baseTypeToType
                                              , kContextPushFunction
                                              , kContextFind
                                              --, isUnaryFunctionParamMatchingFunction
                                              --, getFunctionReturnType
                                              , toIdentifier
                                              )
import qualified Koak.Parser          as KP


data EvaluationResult = EvaluationResult KTC.BaseType KTC.Kcontext

checkKoakTyping :: KP.Stmt -> KTC.Kcontext -> KTC.Kcontext
checkKoakTyping stmt = checkKoakTyping' $ KP.getKdefsFromStmt stmt

checkKoakTyping' :: [KP.Kdefs] -> KTC.Kcontext -> KTC.Kcontext
checkKoakTyping' kdefs context = foldr checkKdefTyping context kdefs

checkKdefTyping :: KP.Kdefs -> KTC.Kcontext -> KTC.Kcontext
checkKdefTyping (KP.KdefDef        defs@(KP.Defs proto _)) context = KTC.kContextPushFunction proto context <-- checkDefsTyping defs (KTC.kContextEnterFunctionCall proto context)
checkKdefTyping (KP.KdefExpression       exprs           ) context = getEvaluatedKcontext $ evaluateExpressionsTyping exprs context

checkDefsTyping :: KP.Defs -> KTC.Kcontext -> ()
checkDefsTyping (KP.Defs _ exprs) = (<--) () . evaluateExpressionsTyping exprs

evaluateExpressionsTyping :: KP.Expressions -> KTC.Kcontext -> EvaluationResult
evaluateExpressionsTyping (KP.ExpressionFor   forExpr        ) = evaluateForTyping            forExpr
evaluateExpressionsTyping (KP.ExpressionIf    ifExpr         ) = evaluateIfTyping             ifExpr
evaluateExpressionsTyping (KP.ExpressionWhile whileExpr      ) = evaluateWhileTyping          whileExpr
evaluateExpressionsTyping (KP.Expressions     expr      exprs) = evaluateExpressionListTyping (expr : exprs)

evaluateExpressionListTyping :: [KP.Expression] -> KTC.Kcontext -> EvaluationResult
evaluateExpressionListTyping []     _       = error "This should never happen :)"
evaluateExpressionListTyping [x]    context = evaluateExpressionTyping x context
evaluateExpressionListTyping (x:xs) context = evaluateExpressionListTyping xs (getEvaluatedKcontext $ evaluateExpressionTyping x context)

evaluateForTyping :: KP.For -> KTC.Kcontext -> EvaluationResult
evaluateForTyping    forExpr@(KP.For assignExpr _ _ _) = evaluateForTyping'   forExpr . getEvaluatedKcontext . evaluateExpressionTyping assignExpr

evaluateForTyping' :: KP.For -> KTC.Kcontext -> EvaluationResult
evaluateForTyping'   forExpr@(KP.For _ condExpr _ _)   = evaluateForTyping''  forExpr . getEvaluatedKcontext . evaluateExpressionTyping condExpr

evaluateForTyping'' :: KP.For -> KTC.Kcontext -> EvaluationResult
evaluateForTyping''  forExpr@(KP.For _ _ incExpr _)    = evaluateForTyping''' forExpr . getEvaluatedKcontext . evaluateExpressionTyping incExpr

evaluateForTyping''' :: KP.For -> KTC.Kcontext -> EvaluationResult
evaluateForTyping'''         (KP.For _ _ _ exprs)      = evaluateExpressionsTyping exprs

evaluateIfTyping :: KP.If -> KTC.Kcontext -> EvaluationResult
evaluateIfTyping  ifExpr@(KP.If condExpr _ _)  = evaluateIfTyping'  ifExpr . getEvaluatedKcontext . evaluateExpressionTyping condExpr

evaluateIfTyping' :: KP.If -> KTC.Kcontext -> EvaluationResult
evaluateIfTyping' ifExpr@(KP.If _ thenExprs _) = evaluateIfTyping'' ifExpr . evaluateExpressionsTyping thenExprs

evaluateIfTyping'' :: KP.If -> EvaluationResult -> EvaluationResult
evaluateIfTyping''       (KP.If _ _ Nothing     ) thenResult                              = thenResult
evaluateIfTyping''       (KP.If _ _ (Just exprs)) (EvaluationResult thenType thenContext) = evaluateIfTyping''' thenType $ evaluateExpressionsTyping exprs thenContext

evaluateIfTyping''' :: KTC.BaseType -> EvaluationResult -> EvaluationResult
evaluateIfTyping''' thenType elseResult
    | thenType == getEvaluatedType elseResult = elseResult
    | otherwise                               = throw $ KTE.KoakTypingMismatchedThenElseType (KTC.baseTypeToType thenType) $ KTC.baseTypeToType $ getEvaluatedType elseResult

evaluateWhileTyping :: KP.While -> KTC.Kcontext -> EvaluationResult
evaluateWhileTyping whileExpr@(KP.While condExpr _) = evaluateWhileTyping' whileExpr . getEvaluatedKcontext . evaluateExpressionTyping condExpr

evaluateWhileTyping' :: KP.While -> KTC.Kcontext -> EvaluationResult
evaluateWhileTyping'          (KP.While _ exprs)    = evaluateExpressionsTyping exprs

evaluateExpressionTyping :: KP.Expression -> KTC.Kcontext -> EvaluationResult
evaluateExpressionTyping expression context = evaluateExpressionTyping' context $ buildExpressionTree context expression

-------------------evalexpr---------------

data BinaryTreeExpression = ExprNode KP.BinaryOp BinaryTreeExpression BinaryTreeExpression | ExprLeaf KP.Unary

data UnitExpression = Un KP.Unary | Bin KP.BinaryOp

buildExpressionTree :: KTC.Kcontext -> KP.Expression -> BinaryTreeExpression
buildExpressionTree context expression@(KP.Expression unary _) = buildExpressionTree' context (convertExpressionToList context expression) $ ExprLeaf unary

buildExpressionTree' :: KTC.Kcontext -> [UnitExpression] -> BinaryTreeExpression -> BinaryTreeExpression
buildExpressionTree' _       []               tree              = tree
buildExpressionTree' context (x:xs)           node@(ExprLeaf _) = callbackCreateBinaryNode context x xs node
buildExpressionTree' context (bin:(Un un):xs) tree              = buildExpressionTree' context xs $ placeTokenInTree context tree bin $ ExprLeaf un
buildExpressionTree' _       _                _                 = error ""

convertExpressionToList :: KTC.Kcontext -> KP.Expression -> [UnitExpression]
convertExpressionToList _            (KP.Expression unary []) = [Un unary]
convertExpressionToList context expr@(KP.Expression first _ ) = Un first : convertExpressionToList' context expr

convertExpressionToList' :: KTC.Kcontext -> KP.Expression -> [UnitExpression]
convertExpressionToList' _       (KP.Expression _ [])                     = []
convertExpressionToList' context (KP.Expression _ ((binop, second) : xs)) =
    Bin binop : Un second : convertExpressionToList' context (KP.Expression second xs)

placeTokenInTree :: KTC.Kcontext -> BinaryTreeExpression -> UnitExpression -> BinaryTreeExpression -> BinaryTreeExpression
placeTokenInTree _       node@(ExprLeaf _) (Bin newOp) tree = ExprNode newOp node tree
placeTokenInTree context base@(ExprNode op left right) (Bin newOp) tree
    | isLessPrio context newOp op                           = ExprNode newOp base tree
    | otherwise                                             = ExprNode op left $ placeTokenInTree context right (Bin newOp) tree
placeTokenInTree _ _ _ _                                    = error ""

callbackCreateBinaryNode :: KTC.Kcontext -> UnitExpression -> [UnitExpression] -> BinaryTreeExpression -> BinaryTreeExpression
callbackCreateBinaryNode context (Bin binop) ((Un unRight):xs) unLeft = buildExpressionTree' context xs $ ExprNode binop unLeft (ExprLeaf unRight)
callbackCreateBinaryNode _ _ _ _ = error "invalid expression"

isLessPrio :: KTC.Kcontext -> KP.BinaryOp -> KP.BinaryOp -> Bool
isLessPrio context left right
    | getBinOpPrecedence context left <= getBinOpPrecedence context right   = True
    | otherwise                                             = False

getBinOpPrecedence :: KTC.Kcontext -> KP.BinaryOp -> Int
getBinOpPrecedence context binop = let signature = KTC.kContextFind context (KTC.toIdentifier binop)
    in case signature of
        Nothing                                                                                -> throw $ KTE.KoakTypingUnknownDefinition $ KTC.toIdentifier binop
        Just (KTC.PrimitiveFunction ((KTC.BinaryFunctionTyping (KP.Precedence  pre) _ _ _):_)) -> pre
        Just (KTC.Function (KTC.BinaryFunctionTyping (KP.Precedence  pre) _ _ _))              -> pre
        _                                                                                      -> error "Not a binOp"

-------------------------------------------

evaluateExpressionTyping' :: KTC.Kcontext -> BinaryTreeExpression -> EvaluationResult
-- evaluateExpressionTyping' context (ExprLeaf unary)             = evaluateUnary context unary
-- evaluateExpressionTyping' context (ExprNode binary left right) = let binResult   = evaluateBinary context binary in
--                                                                  let leftResult  = evaluateExpressionTyping' context left in 
--                                                                  let rightResult = evaluateExpressionTyping' context right in error ""
evaluateExpressionTyping' _ _ = error ""

---------------------------------------------------
{-
evaluateExpressionTyping :: Kcontext -> KP.Expression -> EvaluationResult
evaluateExpressionTyping context (KP.Expression unary [])                     = evaluateUnaryType context unary
evaluateExpressionTyping context (KP.Expression first ((binop, second) : xs)) = EvaluationResult Nil context
    --                                                                            checkPartialExpressionType kdefs first binop second -->
    --                                                                               evaluateExpressionType' kdefs (Expression second xs) (getBinOpPrecedence kdefs binop, getBinOpType kdefs binop)
                                                                               
evaluateExpressionTyping'' :: EvaluationResult -> KP.Expression -> EvaluationResult 

evaluateExpressionType' :: Kcontext -> KP.Expression -> (KP.Precedence, KP.TYPE) -> KP.TYPE
evaluateExpressionType' context (KP.Expression _ []) (_, t) = t
evaluateExpressionType' context (KP.Expression first ((binop, second) : xs)) (prec, t)
    | prec > getBinOpPrecedence context binop = checkPartialExpressionType context first binop second --> evaluateExpressionType' context (KP.Expression second xs) (prec, t)
    | otherwise = checkPartialExpressionType context first binop second -->
                  evaluateExpressionType' context (KP.Expression second xs) (getBinOpPrecedence context binop, getBinOpType context binop)


checkPartialExpressionType :: KP.Kdefs -> KP.Unary -> KP.BinaryOp -> KP.Unary -> ()
checkPartialExpressionType kdefs first op second = ()
-}


-- evaluateUnaryTyping :: KTC.Kcontext -> KP.Unary -> EvaluationResult
-- evaluateUnaryTyping context (KP.Unary (KP.UnaryOp identifier) unary) =
--     let found = KTC.kContextFind    context identifier in
--     let next  = evaluateUnaryTyping context unary      in
--     EvaluationResult
--         (KTC.getFunctionReturnType $
--             findUnaryMatchingFunction
--                 (getEvaluatedKcontext next)
--                 identifier
--                 (getEvaluatedType     next)
--         )
--         (getEvaluatedKcontext next)
-- evaluateUnaryTyping context (KP.UnaryPostfix postfix) = evaluatePostfixTyping context postfix

--findUnaryMatchingFunction :: KTC.Kcontext -> KP.Identifier -> KTC.BaseType -> KTC.FunctionTyping
--findUnaryMatchingFunction context func_name arg_type = findUnaryMatchingFunction' func_name arg_type (KTC.kContextFind context func_name)

--findUnaryMatchingFunction' :: KP.Identifier -> KTC.BaseType -> Maybe KTC.TypeSignature -> KTC.FunctionTyping
--findUnaryMatchingFunction' func_name _        Nothing                                  = throw $ KTE.KoakTypingUnknownDefinition func_name
--findUnaryMatchingFunction' func_name arg_type (Just (KTC.PrimitiveFunction func_list)) =
--    let found = find (KTC.isUnaryFunctionParamMatchingFunction arg_type) func_list
--    in case found of
--    Nothing              -> throw $ KTE.KoakTypingMismatchedArgumentType func_name [KTC.baseTypeToType arg_type]
--    (Just matching_func) -> matching_func
--findUnaryMatchingFunction' func_name arg_type (Just (KTC.Function func))
--    | KTC.isUnaryFunctionParamMatchingFunction arg_type func                           = func
--    | otherwise                                                                        = throw $ KTE.KoakTypingMismatchedArgumentType func_name [KTC.baseTypeToType arg_type]
--findUnaryMatchingFunction' func_name _        (Just _)                                 = throw $ KTE.KoakTypingNotAUnaryFunction func_name

--evaluatePostfixTyping :: KTC.Kcontext -> KP.Postfix -> EvaluationResult
--evaluatePostfixTyping context (KP.Postfix (KP.PrimaryLiteral    (KP.LiteralDecimal _)) Nothing)          = EvaluationResult KTC.Int    context
--evaluatePostfixTyping context (KP.Postfix (KP.PrimaryLiteral    (KP.LiteralDouble  _)) Nothing)          = EvaluationResult KTC.Double context
--evaluatePostfixTyping context (KP.Postfix (KP.PrimaryIdentifier identifier           ) Nothing)          = evaluatePostfixTypingVar    context identifier
--evaluatePostfixTyping context (KP.Postfix (KP.PrimaryIdentifier identifier           ) (Just call_expr)) = evaluateFunctionCallTyping  context identifier call_expr
--evaluatePostfixTyping context (KP.Postfix (KP.PrimaryExpressions expressions         ) Nothing)          = evaluateExpressionsTyping   expressions context
--evaluatePostfixTyping context _ = error ""

--evaluatePostfixTypingVar :: KTC.Kcontext -> KP.Identifier -> EvaluationResult
--evaluatePostfixTypingVar context identifier = evaluatePostfixTypingVar' context identifier $ KTC.kContextFind context identifier
--
--evaluatePostfixTypingVar' :: KTC.Kcontext -> KP.Identifier -> Maybe KTC.TypeSignature -> EvaluationResult
--evaluatePostfixTypingVar' context identifier Nothing                   = throw $ KTE.KoakTypingUnknownDefinition identifier
--evaluatePostfixTypingVar' context identifier (Just (KTC.Var var_type)) = EvaluationResult var_type context
--evaluatePostfixTypingVar' context identifier (Just _                 ) = throw $ KTE.KoakTypingNotAVar identifier
--
--evaluateFunctionCallTyping :: KTC.Kcontext -> KP.Identifier -> KP.CallExpression -> EvaluationResult
--evaluateFunctionCallTyping context identifier (KP.CallExpression Nothing)                                 = evaluateFunctionCallTyping' context identifier []
--evaluateFunctionCallTyping context identifier (KP.CallExpression (Just (KP.CallExpressionArgs arg args))) = evaluateFunctionCallTyping' context identifier []
--
--evaluateFunctionCallTyping' :: KTC.Kcontext -> KP.Identifier -> [KTC.BaseType] -> EvaluationResult
--evaluateFunctionCallTyping' context identifier args = evaluateFunctionCallTyping'' context identifier args (KTC.kContextFind context identifier)
--
--evaluateFunctionCallTyping'' :: KTC.Kcontext -> KP.Identifier -> [KTC.BaseType] -> Maybe KTC.TypeSignature -> EvaluationResult
--evaluateFunctionCallTyping'' _ _ _ Nothing                 = error ""
--evaluateFunctionCallTyping'' _ _ _ (Just (KTC.Function _)) = error ""
--evaluateFunctionCallTyping'' _ _ _ (Just _)                = error ""
--

-- evaluateUnaryTyping' :: Kcontext -> EvaluationResult -> [FunctionTyping] -> EvaluationResult
-- evaluateUnaryTyping' context right_res []                                 = throw $ KTE.KoakTypingMismatchedArgumentType (KP.Identifier "remplacer op") []
-- evaluateUnaryTyping' context right_res (x:xs)
--     | isUnaryFunctionParamMatchingFunction x (getEvaluatedType right_res) = EvaluationResult (getFunctionReturnType x) context
--     | otherwise                                                           = evaluateUnaryTyping' context right_res xs


--     | getUnopType context op == evaluateUnaryType context unary       = evaluateUnaryType context unary
--     | otherwise                                                   = throw $ KTE.KoakTypingMismatchedUnaryType (evaluateUnaryType kdefs unary) $ getUnopType defs op
-- evaluateUnaryTyping context (KP.UNARY_POSTFIX (KP.POSTFIX primary Nothing)) = Int -- get primary type as basic type
-- evaluateUnaryTyping context (KP.UNARY_POSTFIX (KP.POSTFIX primary _))       = Int -- get primary type as function type


--------------------------------------------

-- -------------------échafaudages --------------- (/!\ ne pas escalader /!\)

-- evaluateExpressionTyping :: Kcontext -> Expression -> EvaluationResult
-- evaluateExpressionTyping kdefs (Expression unary []) = evaluateUnaryType kdefs unary
-- evaluateExpressionTyping kdefs (Expression first ((binop, second) : xs)) = checkPartialExpressionType kdefs first binop second -->
--                                                      evaluateExpressionType' kdefs (Expression second xs) (getBinOpPrecedence kdefs binop, getBinOpType kdefs binop)
-- evaluateExpressionTyping _ _ = error "bite"


-- evaluateExpressionType' :: Kdefs -> Expression -> (Precedence, TYPE) -> TYPE
-- evaluateExpressionType' kdefs (Expression _ []) (_, t) = t
-- evaluateExpressionType' kdefs (Expression first ((binop, second) : xs)) (prec, t)
--     | prec > getBinOpPrecedence kdefs binop = checkPartialExpressionType kdefs first binop second --> evaluateExpressionType' kdefs (Expression second xs) (prec, t)
--     | otherwise = checkPartialExpressionType kdefs first binop second -->
--                   evaluateExpressionType' kdefs (Expression second xs) (getBinOpPrecedence kdefs binop, getBinOpType kdefs binop)

-- evaluateUnaryType :: Kdefs -> Unary -> TYPE
-- evaluateUnaryType kdefs (UNARY_UN op unary)
--     | getUnopType kdefs op == evaluateUnaryType kdefs unary = evaluateUnaryType kdefs unary
--     | otherwise                            = throw $ KTE.KoakTypingMismatchedUnaryType (evaluateUnaryType kdefs unary) $ getUnopType defs op
-- evaluateUnaryType kdefs (UNARY_POSTFIX (POSTFIX primary Nothing)) = Int -- get primary type as basic type
-- evaluateUnaryType kdefs (UNARY_POSTFIX (POSTFIX primary _)) = Int -- get primary type as function type


-- --------------------------------------------


{-
 | evaluateUnaryType first == evaluateUnaryType second == Int {-getBinOpType binop -} = ()
    -- faudrait plutôt chopper le type des deux args du binop et les comparer aux unarys
    | otherwise = error "Type error"
-}

-- evaluateExpressionType :: Kdefs -> Expression -> TYPE
-- evaluateExpressionType _ _ = Int
-- evaluateExpressionType kdefs (Expression unary []) = evaluateUnaryType kdefs unary
-- evaluateExpressionType kdefs (Expression first ((binop, second) : xs)) = checkPartialExpressionType kdefs first binop second -->
--                                                      evaluateExpressionType' kdefs (Expression second xs) (getBinOpPrecedence kdefs binop, getBinOpType kdefs binop)

-- evaluateExpressionType' :: Kdefs -> Expression -> (Precedence, TYPE) -> TYPE
-- evaluateExpressionType' kdefs (Expression _ []) (_, t) = t
-- evaluateExpressionType' kdefs (Expression first ((binop, second) : xs)) (prec, t)
--     | prec > getBinOpPrecedence kdefs binop = checkPartialExpressionType kdefs first binop second --> evaluateExpressionType' kdefs (Expression second xs) (prec, t)
--     | otherwise = checkPartialExpressionType kdefs first binop second -->
--                   evaluateExpressionType' kdefs (Expression second xs) (getBinOpPrecedence kdefs binop, getBinOpType kdefs binop)

-- evaluateUnaryType :: Kdefs -> Unary -> TYPE
-- evaluateUnaryType kdefs (UNARY_UN op unary)
--     | getUnopType kdefs op == evaluateUnaryType kdefs unary = evaluateUnaryType kdefs unary
--     | otherwise                            = throw $ KTE.KoakTypingMismatchedUnaryType (evaluateUnaryType kdefs unary) $ getUnopType defs op
-- evaluateUnaryType kdefs (UNARY_POSTFIX (POSTFIX primary Nothing)) = Int -- get primary type as basic type
-- evaluateUnaryType kdefs (UNARY_POSTFIX (POSTFIX primary _)) = Int -- get primary type as function type

-- getFunctionType :: [Kdefs] -> Identifier -> TYPE
-- getFunctionType (KdefDef ((PROTOTYPE identifier (PROTOTYPE_ARGS _ fnType))) :xs) functionName
--   | identifier == functionName = fnType
--   | otherwise                  = getFunctionType xs functionName


-- find :: Foldable t => (a -> Bool) -> t a -> Maybe a

-- (<->) :: () -> () -> ()
-- (<->) _ _ = ()

(<--) :: a -> b -> a
(<--) a _ = a

-- (-->) :: b -> a -> a
-- (-->) _ a = a

getEvaluatedType :: EvaluationResult -> KTC.BaseType
getEvaluatedType (EvaluationResult base_type _) = base_type

getEvaluatedKcontext :: EvaluationResult -> KTC.Kcontext
getEvaluatedKcontext (EvaluationResult _ context) = context
