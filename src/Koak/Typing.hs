--
-- EPITECH PROJECT, 2022
-- koak
-- File description:
-- Koak.Typing
--

module Koak.Typing                  ( checkKoakTyping
                                    ) where

import Data.Foldable                ( find )
import Control.Exception            ( throw )

import qualified Koak.Parser as KP

import Koak.TypingContext           ( Kcontext(..)
                                    , DefContext(..)
                                    , VarContext(..)
                                    , BaseType(..)
                                    , FunctionTyping(..)
                                    , TypeSignature(..)
                                    , getDefaultKContext
                                    , kContextEnterFunctionCall
                                    , baseTypeToType
                                    , kContextFind
                                    , kContextPushFunction
                                    , kContextPushVar
                                    , isUnaryFunctionParamMatchingFunction
                                    , isBinaryFunctionParamMatchingFunction
                                    , getFunctionReturnType
                                    , toIdentifier
                                    , toTypeSignature
                                    )

import Koak.Typing.Exception        ( KoakTypingException(..) )


data EvaluationResult = EvaluationResult BaseType Kcontext

checkKoakTyping :: Kcontext -> [KP.Kdefs] -> Kcontext
checkKoakTyping = checkKoakTyping'

checkKoakTyping' :: Kcontext -> [KP.Kdefs] -> Kcontext
checkKoakTyping' = foldl checkKdefTyping

checkKdefTyping :: Kcontext -> KP.Kdefs -> Kcontext
checkKdefTyping context (KP.KdefDef defs@(KP.Defs proto _)) = kContextPushFunction proto context <--
                                                              checkDefsTyping (kContextEnterFunctionCall proto context) defs
checkKdefTyping context (KP.KdefExpression exprs          ) = getEvaluatedKcontext $ evaluateExpressionsTyping context exprs

checkDefsTyping :: Kcontext -> KP.Defs -> Kcontext
checkDefsTyping context (KP.Defs _ exprs) = getEvaluatedKcontext $ evaluateExpressionsTyping context exprs

evaluateExpressionsTyping :: Kcontext -> KP.Expressions -> EvaluationResult
evaluateExpressionsTyping context (KP.ExpressionFor    for_expr  ) = evaluateForTyping            context for_expr
evaluateExpressionsTyping context (KP.ExpressionIf     if_expr   ) = evaluateIfTyping             context if_expr
evaluateExpressionsTyping context (KP.ExpressionWhile  while_expr) = evaluateWhileTyping          context while_expr
evaluateExpressionsTyping context (KP.Expressions      expr exprs) = evaluateExpressionListTyping context (expr:exprs)

evaluateExpressionListTyping :: Kcontext -> [KP.Expression] -> EvaluationResult
evaluateExpressionListTyping context []     = error "This should never happen :)"
evaluateExpressionListTyping context [x]    = evaluateExpressionTyping context x
evaluateExpressionListTyping context (x:xs) = evaluateExpressionListTyping (getEvaluatedKcontext $ evaluateExpressionTyping context x) xs

evaluateForTyping :: Kcontext -> KP.For -> EvaluationResult
evaluateForTyping context (KP.For assign_expr cmp_expr inc_expr exprs) = evaluateExpressionsTyping (
                                                                        getEvaluatedKcontext $ evaluateExpressionTyping (
                                                                        getEvaluatedKcontext $ evaluateExpressionTyping (
                                                                        getEvaluatedKcontext $ evaluateExpressionTyping context assign_expr
                                                                                                                        ) cmp_expr
                                                                                                                        ) inc_expr
                                                                                                ) exprs
evaluateIfTyping :: Kcontext -> KP.If -> EvaluationResult
evaluateIfTyping context (KP.If cmp_expr then_exprs else_exprs) = evaluateElseTyping (evaluateExpressionsTyping (getEvaluatedKcontext $ evaluateExpressionTyping  context cmp_expr) then_exprs) else_exprs

evaluateElseTyping :: EvaluationResult -> Maybe KP.Expressions -> EvaluationResult
evaluateElseTyping result Nothing                                    = result
evaluateElseTyping (EvaluationResult t context) (Just expr)
    | t == getEvaluatedType (evaluateExpressionsTyping context expr) = evaluateExpressionsTyping context expr
    | otherwise                                                      = throw $ MismatchedThenElseType (baseTypeToType t) (baseTypeToType $ getEvaluatedType $ evaluateExpressionsTyping context expr)

evaluateWhileTyping :: Kcontext -> KP.While -> EvaluationResult
evaluateWhileTyping context (KP.While cmp_expr exprs) = evaluateExpressionsTyping (getEvaluatedKcontext $ evaluateExpressionTyping context cmp_expr) exprs



evaluateExpressionTyping :: Kcontext -> KP.Expression -> EvaluationResult
evaluateExpressionTyping context expression = evaluateExpressionTyping' context $ buildExpressionTree context expression

-------------------evalexpr------------------

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

-- Un (UnaryPostfix (Postfix (PrimaryIdentifier (Identifier "i")) Nothing)) 
-- [Bin (BinaryOp (Identifier "=")),Un (UnaryPostfix (Postfix (PrimaryLiteral (LiteralDecimal (DecimalConst 1))) Nothing))] 
-- ExprLeaf (UnaryPostfix (Postfix (PrimaryIdentifier (Identifier "i")) Nothing)) 0

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

-- getBinOpType = 

-------------------------------------------
evaluateExpressionTyping' :: Kcontext -> BinaryTreeExpression -> EvaluationResult
evaluateExpressionTyping' context (ExprLeaf unary)                                                                = evaluateUnaryTyping context unary
evaluateExpressionTyping' context (ExprNode binary@(KP.BinaryOp (KP.Identifier "=")) left@(ExprLeaf unary) right) = let rightResult = evaluateExpressionTyping' context right in
                                                                                                                    evaluateAssignment context unary (getEvaluatedType rightResult)
evaluateExpressionTyping' context (ExprNode binary@(KP.BinaryOp (KP.Identifier "=")) _ _ )                        = throw AssignmentToRValue
evaluateExpressionTyping' context (ExprNode (KP.BinaryOp binary) left right)                                      = let leftResult  = evaluateExpressionTyping' context left                                   in
                                                                                                                    let rightResult = evaluateExpressionTyping' (getEvaluatedKcontext leftResult)  right       in
                                                                                                                    evaluateBinOperation (getEvaluatedKcontext rightResult) binary (getEvaluatedType leftResult) (getEvaluatedType rightResult)

evaluateAssignment :: Kcontext -> KP.Unary -> BaseType -> EvaluationResult
evaluateAssignment context (KP.UnaryPostfix (KP.Postfix (KP.PrimaryIdentifier identifier) Nothing)) base_type =
        EvaluationResult
            base_type
            (kContextPushVar (KP.VarAssignment identifier (baseTypeToType base_type)) context)
evaluateAssignment _ _ _                                                                                      = throw AssignmentToRValue

-- evaluateAssignment :: Kcontext -> KP.Identifier -> BaseType -> Maybe TypeSignature -> EvaluationResult

evaluateBinOperation :: Kcontext -> KP.Identifier -> BaseType -> BaseType -> EvaluationResult
evaluateBinOperation context binary left_type right_type = EvaluationResult (getFunctionReturnType (findBinaryMatchingFunction context binary left_type right_type)) context

findBinaryMatchingFunction :: Kcontext -> KP.Identifier -> BaseType -> BaseType -> FunctionTyping
findBinaryMatchingFunction context func_name left_type right_type = findBinaryMatchingFunction' func_name left_type right_type (kContextFind context func_name)

findBinaryMatchingFunction' :: KP.Identifier -> BaseType -> BaseType -> Maybe TypeSignature -> FunctionTyping
findBinaryMatchingFunction' func_name _         _          Nothing                              = throw $ UnknownDefinition func_name
findBinaryMatchingFunction' func_name left_type right_type (Just (PrimitiveFunction func_list)) =
    let found = find (isBinaryFunctionParamMatchingFunction left_type right_type) func_list
    in case found of
    Nothing              -> throw $ MismatchedArgumentType func_name [baseTypeToType left_type, baseTypeToType right_type]
    (Just matching_func) -> matching_func
findBinaryMatchingFunction' func_name left_type right_type (Just (Function func))
    | isBinaryFunctionParamMatchingFunction left_type right_type func                           = func
    | otherwise                                                                                 = throw $ MismatchedArgumentType func_name [baseTypeToType left_type, baseTypeToType right_type]
findBinaryMatchingFunction' func_name _        _           (Just _)                             = throw $ NotABinaryFunction func_name


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


evaluateUnaryTyping :: Kcontext -> KP.Unary -> EvaluationResult
evaluateUnaryTyping context (KP.Unary (KP.UnaryOp identifier) unary) =
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

findUnaryMatchingFunction :: Kcontext -> KP.Identifier -> BaseType -> FunctionTyping
findUnaryMatchingFunction context func_name arg_type = findUnaryMatchingFunction' func_name arg_type (kContextFind context func_name)

findUnaryMatchingFunction' :: KP.Identifier -> BaseType -> Maybe TypeSignature -> FunctionTyping
findUnaryMatchingFunction' func_name _        Nothing                              = throw $ UnknownDefinition func_name
findUnaryMatchingFunction' func_name arg_type (Just (PrimitiveFunction func_list)) =
    let found = find (isUnaryFunctionParamMatchingFunction arg_type) func_list
    in case found of
    Nothing              -> throw $ MismatchedArgumentType func_name [baseTypeToType arg_type]
    (Just matching_func) -> matching_func
findUnaryMatchingFunction' func_name arg_type (Just (Function func))
    | isUnaryFunctionParamMatchingFunction arg_type func                           = func
    | otherwise                                                                    = throw $ MismatchedArgumentType func_name [baseTypeToType arg_type]
findUnaryMatchingFunction' func_name _        (Just _)                             = throw $ NotAUnaryFunction func_name

evaluatePostfixTyping :: Kcontext -> KP.Postfix -> EvaluationResult
evaluatePostfixTyping context (KP.Postfix (KP.PrimaryLiteral    (KP.LiteralDecimal _)) Nothing)          = EvaluationResult Int    context
evaluatePostfixTyping context (KP.Postfix (KP.PrimaryLiteral    (KP.LiteralDouble  _)) Nothing)          = EvaluationResult Double context
evaluatePostfixTyping context (KP.Postfix (KP.PrimaryIdentifier identifier           ) Nothing)          = evaluatePostfixTypingVar   context identifier
evaluatePostfixTyping context (KP.Postfix (KP.PrimaryIdentifier identifier           ) (Just call_expr)) = evaluateFunctionCallTyping context identifier call_expr
evaluatePostfixTyping context (KP.Postfix (KP.PrimaryExpressions expressions         ) Nothing)          = evaluateExpressionsTyping  context expressions
evaluatePostfixTyping context _ = error ""

evaluatePostfixTypingVar :: Kcontext -> KP.Identifier -> EvaluationResult
evaluatePostfixTypingVar context identifier = evaluatePostfixTypingVar' context identifier $ kContextFind context identifier

evaluatePostfixTypingVar' :: Kcontext -> KP.Identifier -> Maybe TypeSignature -> EvaluationResult
evaluatePostfixTypingVar' context identifier Nothing               = throw $ UnknownDefinition identifier
evaluatePostfixTypingVar' context identifier (Just (Var var_type)) = EvaluationResult var_type context
evaluatePostfixTypingVar' context identifier (Just _             ) = throw $ NotAVar identifier

evaluateFunctionCallTyping :: Kcontext -> KP.Identifier -> KP.CallExpression -> EvaluationResult
evaluateFunctionCallTyping context identifier (KP.CallExpression Nothing)                                 = evaluateFunctionCallTyping' context identifier []
evaluateFunctionCallTyping context identifier (KP.CallExpression (Just (KP.CallExpressionArgs arg args))) = evaluateFunctionCallTyping' context identifier []

evaluateFunctionCallTyping' :: Kcontext -> KP.Identifier -> [BaseType] -> EvaluationResult
evaluateFunctionCallTyping' context identifier args = evaluateFunctionCallTyping'' context identifier args (kContextFind context identifier)

evaluateFunctionCallTyping'' :: Kcontext -> KP.Identifier -> [BaseType] -> Maybe TypeSignature -> EvaluationResult
evaluateFunctionCallTyping'' context identifier args Nothing             = throw $ UnknownDefinition identifier
evaluateFunctionCallTyping'' context identifier args (Just (Function _)) = error ""
evaluateFunctionCallTyping'' context identifier args (Just _)            = error ""


-- evaluateUnaryTyping' :: Kcontext -> EvaluationResult -> [FunctionTyping] -> EvaluationResult
-- evaluateUnaryTyping' context right_res []                                 = throw $ MismatchedArgumentType (KP.Identifier "remplacer op") []
-- evaluateUnaryTyping' context right_res (x:xs)
--     | isUnaryFunctionParamMatchingFunction x (getEvaluatedType right_res) = EvaluationResult (getFunctionReturnType x) context
--     | otherwise                                                           = evaluateUnaryTyping' context right_res xs


--     | getUnopType context op == evaluateUnaryType context unary       = evaluateUnaryType context unary
--     | otherwise                                                   = throw $ MismatchedUnaryType (evaluateUnaryType kdefs unary) $ getUnopType defs op
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
--     | otherwise                            = throw $ MismatchedUnaryType (evaluateUnaryType kdefs unary) $ getUnopType defs op
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
--     | otherwise                            = throw $ MismatchedUnaryType (evaluateUnaryType kdefs unary) $ getUnopType defs op
-- evaluateUnaryType kdefs (UNARY_POSTFIX (POSTFIX primary Nothing)) = Int -- get primary type as basic type
-- evaluateUnaryType kdefs (UNARY_POSTFIX (POSTFIX primary _)) = Int -- get primary type as function type

-- getFunctionType :: [Kdefs] -> Identifier -> TYPE
-- getFunctionType (KdefDef ((PROTOTYPE identifier (PROTOTYPE_ARGS _ fnType))) :xs) functionName
--   | identifier == functionName = fnType
--   | otherwise                  = getFunctionType xs functionName


-- find :: Foldable t => (a -> Bool) -> t a -> Maybe a

(<->) :: () -> () -> ()
(<->) _ _ = ()

(<--) :: a -> b -> a
(<--) a _ = a

(-->) :: b -> a -> a
(-->) _ a = a

getEvaluatedType :: EvaluationResult -> BaseType
getEvaluatedType (EvaluationResult base_type _) = base_type

getEvaluatedKcontext :: EvaluationResult -> Kcontext
getEvaluatedKcontext (EvaluationResult _ context) = context
