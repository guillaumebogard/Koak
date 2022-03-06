--
-- EPITECH PROJECT, 2022
-- koak
-- File description:
-- Koak.Typing
--

module Koak.Typing          ( checkKoakTyping
                            ) where

import Control.Exception    ( throw )

import Koak.Parser          ( KDEFS(..)
                            , DEFS(..)
                            , PRECEDENCE(..)
                            , PROTOTYPE(..)
                            , PROTOTYPE_ARGS(..)
                            , PROTOTYPE_ID(..)
                            , TYPE(..)
                            , FOR(..)
                            , IF(..)
                            , WHILE(..)
                            , EXPRESSIONS(..)
                            , BIN_OP(..)
                            , EXPRESSION(..)
                            , UN_OP(..)
                            , UNARY(..)
                            , POSTFIX(..)
                            , CALL_EXPR(..)
                            , CALL_EXPR_ARGS(..)
                            , PRIMARY(..)
                            , IDENTIFIER(..)
                            , DECIMAL_CONST(..)
                            , DOUBLE_CONST(..)
                            , LITERAL(..)
                            )

import Koak.TypingContext   ( KCONTEXT
                            , getDefaultKContext
                            )

checkKoakTyping :: [KDEFS] -> ()
checkKoakTyping = checkKoakTyping' getDefaultKContext

checkKoakTyping' :: KCONTEXT -> [KDEFS] -> ()
checkKoakTyping' _       []     = ()
checkKoakTyping' context (x:xs) = checkKoakTyping' (checkKdefTyping context x) xs

checkKdefTyping :: KCONTEXT -> KDEFS -> KCONTEXT
checkKdefTyping context (KDEFS_DEFS defs@(DEFS proto _))  = checkDefsTyping (kContextEnterFunctionCall proto context) defs --> context
checkKdefTyping context (KDEFS_EXPR exprs) = checkExpressionsTyping context exprs

checkDefsTyping :: KCONTEXT -> DEFS -> ()
checkDefsTyping context (DEFS _ exprs) = checkExpressionsTyping context exprs --> ()

checkExpressionsTyping :: KCONTEXT -> EXPRESSIONS -> KCONTEXT
checkExpressionsTyping context (FOR_EXPR    for_expr  ) = checkForTyping            context for_expr --> context
checkExpressionsTyping context (IF_EXPR     if_expr   ) = checkIfTyping             context if_expr --> context
checkExpressionsTyping context (WHILE_EXPR  while_expr) = checkWhileTyping          context while_expr --> context
checkExpressionsTyping context (EXPRESSIONS expr exprs) = checkExpressionListTyping context (expr:exprs)

checkExpressionListTyping :: KCONTEXT -> [EXPRESSION] -> KCONTEXT
checkExpressionListTyping = foldl checkExpressionTyping

checkExpressionTyping :: KCONTEXT -> EXPRESSION -> KCONTEXT
checkExpressionTyping context _ = context

checkForTyping :: KCONTEXT -> FOR -> ()
checkForTyping context (FOR assign_id assign_expr cmp_id cmp_expr inc_expr exprs) = checkExpressionsTyping context exprs

checkIfTyping :: KCONTEXT -> IF -> ()
checkIfTyping context (IF cmp_expr then_exprs (Just else_exprs)) = checkExpressionTyping  context cmp_expr   <->
                                                                   checkExpressionsTyping context then_exprs <->
                                                                   checkExpressionsTyping context else_exprs
checkIfTyping context (IF cmp_expr then_exprs Nothing         )  = checkExpressionTyping  context cmp_expr   <->
                                                                   checkExpressionsTyping context then_exprs

checkWhileTyping :: KCONTEXT -> WHILE -> ()
checkWhileTyping context (WHILE expr exprs) = checkExpressionTyping  context expr <->
                                              checkExpressionsTyping context exprs

evaluateExpressionsType :: KDEFS -> EXPRESSIONS -> TYPE
evaluateExpressionsType kdefs (FOR_EXPR    expr)       = evaluateForType        kdefs expr
evaluateExpressionsType kdefs (IF_EXPR     expr)       = evaluateIfType         kdefs expr
evaluateExpressionsType kdefs (WHILE_EXPR  expr)       = evaluateWhileType      kdefs expr
evaluateExpressionsType kdefs (EXPRESSIONS expr []   ) = evaluateExpressionType kdefs expr
evaluateExpressionsType kdefs (EXPRESSIONS _    exprs) = evaluateExpressionType kdefs $ last exprs

evaluateForType :: KDEFS -> FOR -> TYPE
evaluateForType kdefs (FOR _ _ _ _ _ exprs) = evaluateExpressionsType kdefs exprs

evaluateIfType :: KDEFS -> IF -> TYPE
evaluateIfType kdefs (IF _ exprs _) = evaluateExpressionsType kdefs exprs

evaluateWhileType :: KDEFS -> WHILE -> TYPE
evaluateWhileType kdefs (WHILE _ exprs) = evaluateExpressionsType kdefs exprs

checkPartialExpressionType :: KDEFS -> UNARY -> BIN_OP -> UNARY -> ()
checkPartialExpressionType kdefs first op second = ()
{-| evaluateUnaryType first == evaluateUnaryType second == INT {-getBinOpType binop -} = ()
    -- faudrait plutôt chopper le type des deux args du binop et les comparer aux unarys
    | otherwise = error "Type error"-}

evaluateExpressionType :: KDEFS -> EXPRESSION -> TYPE
evaluateExpressionType kdefs (EXPRESSION unary []) = evaluateUnaryType kdefs unary
evaluateExpressionType kdefs (EXPRESSION first ((binop, second) : xs)) = checkPartialExpressionType kdefs first binop second -->
                                                     evaluateExpressionType' kdefs (EXPRESSION second xs) (getBinOpPrecedence kdefs binop, getBinOpType kdefs binop)

evaluateExpressionType' :: KDEFS -> EXPRESSION -> (PRECEDENCE, TYPE) -> TYPE
evaluateExpressionType' kdefs (EXPRESSION _ []) (_, t) = t
evaluateExpressionType' kdefs (EXPRESSION first ((binop, second) : xs)) (prec, t)
    | prec > getBinOpPrecedence kdefs binop = checkPartialExpressionType kdefs first binop second --> evaluateExpressionType' kdefs (EXPRESSION second xs) (prec, t)
    | otherwise = checkPartialExpressionType kdefs first binop second -->
                  evaluateExpressionType' kdefs (EXPRESSION second xs) (getBinOpPrecedence kdefs binop, getBinOpType kdefs binop)

evaluateUnaryType :: KDEFS -> UNARY -> TYPE
evaluateUnaryType kdefs (UNARY_UN op unary)
    | getUnopType kdefs op == evaluateUnaryType kdefs unary = evaluateUnaryType kdefs unary
    | otherwise                            = throw $ MismatchedUnaryType (evaluateUnaryType kdefs unary) $ getUnopType defs op
evaluateUnaryType kdefs (UNARY_POSTFIX (POSTFIX primary Nothing)) = INT -- get primary type as basic type
evaluateUnaryType kdefs (UNARY_POSTFIX (POSTFIX primary _)) = INT -- get primary type as function type

-- getFunctionType :: [KDEFS] -> IDENTIFIER -> TYPE
-- getFunctionType (KDEFS_DEFS ((PROTOTYPE identifier (PROTOTYPE_ARGS _ fnType))) :xs) functionName
--   | identifier == functionName = fnType
--   | otherwise                  = getFunctionType xs functionName


-- find :: Foldable t => (a -> Bool) -> t a -> Maybe a

(<->) :: () -> () -> ()
(<->) _ _ = ()

(<--) :: a -> b -> a
(<--) a _ = a

(-->) :: b -> a -> a
(-->) _ a = a

