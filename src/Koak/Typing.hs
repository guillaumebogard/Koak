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
                            , VAR_SIGNATURE(..)
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

import Koak.SymbolContext   ( sContextPushNewFrame
                            , sContextPushVar
                            , sContextPushVars
                            , sContextPushPrototype
                            , SYMBOL_CONTEXT(..)
                            , VAR_FRAME_STACK(..)
                            , VAR_FRAME(..)
                            )

checkKoakTyping :: [KDEFS] -> ()
checkKoakTyping kdefs = checkKoakTyping' (SYMBOL_CONTEXT kdefs []) kdefs

checkKoakTyping' :: SYMBOL_CONTEXT -> [KDEFS] -> ()
checkKoakTyping' _       []     = ()
checkKoakTyping' context (x:xs) = checkKdefTyping context x <->
                                  checkKoakTyping' context xs

checkKdefTyping :: SYMBOL_CONTEXT -> KDEFS -> ()
checkKdefTyping context (KDEFS_DEFS defs)  = checkDefsTyping        context defs
checkKdefTyping context (KDEFS_EXPR exprs) = checkExpressionsTyping context exprs

checkDefsTyping :: SYMBOL_CONTEXT -> DEFS -> ()
checkDefsTyping context (DEFS prototype exprs) = checkExpressionsTyping (sContextPushPrototype context prototype) exprs

checkExpressionsTyping :: SYMBOL_CONTEXT -> EXPRESSIONS -> ()
checkExpressionsTyping context (FOR_EXPR    for_expr  ) = checkForTyping            context for_expr
checkExpressionsTyping context (IF_EXPR     if_expr   ) = checkIfTyping             context if_expr
checkExpressionsTyping context (WHILE_EXPR  while_expr) = checkWhileTyping          context while_expr
checkExpressionsTyping context (EXPRESSIONS expr exprs) = checkExpressionTyping     context expr <->
                                                          checkExpressionListTyping context exprs

checkExpressionListTyping :: SYMBOL_CONTEXT -> [EXPRESSION] -> ()
checkExpressionListTyping _ []           = ()
checkExpressionListTyping context (x:xs) = checkExpressionTyping     context x <->
                                           checkExpressionListTyping context xs

checkExpressionTyping :: SYMBOL_CONTEXT -> EXPRESSION -> ()
checkExpressionTyping context _ = ()

checkForTyping :: SYMBOL_CONTEXT -> FOR -> ()
checkForTyping context (FOR assign_id assign_expr cmp_id cmp_expr inc_expr exprs) = checkExpressionsTyping context exprs

checkIfTyping :: SYMBOL_CONTEXT -> IF -> ()
checkIfTyping context (IF cmp_expr then_exprs (Just else_exprs)) = checkExpressionTyping  context cmp_expr   <->
                                                                   checkExpressionsTyping context then_exprs <->
                                                                   checkExpressionsTyping context else_exprs
checkIfTyping context (IF cmp_expr then_exprs Nothing         )  = checkExpressionTyping  context cmp_expr   <->
                                                                   checkExpressionsTyping context then_exprs

checkWhileTyping :: SYMBOL_CONTEXT -> WHILE -> ()
checkWhileTyping context (WHILE expr exprs) = checkExpressionTyping  context expr <->
                                              checkExpressionsTyping context exprs



evaluateExpressionsType :: EXPRESSIONS -> TYPE
evaluateExpressionsType (FOR_EXPR    expr)       = evaluateForType        expr
evaluateExpressionsType (IF_EXPR     expr)       = evaluateIfType         expr
evaluateExpressionsType (WHILE_EXPR  expr)       = evaluateWhileType      expr
evaluateExpressionsType (EXPRESSIONS expr []   ) = evaluateExpressionType expr
evaluateExpressionsType (EXPRESSIONS _    exprs) = evaluateExpressionType $ last exprs

evaluateForType :: FOR -> TYPE
evaluateForType (FOR _ _ _ _ _ exprs) = evaluateExpressionsType exprs

evaluateIfType :: IF -> TYPE
evaluateIfType (IF _ exprs _) = evaluateExpressionsType exprs

evaluateWhileType :: WHILE -> TYPE
evaluateWhileType  (WHILE _ exprs) = evaluateExpressionsType exprs

checkPartialExpressionType :: UNARY -> BIN_OP -> UNARY -> ()
checkPartialExpressionType first op second = ()
{-| evaluateUnaryType first == evaluateUnaryType second == INT {-getBinOpType binop -} = ()
    -- faudrait plutôt chopper le type des deux args du binop et les comparer aux unarys
    | otherwise = error "Type error"-}

evaluateExpressionType :: EXPRESSION -> TYPE
evaluateExpressionType (EXPRESSION unary []) = evaluateUnaryType unary
evaluateExpressionType (EXPRESSION first ((binop, second) : xs)) = checkPartialExpressionType first binop second -->
                                                     evaluateExpressionType' (EXPRESSION second xs) (getBinOpPrecedence binop, getBinOpType binop)

evaluateExpressionType' :: EXPRESSION -> (PRECEDENCE, TYPE) -> TYPE
evaluateExpressionType' (EXPRESSION _ []) (_, t) = t
evaluateExpressionType' (EXPRESSION first ((binop, second) : xs)) (prec, t)
    | prec > getBinOpPrecedence binop = checkPartialExpressionType first binop second --> evaluateExpressionType' (EXPRESSION second xs) (prec, t)
    | otherwise = checkPartialExpressionType first binop second -->
                  evaluateExpressionType' (EXPRESSION second xs) (getBinOpPrecedence binop, getBinOpType binop)

evaluateUnaryType :: UNARY -> TYPE
evaluateUnaryType (UNARY_UN op unary)
    | getUnopType op == evaluateUnaryType unary = evaluateUnaryType unary
    | otherwise                            = throw $ MismatchedUnaryType (evaluateUnaryType unary) $ getUnopType op
evaluateUnaryType (UNARY_POSTFIX (POSTFIX primary Nothing)) = INT -- get primary type as basic type
evaluateUnaryType (UNARY_POSTFIX (POSTFIX primary _)) = INT -- get primary type as function type

-- getFunctionType :: [KDEFS] -> IDENTIFIER -> TYPE
-- getFunctionType (KDEFS_DEFS ((PROTOTYPE identifier (PROTOTYPE_ARGS _ fnType))) :xs) functionName
--   | identifier == functionName = fnType
--   | otherwise                  = getFunctionType xs functionName


-- find :: Foldable t => (a -> Bool) -> t a -> Maybe a

(<->) :: () -> () -> ()
(<->) _ _ = ()

(-->) :: () -> a -> a
(-->) _ a = a
