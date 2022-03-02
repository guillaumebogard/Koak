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

import Koak.SymbolContext  ( sContextPushNewFrame
                            , sContextPushVar
                            , sContextPushVars
                            , sContextPushPrototype
                            , SYMBOL_CONTEXT(..)
                            , VAR_FRAME_STACK(..)
                            , VAR_FRAME(..)
                            , VAR_SIGNATURE(..)
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

evaluateExpressionType :: EXPRESSION -> TYPE
evaluateExpressionType _ = INT --getLastOp . getFunctionType

-- getLastOp :: EXPRESSION -> TYPE
-- getLastOp (EXPRESSION unary []) = getUnaryType unary
-- getLastOp expr = getLastOp' expr IDENTIFIER ""


--getLastOp' :: EXPRESSION -> IDENTIFIER -> IDENTIFIER 
--getLastOp' (EXPRESSION unary []) bestIdentifier -- c'est le type du dernier BINOP avec la plus haute précédence

getUnaryType :: UNARY -> TYPE
getUnaryType = error "pute"

-- getFunctionType :: [KDEFS] -> IDENTIFIER -> TYPE
-- getFunctionType (KDEFS_DEFS ((PROTOTYPE identifier (PROTOTYPE_ARGS _ fnType))) :xs) functionName
--   | identifier == functionName = fnType
--   | otherwise                  = getFunctionType xs functionName


-- find :: Foldable t => (a -> Bool) -> t a -> Maybe a

(<->) :: () -> () -> ()
(<->) _ _ = ()
