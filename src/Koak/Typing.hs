--
-- EPITECH PROJECT, 2022
-- koak
-- File description:
-- Koak.Typing
--

module Koak.Typing          ( checkKoakTyping
                            ) where

import Control.Exception    ( throw, evaluate )

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

newtype VAR_FRAME  = VAR_FRAME [(IDENTIFIER, TYPE)]

type VAR_FRAME_STACK = [VAR_FRAME]

data VAR_CONTEXT = VAR_CONTEXT [KDEFS] VAR_FRAME_STACK

-- Au lieU de se balader avec plein de KDEF, on va se balader avec la structure suivante
-- La structure est pas fini, mais elle contient toute les [KDEFS], et la liste de variables locales déclarés, comme une stack
-- Et il faudra faire une fonction pour push et pop des variables et des stack frame

-- J'utilise l'op <-> pour chainer mes appelle de check___Typing
    
checkKoakTyping :: [KDEFS] -> ()
checkKoakTyping _ = ()
-- checkKoakTyping kdefs = map checkKdefTyping kdefs

checkKdefTyping :: [KDEFS] -> KDEFS -> ()
checkKdefTyping kdefs (KDEFS_DEFS defs)  = checkDefsTyping kdefs defs
checkKdefTyping kdefs (KDEFS_EXPR exprs) = checkExpressionsTyping kdefs exprs

checkDefsTyping :: [KDEFS] -> DEFS -> ()
checkDefsTyping _ _ = ()

checkExpressionsTyping :: [KDEFS] -> EXPRESSIONS -> ()
checkExpressionsTyping kdefs (FOR_EXPR    for_expr  ) = checkForTyping   kdefs for_expr
checkExpressionsTyping kdefs (IF_EXPR     if_expr   ) = checkIfTyping    kdefs if_expr
checkExpressionsTyping kdefs (WHILE_EXPR  while_expr) = checkWhileTyping kdefs while_expr
checkExpressionsTyping kdefs (EXPRESSIONS expr exprs) = checkExpressionTyping kdefs expr <->
                                                        checkExpressionListTyping kdefs exprs

checkExpressionListTyping :: [KDEFS] -> [EXPRESSION] -> ()
checkExpressionListTyping _ []         = ()
checkExpressionListTyping kdefs (x:xs) = checkExpressionTyping kdefs x <-> checkExpressionListTyping kdefs xs

checkExpressionTyping :: [KDEFS] -> EXPRESSION -> ()
checkExpressionTyping kdefs _= ()

checkForTyping :: [KDEFS] -> FOR -> ()
checkForTyping kdefs (FOR assign_id assign_expr cmp_id cmp_expr inc_expr exprs) = checkExpressionsTyping kdefs exprs

checkIfTyping :: [KDEFS] -> IF -> ()
checkIfTyping kdefs (IF cmp_expr then_exprs (Just else_exprs)) = checkExpressionTyping  kdefs cmp_expr   <->
                                                                 checkExpressionsTyping kdefs then_exprs <->
                                                                 checkExpressionsTyping kdefs else_exprs
checkIfTyping kdefs (IF cmp_expr then_exprs Nothing         )  = checkExpressionTyping  kdefs cmp_expr   <->
                                                                 checkExpressionsTyping kdefs then_exprs

checkWhileTyping :: [KDEFS] -> WHILE -> ()
checkWhileTyping kdefs (WHILE expr exprs) = checkExpressionTyping  kdefs expr <->
                                            checkExpressionsTyping kdefs exprs



evaluateExpressionsType :: EXPRESSIONS -> TYPE
evaluateExpressionsType (FOR_EXPR    expr)       = evaluateForType   expr
evaluateExpressionsType (IF_EXPR     expr)       = evaluateIfType    expr
evaluateExpressionsType (WHILE_EXPR  expr)       = evaluateWhileType expr
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
