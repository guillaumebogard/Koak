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

checkKoakTyping :: [KDEFS] -> () 
checkKoakTyping _ = ()
-- checkKoakTyping kdefs = map checkKdefTyping kdefs

checkKdefTyping :: [KDEFS] -> KDEFS -> () 
checkKdefTyping kdefs (KDEFS_DEFS defs)  = checkDefsTyping kdefs defs
checkKdefTyping kdefs (KDEFS_EXPR exprs) = checkExpressionsTyping kdefs exprs

checkDefsTyping :: [KDEFS] -> DEFS -> ()
checkDefsTyping _ _ = ()

checkExpressionsTyping :: [KDEFS] -> EXPRESSIONS -> ()
checkExpressionsTyping kdefs (FOR_EXPR    expr)       = ()
checkExpressionsTyping kdefs (IF_EXPR     expr)       = ()
checkExpressionsTyping kdefs (WHILE_EXPR  expr)       = ()
checkExpressionsTyping kdefs (EXPRESSIONS expr exprs) = ()

checkForTyping :: [KDEFS] -> FOR -> ()
checkForTyping kdefs (FOR _ _ _ _ _ exprs) = checkExpressionsTyping kdefs exprs

checkIfTyping :: [KDEFS] -> IF -> ()
checkIfTyping kdefs (IF _ exprs _) = checkExpressionsTyping kdefs exprs

checkWhileTyping :: [KDEFS] -> WHILE -> ()
checkWhileTyping kdefs (WHILE _ exprs) = checkExpressionsTyping kdefs exprs

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
evaluateExpressionType = getLastOp . getFunctionType

getLastOp :: EXPRESSION -> TYPE
getLastOp (EXPRESSION unary []) = getUnaryType unary
getLastOp expr = getLastOp' expr IDENTIFIER ""


--getLastOp' :: EXPRESSION -> IDENTIFIER -> IDENTIFIER 
--getLastOp' (EXPRESSION unary []) bestIdentifier -- c'est le type du dernier BINOP avec la plus haute précédence

getUnaryType :: UNARY -> TYPE
getUnaryType = error "pute"

getFunctionType :: [KDEFS] -> IDENTIFIER -> TYPE
getFunctionType (KDEFS_DEFS ((PROTOTYPE identifier (PROTOTYPE_ARGS _ fnType))) :xs) functionName
  | identifier == functionName = fnType
  | otherwise                  = getFunctionType xs functionName


-- find :: Foldable t => (a -> Bool) -> t a -> Maybe a