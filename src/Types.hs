data KDEFS          = KDEFS_DEFS DEFS
                    | KDEFS_EXPR EXPRESSIONS

data DEFS           = DEFS PROTOTYPE EXPRESSIONS

data OPERATOR       = UNARY (Maybe DECIMAL_CONST) | BINARY (Maybe DECIMAL_CONST)

data PROTOTYPE      = PROTOTYPE IDENTIFIER PROTOTYPE_ARGS

data PROTOTYPE_ARGS = PROTOTYPE_ARGS [PROTOTYPE_ID] TYPE

data PROTOTYPE_ID   = PROTOTYPE_ID IDENTIFIER TYPE

data TYPE           = INT
                    | DOUBLE
                    | VOID

data FOR            = FOR_EQ IDENTIFIER EXPRESSION
                    | FOR_CMP IDENTIFIER EXPRESSION
                    | FOR_IN EXPRESSION EXPRESSION

data IF             = IF EXPRESSION EXPRESSIONS (Maybe EXPRESSIONS)

data WHILE          = WHILE EXPRESSION EXPRESSIONS

data EXPRESSIONS    = FOR_EXPR FOR
                    | IF_EXPR IF
                    | WHILE_EXPR WHILE
                    | EXPRESSIONS [EXPRESSION]

data BIN_OP         = PLUS
                    | MINUS
                    | MULT
                    | DIV
                    | MOD
                    | LT
                    | GT
                    | EQ
                    | NEQ
                    | ASSIGN

data BINARY_OP      = BINARY_OP_UN BIN_OP UNARY
                    | BINARY_OP_EXPR BIN_OP EXPRESSION

data EXPRESSION     = EXPRESSION UNARY [BINARY_OP]

data UN_OP          = NOT
                    | NEG

data UNARY          = UNARY_UN UN_OP UNARY
                    | UNARY_POSTFIX POSTFIX

data POSTFIX        = POSTFIX PRIMARY (Maybe CALL_EXPR)

newtype CALL_EXPR   = CALL_EXPR (Maybe CALL_EXPR_ARGS)

data CALL_EXPR_ARGS = CALL_EXPR_ARGS EXPRESSION [EXPRESSION]

data PRIMARY        = PRIMARY_IDENTIFIER IDENTIFIER
                    | PRIMARY_LITERAL LITERAL
                    | PRIMARY_EXPRS EXPRESSIONS

newtype IDENTIFIER  = IDENTIFIER String

data DOT

newtype DECIMAL_CONST = DECIMAL_CONST Int

newtype DOUBLE_CONST  = DOUBLE_CONST Double

data LITERAL          = LITERAL_DECIMAL DECIMAL_CONST
                      | LITERAL_DOUBLE DOUBLE_CONST
