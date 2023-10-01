module Expression

type BinOp = 
    | Add 
    | Subtract

type Expr =
    | Literal of int
    | BinOpExpr of BinOp * Expr * Expr
    | Identifier of string
    | TupleValue of Expr * Expr

type DataType =
    | Integer
    | Tuple
    | Unspecified

type BindingPattern = 
    | SingleBind of string              // variable name
    | TupleBind of string * string      // (x, y) =

type BindingStatement = {
    pattern: BindingPattern;               // variable name or LHS of tuple deconstruction
    dataType: DataType;
    value: Expr;
}

type CodeBlock =
    BindingStatement list * Expr
