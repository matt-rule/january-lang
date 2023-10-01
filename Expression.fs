module Expression

type BinOp = 
    | Add 
    | Subtract

type Expr =
    | Literal of int
    | BinOpExpr of BinOp * Expr * Expr
    | Identifier of string
    //| TupleValue of int * int

type DataType =
    | Integer
    //| Tuple
    | Unspecified

type BindingStatement = {
    name: string;
    dataType: DataType;
    value: Expr;
}

type CodeBlock =
    BindingStatement list * Expr
