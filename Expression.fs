module Expression

type BinOp = 
    | Add 
    | Subtract

type Expr =
    | Literal of int
    | BinOpExpr of BinOp * Expr * Expr
    | Identifier of string

type BindingStatement = {
    name: string;
    value: Expr;
}

type CodeBlock =
    BindingStatement list * Expr
