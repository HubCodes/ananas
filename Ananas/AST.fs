module AST

type Expr =
    | Int of int
    | Bool of bool
    | String of string
    | Binop of left: Expr * op: BinOperator * right: Expr
    | FuncDec of ID * Expr
    | FuncCall of Expr * Expr

and ID = string

and BinOperator =
    | Add
    | Sub
    | Mul
    | Div
    | Mod
    | Gt
    | Lt
    | Eq
    | Neq
