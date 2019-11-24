module AST

type Expr =
  | Int of int
  | Bool of bool
  | String of string
  | Binop of left: Expr * op: ID * right: Expr
  | Var of ID
  | Let of ID * Expr * Expr
  | FuncDec of arg: Expr * body: Expr
  | FuncCall of Expr * Expr

and ID = ID of string
