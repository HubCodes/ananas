module AST

type Program = Stmt list

and Stmt =
    | Block of Stmt list
    | For of init: Expr option * cond: Expr option * incr: Expr option * body: Stmt
    | While of cond: Expr * body: Stmt
    | Break
    | Continue
    | If of cond: Expr * ifthen: Stmt * ifnot: Stmt option
    | Return of retval: Expr option
    | Var of typ: string * name: string * init: Expr option
    | Func of name: string * args: Arg list * body: Stmt list
    | Use of name: string

and Arg = {
    name: string
    typ: string
}

and Expr =
    | Primary of Primary
    | Binop of left: Expr * right: Expr * op: Binop
    | Unop of left: Expr * op: Unop

and Primary =
    | Var of string
    | Int of int
    | Double of double
    | String of string

and Binop =
    | Assign
    | Add
    | Sub
    | Mul
    | Div
    | Mod
    | And
    | Or
    | Bitand
    | Bitor
    | Bitxor
    | Eq
    | Neq
    | Gt
    | Lt
    | Gte
    | Lte

and Unop =
    | Not
    | Bitnot
