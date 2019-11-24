module AST

open Type

type Expr =
  | Int of int
  | Bool of bool
  | String of string
  | Binop of left: Expr * op: ID * right: Expr
  | Var of ID
  | Let of ID * Expr * Expr
  | FuncDec of arg: ID * body: Expr
  | FuncCall of Expr * Expr

and ID = ID of string

type MetaExpr =
  | MetaInt of int * Type
  | MetaBool of bool * Type
  | MetaString of string * Type
  | MetaVar of ID * Type
  | MetaLet of ID * MetaExpr * MetaExpr * Type
  | MetaFuncDec of arg: MetaExpr * body: MetaExpr * Type
  | MetaFuncCall of MetaExpr * MetaExpr * Type

let getTypeFromMeta = function
  | MetaInt (_, t) -> t
  | MetaBool (_, t) -> t
  | MetaString (_, t) -> t
  | MetaVar (_, t) -> t
  | MetaLet (_, _, _, t) -> t
  | MetaFuncDec (_, _, t) -> t
  | MetaFuncCall (_, _, t) -> t