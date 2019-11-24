module Inference

open AST
open Type

type Env = Map<string, Type>

let private nextTypeChar = ref (int 'a')

let getNextType () =
  let nextType = string (char !nextTypeChar)
  incr nextTypeChar

  TypeVar nextType

let rec exprToMeta ast (context: Env) =
  match ast with
  | AST.Int i -> MetaInt (i, Type.Int)
  | AST.Bool b -> MetaBool (b, Type.Bool)
  | AST.String s -> MetaString (s, Type.String)
  | AST.Binop (left, op, right) ->
    let metaLeft = exprToMeta left context
    let metaRight = exprToMeta right context
    let newType = getNextType ()
    MetaBinop (metaLeft, op, metaRight, newType)
  | AST.Var x ->
    match context.TryGetValue x with
    | true, typ -> MetaVar (x, typ)
    | false, _ -> failwithf "Undefined variable %s" x
  | AST.Let (id, left, right) ->
    let newContext = context.Add (id, getNextType ())
    let metaLeft = exprToMeta left newContext
    let metaRight = exprToMeta right newContext
    let newType = getNextType ()
    MetaLet (id, metaLeft, metaRight, newType)
  | AST.FuncDec (arg, body) ->
    let argType = getNextType ()
    let newContext = context.Add (arg, argType)
    let metaBody = exprToMeta body newContext
    let typ = Map.find arg newContext
    MetaFuncDec (arg, metaBody, Function1 (typ, getTypeFromMeta metaBody)) // getNextType ()
  | AST.FuncCall (fn, arg) ->
    let metaFn = exprToMeta fn context
    let metaArg = exprToMeta arg context in
    MetaFuncCall (metaFn, metaArg, getNextType ())

let analyze ast context =
  let result = exprToMeta ast context
  nextTypeChar := int 'a'
  result
