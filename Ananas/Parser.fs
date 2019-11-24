module Parser

open System
open FParsec
open AST

#nowarn "40"

let skipWs = spaces

let isKeyword x =
  match x with
  | "let" -> true
  | _ -> false

let rec parseInt = pint32 .>> skipWs |>> Int

and parseBool =
  (pstring "true" <|> pstring "false") .>> skipWs |>> Boolean.Parse |>> Bool

and normalChar = satisfy (fun c -> c <> '\\' && c <> '"')

and parseString =
  between (pstring "\"") (pstring "\"") (manyChars normalChar)
  .>> skipWs
  |>> String

and parseLiteral = parseInt <|> parseBool <|> parseString

and parseIdentifier =
  (attempt (identifier <| IdentifierOptions () >>= (fun str ->
    if not (isKeyword str) then preturn str .>> skipWs else pzero)))

and parseVar = parseIdentifier |>> ID |>> Var

and parseLet =
  ((pstring "let ") >>. parseIdentifier |>> ID)
  .>>. ((pstring "=" >>. skipWs) >>. parseExpr)
  .>>. ((pstring "in" >>. skipWs) >>. parseExpr)
  |>> (fun ((name, expr1), expr2) -> Let (name, expr1, expr2))

and parseFuncDec =
  (pstring "\\" >>. parseIdentifier |>> ID)
  .>>. (pstring "->" .>> skipWs >>. parseExpr)
  |>> FuncDec

and parseFuncCall =
  pstring "(" >>. parseExpr .>>. parseExpr .>> pstring ")" |>> FuncCall

and parseOperation = new OperatorPrecedenceParser<Expr, string, unit>()

and parseExpr = parse {
  return! choice
    [
      parseFuncDec
      parseOperation.ExpressionParser
      parseLiteral
      parseLet
      parseFuncCall
      parseVar
      pstring "(" >>. parseExpr .>> pstring ")"
    ]
}

parseOperation.TermParser <- parseLiteral <|> parseVar <|> parseFuncDec

let isSymbolicOperatorChar = isAnyOf "+-*/%><=!"
let remainingOpChars = manySatisfy isSymbolicOperatorChar .>> skipWs

let addSymbolicInfixOperators prefix precedence associativity =
  let op =
    InfixOperator (
      prefix,
      remainingOpChars,
      precedence,
      associativity,
      (),
      fun remOpChars expr1 expr2 ->
        Binop (expr1, ID (prefix + remOpChars), expr2))
  parseOperation.AddOperator (op)

addSymbolicInfixOperators "+" 10 Associativity.Left
addSymbolicInfixOperators "-" 10 Associativity.Left
addSymbolicInfixOperators "*" 20 Associativity.Left
addSymbolicInfixOperators "/" 20 Associativity.Left
addSymbolicInfixOperators "%" 20 Associativity.Left
addSymbolicInfixOperators ">" 5 Associativity.None
addSymbolicInfixOperators "<" 5 Associativity.None
addSymbolicInfixOperators "=" 5 Associativity.None
addSymbolicInfixOperators "!=" 5 Associativity.None

let parseProgram = run parseExpr
