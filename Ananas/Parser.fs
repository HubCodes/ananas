module Parser

open System
open FParsec
open AST

#nowarn "40"

let skipWs = manyChars <| pchar ' '

let stringToBinOperator = function
  | "+" -> Add
  | "-" -> Sub
  | "*" -> Mul
  | "/" -> Div
  | "%" -> Mod
  | ">" -> Gt
  | "<" -> Lt
  | "=" -> Eq
  | "!=" -> Neq
  | str -> failwithf "%s is not an operator." str

let rec parseInt = pint32 .>> skipWs |>> Int

and parseBool =
  (pstring "true" <|> pstring "false") .>> skipWs |>> Boolean.Parse |>> Bool

and normalChar = satisfy (fun c -> c <> '\\' && c <> '"')

and parseString =
  between (pstring "\"") (pstring "\"") (manyChars normalChar)
  .>> skipWs
  |>> String

and parseLiteral = parseInt <|> parseBool <|> parseString

and parseVar = identifier <| IdentifierOptions () .>> skipWs |>> Var

and parseFuncDec =
  (pstring "\\" >>. parseVar)
  .>>. (pstring "->" .>> skipWs >>. parseExpr)
  |>> FuncDec

and parseOperation = new OperatorPrecedenceParser<Expr, string, unit>()

and parseExpr = parse {
  return! choice
      [
          parseFuncDec
          parseOperation.ExpressionParser
          parseLiteral
          parseVar
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
        Binop (expr1, stringToBinOperator (prefix + remOpChars), expr2))
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
