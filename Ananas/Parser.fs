module Parser

open System
open FParsec
open AST

#nowarn "40"

let skipWs = manyChars <| pchar ' '

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
    (pstring "\\" >>. parseVar) .>>. (pstring "->" .>> skipWs >>. parseExpr) |>> FuncDec

and parseExpr = parse {
    return! choice
        [
            parseLiteral
            parseVar
            parseFuncDec
        ]
}

let parseProgram = run parseExpr
