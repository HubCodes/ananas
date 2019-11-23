module Parser

open System
open FParsec
open AST

#nowarn "40"

let rec parseInt = pint32 |>> Int

and parseBool = (pstring "true" <|> pstring "false") |>> Boolean.Parse |>> Bool

and normalChar = satisfy (fun c -> c <> '\\' && c <> '"')

and parseString =
    between (pstring "\"") (pstring "\"") (manyChars normalChar) |>> String

and parseLiteral = parseInt <|> parseBool <|> parseString

and parseVar = identifier <| IdentifierOptions () |>> Var

and parseFuncDec =
    (pstring "\\" >>. parseVar) .>>. (pstring "->" >>. parseExpr) |>> FuncDec

and parseExpr = parse {
    return! choice
        [
            parseLiteral
            parseVar
            parseFuncDec
        ]
}

let parseProgram = run parseExpr
