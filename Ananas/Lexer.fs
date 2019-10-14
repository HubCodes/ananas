module Lexer

type LexerState =
    | Start
    | Name
    | String
    | NumberInteger
    | NumberReal

let rec lex (code: char list) state = ignore