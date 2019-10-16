module Lexer

open System
open System.Text.RegularExpressions

type LexerState =
    | Start
    | Name
    | String
    | NumberInteger
    | NumberReal

type Token =
    | Name of string
    | String of string
    | Integer of int
    | Double of double
    | OpenParenthesis
    | CloseParenthesis
    | None

let nameRegexPattern = new Regex("[a-zA-Z0-9\~\!\@\#\$\%\^\&\*_\-\+\=]", RegexOptions.Compiled)

let isName (c: char) =
    nameRegexPattern.Match(c.ToString()).Success

let isQuote = function
    | '"' -> true
    | _ -> false

let isSpace = Char.IsWhiteSpace

let isDigit = Char.IsDigit

let isParenthesis = function
    | '('
    | ')' -> true
    | _ -> false

let lex (code: char list) =
    let mutable code = code
    let mutable state = LexerState.Start
    let mutable lexeme = ""
    let mutable token = Token.None

    let rec doLex () =
        let head = List.head code
        let tail = List.tail code

        code <- tail

        match (head, state) with
        | (head, Start) when isName(head) ->
            state <- LexerState.Name
            lexeme <- head.ToString()
            token <- Token.None
            doLex ()
        | (head, Start) when isSpace(head) ->
            state <- LexerState.Start
            lexeme <- ""
            token <- Token.None
            doLex ()
        | (head, Start) when isQuote(head) -> 
            state <- LexerState.String
            lexeme <- ""
            token <- Token.None
            doLex ()
        | (head, Start) when isDigit(head) ->
            state <- LexerState.NumberInteger
            lexeme <- head.ToString()
            token <- Token.None
            doLex ()
        | (head, Start) when isParenthesis(head) ->
            state <- LexerState.Start
            lexeme <- "("
            token <- Token.OpenParenthesis
            token

    doLex