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

let nameRegexPattern = new Regex("[a-zA-Z\~\!\@\#\$\%\^\&\*_\-\+\=\/]", RegexOptions.Compiled)

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

let isNewline = function
    | '\n' -> true
    | _ -> false

let isDot = function
    | '.' -> true
    | _ -> false

let parenthesisCharacterToToken = function
    | '(' -> Token.OpenParenthesis
    | ')' -> Token.CloseParenthesis
    | _ -> Token.None

let lex (code: string) =
    let mutable code = code |> Seq.toList
    let mutable state = LexerState.Start
    let mutable lexeme = ""
    let mutable token = Token.None

    let rec doLex () =
        match code with
        | [] ->
            Token.None
        | head :: tail ->
            code <- tail

            match (head, state) with
            | (head, LexerState.Start) when isName head ->
                state <- LexerState.Name
                lexeme <- head.ToString ()
                token <- Token.None
                doLex ()
            | (head, LexerState.Start) when isSpace head ->
                lexeme <- ""
                token <- Token.None
                doLex ()
            | (head, LexerState.Start) when isQuote head ->
                state <- LexerState.String
                lexeme <- ""
                token <- Token.None
                doLex ()
            | (head, LexerState.Start) when isDigit head ->
                state <- LexerState.NumberInteger
                lexeme <- head.ToString ()
                token <- Token.None
                doLex ()
            | (head, LexerState.Start) when isParenthesis head ->
                lexeme <- head.ToString ()
                token <- parenthesisCharacterToToken head
                token
            | (head, LexerState.Name) when isName head ->
                lexeme <- lexeme + head.ToString ()
                token <- Token.None
                doLex ()
            | (head, LexerState.Name) when isSpace head ->
                state <- LexerState.Start
                token <- Token.Name lexeme
                token
            | (head, LexerState.Name) when isParenthesis head ->
                state <- LexerState.Start
                token <- Token.Name lexeme
                code <- head :: tail
                token
            | (head, LexerState.String) when isQuote head ->
                state <- LexerState.Start
                token <- Token.String lexeme
                token
            | (head, LexerState.String) when not <| isNewline head ->
                lexeme <- lexeme + head.ToString ()
                token <- Token.None
                doLex ()
            | (head, LexerState.NumberInteger) when isDigit head ->
                lexeme <- lexeme + head.ToString ()
                token <- Token.None
                doLex ()
            | (head, LexerState.NumberInteger) when isSpace head ->
                state <- LexerState.Start
                token <- Token.Integer <| Convert.ToInt32 lexeme
                token
            | (head, LexerState.NumberInteger) when isParenthesis head ->
                state <- LexerState.Start
                token <- Token.Integer <| Convert.ToInt32 lexeme
                code <- head :: tail
                token
            | (head, LexerState.NumberInteger) when isDot head ->
                state <- LexerState.NumberReal
                lexeme <- lexeme + head.ToString ()
                token <- Token.None
                doLex ()
            | (head, LexerState.NumberReal) when isDigit head ->
                lexeme <- lexeme + head.ToString ()
                token <- Token.None
                doLex ()
            | (head, LexerState.NumberReal) when isSpace head ->
                state <- LexerState.Start
                token <- Token.Double <| Convert.ToDouble lexeme
                token
            | (head, LexerState.NumberReal) when isParenthesis head ->
                state <- LexerState.Start
                token <- Token.Double <| Convert.ToDouble lexeme
                code <- head :: tail
                token
            | _ ->
                Token.None

    doLex