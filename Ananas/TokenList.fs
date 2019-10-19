module TokenList

open System

let makeTokenList lexer =
    let rec doMakeTokenList acc =
        let nextToken = lexer ()
        match nextToken with
        | Lexer.Token.None -> List.rev acc
        | _ -> doMakeTokenList <| nextToken :: acc
    doMakeTokenList []
