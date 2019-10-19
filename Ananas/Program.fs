[<EntryPoint>]
let main _ =
    let lexer = Lexer.lex "(+ i 3.14 \"Hello\" 12324) ((("
    let tokenList = TokenList.makeTokenList lexer

    tokenList |> printfn "%A"

    0
