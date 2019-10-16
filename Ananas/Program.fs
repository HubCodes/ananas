[<EntryPoint>]
let main _ =
    let lexer = Lexer.lex "(+ i 3.14 \"Hello\" 12324) ((("

    let rec lexUntilEnd () =
        let token = lexer ()

        match token with
        | Lexer.Token.None ->
            printfn "End."
        | _ ->
            printfn "%A" token
            lexUntilEnd ()

    lexUntilEnd ()

    0
