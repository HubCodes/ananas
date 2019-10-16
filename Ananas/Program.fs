[<EntryPoint>]
let main argv =
    let lexer = Lexer.lex <| Seq.toList "((("
    printfn "%A" <| lexer ()
    printfn "%A" <| lexer ()
    printfn "%A" <| lexer ()
    0 // return an integer exit code
