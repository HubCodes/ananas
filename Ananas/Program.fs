open FParsec
open Parser

[<EntryPoint>]
let main argv =
  let result =
    match parseProgram "let y = \\x -> x = 1 in let zzz = 1 in zzz" with
    | Success (result, startPos, endPos) -> printfn "%A" result
    | Failure (message, parseError, _) -> printfn "%s" message
  0 // return an integer exit code
