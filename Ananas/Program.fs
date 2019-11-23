open FParsec
open Parser

[<EntryPoint>]
let main argv =
  let result =
    match parseProgram "\\x -> x = 1" with
    | Success (result, startPos, endPos) -> printfn "%A" result
    | Failure (message, parseError, _) -> printfn "%s" message
  0 // return an integer exit code
