open System
open FParsec
open Parser
open Inference

[<EntryPoint>]
let main argv =
  let rec repl () =
    printf ">>= "
    let code = Console.ReadLine ()
    if code = "" then repl () else
    let ast = parseProgram code

    match ast with
    | Success (ast, _, _) ->
      let metaExpr = analyze ast Map.empty
      printfn "%A" metaExpr
    | Failure (message, _, _) -> printfn "%A" message

    repl ()
  repl ()
  0 // return an integer exit code
