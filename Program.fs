open System.IO

open LexicalTokenisation
open Parsing
open Eval

[<EntryPoint>]
let main args =
    match args with
    | [| filename |] -> 
        let processFile file =
            let tokens = lex file

            let codeBlock = parse tokens
            let res = evalCodeBlock codeBlock
            printfn "%A" res

        File.ReadAllText(filename)
        |> processFile
        0
    | _ -> 
        printfn "Usage: program <filename>"
        1
