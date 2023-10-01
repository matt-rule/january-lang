open System.IO

open LexicalTokenisation
open Expression
open Parsing

let rec eval (env: Map<string, int>) expr = 
    match expr with
    | Literal n -> n
    | BinOpExpr(op, left, right) -> 
        let leftValue = eval env left
        let rightValue = eval env right
        match op with
        | Add -> leftValue + rightValue
        | Subtract -> leftValue - rightValue
    | Identifier name ->
        match Map.tryFind name env with
        | Some value -> value
        | None -> failwith ("Identifier " + name + " not found!")

let evalCodeBlock (codeBlock: CodeBlock) =
    let (statements, finalExpr) = codeBlock

    let rec evalStatements env statements =
        match statements with
        | [] -> env
        | statement :: rest ->
            let newEnvValue = eval env statement.value
            let newEnv = Map.add statement.name newEnvValue env
            evalStatements newEnv rest

    let finalEnv = evalStatements Map.empty statements
    eval finalEnv finalExpr

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
