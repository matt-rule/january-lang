module Eval

open Expression

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
