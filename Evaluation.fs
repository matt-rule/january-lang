module Eval

open Expression

type DataValue =
    | DataInteger of int
    | DataTuple of int * int

type DataStore = Map<string, DataValue>

let rec eval (env: DataStore) (expr: Expr): DataValue = 
    match expr with
    | Literal n -> DataInteger n
    | BinOpExpr(op, left, right) -> 
        let leftValue = 
            match eval env left with
            | DataInteger i -> i
            | DataTuple _ -> failwith "Cannot perform arithmetic on a tuple"
            
        let rightValue = 
            match eval env right with
            | DataInteger i -> i
            | DataTuple _ -> failwith "Cannot perform arithmetic on a tuple"
            
        match op with
        | Add -> DataInteger (leftValue + rightValue)
        | Subtract -> DataInteger (leftValue - rightValue)
    | Identifier name ->
        match Map.tryFind name env with
        | Some value -> value
        | None -> failwith ("Identifier " + name + " not found!")
    | TupleValue (left, right) ->
        let leftValue = 
            match eval env left with
            | DataInteger i -> i
            | DataTuple _ -> failwith "Only int * int tuples are supported"
            
        let rightValue = 
            match eval env right with
            | DataInteger i -> i
            | DataTuple _ -> failwith "Only int * int tuples are supported"
            
        DataTuple (leftValue, rightValue)

let evalCodeBlock (codeBlock: CodeBlock) =
    let (statements, finalExpr) = codeBlock

    let rec evalStatements env statements =
        match statements with
        | [] -> env
        | statement :: rest ->
            let rhsValue = eval env statement.value
            let newEnv = 
                match statement.pattern, rhsValue with
                | SingleBind name, _ -> 
                    Map.add name rhsValue env
                | TupleBind (name1, name2), DataTuple (val1, val2) -> 
                    Map.add name1 (DataInteger val1) (Map.add name2 (DataInteger val2) env)
                | _, _ -> failwith "Mismatch between binding pattern and value type."
            evalStatements newEnv rest

    let finalEnv = evalStatements Map.empty statements
    eval finalEnv finalExpr
