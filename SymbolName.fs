module SymbolName

open System

let (|ValidSymbolName|_|) (input: string) =
    if System.Text.RegularExpressions.Regex.IsMatch(input, @"^[a-zA-Z][a-zA-Z_]*$")
    then Some input
    else None

type SymbolName private (value: string) = 
    member this.Value = value

    static member Create (input: string) =
        match input with
        | ValidSymbolName valid -> Some (SymbolName valid)
        | _ -> None
