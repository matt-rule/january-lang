module LexicalTokenisation

open System
open LexicalToken

let lex input =
    let rec aux (s: string) (i: int) (acc: LexicalToken list) =
        if i >= s.Length then
            List.rev acc  // Reverse the list once at the end because we prepended each element
        else
            match s.[i] with
            | '(' -> aux s (i+1) (Sep LeftParen :: acc)
            | ')' -> aux s (i+1) (Sep RightParen :: acc)
            | '+' -> aux s (i+1) (Op OpAdd :: acc)
            | '-' -> aux s (i+1) (Op OpSubtract :: acc)
            | '=' -> aux s (i+1) (Op OpBinding :: acc)
            | ':' -> aux s (i+1) (Op OpColon :: acc)
            // | ',' -> aux s (i+1) (Op OpComma :: acc)
            // | '*' -> aux s (i+1) (Op OpAsterisk :: acc)
            | c when Char.IsDigit(c) ->
                // Parse number literals that may have more than one digit
                let endIdx, value = s.[i..].ToString().Split([|'('; ')'; '+'; '-'; '='; '\n'|], 2) |> fun arr -> (i + arr.[0].Length, int arr.[0])
                aux s endIdx (LexLiteral value :: acc)
            | 'l' when s.Length - i >= 3 && s.Substring(i, 3) = "let" ->  // Check for "let" keyword
                aux s (i+3) (LexKeyword Let :: acc) 
            | 'i' when s.Length - i >= 3 && s.Substring(i, 3) = "int" ->  // Check for ": int" type specification
                aux s (i+3) (LexKeyword IntType :: acc)
            | c when Char.IsLetter(c) ->
                // Parse identifier
                let endIdx, ident = s.[i..].ToString().Split([|'('; ')'; '+'; '-'; '='; ' '; '\n'|], 2) |> fun arr -> (i + arr.[0].Length, arr.[0])
                aux s endIdx (LexIdentifier ident :: acc)
            | '\n' -> aux s (i+1) (Newline :: acc)  // Added support for newline
            | _ -> aux s (i+1) acc  // Skip other characters
    aux input 0 []
