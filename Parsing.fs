module Parsing

open LexicalToken
open Expression

let parse (tokens: LexicalToken list) : CodeBlock =
    let parseOp tokens =
        match tokens with
        | (Op OpAdd) :: rest -> (Add, rest)
        | (Op OpSubtract) :: rest -> (Subtract, rest)
        | _ -> failwith "Expected operator"

    let rec parseExpr tokens =
        let (lhs, remaining) =
            match tokens with
            | (LexIdentifier name) :: rest -> (Identifier name, rest)
            | _ -> parseTerm tokens

        match remaining with
        | (Op _) :: _ ->
            let (op, afterOp) = parseOp remaining
            let (rhs, finalTokens) = parseExpr afterOp
            (BinOpExpr(op, lhs, rhs), finalTokens)
        | _ -> (lhs, remaining)
            
    and parseTerm tokens =
        match tokens with
        | (Sep LeftParen) :: rest ->
            let (firstExpr, afterFirst) = parseExpr rest
            (match afterFirst with
            | (Sep SepComma) :: afterComma ->  (* Check for comma *)
                let (secondExpr, afterSecond) = parseExpr afterComma
                (match afterSecond with
                | (Sep RightParen) :: tail -> (TupleValue(firstExpr, secondExpr), tail) (* Return tuple value *)
                | _ -> failwith "Expected right parenthesis after tuple value")
            | (Sep RightParen) :: tail -> (firstExpr, tail) (* This is a regular parenthesis, not a tuple *)
            | _ -> failwith "Expected comma or right parenthesis")
        | (LexLiteral value) :: rest -> (Literal value, rest)
        | _ -> failwith "Expected term"

    let parseBinding tokens =
        match tokens with
        | (LexKeyword Let) :: (LexIdentifier name) :: (Sep SepColon) :: (LexKeyword IntType) :: (Op OpAsterisk) :: (LexKeyword IntType) :: (Op OpBinding) :: rest ->
            let (expr, remaining) = parseExpr rest
            (match remaining with
            | Newline :: tail -> ({name = name; dataType = Tuple; value = expr}, tail)
            | _ -> failwith "Expected newline after binding")
        | (LexKeyword Let) :: (LexIdentifier name) :: (Sep SepColon) :: (LexKeyword IntType) :: (Op OpBinding) :: rest ->
            let (expr, remaining) = parseExpr rest
            (match remaining with
            | Newline :: tail -> ({name = name; dataType = Integer; value = expr}, tail)
            | _ -> failwith "Expected newline after binding")
        | (LexKeyword Let) :: (LexIdentifier name) :: (Op OpBinding) :: rest ->
            let (expr, remaining) = parseExpr rest
            (match remaining with
            | Newline :: tail -> ({name = name; dataType = Integer; value = expr}, tail)
            | _ -> failwith "Expected newline after binding")
        | _ -> failwith "Expected binding statement"

    let rec parseCodeBlock tokens =
        let rec parseBindings acc tokens =
            match tokens with
            | (LexKeyword Let) :: _ -> 
                let (binding, remaining) = parseBinding tokens
                parseBindings (binding :: acc) remaining
            | _ -> (List.rev acc, tokens) (* Return accumulated bindings and remaining tokens *)
        
        let (bindings, afterBindings) = parseBindings [] tokens
        let (expr, remaining) = parseExpr afterBindings
        if remaining = [] then (bindings, expr)
        else failwith "Unconsumed tokens remaining after parsing code block"

    parseCodeBlock tokens