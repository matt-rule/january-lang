module LexicalToken

type Separator = LeftParen | RightParen
type Operator = OpAdd | OpSubtract | OpBinding
type Keyword = Let

type LexicalToken =
    | Sep of Separator
    | Op of Operator
    | LexLiteral of int
    | LexKeyword of Keyword
    | LexIdentifier of string
    | Newline
