module LexicalToken

type Separator = LeftParen | RightParen | SepColon | SepComma
type Operator = OpAdd | OpSubtract | OpBinding | OpAsterisk
type Keyword = Let | IntType

type LexicalToken =
    | Sep of Separator
    | Op of Operator
    | LexLiteral of int
    | LexKeyword of Keyword
    | LexIdentifier of string
    | Newline
