module LexicalToken

type Separator = LeftParen | RightParen
type Operator = OpAdd | OpSubtract | OpBinding | OpColon// | OpComma | OpAsterisk
type Keyword = Let | IntType

type LexicalToken =
    | Sep of Separator
    | Op of Operator
    | LexLiteral of int
    | LexKeyword of Keyword
    | LexIdentifier of string
    | Newline
