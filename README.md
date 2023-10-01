When adding a new feature, always remember to consider the following, in order:

1. The input file (test.jan)
2. Changes to LexicalToken.fs
3. Changes to Lexing.fs (which converts from input to lexical tokens)
4. Changes to Expression.fs
5. Changes to Parsing.fs (which converts to lexical tokens to expressions and their relatives)
6. Changes to Evaluation.fs (which operates on expressions)
