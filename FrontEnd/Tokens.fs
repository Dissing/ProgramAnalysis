namespace FrontEnd

type TokenKind =
    | Invalid
    // GENERAL
    | EOF
    | IDENTIFIER of string
    // keywords
    | READ
    | WRITE
    | IF
    | ELSE
    | WHILE
    | INT
    // Operators
    | PLUS
    | MINUS
    | MULTIPLICATION
    | DIVISION
    | MODULO
    | GREATER
    | LESSER
    | GREATER_EQUAL
    | LESSER_EQUAL
    | EQUAL
    | NOT_EQUAL
    | NOT
    | AND
    | OR
    // Punctuation
    | LEFT_CURLY
    | LEFT_SQUARE
    | LEFT_PAREN
    | RIGHT_CURLY
    | RIGHT_SQUARE
    | RIGHT_PAREN
    | SEMI_COLON
    | ASSIGN
    | DOT
    | COMMA
    // Literals
    | TRUE
    | FALSE
    | INTEGER of int

type Token = TokenKind * Span
