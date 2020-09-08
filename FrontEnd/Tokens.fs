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
    // Operator
    | PLUS
    | MINUS
    | MULTIPLICATION
    | DIVISION
    | MODULO
    | LEFT_CURLY
    | LEFT_SQUARE
    | LEFT_PAREN
    | RIGHT_CURLY
    | RIGHT_SQUARE
    | RIGHT_PAREN
    | SEMI_COLON
    | GREATER
    | LESSER
    | GREATER_EQUAL
    | LESSER_EQUAL
    | EQUAL
    | NOT_EQUAL
    | NOT
    | AND
    | OR
    | ASSIGN
    | DOT
    | COMMA
    // Literals
    | TRUE
    | FALSE
    | INTEGER of int

type Token = TokenKind * Span
