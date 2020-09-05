namespace FrontEnd


type Token =
    | Invalid
    // GEN
    | EOF
    | IDENTIFIER of string
    // keywords
    | READ
    | WRITE
    | IF
    | ELSE
    | WHILE
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
    // Literals
    | TRUE
    | FALSE
    | INTEGERS of string
    
   
module Lexer =
    
    let Match (token : Token List) = function
        | "+" -> PLUS
        | "-" -> MINUS
        | "*" -> MULTIPLICATION
        | "/" -> DIVISION
        | "%" -> MODULO
        | "{" -> LEFT_CURLY
        | "[" -> LEFT_SQUARE
        | "(" -> LEFT_PAREN
        | "}" -> RIGHT_CURLY
        | "]" -> RIGHT_SQUARE
        | ")" -> RIGHT_PAREN
        | ";" -> SEMI_COLON
        | "<" -> GREATER
        | ">" -> LESSER
        | "<=" -> GREATER_EQUAL
        
        
        
    let Scanner (start : uint32) (current : uint32)
    let Lex (source : string) = 
        if source.Length = 0 then
            []
        else 
            let start = 0
            let current = 0
            let tokens = []
            
            while current < source.Length do
                current++
                
            
            []
            
