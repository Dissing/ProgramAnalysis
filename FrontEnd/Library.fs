namespace FrontEnd

open System
open System.Linq.Expressions


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
    | INTEGERS of string
    
   
module Lexer =
    
    
    let lexSingle (token: Token) (tokens: Token List) (source: string) =
        (token :: tokens, source.[1..])
        
    let lexInteger (tokens: Token List) (source: string) =
        let rec extractLexeme (source: string) (lexeme: string) =
                if System.Char.IsDigit (source.[0]) then
                    extractLexeme source.[1..] (lexeme + source.[..0])
                else
                    (source, lexeme)
        let (source, lexeme) = extractLexeme source ""
        (INTEGERS(lexeme)::tokens, source)
        
    let lexIdentifier (tokens: Token List) (source: string) =
        let rec extractLexeme (source: string) (lexeme: string) =
            if System.Char.IsLetter (source.[0]) then
                extractLexeme source.[1..] (lexeme + source.[..0])
            else
                (source, lexeme)
        let (source, lexeme) = extractLexeme source ""
        
        match lexeme with
            | "true" -> (TRUE::tokens, source)
            | "false" -> (FALSE::tokens, source)
            | "if" -> (IF::tokens, source)
            | "else" -> (ELSE::tokens, source)
            | "while" -> (WHILE::tokens, source)
            | "read" -> (READ::tokens, source)
            | "write" -> (WRITE::tokens, source)
            | "int" -> (INT::tokens, source)
            | other -> (IDENTIFIER(other)::tokens, source)
        
        
    
    let rec Match ((tokens : Token List), (source : string)) = 
        if source.Length = 0 then
                    (List.rev tokens, "")
        else match source.[0] with
            | ' ' | '\t' | '\n' | '\r' -> Match (tokens, source.[1..])
            | '+' -> Match (lexSingle PLUS tokens source)
            | '-' -> Match (lexSingle MINUS tokens source)
            | '*' -> Match (lexSingle MULTIPLICATION tokens source)
            | '/' -> Match (lexSingle DIVISION tokens source)
            | '%' -> Match (lexSingle MODULO tokens source)
            | '{' -> Match (lexSingle LEFT_CURLY tokens source)
            | '[' -> Match (lexSingle LEFT_SQUARE tokens source)
            | '(' -> Match (lexSingle LEFT_PAREN tokens source)
            | '}' -> Match (lexSingle RIGHT_CURLY tokens source)
            | ']' -> Match (lexSingle RIGHT_SQUARE tokens source)
            | ')' -> Match (lexSingle RIGHT_PAREN tokens source)
            | ';' -> Match (lexSingle SEMI_COLON tokens source)
            | '.' -> Match (lexSingle DOT tokens source)
            | ',' -> Match (lexSingle COMMA tokens source)
            | '<' -> Match (match source.[1] with
                | '=' -> (lexSingle LESSER_EQUAL tokens source.[1..])
                | _ -> (lexSingle LESSER tokens source)
                )
            | '>' -> Match (match source.[1] with
                            | '=' -> (lexSingle GREATER_EQUAL tokens source.[1..])
                            | _ -> (lexSingle GREATER tokens source)
                            )
            | ':' -> Match (match source.[1] with
                            | '=' -> (lexSingle ASSIGN tokens source.[1..])
                            | other -> failwithf "Colon followed by unexpected %c" other
                            )
            | c -> if System.Char.IsLetter(c) then Match (lexIdentifier tokens source)
                   else if System.Char.IsDigit(c) then Match (lexInteger tokens source)
                   else failwithf "Unknown character '%c'" c
        
    let Lex (source : string) =
        Match ([], source)

type UnaryOperator =
    | Not
    
type BinaryOperator =
    | Add
    | Subtract
    | Multiply
    | Divide
    | Modulo
    | Equal
    | NotEqual
    | Greater
    | GreaterEqual
    | Lesser
    | LesserEqual
    | And
    | Or

type AST = Declaration List * Statement List
and Ident = string
and Field = string * string
and Declaration =
    | Integer of Ident
    | Array of Ident * int
    | Struct of Field List
and Statement =
    | Assign of Ident * Expr
    | StructAssign of Ident * Expr list
    | If of Expr * Block * Block option
    | While of Expr * Block
    | Read of Ident
    | Write of Ident
and Block = Statement list
and Expr =
    | Variable of Ident
    | ArrayAccess of Ident * Expr
    | FieldAccess of Ident * Ident
    | Unary of UnaryOperator * Expr
    | Binary of Expr * BinaryOperator * Expr
    

module Parser =
    
    let rec parseItem ((tokens: Token List), ((decls, stmts) as ast: AST)) =
        if tokens = [] then
            ([], (List.rev decls, List.rev stmts))
        else
            match tokens.[0] with
                | IF ->
                    let (tokens, stmt) = parseIf tokens.[1..]
                    parseItem (tokens, (decls, stmt::stmts))
                | WHILE ->
                    let (tokens, stmt) = parseWhile tokens.[1..]
                    parseItem (tokens, (decls, stmt::stmts))
                | READ ->
                    let (tokens, stmt) = parseRead tokens.[1..]
                    parseItem (tokens, (decls, stmt::stmts))
                | WRITE ->
                    let (tokens, stmt) = parseWrite tokens.[1..]
                    parseItem (tokens, (decls, stmt::stmts))
                | LEFT_CURLY ->
                    let (tokens, decl) = parseStructDecl tokens.[1..]
                    parseItem (tokens, (decl::decls, stmts))
                | IDENTIFIER ->
                    let (new_tokens, entry) = parseEntry tokens.[1..]
                    match new_tokens.[0] with
                        | ASSIGN -> parseAssign entry tokens.[1..]
                        | IDENTIFIER -> parse
    and parseBlock (tokens: Token List) =
        //Thomas
        //expect left curly
        //parse item
        //expect right curly
        failwith "Not yet implemented"
    and parseIf (tokens: Token List) =
        //Thomas
        failwith "Not yet implemented"
    and parseWhile (tokens: Token List) =
        //Nicolai
        failwith "Not yet implemented"
    and parseRead (tokens: Token List) =
        //Nicolai
        failwith "Not yet implemented"
    and parseWrite (tokens: Token List) =
        //Nicolai
        failwith "Not yet implemented"
    and parseStructDecl (tokens: Token List) =
        //Thomas
        failwith "Not yet implemented"
    and parseEntry (tokens: Token List) =
        //Thomas
        failwith "Not yet implemented"
    and parseArithmeticExpr (tokens: Token List) =
        //Lasse
        failwith "Not yet implemented"
    and parseBooleanExpr (tokens: Token List) =
        //Lasse
        failwith "Not yet implemented"
    
    let parse (tokens: Token List) =
        parseItem (tokens ([],[]))
        
