namespace FrontEnd

open System
   

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
        
