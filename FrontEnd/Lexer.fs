namespace FrontEnd

module Lexer =
    
    type LexingContext = string * int
    let head (ctx: LexingContext) = (fst ctx).[snd ctx]
    let tail (ctx: LexingContext) = (fst ctx, (snd ctx) + 1)
    let currentPos (ctx: LexingContext) = snd ctx
    
    let isDone (ctx: LexingContext) = (fst ctx).Length = (snd ctx)
    
    let lexSingle (token: TokenKind) (tokens: Token List) (ctx: LexingContext) =
        let pos = currentPos ctx
        let span = { From = pos; To = pos; }
        ((token, span)::tokens, tail ctx)
        
    let lexInteger (tokens: Token List) (ctx: LexingContext) =
        let from_pos = currentPos ctx
        let rec extractLexeme (ctx: LexingContext) (lexeme: string) =
                if not (isDone ctx) && System.Char.IsDigit (head ctx) then
                    extractLexeme (tail ctx) (lexeme + string (head ctx))
                else
                    (ctx, lexeme)
        let (ctx, lexeme) = extractLexeme ctx ""
        let number = int lexeme
        let to_pos = (currentPos ctx) - 1
        let span = { From = from_pos; To = to_pos; }
        ((INTEGER(number), span)::tokens, ctx)
        
    let lexIdentifier (tokens: Token List) (ctx: LexingContext) =
        let from_pos = currentPos ctx
        let rec extractLexeme (ctx: LexingContext) (lexeme: string) =
            if not (isDone ctx) && System.Char.IsLetter (head ctx) then
                extractLexeme (tail ctx) (lexeme + string (head ctx))
            else
                (ctx, lexeme)
        let (ctx, lexeme) = extractLexeme ctx ""
        let to_pos = (currentPos ctx) - 1
        
        let kind =
            match lexeme with
            | "true" -> TRUE
            | "false" -> FALSE
            | "if" -> IF
            | "else" -> ELSE
            | "while" -> WHILE
            | "read" -> READ
            | "write" -> WRITE
            | "int" -> INT
            | other -> IDENTIFIER(other)
            
        let span = { From = from_pos; To = to_pos; }
        
        ((kind, span)::tokens, ctx)
        
        
    
    let rec Scan ((tokens : Token List), (ctx : LexingContext)) = 
        if isDone ctx then
            (List.rev tokens, "")
        else
            match head ctx with
            | ' ' | '\t' | '\n' | '\r' -> Scan (tokens, tail ctx)
            | '+' -> Scan (lexSingle PLUS tokens ctx)
            | '-' -> Scan (lexSingle MINUS tokens ctx)
            | '*' -> Scan (lexSingle MULTIPLICATION tokens ctx)
            | '/' -> Scan (lexSingle DIVISION tokens ctx)
            | '%' -> Scan (lexSingle MODULO tokens ctx)
            | '{' -> Scan (lexSingle LEFT_CURLY tokens ctx)
            | '[' -> Scan (lexSingle LEFT_SQUARE tokens ctx)
            | '(' -> Scan (lexSingle LEFT_PAREN tokens ctx)
            | '}' -> Scan (lexSingle RIGHT_CURLY tokens ctx)
            | ']' -> Scan (lexSingle RIGHT_SQUARE tokens ctx)
            | ')' -> Scan (lexSingle RIGHT_PAREN tokens ctx)
            | ';' -> Scan (lexSingle SEMI_COLON tokens ctx)
            | '.' -> Scan (lexSingle DOT tokens ctx)
            | ',' -> Scan (lexSingle COMMA tokens ctx)
            | '<' -> Scan (
                            match head ctx with
                            | '=' -> (lexSingle LESSER_EQUAL tokens (tail ctx))
                            | _ -> (lexSingle LESSER tokens ctx)
                            )
            | '>' -> Scan (
                            match head ctx with
                            | '=' -> (lexSingle GREATER_EQUAL tokens (tail ctx))
                            | _ -> (lexSingle GREATER tokens ctx)
                            )
            | ':' -> Scan (
                            match head (tail ctx) with
                            | '=' -> let pos = currentPos ctx
                                     let span = { From = pos; To = pos+1; }
                                     ((ASSIGN, span)::tokens, tail (tail ctx))
                            | other -> failwithf "Colon followed by unexpected %c" other
                            )
            | c -> if System.Char.IsLetter(c) then Scan (lexIdentifier tokens ctx)
                   else if System.Char.IsDigit(c) then Scan (lexInteger tokens ctx)
                   else failwithf "Unknown character '%c'" c
        
    let lex (source : string) =
        let ctx = (source, 0)
        let tokens, _ = Scan ([], ctx)
        tokens
