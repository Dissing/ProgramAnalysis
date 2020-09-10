namespace FrontEnd

open FrontEnd.AST

module Parser =
    
    type ParsingContext = SourceFile * Token List
    
    let head (ctx: ParsingContext) = (snd ctx).Head
    let tail (ctx: ParsingContext) = (fst ctx, (snd ctx).Tail)
    let kindOfHead (ctx: ParsingContext) = fst (snd ctx).Head
    let isDone (ctx: ParsingContext) = (snd ctx).IsEmpty
    
    let accept (kind: TokenKind) (ctx: ParsingContext) =
            if not (isDone ctx) && (kindOfHead ctx) = kind then
                (true, tail ctx)
            else
                (false, ctx)
    
    let expect (kind: TokenKind) (ctx: ParsingContext) =
        let (correct, ctx) = accept kind ctx
        if correct then ctx
        else failwithf "Expected token kind %A but got %A" kind (head ctx)
        
    let prefixPrecedence = 6
    let precedenceOf = function
        | MULTIPLICATION -> 5
        | DIVISION -> 5
        | MODULO -> 5
        | PLUS -> 4
        | MINUS -> 4
        | LESSER -> 3
        | LESSER_EQUAL -> 3
        | GREATER -> 3
        | GREATER_EQUAL -> 3
        | EQUAL -> 3
        | NOT_EQUAL -> 3
        | AND -> 2
        | OR -> 1
        | _ -> 0
    
    let rec parseItems (ctx: ParsingContext, ((decls, stmts): AST.AST)) =
        if (isDone ctx) || kindOfHead ctx = RIGHT_CURLY then
            (ctx, (List.rev decls, List.rev stmts))
        else
            match kindOfHead ctx with
                | IF ->
                    let (ctx, stmt) = parseIf ctx
                    parseItems (ctx, (decls, stmt::stmts))
                | WHILE ->
                    let (ctx, stmt) = parseWhile ctx
                    parseItems (ctx, (decls, stmt::stmts))
                | READ ->
                    let (ctx, stmt) = parseRead ctx
                    parseItems (ctx, (decls, stmt::stmts))
                | WRITE ->
                    let (ctx, stmt) = parseWrite ctx
                    parseItems (ctx, (decls, stmt::stmts))
                | LEFT_CURLY ->
                    let (ctx, decl) = parseStructDecl ctx
                    parseItems (ctx, (decl::decls, stmts))
                | IDENTIFIER(_) ->
                    let (ctx, location) = parseLocation ctx
                    let ctx = expect ASSIGN ctx
                    let (ctx, rhs) = parseArithmeticExpr ctx
                    let ctx = expect SEMI_COLON ctx
                    let stmt = Assign (location, rhs)
                    parseItems (ctx, (decls, stmt::stmts))
                        
                | other -> failwithf "Unexpected token %A" other
    and parseBlock (ctx: ParsingContext) =
        let ctx = expect LEFT_CURLY ctx
        let (ctx, ast) = parseItems (ctx, ([], []))
        let ctx = expect RIGHT_CURLY ctx
        (ctx, ast)
    and parseIf (ctx: ParsingContext) =
        //Thomas
        failwith "Not yet implemented"
    and parseWhile (ctx: ParsingContext) =
        let ctx = expect WHILE ctx
        let ctx = expect LEFT_PAREN ctx
        let (ctx, condition) = parseBooleanExpr ctx
        let ctx = expect RIGHT_PAREN ctx
        let (ctx, block) = parseBlock ctx
        (ctx, Statement.While (condition, block))
    and parseRead (ctx: ParsingContext) =
        let ctx = expect READ ctx
        let (ctx, location) = parseLocation ctx
        let ctx = expect SEMI_COLON ctx
        (ctx, Statement.Read location)
    and parseWrite (ctx: ParsingContext) =
        let ctx = expect WRITE ctx
        let (ctx, expr) = parseArithmeticExpr ctx
        let ctx = expect SEMI_COLON ctx
        (ctx, Statement.Write expr)
    and parseStructDecl (ctx: ParsingContext) =
        failwith "Not yet implemented"
    and parseIdent (ctx: ParsingContext) =
        match kindOfHead ctx with
        | IDENTIFIER(s) -> (tail ctx, s)
        | other -> failwithf "Expected identifier but found %A " other
        
        
    and parseLocation (ctx: ParsingContext) =
        let (ctx, ident) = parseIdent ctx
        match kindOfHead ctx with
        | LEFT_SQUARE ->
            let ctx = expect LEFT_SQUARE ctx
            let (ctx, index) = parseArithmeticExpr ctx
            let ctx = expect RIGHT_SQUARE ctx
            (ctx, Location.Array (ident, index))
        | DOT ->
            let ctx = expect DOT ctx
            let (ctx, field) = parseIdent ctx
            (ctx, Location.Field (ident, field))
        | _ ->
            (ctx, Location.Identifier ident)
        
    and parseArithmeticExpr (ctx: ParsingContext) =
        let rec parseArithmeticExpr' (ctx: ParsingContext) (precedence: int) =
            let (ctx, left) =
                match kindOfHead ctx with
                | IDENTIFIER(_) -> let (ctx, location) = parseLocation ctx
                                   (ctx, ArithmeticExpr.Loc location)
                | MINUS -> let (ctx, inner) = parseArithmeticExpr' (tail ctx) prefixPrecedence
                           (ctx, ArithmeticUnary (ArithmeticUnaryOperator.Negation, inner))
                | LEFT_PAREN -> let (ctx, inner) = parseArithmeticExpr (tail ctx)
                                (expect RIGHT_PAREN ctx, inner)
                | INTEGER(i) -> (tail ctx, Literal i)
                | other -> failwithf "Unexpected token %A in arithmetic expression" other
        
            let rec precedenceHelper (ctx: ParsingContext) (left: ArithmeticExpr) =
                let currentPrecedence = precedenceOf (kindOfHead ctx)
                if precedence >= currentPrecedence then
                    (ctx, left)
                else
                    let operator = match kindOfHead ctx with
                                   | TokenKind.PLUS -> Add
                                   | TokenKind.MINUS -> Subtract
                                   | TokenKind.MULTIPLICATION -> Multiply
                                   | TokenKind.DIVISION -> Divide
                                   | other -> failwithf "Unexpected infix arithmetic operator %A" other
                    let (ctx, right) = parseArithmeticExpr' (tail ctx) currentPrecedence
                    precedenceHelper ctx (ArithmeticExpr.ArithmeticBinary (left, operator, right))
            
            precedenceHelper ctx left
        parseArithmeticExpr' ctx 0
    and parseBooleanExpr (ctx: ParsingContext) =
        failwith "Not yet implemented"
    
    let parse (source: SourceFile) (tokens: Token List) =
        let ctx = (source, tokens)
        let (_, ast) = parseItems (ctx, ([],[]))
        ast