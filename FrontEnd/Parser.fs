namespace FrontEnd

open FrontEnd.AST

module Parser =
    
    type ParsingContext = SourceFile * Token List
    
    let head (ctx: ParsingContext) = (snd ctx).Head
    let tail (ctx: ParsingContext) = (fst ctx, (snd ctx).Tail)
    let kindOfHead (ctx: ParsingContext) = fst (snd ctx).Head
    let isDone (ctx: ParsingContext) = (kindOfHead ctx) = EOF
    
    let accept (kind: TokenKind) (ctx: ParsingContext) =
            if not (isDone ctx) && (kindOfHead ctx) = kind then
                (tail ctx, true)
            else
                (ctx, false)
    
    let expect (kind: TokenKind) (ctx: ParsingContext) =
        let (ctx, correct) = accept kind ctx
        if correct then ctx
        else failwithf "Expected token kind %A but got %A" kind (head ctx)
        
    let prefixPrecedence = 6
    let arithmeticPrecedenceOf = function
        | MULTIPLICATION -> 5
        | DIVISION -> 5
        | MODULO -> 5
        | PLUS -> 4
        | MINUS -> 4
        | _ -> 0
        
    let booleanPrecedenceOf = function
            | LESSER -> 3
            | LESSER_EQUAL -> 3
            | GREATER -> 3
            | GREATER_EQUAL -> 3
            | EQUAL -> 3
            | NOT_EQUAL -> 3
            | AND -> 2
            | OR -> 1
            | _ -> 0
        
    type AorB =
        | A of ArithmeticExpr
        | B of BooleanExpr

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
                | INT ->
                    //Skip over the initial int
                    let ctx = tail ctx
                    
                    let (ctx, is_array) = accept LEFT_SQUARE ctx
                    let (ctx, array_size) =
                        if is_array then
                            match kindOfHead ctx with
                            | INTEGER(n) ->
                                let ctx = expect RIGHT_SQUARE (tail ctx)
                                (ctx, Some(n))
                            | other -> failwithf "Expected array to have integer size but got %A" other
                        else
                            (ctx, None)
                    
                    let (ctx, ident) = parseIdent ctx
                    let ctx = expect SEMI_COLON ctx
                    let decl = if array_size.IsSome then
                                    ArrayDecl(ident, array_size.Value)
                                else
                                    Integer(ident)
                    parseItems (ctx, (decl::decls, stmts))
                        
                | other -> failwithf "Parsing items but got unexpected token %A" other
    and parseBlock (ctx: ParsingContext) =
        let ctx = expect LEFT_CURLY ctx
        let (ctx, ast) = parseItems (ctx, ([], []))
        let ctx = expect RIGHT_CURLY ctx
        (ctx, ast)
    and parseIf (ctx: ParsingContext) =
        let ctx = expect IF ctx
        let ctx = expect LEFT_PAREN ctx
        let (ctx, condition) = parseBooleanExpr ctx
        let ctx = expect RIGHT_PAREN ctx
        let (ctx, then_block) = parseBlock ctx
        let (ctx, has_else) = accept ELSE ctx
        let (ctx, else_block) =
            if has_else then 
                let (ctx, else_block) = parseBlock ctx
                (ctx, Some (else_block))
            else
                (ctx, None)
        (ctx, Statement.If (condition, then_block, else_block))
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
        
    and parseArithmeticExpr' (ctx: ParsingContext) (precedence: int) =
        let (ctx, left) =
            match kindOfHead ctx with
            | IDENTIFIER(_) -> let (ctx, location) = parseLocation ctx
                               (ctx, ArithmeticExpr.Loc location)
            | MINUS -> let (ctx, inner) = parseArithmeticExpr' (tail ctx) prefixPrecedence
                       (ctx, ArithmeticUnary (ArithmeticUnaryOperator.Negation, inner))
            | LEFT_PAREN -> let (ctx, inner) = parseArithmeticExpr (tail ctx)
                            (expect RIGHT_PAREN ctx, inner)
            | INTEGER(i) -> (tail ctx, IntLiteral i)
            | other -> failwithf "Unexpected token %A in arithmetic expression" other
    
        let rec precedenceHelper (ctx: ParsingContext) (left: ArithmeticExpr) =
            let currentPrecedence = arithmeticPrecedenceOf (kindOfHead ctx)
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
    and parseArithmeticExpr (ctx: ParsingContext) =
        parseArithmeticExpr' ctx 0
        
    //Consider unifying the parsing of arithmetic and boolean expressions
    and parseBooleanExpr (ctx: ParsingContext) =
        let rec parseBooleanExpr' (ctx: ParsingContext) (precedence: int) =
            let (ctx, left) =
                match kindOfHead ctx with
                | TRUE -> (tail ctx, B (BooleanLiteral true))
                | FALSE -> (tail ctx, B(BooleanLiteral false))
                | LEFT_PAREN -> let (ctx, inner) = parseBooleanExpr (tail ctx)
                                (expect RIGHT_PAREN ctx, B(inner))
                | NOT -> let (ctx, inner) = parseBooleanExpr (tail ctx)
                         (ctx, B(BooleanUnary (BooleanUnaryOperator.Not, inner)))
                | IDENTIFIER(_) | MINUS | INTEGER(_) ->
                                let (ctx, inner) = parseArithmeticExpr ctx
                                (ctx, A(inner))
                | other -> failwithf "Unexpected boolean expression prefix %A" other
            
            let rec precedenceHelper (ctx: ParsingContext) (left: AorB) =
                let currentPrecedence = booleanPrecedenceOf (kindOfHead ctx)
                if precedence >= currentPrecedence then
                    (ctx, left)
                else
                    match left with
                    | A(l) ->
                        let operator =
                            match kindOfHead ctx with
                            | TokenKind.EQUAL -> Equal
                            | TokenKind.NOT_EQUAL -> NotEqual
                            | TokenKind.GREATER -> Greater
                            | TokenKind.GREATER_EQUAL -> GreaterEqual
                            | TokenKind.LESSER -> Lesser
                            | TokenKind.LESSER_EQUAL -> LesserEqual
                            | other -> failwithf "Unexpected boolean expression infix comparison operator %A" other
                        let (ctx, right) = parseArithmeticExpr' (tail ctx) currentPrecedence
                        precedenceHelper ctx (B(BooleanExpr.Comparison (l, operator, right)))
                    | B(l) ->
                        let operator =
                            match kindOfHead ctx with
                            | TokenKind.AND -> And
                            | TokenKind.OR -> Or
                            | other -> failwithf "Unexpected boolean expression infix operator %A" other
                        let (ctx, right) = parseBooleanExpr' (tail ctx) currentPrecedence
                        match right with
                        | B(r) -> precedenceHelper ctx (B(BooleanExpr.BooleanBinary (l, operator, r)))
                        | A(a) -> failwithf "Cant combine a boolean expression and an arithmetic expression with an %A operator" operator
                        
            precedenceHelper ctx left

        let (ctx, expr) = parseBooleanExpr' ctx 0
        match expr with
          | B(b) -> (ctx, b)
          | A(a) -> failwithf "Expected boolean expression but got pure arithmetic expression %A" a
    
    let parse (source: SourceFile) (tokens: Token List) =
        let ctx = (source, tokens)
        let (_, ast) = parseItems (ctx, ([],[]))
        ast