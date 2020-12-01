namespace FrontEnd
open System.Text.RegularExpressions

module Lexer =

    type LexingState =
        { Content: string
          CurrentPosition: int }

    let head (s: LexingState) =
        if s.CurrentPosition < s.Content.Length
        then Ok(s.Content.[s.CurrentPosition])
        else Error("ICE: Lexer out of bounds!", {From = s.CurrentPosition; To = s.CurrentPosition})

    let consume (s: LexingState) =
        Ok
            ({ Content = s.Content
               CurrentPosition = s.CurrentPosition + 1 })

    let currentPos (s: LexingState) = s.CurrentPosition

    let isDone (s: LexingState) = s.Content.Length = s.CurrentPosition

    let lexSingle (token: TokenKind) (tokens: Token List) (s: LexingState) =
        let pos = currentPos s
        let span = { From = pos; To = pos }
        context.map (consume s, (fun s' -> ((token, span) :: tokens, s')))

    let lexDouble (expected: char) (token: TokenKind) (tokens: Token List) (s: LexingState) =
        context {
            let pos = currentPos s
            let span = { From = pos; To = pos + 1 }
            let! consumedFirst = consume s
            let! nextChar = head consumedFirst

            if nextChar = expected then
                let! consumedSecond = consume consumedFirst
                return ((token, span) :: tokens, consumedSecond)
            else
                let! firstChar = head s

                let err_msg =
                    sprintf "'%c' followed by unexpected '%c' instead of expected '%c'" firstChar nextChar expected

                return! Error(err_msg, span)
        }

    let lexConditionalDouble (condition: char)
                             (ifToken: TokenKind)
                             (elseToken: TokenKind)
                             (tokens: Token List)
                             (s: LexingState)
                             =
        if consume s >>= head = Ok(condition) then
            consume s >>= lexSingle ifToken tokens
        else
            lexSingle elseToken tokens s
            
    let rec extractLexeme (lexeme: string) (requirement: (char -> bool)) (s: LexingState) =
        match head s with
        | Ok (c) when requirement(c) -> (consume s) >>= extractLexeme (lexeme + string c) requirement
        | _ -> Ok((lexeme, s))

    let isDigit (c: char) =
        Regex.IsMatch(string c, "[0-9]")

    let lexInteger (tokens: Token List) (s: LexingState) =
        context {
            let fromPos = currentPos s
            let! (lexeme, consumedLexeme) = extractLexeme "" isDigit s
            let number = int lexeme
            let toPos = (currentPos consumedLexeme) - 1
            let span = { From = fromPos; To = toPos }
            return ((INTEGER(number), span) :: tokens, consumedLexeme)
        }
        
    let isLetter (c: char) =
            Regex.IsMatch(string c, "([A-Z]|[a-z])")

    let lexIdentifier (tokens: Token List) (s: LexingState) =
        context {
            let fromPos = currentPos s
            let! (lexeme, consumedLexeme) = extractLexeme "" isLetter s
            let toPos = (currentPos consumedLexeme) - 1
            let span = { From = fromPos; To = toPos }

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
                | "not" -> NOT
                | other -> IDENTIFIER(other)

            return ((kind, span) :: tokens, consumedLexeme)
        }

    let rec Scan ((tokens: Token List), (s: LexingState)) =
        if isDone s then
            let eof_span =
                { From = s.CurrentPosition
                  To = s.CurrentPosition }

            let eof_token = (EOF, eof_span)
            Ok(List.rev (eof_token :: tokens))
        else
            context {
                let! nextChar = head s

                let! (updatedTokens, consumedState) =
                    match nextChar with
                    | ' '
                    | '\t'
                    | '\n'
                    | '\r' -> context.map(consume s, fun s' -> (tokens, s'))
                    | '+' -> lexSingle PLUS tokens s
                    | '-' -> lexSingle MINUS tokens s
                    | '*' -> lexSingle MULTIPLICATION tokens s
                    | '/' -> lexSingle DIVISION tokens s
                    | '%' -> lexSingle MODULO tokens s
                    | '{' -> lexSingle LEFT_CURLY tokens s
                    | '[' -> lexSingle LEFT_SQUARE tokens s
                    | '(' -> lexSingle LEFT_PAREN tokens s
                    | '}' -> lexSingle RIGHT_CURLY tokens s
                    | ']' -> lexSingle RIGHT_SQUARE tokens s
                    | ')' -> lexSingle RIGHT_PAREN tokens s
                    | ';' -> lexSingle SEMI_COLON tokens s
                    | '.' -> lexSingle DOT tokens s
                    | ',' -> lexSingle COMMA tokens s
                    | '<' -> lexConditionalDouble '=' LESSER_EQUAL LESSER tokens s
                    | '>' -> lexConditionalDouble '=' GREATER_EQUAL GREATER tokens s
                    | '!' -> lexDouble '=' NOT_EQUAL tokens s
                    | ':' -> lexDouble '=' ASSIGN tokens s
                    | '|' -> lexDouble '|' OR tokens s
                    | '&' -> lexDouble '&' AND tokens s
                    | '=' -> lexDouble '=' EQUAL tokens s
                    | c ->
                        if isLetter(c) then lexIdentifier tokens s
                        else if isDigit(c) then lexInteger tokens s
                        else Error(sprintf "Unknown character '%c'" c, {From = s.CurrentPosition; To = s.CurrentPosition})

                return! Scan(updatedTokens, consumedState)
            }

    let lex (source: string) =
        let s =
            { Content = source
              CurrentPosition = 0 }

        let tokens = Scan([], s)
        tokens
