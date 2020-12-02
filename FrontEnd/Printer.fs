namespace FrontEnd

open FrontEnd.AST;

module Printer =
    
    let BinaryPrint binary =
        match binary with
        | Add -> "+"
        | Subtract -> "-"
        | Multiply -> "*"
        | Divide -> "/"
        | Modulo -> "%"
    
    let ComparatorPrint comparator =
        match comparator with
        | Equal -> "=="
        | NotEqual -> "!="
        | Greater -> ">"
        | GreaterEqual -> ">="
        | Lesser -> "<"
        | LesserEqual -> "=<"
        
    let BooleanBinaryPrint boolBinary =
        match boolBinary with
        | And -> "&&"
        | Or -> "||"
    
    
    let rec ArithmeticPrinter (output : string) (expr : ArithmeticExpr) =
        match expr with
        | Loc loc -> match loc with
                        | Location.Variable ident ->
                            let output = output + ident
                            (output, false)
                        | Location.Array (ident, expr) ->
                            let (inner, _) = ArithmeticPrinter "" expr
                            let output = output + ident + "[" + inner + "]"
                            (output, false)
                        | Location.Field (leftIdent, rightIdent) ->
                            let output = output + leftIdent + "." + rightIdent
                            (output, false)
        | IntLiteral int ->
            let output = output + int.ToString()
            (output, false)
        | ArithmeticUnary (_, expr) ->
            let (inner, paren) = ArithmeticPrinter "" expr
            if paren then
                let output = output + "-(" + inner + ")"
                (output, true)
            else
                let output = output + "-" + inner
                (output, true)
        | ArithmeticBinary (leftExpr, binary, rightExpr) ->
            let (leftInner, leftParen) = ArithmeticPrinter "" leftExpr
            let (rightInner, rightParen) = ArithmeticPrinter "" rightExpr
            if leftParen && rightParen then
                let output = output + "(" + leftInner + ") " + BinaryPrint binary + " (" + rightInner + ")"
                (output, true)
            else if leftParen then
                let output = output + "(" + leftInner + ") " + BinaryPrint binary + " " + rightInner
                (output, true)
            else if rightParen then
                let output = output + leftInner + " " + BinaryPrint binary + " (" + rightInner + ")"
                (output, true)
            else 
                let output = output + leftInner + " " + BinaryPrint binary + " " + rightInner
                (output, true)
                
    let rec BooleanPrinter (output : string) (expr : BooleanExpr) =
        match expr with
        | BooleanLiteral bool ->
            let output = output + bool.ToString()
            (output, false)
        | BooleanUnary (_, expr) ->
            let (inner, paren) = BooleanPrinter "" expr
            if paren then
                let output = output + "!(" + inner + ")"
                (output, false)
            else
                let output = output + "!" + inner
                (output, false)
            
        | Comparison (leftAExpr, comparator, rightAExpr) ->
            let (leftInner, leftParen) = ArithmeticPrinter "" leftAExpr
            let (rightInner, rightParen) = ArithmeticPrinter "" rightAExpr
            if leftParen && rightParen then
                let output = output + "(" + leftInner + ") " + ComparatorPrint comparator + " (" + rightInner + ")"
                (output, true)
            else if leftParen then
                let output = output + "(" + leftInner + ") " + ComparatorPrint comparator + " " + rightInner
                (output, true)
            else if rightParen then
                let output = output + leftInner + " " + ComparatorPrint comparator + " (" + rightInner + ")"
                (output, true)
            else 
                let output = output + leftInner + " " + ComparatorPrint comparator + " " + rightInner
                (output, true)
            
        | BooleanBinary (leftBExpr, binary, rightBExpr) ->
            let (leftInner, leftParen) = BooleanPrinter "" leftBExpr
            let (rightInner, rightParen) = BooleanPrinter "" rightBExpr
            if leftParen && rightParen then
                let output = output + "(" + leftInner + ") " + BooleanBinaryPrint binary + " (" + rightInner + ")"
                (output, true)
            else if leftParen then
                let output = output + "(" + leftInner + ") " + BooleanBinaryPrint binary + " " + rightInner
                (output, true)
            else if rightParen then
                let output = output + leftInner + " " + BooleanBinaryPrint binary + " (" + rightInner + ")"
                (output, true)
            else 
                let output = output + leftInner + " " + BooleanBinaryPrint binary + " " + rightInner
                (output, true)
                