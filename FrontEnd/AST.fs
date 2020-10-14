namespace FrontEnd

module AST =
    type Ident = string
     
    type ArithmeticUnaryOperator =
        | Negation
         
    type BooleanUnaryOperator =
        | Not
         
    type ArithmeticBinaryOperator =
        | Add
        | Subtract
        | Multiply
        | Divide
        | Modulo
    type ComparisonOperator =
        | Equal
        | NotEqual
        | Greater
        | GreaterEqual
        | Lesser
        | LesserEqual
    type BooleanBinaryOperator =
        | And
        | Or  

     
    type Location =
        | Identifier of Ident
        | Array of Ident * ArithmeticExpr
        | Field of Ident * Ident
    and ArithmeticExpr =
        | Loc of Location
        | IntLiteral of int
        | ArithmeticUnary of ArithmeticUnaryOperator * ArithmeticExpr
        | ArithmeticBinary of ArithmeticExpr * ArithmeticBinaryOperator * ArithmeticExpr
    and BooleanExpr =
        | BooleanLiteral of bool
        | BooleanUnary of BooleanUnaryOperator * BooleanExpr
        | Comparison of ArithmeticExpr * ComparisonOperator * ArithmeticExpr
        | BooleanBinary of BooleanExpr * BooleanBinaryOperator * BooleanExpr
         
    type Declaration =
           | Integer of Ident
           | ArrayDecl of Ident * int
           | Struct of Ident * Ident List
    and Statement =
        | Allocate of Declaration
        | Free of Ident
        | Assign of Location * ArithmeticExpr
        | StructAssign of Ident * (Ident * ArithmeticExpr) list
        | If of BooleanExpr * Block * Block option
        | While of BooleanExpr * Block
        | Read of Location
        | Write of ArithmeticExpr
    and Block = Statement list
    
    type DeclarationInfo = Map<Ident,Declaration>
    type AST = DeclarationInfo * Statement List
 