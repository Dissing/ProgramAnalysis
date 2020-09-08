namespace FrontEnd

module AST =
    type Ident = string
     
    type ArithmeticUnaryOperator =
        | Not
         
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
        | Location of Location
        | Unary of ArithmeticUnaryOperator * ArithmeticExpr
        | Binary of ArithmeticExpr * ArithmeticBinaryOperator * ArithmeticExpr
    and BooleanExpr =
        | Unary of BooleanUnaryOperator * BooleanExpr
        | Comparison of ArithmeticExpr * ComparisonOperator * ArithmeticExpr
        | Binary of BooleanExpr * BooleanBinaryOperator * BooleanExpr
         
    type Field = string * string
         
    type Declaration =
           | Integer of Ident
           | Array of Ident * int
           | Struct of Field List
    and Statement =
           | Assign of Ident * ArithmeticExpr
           | StructAssign of Ident * ArithmeticExpr list
           | If of BooleanExpr * Block * Block option
           | While of BooleanExpr * Block
           | Read of Location
           | Write of ArithmeticExpr
    and Block = Declaration List * Statement list
    
    //TODO Should probably rename to avoid name clash with module
    //TODO Note how Block and AST have essentially become the same
    type AST = Declaration List * Statement List
 