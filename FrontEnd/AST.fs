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
         | Equal
         | NotEqual
         | Greater
         | GreaterEqual
         | Lesser
         | LesserEqual
         | And
         | Or
     type BooleanBinaryOperator =
         | Equal
         | NotEqual
         | Greater
         | GreaterEqual
         | Lesser
         | LesserEqual
         | And
         | Or  

     
     type Destination =
         | Identifier of Ident
         | Array of Ident * ArithmeticExpr
         | Field of Ident * Ident
     and ArithmeticExpr =
         | Variable of Ident
         | ArrayAccess of Ident * ArithmeticExpr
         | FieldAccess of Ident * Ident
         | Unary of ArithmeticUnaryOperator * ArithmeticExpr
         | Binary of ArithmeticExpr * ArithmeticBinaryOperator * ArithmeticExpr
     and BooleanExpr =
         | Unary of BooleanUnaryOperator * BooleanExpr
         | ArithmeticBinary of ArithmeticExpr * ArithmeticBinaryOperator * ArithmeticExpr
         | BooleanBinary of BooleanExpr * BooleanBinaryOperator * BooleanExpr
    
    type AST = Declaration List * Statement List
    and Field = string * string
    and Declaration =
        | Integer of Ident
        | Array of Ident * int
        | Struct of Field List
    and Statement =
        | Assign of Ident * ArithmeticExpr
        | StructAssign of Ident * ArithmeticExpr list
        | If of BooleanExpr * Block * Block option
        | While of BooleanExpr * Block
        | Read of Ident
        | Write of Ident
    and Block = Statement list
 