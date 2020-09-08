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
        | Integer of AST.Ident
        | Array of AST.Ident * int
        | Struct of Field List
    and Statement =
        | Assign of AST.Ident * AST.ArithmeticExpr
        | StructAssign of AST.Ident * AST.ArithmeticExpr list
        | If of AST.BooleanExpr * Block * Block option
        | While of AST.BooleanExpr * Block
        | Read of AST.Ident
        | Write of AST.Ident
    and Block = Statement list
 