namespace FrontEnd

module AST =
    type Ident = string
     
    type ArithmeticUnaryOperator =
        | Negative
         
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
        | Free of Declaration
        | Assign of Location * ArithmeticExpr
        | StructAssign of Ident * (Ident * ArithmeticExpr) list
        | If of BooleanExpr * Block * Block option
        | While of BooleanExpr * Block
        | Read of Location
        | Write of ArithmeticExpr
    and Block = Statement list
    

    type DeclarationInfo = Map<Ident,Declaration>
    type AST = DeclarationInfo * Statement List
    
    
    let rec arithmeticFreeVariables (a: ArithmeticExpr) =
        match a with
        | Loc(Array(name, index)) -> Set.union (Set.singleton (Array(name, index))) (arithmeticFreeVariables index)
        | Loc(other) -> Set.singleton other
        | IntLiteral(_) -> Set.empty
        | ArithmeticUnary(_, inner) -> arithmeticFreeVariables inner
        | ArithmeticBinary(left, _, right) -> Set.union (arithmeticFreeVariables left) (arithmeticFreeVariables right)

    let rec booleanFreeVariables (a: BooleanExpr) =
            match a with
            | BooleanLiteral(_) -> Set.empty
            | BooleanUnary(_, inner) -> booleanFreeVariables inner
            | Comparison(left, _, right) -> Set.union (arithmeticFreeVariables left) (arithmeticFreeVariables right)
            | BooleanBinary(left, _, right) -> Set.union (booleanFreeVariables left) (booleanFreeVariables right)