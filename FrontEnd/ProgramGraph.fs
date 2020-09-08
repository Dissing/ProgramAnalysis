namespace FrontEnd

module ProgramGraph =
    type Action =
        | Assign of AST.Destination * AST.ArithmeticExpr
        | AssignLiteral of AST.Ident * (Field * AST.ArithmeticExpr) List
        | Condition of AST.BooleanExpr
        | Read of AST.Destination
        | Write of AST.ArithmeticExpr
       
    type Node = int
    type Edge = Node * Action * Node

    type Graph = Node List * Edge List