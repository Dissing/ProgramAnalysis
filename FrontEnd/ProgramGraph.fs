namespace FrontEnd

open FrontEnd.AST

module ProgramGraph =
    type Action =
        | Assign of Location * ArithmeticExpr
        | AssignLiteral of Ident * (Field * ArithmeticExpr) List
        | Condition of BooleanExpr
        | Read of Location
        | Write of ArithmeticExpr
       
    type Node = int
    type Edge = Node * Action * Node

    type Graph = Node List * Edge List