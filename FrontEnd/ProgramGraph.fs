namespace FrontEnd

open System

module ProgramGraph =
    type Action =
        | Assign of Destination * ArithmeticExpr
        | AssignLiteral of Ident * (Field * ArithmeticExpr) List
        | Condition of BooleanExpr
        | Read of Destination
        | Write of ArithmeticExpr
       
    type Node = int
    type Edge = Node * Action * Node

    type Graph = Node List * Edge List