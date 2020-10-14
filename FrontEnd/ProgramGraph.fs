namespace FrontEnd

open FrontEnd.AST

module ProgramGraph =
    
    type AssignExpr = Location * ArithmeticExpr
    type AssignLiteralExpr = Ident * (Ident * ArithmeticExpr) List
    
    type Action =
        | Allocate of Ident
        | Free of Ident
        | Assign of AssignExpr
        | AssignLiteral of AssignLiteralExpr
        | Condition of BooleanExpr
        | Read of Location
        | Write of ArithmeticExpr
       
    type Node = int
    type Edge = Node * Action * Node

    type Graph = Node List * Edge List
       
    type AnnotatedGraph = Map<Ident, DeclarationInfo> * Graph