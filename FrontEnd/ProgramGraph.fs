namespace FrontEnd

open FrontEnd.AST

module ProgramGraph =
    
    type AssignExpr = Location * ArithmeticExpr
    type AssignStructExpr = Ident * (Ident * ArithmeticExpr) List
    
    type Action =
        | Allocate of Declaration
        | Free of Ident
        | Assign of AssignExpr
        | AssignLiteral of AssignStructExpr
        | Condition of BooleanExpr
        | Read of Location
        | Write of ArithmeticExpr
        | Skip
       
    type Node = int
    type Edge = Node * Action * Node

    type Graph = Node List * Edge List
       
    type AnnotatedGraph = Map<Ident, DeclarationInfo> * Graph