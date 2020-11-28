namespace FrontEnd

open FrontEnd.AST

module ProgramGraph =
    
    type AssignExpr = Location * ArithmeticExpr
    type AssignStructExpr = Ident * (Ident * ArithmeticExpr) List
    
    type Action =
        | Allocate of Declaration
        | Free of Declaration
        | Assign of AssignExpr
        | AssignLiteral of AssignStructExpr
        | Condition of BooleanExpr
        | Read of Location
        | Write of ArithmeticExpr
       
    type Node = int
    type Edge = Node * Action * Node

    type Graph = Node List * Edge List
       
    type AnnotatedGraph = DeclarationInfo * Graph
    
    let reverse ((nodes, edges): Graph) =
        let revNodes = List.rev nodes
        let revEdges = List.map (fun (src,action,dst) -> (dst,action,src)) edges |> List.rev
        (revNodes, revEdges)