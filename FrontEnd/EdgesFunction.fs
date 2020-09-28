namespace FrontEnd

open FrontEnd.AST;
open FrontEnd.ProgramGraph;


module EdgesFunction = 

    let rec edges (stmts : Statement List) (startNode : int) (nodeIndex : int) (endNode : int) (nList : Node List) (eList : Edge List) =
        match stmts with
            | [] -> (nodeIndex, nList, eList)
            | Statement.Assign(loc, aExpr) :: tail ->
                if tail.IsEmpty && endNode <> -1 then
                    let eList = (startNode, Assign (AssignExpr (loc, aExpr)), endNode) :: eList
                    (nodeIndex, nList, eList)
                else
                    let nList = nodeIndex :: nList
                    let eList = (startNode, Assign (AssignExpr (loc, aExpr)), nodeIndex) :: eList
                    edges tail nodeIndex (nodeIndex+1) endNode nList eList
                    
            | StructAssign(id, faExprL) :: tail ->
                if tail.IsEmpty && endNode <> -1 then
                    let eList = (startNode, AssignLiteral (AssignLiteralExpr (id, faExprL)), endNode) :: eList
                    (nodeIndex, nList, eList)
                else
                    let nList = nodeIndex :: nList
                    let eList = (startNode, AssignLiteral (AssignLiteralExpr (id, faExprL)), nodeIndex) :: eList
                    edges tail nodeIndex (nodeIndex+1) endNode nList eList
                    
            | If(bExpr, block, None) :: tail ->
                //Handle the branch
                let (_, bStmts) = block
                let nList = nodeIndex :: nList
                let eList = (startNode, Condition bExpr, nodeIndex) :: eList
                let (nodeIndex, nList, eList) = edges bStmts nodeIndex (nodeIndex+1) endNode nList eList
                    
                //Fix if last part of inner operation
                if tail.IsEmpty && endNode <> -1 then
                    let leavingEdge = (startNode, Condition (BooleanUnary (Not, bExpr)), endNode)
                    let eList = leavingEdge :: eList
                    (nodeIndex, nList, eList)
                else
                    let leavingEdge = (startNode, Condition (BooleanUnary (Not, bExpr)), nodeIndex-1)
                    let eList = leavingEdge :: eList
                    edges tail (nodeIndex-1) nodeIndex endNode nList eList
                    
            | If(bExpr, block, Some blockOp) :: tail ->
                //Fix if last part of inner operation
                if tail.IsEmpty && endNode <> -1 then
                    //Handle the first branch
                    let (_, bStmts) = block
                    let nList = nodeIndex :: nList
                    let eList = (startNode, Condition bExpr, nodeIndex) :: eList
                    let (nodeIndex, nList, eList) = edges bStmts nodeIndex (nodeIndex+1) endNode nList eList
                        
                    //Handle the second branch
                    let (_, bStmts) = blockOp
                    let nList = nodeIndex :: nList
                    let eList = (startNode, Condition (BooleanUnary (Not, bExpr)), nodeIndex) :: eList
                    let (NextNode, nList, eList) = edges bStmts nodeIndex (nodeIndex+1) endNode nList eList
                        
                    (NextNode, nList, eList)
                else 
                    //Handle the first branch
                    let (_, bStmts) = block
                    let nList = nodeIndex :: nList
                    let eList = (startNode, Condition bExpr, nodeIndex) :: eList
                    let (nodeIndex, nList, eList) = edges bStmts nodeIndex (nodeIndex+1) -1 nList eList
                        
                    //Handle the second branch
                    let (_, bStmts) = blockOp
                    let nList = nodeIndex :: nList
                    let eList = (startNode, Condition (BooleanUnary (Not, bExpr)), nodeIndex) :: eList
                    let (NextNode, nList, eList) = edges bStmts nodeIndex (nodeIndex+1) (nodeIndex-1) nList eList
                        
                    edges tail (nodeIndex-1) NextNode endNode nList eList
                    
            | While(bExpr, block) :: tail ->
                //Handle the loop
                let (_, bStmts) = block
                let nList = nodeIndex :: nList
                let eList = (startNode, Condition bExpr, nodeIndex) :: eList
                let (nodeIndex, nList, eList) = edges bStmts nodeIndex (nodeIndex+1) startNode nList eList
                    
                //Fix if last part of inner operation
                if tail.IsEmpty && endNode <> -1 then
                    let leavingEdge = (startNode, Condition (BooleanUnary (Not, bExpr)), endNode)
                    let eList = leavingEdge :: eList
                        
                    (nodeIndex, nList, eList)
                else
                    let leavingEdge = (startNode, Condition (BooleanUnary (Not, bExpr)), nodeIndex)
                    let nList = nodeIndex :: nList
                    let eList = leavingEdge :: eList
                        
                    edges tail nodeIndex (nodeIndex+1) endNode nList eList
                
            | Statement.Read(loc) :: tail ->
                if tail.IsEmpty && endNode <> -1 then
                    let eList = (startNode, Read loc, endNode) :: eList
                    (nodeIndex, nList, eList)
                else
                    let nList = nodeIndex :: nList
                    let eList = (startNode, Read loc, nodeIndex) :: eList
                    edges tail nodeIndex (nodeIndex+1) endNode nList eList
                    
            | Statement.Write(aExpr) :: tail ->
                if tail.IsEmpty && endNode <> -1 then
                    let eList = (startNode, Write aExpr, endNode) :: eList
                    (nodeIndex, nList, eList)
                else
                    let nList = nodeIndex :: nList
                    let eList = (startNode, Write aExpr, nodeIndex) :: eList
                    edges tail nodeIndex (nodeIndex+1) endNode nList eList
                    
                    
    let runEdges (ast : AST) =
        let (_, stmts) = ast
        let (_, nList, eList) = edges stmts 0 1 -1 [0] []
        printfn "%A" (List.rev nList)
        printfn "%A" (List.rev eList)
        (List.rev nList, List.rev eList)
     