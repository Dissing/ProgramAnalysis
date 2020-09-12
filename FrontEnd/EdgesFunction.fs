namespace FrontEnd

open FrontEnd.AST;
open FrontEnd.ProgramGraph;


module EdgesFunction = 

    let rec edges (stmts : Statement List) (startNode : int) (nodeIndex : int) (nList : Node List) (eList : Edge List) =
        if stmts.IsEmpty then
            (stmts, startNode, nodeIndex, nList, eList)
        else 
            match stmts with
                | Statement.Assign(loc, aExpr) :: tail ->
                    edges tail nodeIndex (nodeIndex+1) (nodeIndex :: nList) ((startNode, Assign (AssignExpr (loc, aExpr)), nodeIndex) :: eList)
                    
                | StructAssign(id, faExprL) :: tail ->
                    edges tail nodeIndex (nodeIndex+1) (nodeIndex :: nList) ((startNode, AssignLiteral (AssignLiteralExpr (id, faExprL)), nodeIndex) :: eList)
                    
                | If(bExpr, block, None) :: tail ->
                    let (_, bStmts) = block
                    let (_, nodeIndex, _, nList, eList) = edges bStmts nodeIndex (nodeIndex+1) (nodeIndex :: nList) ((startNode, Condition bExpr, nodeIndex) :: eList)

                    let (fromNode, act, _) = eList.Head //Fix last edge of the loop
                    edges tail nodeIndex (nodeIndex+1) (nodeIndex :: nList) ((startNode, Condition (BooleanUnary (Not, bExpr)), nodeIndex) :: (fromNode, act, nodeIndex) :: eList)
                    
                | If(bExpr, block, Some blockOp) :: tail ->
                    let (_, bStmts) = block
                    let (_, nodeIndex, _, nList, eList) = edges bStmts nodeIndex (nodeIndex+1) (nodeIndex :: nList) ((startNode, Condition bExpr, nodeIndex) :: eList)
                    
                    let (_, bStmts) = blockOp
                    let (_, NextNode, _, nList, eList) = edges bStmts (nodeIndex+1) (nodeIndex+2) (nodeIndex+1 :: nList) ((startNode, Condition (BooleanUnary (Not, bExpr)), nodeIndex+1) :: eList)
                    
                    let (fromNode, act, _) = eList.Head //Fix last edge of the loop
                    edges tail nodeIndex NextNode nList.Tail ((fromNode, act, nodeIndex) :: eList.Tail)
                    
                | While(bExpr, block) :: tail ->
                    let (_, bStmts) = block
                    let (_, nodeIndex, _, nList, eList) = edges bStmts nodeIndex (nodeIndex+1) (nodeIndex :: nList) ((startNode, Condition bExpr, nodeIndex) :: eList)
                    
                    let (fromNode, act, _) = eList.Head //Fix last edge of the loop                            
                    edges tail nodeIndex (nodeIndex+1) (nodeIndex :: nList.Tail) ((startNode, Condition (BooleanUnary (Not, bExpr)), nodeIndex) :: (fromNode, act, startNode) :: eList.Tail)
                
                | Statement.Read(loc) :: tail ->
                    edges tail nodeIndex (nodeIndex+1) (nodeIndex :: nList) ((startNode, Read loc, nodeIndex) :: eList)
                    
                | Statement.Write(aExpr) :: tail ->
                    edges tail nodeIndex (nodeIndex+1) (nodeIndex :: nList) ((startNode, Write aExpr, nodeIndex) :: eList)
                    
    let runEdges (ast : AST) =
        let (_, stmts) = ast
        let (_, _, _, nList, eList) = edges stmts 0 1 [0] []
        //printfn "%A" (List.rev nList)
        //printfn "%A" (List.rev eList)
        (List.rev nList, List.rev eList)
     