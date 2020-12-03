namespace FrontEnd

open FrontEnd.AST;
open FrontEnd.ProgramGraph;


module EdgesFunction = 

    let rec fixEdges (nodeNumber : int) (highestNumber: int) (eList : Edge List) (newList : Edge List) =
        match eList with
        | [] -> List.rev newList
        | (n1, expr, n2) :: tail -> if n1 > nodeNumber then
                                        if n2 = nodeNumber then fixEdges nodeNumber highestNumber tail (((n1-1), expr, highestNumber) :: newList)
                                        else if n2 > nodeNumber then fixEdges nodeNumber highestNumber tail (((n1-1), expr, (n2-1)) :: newList)
                                        else fixEdges nodeNumber highestNumber tail (((n1-1), expr, n2) :: newList)
                                    else
                                        if n2 = nodeNumber then fixEdges nodeNumber highestNumber tail ((n1, expr, highestNumber) :: newList)
                                        else if n2 > nodeNumber then fixEdges nodeNumber highestNumber tail ((n1, expr, (n2-1)) :: newList)
                                        else fixEdges nodeNumber highestNumber tail ((n1, expr, n2) :: newList)
                                              
    let rec edges (stmts : Statement List) (startNode : int) (nodeIndex : int) (endNode : int) (nList : Node List) (eList : Edge List) =
        match stmts with
            | [] -> (nodeIndex, nList, eList)
            | Statement.Allocate decl :: tail ->
                if tail.IsEmpty && endNode <> -1 then
                    let eList = (startNode, Allocate(decl), endNode) :: eList
                    (nodeIndex, nList, eList)
                else
                    let nList = nodeIndex :: nList
                    let eList = (startNode, Allocate(decl), nodeIndex) :: eList
                    edges tail nodeIndex (nodeIndex+1) endNode nList eList
                    
            | Statement.Free(id) :: tail ->
                if tail.IsEmpty && endNode <> -1 then
                    let eList = (startNode, Free(id), endNode) :: eList
                    (nodeIndex, nList, eList)
                else
                    let nList = nodeIndex :: nList
                    let eList = (startNode, Free(id), nodeIndex) :: eList
                    edges tail nodeIndex (nodeIndex+1) endNode nList eList
                    
            | Statement.Assign(loc, aExpr) :: tail ->
                if tail.IsEmpty && endNode <> -1 then
                    let eList = (startNode, Assign (loc, aExpr), endNode) :: eList
                    (nodeIndex, nList, eList)
                else
                    let nList = nodeIndex :: nList
                    let eList = (startNode, Assign (loc, aExpr), nodeIndex) :: eList
                    edges tail nodeIndex (nodeIndex+1) endNode nList eList

            | AST.RecordAssign(id, faExprL) :: tail ->
                if tail.IsEmpty && endNode <> -1 then
                    let eList = (startNode, RecordAssign (id, faExprL), endNode) :: eList
                    (nodeIndex, nList, eList)
                else
                    let nList = nodeIndex :: nList
                    let eList = (startNode, RecordAssign (id, faExprL), nodeIndex) :: eList
                    edges tail nodeIndex (nodeIndex+1) endNode nList eList

            | If(bExpr, block, None) :: tail ->
                //Handle the branch
                let nList = nodeIndex :: nList
                let eList = (startNode, Condition bExpr, nodeIndex) :: eList
                let (nodeIndex, nList, eList) = edges block nodeIndex (nodeIndex+1) endNode nList eList
                
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
                    let nList = nodeIndex :: nList
                    let eList = (startNode, Condition bExpr, nodeIndex) :: eList
                    let (nodeIndex, nList, eList) = edges block nodeIndex (nodeIndex+1) endNode nList eList
                    
                    //Handle the second branch
                    let nList = nodeIndex :: nList
                    let eList = (startNode, Condition (BooleanUnary (Not, bExpr)), nodeIndex) :: eList

                    let (NextNode, nList, eList) = edges blockOp nodeIndex (nodeIndex+1) endNode nList eList
                    
                    (NextNode, nList, eList)
                else 
                    //Handle the first branch
                    let nList = nodeIndex :: nList
                    let eList = (startNode, Condition bExpr, nodeIndex) :: eList
                    let (nodeIndex, nList, eList) = edges block nodeIndex (nodeIndex+1) -1 nList eList
                    
                    //Handle the second branch
                    let nList = nodeIndex :: nList
                    let eList = (startNode, Condition (BooleanUnary (Not, bExpr)), nodeIndex) :: eList
                    let (NextNode, nList, eList) = edges blockOp nodeIndex (nodeIndex+1) (nodeIndex-1) nList eList
                    
                    if tail.IsEmpty then
                        let eList = fixEdges (nodeIndex-1) (NextNode-1) eList []
                        edges tail (nodeIndex-1) NextNode endNode nList eList
                    else
                        edges tail (nodeIndex-1) NextNode endNode nList eList
                                       
            | While(bExpr, block) :: tail ->
                //Handle the loop
                let nList = nodeIndex :: nList
                let eList = (startNode, Condition bExpr, nodeIndex) :: eList
                let (nodeIndex, nList, eList) = edges block nodeIndex (nodeIndex+1) startNode nList eList
                
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
        let (declMap, stmts) = ast
        let (_, nList, eList) = edges stmts 0 1 -1 [0] []
             
        (declMap, (List.rev nList, List.rev eList))
        