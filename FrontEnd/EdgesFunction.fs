namespace FrontEnd

open FrontEnd.AST;
open FrontEnd.ProgramGraph;


module EdgesFunction = 

    let rec edges (stmts : Statement List) (startIndex : int) (endIndex : int) (nList : Node List) (eList : Edge List) =
        if stmts.IsEmpty then
            (stmts, startIndex, startIndex, nList, eList)
        else 
            match stmts with
                | Statement.Assign(loc, aExpr) :: tail ->
                    if tail.IsEmpty then
                        edges tail (startIndex+1) endIndex nList ((startIndex, Assign (AssignExpr (loc, aExpr)), endIndex) :: eList)
                    else
                        edges tail (startIndex+1) endIndex (startIndex+1 :: nList) ((startIndex, Assign (AssignExpr (loc, aExpr)), startIndex+1) :: eList)
                    
                | StructAssign(id, faExprL) :: tail ->
                    if tail.IsEmpty then
                        edges tail (startIndex+1) endIndex nList ((startIndex, AssignLiteral (AssignLiteralExpr (id, faExprL)), endIndex) :: eList)
                    else
                        edges tail (startIndex+1) endIndex (startIndex+1 :: nList) ((startIndex, AssignLiteral (AssignLiteralExpr (id, faExprL)), startIndex+1) :: eList)
                    
                | If(bExpr, block, None) :: tail ->
                    let (_, bStmts) = block
                    let (_, blockEnd, _, nList, eList) = edges bStmts (startIndex+1) endIndex (startIndex+1 :: nList) ((startIndex, Condition bExpr, startIndex+1) :: eList)
                
                    //Fix branch endings
                    let blockEnd = blockEnd+1
                    let nList = blockEnd :: nList
                    let (n1, act, _) = eList.Head
                    let eList = (n1, act, blockEnd) :: eList.Tail
                    
                    let eList = (startIndex, Condition (BooleanUnary (Not, bExpr)), blockEnd) :: eList
                    edges tail blockEnd endIndex nList eList
                    
                //| If(bExpr, block, Some blockOp) :: tail ->
                //    let eList = (startIndex, Condition bExpr, startIndex+1) :: eList //Create edge into branch 1
                //    let (_, bStmts) = block
                //    let startIndex = startIndex+1
                //    let (_, block1OutIndex, endIndex, nList, eList) = edges bStmts startIndex endIndex nList eList
                //    
                //    let eList =  (currentIndex, BooleanUnary (Not, bExpr), block1OutIndex) :: eList //Create edge into branch 2
                //    let (_, bStmts) = blockOp
                //    let (_, block2OutIndex, nList, eList) = edges bStmts block1OutIndex nList eList
                //    
                //    let nList = block2OutIndex::nList
                //    
                //     //Find last edges of both blocks, redirect to toIndex
                //    let eList = fixBranchLastEdges eList [] Set.ofList [block1OutIndex-1, block2OutIndex-1] block2OutIndex
                //
                //    edges tail block2From+1 nList eList
                
                | While(bExpr, block) :: tail ->
                    let (_, bStmts) = block
                    let (_, _, toIndex, nList, eList) = edges bStmts (startIndex+1) startIndex (startIndex+1 :: nList) ((startIndex, Condition bExpr, startIndex+1) :: eList)
                    
                    edges tail toIndex (toIndex+1) (toIndex::nList) ((startIndex, Condition (BooleanUnary (Not, bExpr)), toIndex) :: eList)
                
                | Statement.Read(loc) :: tail ->
                    if tail.IsEmpty then
                        edges tail (startIndex+1) endIndex nList ((startIndex, Read loc, endIndex) :: eList)
                    else
                        edges tail (startIndex+1) endIndex (startIndex+1 :: nList) ((startIndex, Read loc, startIndex+1) :: eList)
                    
                | Statement.Write(aExpr) :: tail ->
                    if tail.IsEmpty then
                        edges tail (startIndex+1) endIndex nList ((startIndex, Write aExpr, endIndex) :: eList)
                    else
                        edges tail (startIndex+1) endIndex (startIndex+1 :: nList) ((startIndex, Write aExpr, startIndex+1) :: eList)
                    
    let runEdges (ast : AST) =
        let (_, stmts) = ast
        let (_, _, _, nList, eList) = edges stmts 0 -1 [0] []
        printfn "%A" (List.rev nList)
        printfn "%A" (List.rev eList)
        
        (List.rev nList, List.rev eList)
     