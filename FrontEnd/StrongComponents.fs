namespace FrontEnd

open FrontEnd.ProgramGraph
open FrontEnd.ReversePostorder

module StrongComponents =
    
    // Compute edges going into nodes 
    let rec EdgesIn (node : Node) (inputEdges : Edge List) (outputEdges : Edge List) =
        match inputEdges with
        | [] -> outputEdges
        | (n1, act, n2)::tail -> if node = n2
                                 then let outputEdges = (n1, act, n2)::outputEdges
                                      EdgesIn node tail outputEdges
                                 else EdgesIn node tail outputEdges
    
    
    // Strong components function
    let StrongComps (pg : Graph) (rpIdxNode : rPOrder) =
        // Assign function and loop recursion
        let rec Assign (node : Node) (scList : (Node Set) List) (sc : Node Set) (v : Node Set) (edges : Edge List) =
            let sc = sc.Add(node)
            let v = v.Add(node)
            let edgesIn = EdgesIn node edges []
            AssignRec node edgesIn scList sc v edges
        and AssignRec (node : Node) (edgesIn : Edge List) (scList : (Node Set) List) (sc : Node Set) (v : Node Set) (totalEdgeList : Edge List) =            
            match edgesIn with
            | [] -> (sc, v)
            | (n1, _, _) :: tail -> if not (v.Contains(n1))
                                    then let (sc, v) = Assign n1 scList sc v totalEdgeList
                                         AssignRec node tail scList sc v totalEdgeList
                                    else AssignRec node tail scList sc v totalEdgeList
        
        let mutable scList = []
        let mutable v = Set.empty
        let (n, e) = pg
        
        for i in 1..n.Length do
            let node = rpIdxNode.Item(i)
            if v.Contains(node)
                then scList <- scList
                else let (newSC, newV) = Assign node scList Set.empty v e
                     scList <- newSC :: scList
                     v <- newV
        scList             