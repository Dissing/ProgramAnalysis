namespace Analysis.Worklists

open FrontEnd

type StrongComponentsWorklist(currentNodes: List<ProgramGraph.Node>, pendingNodes: Set<ProgramGraph.Node>, rpOrdering: ReversePostorder.RPOrder) = //, strongComponents: (ProgramGraph.Node Set) List
    
    let rec getNodesInTopmost (nodes : ProgramGraph.Node Set) (components : (ProgramGraph.Node Set) List) =
        match components with
        | [] -> (Set.empty, Set.empty)
        | c::tail -> let topMost = Set.intersect nodes c
                     if topMost.IsEmpty
                     then getNodesInTopmost nodes tail
                     else let remainder = Set.difference nodes topMost
                          (topMost, remainder)
                
    static member empty(rpOrdering) = StrongComponentsWorklist([], Set.empty, rpOrdering)
    
    interface Analysis.IWorklist with
        //NOTE: Upcast is for some strange reason not implicit in F#, so we need to explicitly add the
        //'upcast' keyword in front of the constructor in order to cast the concrete type to the interface type
        member this.extract() =
            match currentNodes with
            | q::qs -> Some(q, upcast StrongComponentsWorklist(qs, pendingNodes, rpOrdering))
            | [] -> let (S, pPrime) = getNodesInTopmost pendingNodes []//strongComponents
                    let VrP = rpOrdering.getOrder(S)
                    Some((VrP.Head), upcast StrongComponentsWorklist(VrP.Tail, pPrime, rpOrdering))
            
        member this.insert(q) =
            if List.contains q currentNodes
            then upcast StrongComponentsWorklist(currentNodes, pendingNodes, rpOrdering)
            else upcast StrongComponentsWorklist(currentNodes, (pendingNodes.Add q), rpOrdering)