namespace Analysis.Worklists

open FrontEnd


type RoundRobinWorklist(currentNodes: List<ProgramGraph.Node>, pendingNodes: Set<ProgramGraph.Node>, rpOrdering: ReversePostorder.RPOrder) =
    
    static member empty(graph: ProgramGraph.Graph) =
        let (_, rpOrdering) = ReversePostorder.DFST graph
        rpOrdering.print()
        RoundRobinWorklist([], Set.empty, rpOrdering)
    
    interface Analysis.IWorklist with
        member this.name = "Round Robin"

        member this.extract() =
            match currentNodes with
            | q::qs -> Some(q, upcast RoundRobinWorklist(qs, pendingNodes, rpOrdering))
            | [] -> if pendingNodes.IsEmpty
                    then None
                    else let VrP = rpOrdering.sortNodes(pendingNodes)
                         printfn
                         Some((VrP.Head), upcast RoundRobinWorklist(VrP.Tail, Set.empty, rpOrdering))
            
        member this.insert(q) =
            if List.contains q currentNodes
            then upcast RoundRobinWorklist(currentNodes, pendingNodes, rpOrdering)
            else upcast RoundRobinWorklist(currentNodes, (pendingNodes.Add q), rpOrdering)