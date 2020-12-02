namespace Analysis.Worklists

open FrontEnd


type StrongComponentsWorklist(currentNodes: List<ProgramGraph.Node>, pendingNodes: Set<ProgramGraph.Node>, rpOrdering: ReversePostorder.RPOrder, scRelation : ComponentRelation.ComponentRelation) =
    
    static member empty(graph: ProgramGraph.Graph) =
        let (_, rpOrdering) = ReversePostorder.DFST graph
        let components = StrongComponents.StrongComps graph rpOrdering
        let relation = ComponentRelation.ComponentRelation(components, graph)
        StrongComponentsWorklist([], Set.empty, rpOrdering, relation)
    
    interface Analysis.IWorklist with
        member this.name = "Strong Components"

        member this.extract() =
            match currentNodes with
            | q::qs -> Some(q, upcast StrongComponentsWorklist(qs, pendingNodes, rpOrdering, scRelation))
            | [] -> if pendingNodes.IsEmpty
                    then None
                    else let (S, pPrime) = scRelation.GetTopNodes pendingNodes
                         let VrP = rpOrdering.sortNodes(S)
                         Some((VrP.Head), upcast StrongComponentsWorklist(VrP.Tail, pPrime, rpOrdering, scRelation))
            
        member this.insert(q) =
            if List.contains q currentNodes
            then upcast StrongComponentsWorklist(currentNodes, pendingNodes, rpOrdering, scRelation)
            else upcast StrongComponentsWorklist(currentNodes, (pendingNodes.Add q), rpOrdering, scRelation)