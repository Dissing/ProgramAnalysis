namespace Analysis.Worklists

open FrontEnd

type NaturalComponentsWorklist(currentNodes: List<ProgramGraph.Node>, pendingNodes: Set<ProgramGraph.Node>, rpOrdering: ReversePostorder.RPOrder, ncRelation : ComponentRelation.ComponentRelation) =
                    
    static member empty(graph: ProgramGraph.Graph) =
            let (_, rpOrdering) = ReversePostorder.DFST graph
            let components = NaturalLoops.NaturalLoops graph rpOrdering
            let relation = ComponentRelation.ComponentRelation(components, graph)
            NaturalComponentsWorklist([], Set.empty, rpOrdering, relation)
    
    interface Analysis.IWorklist with
    
        member this.name = "Natural Components"
        
        member this.extract() =                       
            match currentNodes with
            | q::qs -> Some(q, upcast NaturalComponentsWorklist(qs, pendingNodes, rpOrdering, ncRelation))
            | [] -> if pendingNodes.IsEmpty
                    then None
                    else let (S, pPrime) = ncRelation.GetTopNodes pendingNodes
                         //printf "Top nodes: "
                         //S |> Seq.iter (printf "%A ")
                         //printfn ""
                         //printf "Non-top nodes: "
                         //pPrime |> Seq.iter (printf "%A ")
                         //printfn ""
                         let VrP = rpOrdering.sortNodes(S)
                         //printfn "Ordered top nodes: %A" VrP
                         Some((VrP.Head), upcast NaturalComponentsWorklist(VrP.Tail, pPrime, rpOrdering, ncRelation))
            
        member this.insert(q) =
            if List.contains q currentNodes
            then upcast NaturalComponentsWorklist(currentNodes, pendingNodes, rpOrdering, ncRelation)
            else upcast NaturalComponentsWorklist(currentNodes, (pendingNodes.Add q), rpOrdering, ncRelation)