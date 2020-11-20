namespace Analysis.Worklists

open FrontEnd

type NaturalComponentsWorkList(currentNodes: List<ProgramGraph.Node>, pendingNodes: Set<ProgramGraph.Node>, rpOrdering: ReversePostorder.RPOrder, ncRelation : ComponentRelation.ComponentRelation) =
                    
    //static member empty(rpOrdering) = NaturalComponentsWorkList([], Set.empty, rpOrdering, ???)
    
    interface Analysis.IWorklist with
        member this.extract() =
            match currentNodes with
            | q::qs -> Some(q, upcast NaturalComponentsWorkList(qs, pendingNodes, rpOrdering, ncRelation))
            | [] -> let (S, pPrime) = ncRelation.GetTopNodes pendingNodes
                    let VrP = rpOrdering.getOrder(S)
                    Some((VrP.Head), upcast NaturalComponentsWorkList(VrP.Tail, pPrime, rpOrdering, ncRelation))
            
        member this.insert(q) =
            if List.contains q currentNodes
            then upcast NaturalComponentsWorkList(currentNodes, pendingNodes, rpOrdering, ncRelation)
            else upcast NaturalComponentsWorkList(currentNodes, (pendingNodes.Add q), rpOrdering, ncRelation)