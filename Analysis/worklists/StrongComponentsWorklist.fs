﻿namespace Analysis.Worklists

open FrontEnd

type StrongComponentsWorklist(currentNodes: List<ProgramGraph.Node>, pendingNodes: Set<ProgramGraph.Node>, rpOrdering: ReversePostorder.RPOrder, scRelation : ComponentRelation.ComponentRelation) =
                  
    //static member empty(rpOrdering) = StrongComponentsWorklist([], Set.empty, rpOrdering, ???)
    
    interface Analysis.IWorklist with
    
        member this.name = "Strong Components"
        
        //NOTE: Upcast is for some strange reason not implicit in F#, so we need to explicitly add the
        //'upcast' keyword in front of the constructor in order to cast the concrete type to the interface type
        member this.extract() =
            match currentNodes with
            | q::qs -> Some(q, upcast StrongComponentsWorklist(qs, pendingNodes, rpOrdering, scRelation))
            | [] -> let (S, pPrime) = scRelation.GetTopNodes pendingNodes
                    let VrP = rpOrdering.getOrder(S)
                    Some((VrP.Head), upcast StrongComponentsWorklist(VrP.Tail, pPrime, rpOrdering, scRelation))
            
        member this.insert(q) =
            if List.contains q currentNodes
            then upcast StrongComponentsWorklist(currentNodes, pendingNodes, rpOrdering, scRelation)
            else upcast StrongComponentsWorklist(currentNodes, (pendingNodes.Add q), rpOrdering, scRelation)