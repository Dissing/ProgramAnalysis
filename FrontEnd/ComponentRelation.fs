namespace FrontEnd

open FrontEnd.ProgramGraph

module ComponentRelation =
    
    type ComponentRelation(components : (Node Set) List, pg : Graph) =       
        let (nodes, edges) = pg
        
        let rec GetComponentIdxFromNode (node : Node) (components : (Node Set) List) (idx : int) =
            match components with
            | [] -> idx
            | ns::tail -> if ns.Contains(node)
                          then idx
                          else GetComponentIdxFromNode node tail (idx+1)
        
        let NodeToComponentIdx =
            List.fold (fun (m : Map<Node, int>) node -> m.Add(node, (GetComponentIdxFromNode node components 0))) Map.empty nodes
        
        let rec ComponentsContainedInOther (comps : (Node Set) List) (relation : (int * int) Set)=
            match comps with
            | [] -> relation
            | comp1::tail -> let relation = List.fold (fun (rel : (int * int) Set) comp2 -> if Set.isSubset comp1 comp2
                                                                                            then rel.Add((NodeToComponentIdx.Item(comp1.MinimumElement)), (NodeToComponentIdx.Item(comp2.MinimumElement)))
                                                                                            else if Set.isSubset comp2 comp1
                                                                                            then rel.Add((NodeToComponentIdx.Item(comp2.MinimumElement)), (NodeToComponentIdx.Item(comp1.MinimumElement)))
                                                                                            else rel
                                                      ) relation tail
                             ComponentsContainedInOther tail relation
        
        let componentRelation =
            let relation = ComponentsContainedInOther components Set.empty
            List.fold (fun (rel : (int * int) Set) (n1, _, n2) -> let idx1 = NodeToComponentIdx.Item(n1)
                                                                  let idx2 = NodeToComponentIdx.Item(n2)
                                                                  if (idx1 <> idx2) && not (rel.Contains(idx2, idx1)) then rel.Add(idx1, idx2) else rel
                       ) relation edges
        
        let rec CollectToNodes (idx : int) (compRelation : (int * int) Set) =
            Set.fold (fun toNodes (idxFrom, idxTo) -> if idx = idxFrom
                                                      then Set.union toNodes (components.Item(idxTo))
                                                      else toNodes
            ) Set.empty compRelation

            
        member this.GetRelation() =
            componentRelation
        
        member this.GetTopNodes (pendingNodes : Node Set) =
            let topNodes = Set.fold (fun (top : Node Set) node -> let idx = NodeToComponentIdx.Item(node)
                                                                  //printfn "%d removes %A" node (CollectToNodes idx componentRelation)
                                                                  Set.difference top (Set.difference (CollectToNodes idx componentRelation) (components.Item(idx)))
                           ) pendingNodes pendingNodes
            let remainder = Set.difference pendingNodes topNodes
            (topNodes, remainder)
        