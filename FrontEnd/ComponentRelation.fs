namespace FrontEnd

open FrontEnd.ProgramGraph

module ComponentRelation =
    
    type ComponentRelation(components : (Node Set) List, pg : Graph) =       
        let (_, edges) = pg
        
        //ONLY place we use the actual components
        //Create list of (idx, reducedComponent)
        let idxComponentList = List.fold (fun nc c -> (nc.Length, c)::nc) [] components
        
        //Compute the components contained in other components
        let rec ComponentsContainedInOther (comps : (int * (Node Set)) List) (relation : (int * int) Set)=
            match comps with
            | [] -> relation
            | (idx1, comp1)::tail -> let relation = List.fold (fun (rel : (int * int) Set) (idx2, comp2) -> if (Set.difference comp1 comp2) = Set.empty
                                                                                                            then rel
                                                                                                            else if Set.isSubset comp1 comp2
                                                                                                            then rel.Add(idx1, idx2)
                                                                                                            else if Set.isSubset comp2 comp1
                                                                                                            then rel.Add(idx2, idx1)
                                                                                                            else rel
                                                      ) relation tail
                                     ComponentsContainedInOther tail relation
        
        //Create list of reduced components
        let reducedComponents = List.foldBack (fun curComponent componentList ->
                                                  let removeNodes = List.fold (fun nodesToRemove realComponent ->
                                                                                    if Set.isSuperset curComponent realComponent && curComponent <> realComponent
                                                                                    then Set.union nodesToRemove realComponent
                                                                                    else nodesToRemove
                                                                               ) Set.empty components
                                                  (Set.difference curComponent removeNodes)::componentList
                                              ) components []
        
        //Create list of (idx, reducedComponent)
        let idxReducedComponentList = List.fold (fun nc c -> (nc.Length, c)::nc) [] reducedComponents
        
        //Create mapping idx -> reducedComponent
        let idxComponentsMap = Map.ofList idxReducedComponentList        
        
        //Create mapping node -> reducedComponent index 
        let rec createNodeToComponentIdxMap (componentList : (Node Set) List) (componentNum : int) (res : Map<Node, int>)=
            match componentList with
            | [] -> res
            | c::tail -> let newMap = Set.fold (fun (mapping : Map<Node, int>) node -> mapping.Add(node, componentNum)) res c
                         createNodeToComponentIdxMap tail (componentNum+1) newMap
        
        let NodeToComponentIdxMap = createNodeToComponentIdxMap reducedComponents 0 Map.empty
        
        //Get relation between nodes
        let componentRelation =
            //printfn "Components: %A" components
            //printfn "Reduced Components %A" reducedComponents
            //printfn "NodeToComponentIdxMap"
            //(NodeToComponentIdxMap |> Seq.iter (printf "%A "))
            //printfn ""
            
            let relation = ComponentsContainedInOther idxComponentList Set.empty
            List.fold (fun (rel : (int * int) Set) (n1, _, n2) -> let idx1 = NodeToComponentIdxMap.Item(n1)
                                                                  let idx2 = NodeToComponentIdxMap.Item(n2)
                                                                  if (idx1 <> idx2) && (not (rel.Contains(idx2, idx1)))
                                                                  then rel.Add(idx1, idx2)
                                                                  else rel 
                      ) relation edges
            
        //Collect the nodes in components this component (idx) has relations TO
        let rec CollectToNodes (idx : int) (compRelation : (int * int) Set) =            
            Set.fold (fun toNodes (idxFrom, idxTo) -> if idx = idxFrom
                                                      then Set.union toNodes (idxComponentsMap.Item(idxTo))
                                                      else toNodes
            ) Set.empty compRelation
        
        member this.GetRelation() =
            componentRelation
        
        member this.GetTopNodes (pendingNodes : Node Set) =
            let topNodes = Set.fold (fun (top : Node Set) node -> let idx = NodeToComponentIdxMap.Item(node)
                                                                  //printfn "NodeIdx: %d" node
                                                                  //printfn "ComponentIdx: %d" idx
                                                                  //printfn "Remove: %A" (CollectToNodes idx componentRelation)
                                                                  Set.difference top (CollectToNodes idx componentRelation)
                           ) pendingNodes pendingNodes
            let remainder = Set.difference pendingNodes topNodes
            (topNodes, remainder)
        