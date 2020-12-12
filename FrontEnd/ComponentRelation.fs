namespace FrontEnd

open FrontEnd.ProgramGraph

module ComponentRelation =
    
    type ComponentRelation(components : (Node Set) List, pg : Graph) =       
        let (nodes, edges) = pg
        
        //---------- Component Lists ----------\\
        //Create list of (idx, reducedComponent)
        let idxComponentList = List.fold (fun nc c -> (nc.Length, c)::nc) [] components
        
        //Create list of reduced components
        let reducedComponents = List.foldBack (fun curComponent componentList ->
            let removeNodes = List.fold (fun nodesToRemove realComponent ->
                if Set.isSuperset curComponent realComponent && curComponent <> realComponent
                    then Set.union nodesToRemove realComponent
                else nodesToRemove
                                        ) Set.empty components
            (Set.difference curComponent removeNodes)::componentList) components []
        
        //---------- Node & Component Mappings ----------\\
        //Create mapping component index -> nodes
        let idxComponentsMap = Map.ofList (List.fold (fun nc c -> (nc.Length, c)::nc) [] reducedComponents)
        
        //Create mapping node -> component indices
        let rec createNodeToComponentsMap (componentList : (Node Set) List) (componentIdx : int) (res : Map<Node, int Set>)=
            match componentList with
            | [] -> res
            | c::tail -> let newMap = Set.fold (fun (mapping : Map<Node, int Set>) node -> mapping.Add(node, (mapping.Item(node).Add(componentIdx)))) res c
                         createNodeToComponentsMap tail (componentIdx+1) newMap
        
        let NodeToComponentsMap = createNodeToComponentsMap components 0 (List.fold (fun (m: Map<int, int Set>) idx -> m.Add(idx, Set.empty)  ) Map.empty [0..nodes.Length-1])
        
        
        
        //---------- Component Relation ----------\\
        //Compute the components contained in other components
        let rec ComponentsContainedInOther (comps : (int * (Node Set)) List) (relation : Map<int, int Set>)=
            match comps with
            | [] -> relation
            | (idx1, comp1)::tail ->
                let relation = List.fold (fun (rel : Map<int, int Set>) (idx2, comp2) ->
                                    if (Set.difference comp1 comp2) = Set.empty then rel
                                    else if Set.isSubset comp1 comp2 then rel.Add(idx1, (rel.Item(idx1).Add(idx2)))
                                    else if Set.isSubset comp2 comp1 then rel.Add(idx2, (rel.Item(idx2).Add(idx1)))
                                    else rel
                                ) relation tail
                ComponentsContainedInOther tail relation
        
        //Get relation between nodes
        let componentRelation =
            //printfn "Components: %A" components
            //printfn "Reduced Components %A" reducedComponents
            //printfn "NodeToComponentIdxMap"
            //(NodeToComponentsMap |> Seq.iter (printf "%A "))
            //printfn ""
            
            let relation = List.fold (fun (m: Map<int, int Set>) idx -> m.Add(idx, Set.empty)  ) Map.empty [0..components.Length-1]
            let relation = ComponentsContainedInOther idxComponentList relation
            List.fold (fun (rel : Map<int, int Set>) (n1, _, n2) ->
                                         let components1 = NodeToComponentsMap.Item(n1)
                                         let components2 = NodeToComponentsMap.Item(n2)
                                         Set.fold (fun (rel : Map<int, int Set>) (cIdx1 : int) ->
                                                    Set.fold(fun (rel : Map<int, int Set>) (cIdx2 : int) ->
                                                                if (cIdx1 <> cIdx2) && (not (rel.Item(cIdx2).Contains(cIdx1)))
                                                                then rel.Add(cIdx1, (rel.Item(cIdx1).Add(cIdx2)))
                                                                else rel
                                                            ) rel components2
                                                  ) rel components1
                       ) relation edges
        
        //---------- Compute Top Nodes ----------\\
        //Recursively collect the predecessor nodes of the component of this node
        let rec CollectPredecessorNodes (idx : int) (nonTopNodes : Node Set)=
            let nonTopComponents = componentRelation.Item(idx)
            let nonTopNodes = (Set.fold (fun toNodes idxTo ->
                                 Set.union toNodes (idxComponentsMap.Item(idxTo))
                              ) nonTopNodes nonTopComponents)
            Set.fold (fun (nodes : Node Set) cIdx -> CollectPredecessorNodes cIdx nodes ) nonTopNodes nonTopComponents
                    
        member this.GetRelation() =
            componentRelation
        
        member this.GetTopNodes (pendingNodes : Node Set) =
            let topNodes = Set.fold (fun (top : Node Set) node ->
                                     let indices = NodeToComponentsMap.Item(node)
                                     let nonTopNodes = Set.fold (fun (s : int Set) (idx : int) -> Set.union s (CollectPredecessorNodes idx Set.empty)) Set.empty indices
                                     Set.difference top nonTopNodes
                           ) pendingNodes pendingNodes
            let remainder = Set.difference pendingNodes topNodes
            (topNodes, remainder)
        