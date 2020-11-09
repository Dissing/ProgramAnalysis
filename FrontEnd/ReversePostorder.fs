namespace FrontEnd

open FrontEnd.ProgramGraph

module ReversePostorder =
    
    // Computes a map of the reverse postordering as (idx -> Node) 
    type rPOrder = Map<int, Node>
        
    // The DFS tree given in edges of (Node * Node)
    type Tree = (Node * Node) list
        
    // Maps the reverse postordering map to a list
    let rec RPMapToList (rpMap : Map<Node, int>) =
            let rpList = Map.toList(rpMap) 
            let filledList = recurse rpList (Array.create rpList.Length 0)
            Array.toList filledList
        and recurse (rpList : (Node * int) List) newList =
            match rpList with
            | [] -> newList
            | (pos, node)::tail -> Array.set newList pos node
                                   recurse tail newList
    
    //Reverse rP mapping from: idx -> Node to Node -> idx or backwards
    let revRPMap map: Map<'a,'b> = Map.fold (fun m key value -> m.Add(value,key)) Map.empty map                               
        
    
            
    let DFST (pg : Graph) =
        let rec DFSTProc (node : Node) (edges : Edge List) (tree : Tree) (v : Node Set) (k : int) (rp : rPOrder) =
            let v = v.Add(node)
            let (t, v, k, rp) = EdgesOut node edges tree v k rp
            (t, v, k, rp)
        and EdgesOut (node : Node) (edges : Edge List) (tree : Tree) (v : Node Set) (k : int) (rp : rPOrder) = 
                match edges with
                | [] -> let rp = rp.Add(k, node)
                        (tree, v, k, rp)
                | (n1, _, n2) :: tail -> if n1 = node && not (v.Contains(n2))
                                         then let (tree, v, k, rp) = DFSTProc n2 edges ((n1, n2)::tree) v k rp
                                              EdgesOut node tail tree v (k-1) rp
                                         else EdgesOut node tail tree v k rp
        
        let (n, e) = pg
        let k = n.Length
        let (tree, _, _, rPOrder) = DFSTProc n.Head e [] Set.empty k Map.empty
        (tree, rPOrder)
