namespace FrontEnd

open FrontEnd.ProgramGraph

module ReversePostorder =
    
    type RPOrder(order: Map<Node, int>) =
        
        let revOrder = Map.fold (fun (m : Map<Node, int>) node idx -> m.Add(idx, node)) Map.empty order
        
        let orderSort n1 n2 =
                if order.Item(n1) < order.Item(n2) then -1 else
                if order.Item(n1) > order.Item(n2) then 1 else
                0
        
        member this.addNode(n, idx) =
            RPOrder(order.Add(n,idx))
            
        member this.nodeAtIdx(i) =
            revOrder.Item(i)
        
        member this.before(n1, n2) =
            order.Item(n1) <= order.Item(n2)
            
        member this.after(n1, n2) =
            order.Item(n1) > order.Item(n2)
            
        member this.getOrder(nodes) =
            List.sortWith orderSort (Set.toList nodes)
            
        member this.print() =
            order |> Map.toList |> printfn "Reverse Postorder: %A"
            
        
    // The DFS tree given in edges of (Node * Node)
    type Tree = (Node * Node) list            
                    
    let DFST (pg : Graph) =
        let rec DFSTProc (node : Node) (edges : Edge List) (tree : Tree) (v : Node Set) (k : int) (rp : RPOrder) =
            let v = v.Add(node)
            let (t, v, k, rp) = EdgesOut node edges tree v k rp
            (t, v, k, rp)
        and EdgesOut (node : Node) (edges : Edge List) (tree : Tree) (v : Node Set) (k : int) (rp : RPOrder) = 
                match edges with
                | [] -> let rp = rp.addNode(node, k)
                        (tree, v, k, rp)
                | (n1, _, n2) :: tail -> if n1 = node && not (v.Contains(n2))
                                         then let (tree, v, k, rp) = DFSTProc n2 edges ((n1, n2)::tree) v k rp
                                              EdgesOut node tail tree v (k-1) rp
                                         else EdgesOut node tail tree v k rp
        
        let (n, e) = pg
        let k = n.Length
        let (tree, _, _, rPOrder) = DFSTProc n.Head e [] Set.empty k (RPOrder(Map.empty))
        (tree, rPOrder)
