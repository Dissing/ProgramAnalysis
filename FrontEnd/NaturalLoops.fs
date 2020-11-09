namespace FrontEnd

open FrontEnd
open FrontEnd.ProgramGraph
open FrontEnd.ReversePostorder

module NaturalLoops =
    
    // Compute backedges in a program graph
    let rec GetBackEdges (edges : Edge List) (rpNodeIdx : Map<Node, int>) (backEdges : (Node * Node) List) =
        match edges with
        | [] -> backEdges
        | (n1, _, n2)::tail -> if rpNodeIdx.Item(n2) <= rpNodeIdx.Item(n1)
                               then GetBackEdges tail rpNodeIdx ((n1,n2)::backEdges)
                               else GetBackEdges tail rpNodeIdx backEdges                           
                               
                               
    
    let NaturalLoops (pg : Graph) (rp : rPOrder) =
        // Build function and loop recursion
        let rec Build (n1 : Node) (n2 : Node) (edges : Edge List) (rpNodeIdx : Map<Node, int>) (L : Map<Node, Set<Node>>) =
            if rpNodeIdx.Item(n2) > rpNodeIdx.Item(n1)
               then failwith "Failure: The graph is non-reducible."
               else if not (L.Item(n2).Contains(n1))
                    then let L = L.Add(n2, L.Item(n2).Add(n1))
                         let edgesIn = StrongComponents.EdgesIn n1 edges []
                         BuildRecurse n2 edgesIn edges rpNodeIdx L
                    else L
        and BuildRecurse (node : Node) (edgesIn : Edge List) (edges : Edge List) (rpNodeIdx : Map<Node, int>) (L : Map<Node, Set<Node>>) =
            match edgesIn with
            | [] -> L
            | (n1, _, _)::tail -> let L = Build n1 node edges rpNodeIdx L
                                  BuildRecurse node tail edges rpNodeIdx L
        
        // Natural loops recursion / recursion over backedges                          
        let rec NaturalLoopsRecurse (backEdges : (Node * Node) List) (edges : Edge List) (rpNodeIdx : Map<Node, int>) (L : Map<Node, Set<Node>>) =
            match backEdges with
            | [] -> L
            | (n1, n2)::tail -> if L.ContainsKey(n2)
                                then let L = Build n1 n2 edges rpNodeIdx L
                                     NaturalLoopsRecurse tail edges rpNodeIdx L
                                else let L = L.Add(n2, Set.empty.Add(n2))
                                     let L = Build n1 n2 edges rpNodeIdx L
                                     NaturalLoopsRecurse tail edges rpNodeIdx L
        
        let (_, edges) = pg
        let L = Map.empty
        let rpNodeIdx = revRPMap rp
        
        let backEdges = GetBackEdges edges rpNodeIdx []
        NaturalLoopsRecurse backEdges edges rpNodeIdx L