namespace FrontEnd

open FrontEnd
open FrontEnd.ProgramGraph
open FrontEnd.ReversePostorder

module NaturalLoops =
    
    // Compute backedges in a program graph
    let rec GetBackEdges (edges : Edge List) (rp : RPOrder) (backEdges : (Node * Node) List) =
        match edges with
        | [] -> backEdges
        | (n1, _, n2)::tail -> if rp.before(n2, n1)
                               then GetBackEdges tail rp ((n1,n2)::backEdges)
                               else GetBackEdges tail rp backEdges                           
                               
                               
    
    let NaturalLoops (pg : Graph) (rp : RPOrder) =
        // Build function and loop recursion
        let rec Build (n1 : Node) (n2 : Node) (edges : Edge List) (rp : RPOrder) (L : Map<Node, Set<Node>>) =
            if rp.after(n2, n1)
               then failwithf "Failure: The graph is non-reducible. Error spotted between nodes %d and %d.\n Current L : %A" n1 n2 L
               else if not (L.Item(n2).Contains(n1))
                    then let L = L.Add(n2, L.Item(n2).Add(n1))
                         let edgesIn = StrongComponents.EdgesIn n1 edges []
                         BuildRecurse n2 edgesIn edges rp L
                    else L
        and BuildRecurse (node : Node) (edgesIn : Edge List) (edges : Edge List) (rp : RPOrder) (L : Map<Node, Set<Node>>) =
            match edgesIn with
            | [] -> L
            | (n1, _, _)::tail -> let L = Build n1 node edges rp L
                                  BuildRecurse node tail edges rp L
        
        // Natural loops recursion / recursion over backedges                          
        let rec NaturalLoopsRecurse (backEdges : (Node * Node) List) (edges : Edge List) (rp : RPOrder) (L : Map<Node, Set<Node>>) =
            match backEdges with
            | [] -> L
            | (n1, n2)::tail -> if L.ContainsKey(n2)
                                then let L = Build n1 n2 edges rp L
                                     NaturalLoopsRecurse tail edges rp L
                                else let L = L.Add(n2, Set.empty.Add(n2))
                                     let L = Build n1 n2 edges rp L
                                     NaturalLoopsRecurse tail edges rp L
        
        let (_, edges) = pg
        let L = Map.empty
        
        let backEdges = GetBackEdges edges rp []
        NaturalLoopsRecurse backEdges edges rp L