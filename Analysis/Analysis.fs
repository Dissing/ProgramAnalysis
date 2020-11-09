module Analysis.Analysis
open FrontEnd.ProgramGraph

type IWorklist =
    
    abstract member extract: unit -> option<Node * IWorklist>
    
    abstract member insert: Node -> IWorklist
    
    
[<AbstractClass>]
type Analysis<'L when 'L : comparison> =
    
    abstract member lessThanOrEqual: 'L -> 'L -> bool
    
    abstract member leastUpperBound: 'L -> 'L -> 'L
    
    abstract member leastElement: unit -> 'L
    
    abstract member greatestElement: unit -> 'L

    abstract member analyseEdge: Edge -> 'L -> 'L
    
    member this.analyse ((annotation, (nodes, edges)): AnnotatedGraph) (worklist: IWorklist) (initial: 'L) =
 
        let initial_labelling = Map.ofList (List.map (fun (q: Node) -> (q, this.leastElement())) nodes)
        
        let worklist = List.fold (fun (w: IWorklist) (q: Node) -> w.insert(q)) worklist nodes
        
        let initial_labelling = initial_labelling.Add(nodes.Head, initial)
        
        let rec work (sol: Map<Node, 'L>) (worklist: IWorklist) =
            match worklist.extract() with
            | None -> sol
            | Some((q,w)) ->
                let (sol'',w'') =
                    List.filter (fun ((src, _, _): Edge) -> src = q) edges
                    |> List.fold (fun ((sol',w'): Map<Node, 'L> * IWorklist) ((src,action,dst): Edge) ->
                            let label = this.analyseEdge (src,action,dst) (sol'.[src])
                            if not (this.lessThanOrEqual label (sol'.[dst])) then
                                (sol'.Add(dst, (this.leastUpperBound label (sol'.[dst]))),
                                 w'.insert dst)
                            else
                                (sol', w')
                        ) (sol,w)
                work sol'' w''
                
        work initial_labelling worklist

