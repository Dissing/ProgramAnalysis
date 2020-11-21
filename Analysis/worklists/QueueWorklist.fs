namespace Analysis.Worklists

open FrontEnd.ProgramGraph

type Queue(inputs: List<Node>, outputs: List<Node>) =
        
        static member empty() = Queue([],[])
        
        member this.push(node: Node) =
            Queue(node::inputs, outputs)
            
        member this.pop() =
            match outputs with
            | x::xs -> Some(x, Queue(inputs, xs))
            | [] ->
                if not inputs.IsEmpty then
                    let newOutputs = List.rev inputs
                    Some(newOutputs.Head, Queue([], newOutputs.Tail))
                else
                    None

type QueueWorklist(queue: Queue) =
    
    static member empty() = QueueWorklist(Queue.empty())
    
    interface Analysis.IWorklist with
    
        member this.name = "Queue"
        
        //NOTE: Upcast is for some strange reason not implicit in F#, so we need to explicitly add the
        //'upcast' keyword in front of the constructor in order to cast the concrete type to the interface type
        member this.extract() =
            match queue.pop() with
            | Some(q, qs) -> Some(q, upcast QueueWorklist(qs))
            | None -> None
            
        member this.insert(q) =
            upcast QueueWorklist(queue.push(q))