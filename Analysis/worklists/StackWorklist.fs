namespace Analysis.Worklists

open FrontEnd

type StackWorklist(nodes: List<ProgramGraph.Node>) =
    
    static member empty() = StackWorklist([])
    
    interface Analysis.IWorklist with

        member this.name = "Stack"

        //NOTE: Upcast is for some strange reason not implicit in F#, so we need to explicitly add the
        //'upcast' keyword in front of the constructor in order to cast the concrete type to the interface type
        member this.extract() =
            match nodes with
            | q::qs -> Some(q, upcast StackWorklist(qs))
            | [] -> None
            
        member this.insert(q) =
            upcast StackWorklist(q :: nodes)