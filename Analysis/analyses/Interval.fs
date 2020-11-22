namespace Analysis.Analyses
open Analysis
open FrontEnd
open FrontEnd.ProgramGraph

type LowerBound =
    | NegInf
    | Lower of int
    
    member this.greaterThanOrEqual (other: LowerBound) =
        match (this, other) with
        | (NegInf, NegInf) -> true
        | (NegInf, _) -> false
        | (_, NegInf) -> true
        | (x, y) -> x >= y
        
    member this.least (other: LowerBound) =
        if this.greaterThanOrEqual(other) then other else this

type UpperBound =
    | Inf
    | Upper of int
    
    member this.lessThanOrEqual (other: UpperBound) =
            match (this, other) with
            | (Inf, Inf) -> true
            | (Inf, _) -> false
            | (_, Inf) -> true
            | (x, y) -> x <= y
            
    member this.greatest (other: UpperBound) =
        if this.lessThanOrEqual(other) then other else this
        

type Interval =
    | Bot
    | I of LowerBound * UpperBound
    
    member this.lessThanOrEqual (other: Interval) =
        match (this, other) with
        | (Bot, Bot) -> true
        | (Bot, _) -> true
        | (_, Bot) -> false
        | (I(xl,xu), I(yl, yu)) -> (xl.greaterThanOrEqual(yl)) && (xu.lessThanOrEqual(yu))
    
    member this.leastUpperBound (other: Interval) =
        match (this, other) with
        | (Bot, Bot) -> Bot
        | (Bot, y) -> y
        | (x, Bot) -> x
        | (I(xl,xu), I(yl,yu)) -> I(xl.least(yl), xu.greatest(yu))

type IA = Map<AmalgamatedLocation, Interval>

type IntervalAnalysis(graph: AnnotatedGraph, min: int, max: int) =
    inherit IAnalysis<IA>()
    
    let locations =
        let (annotation, _) = graph
        annotation |> Map.toSeq |> Seq.fold (fun s (k,v) -> Set.union s (AmalgamatedLocation.fromDeclaration v)) Set.empty |> Set.ofSeq
    
    override this.name = "Interval Analysis"
        
    override this.isReverseAnalysis () = false
        
    override this.lessThanOrEqual (x: IA) (y: IA) =
        locations |> Set.forall (fun loc ->
            x.[loc].lessThanOrEqual(y.[loc]))
    
    override this.leastUpperBound (x: IA) (y: IA) =
        locations |> Set.toSeq |> Seq.map (fun loc -> (loc, x.[loc].leastUpperBound(y.[loc]))) |> Map.ofSeq
        
    override this.leastElement() =
        locations |> Set.toSeq |> Seq.map (fun loc -> (loc, Bot)) |> Map.ofSeq
        
    override this.initialElement ((annotation, _): AnnotatedGraph) = failwith "Not yet implemented"
        
    override this.analyseEdge ((_, action, _): Edge) (labeling: IA) =
        match action with
        | Allocate(_) | Free(_) -> failwith "Not yet implemented"
        | Assign((AST.Array(x,index), expr)) -> failwith "Not yet implemented"
        | Assign((x, expr)) -> failwith "Not yet implemented"
        | AssignLiteral(s, exprs) -> failwith "Not yet implemented"
        | Condition(expr) -> failwith "Not yet implemented"
        | Read(AST.Array(x, index)) -> failwith "Not yet implemented"
        | Read(x) -> failwith "Not yet implemented"
        | Write(expr) -> failwith "Not yet implemented"