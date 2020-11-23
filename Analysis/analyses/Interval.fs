namespace Analysis.Analyses
open Analysis
open FrontEnd
open FrontEnd.ProgramGraph

[<CustomComparison>]
type LowerBound =
    | NegInf
    | Lower of int
    
    member this.greaterThanOrEqual (other: LowerBound) =
        match (this, other) with
        | (NegInf, NegInf) -> true
        | (NegInf, _) -> false
        | (_, NegInf) -> true
        | (Lower x, Lower y) -> x >= y
        
    member this.least (other: LowerBound) =
        if this.greaterThanOrEqual(other) then other else this
        
    interface System.IComparable<LowerBound> with 
        member this.CompareTo other =
            match (this, other) with
            | (NegInf, NegInf) -> 0
            | (NegInf, _) -> -1
            | (_, NegInf) -> 1
            | (Lower x, Lower y) -> compare x y
        
    static member (+) (left, right) =
        match (left,right) with
        | (NegInf, _) -> NegInf
        | (_, NegInf) -> NegInf
        | (Lower x, Lower y) -> Lower (x + y)
    
    static member (-) (left, right) =
        match (left,right) with
        | (NegInf, _) -> NegInf
        | (_, NegInf) -> NegInf
        | (Lower x, Lower y) -> Lower (x - y)
                
    static member (*) (left, right) =
        match (left,right) with
        | (NegInf, _) -> NegInf
        | (_, NegInf) -> NegInf
        | (Lower x, Lower y) -> Lower (x * y)
                
    static member (/) (left, right) =
        match (left,right) with
        | (NegInf, _) -> NegInf
        | (_, NegInf) -> NegInf
        | (Lower x, Lower y) -> Lower (x / y)
            
[<CustomComparison>]
type UpperBound =
    | Inf
    | Upper of int
    
    member this.lessThanOrEqual (other: UpperBound) =
            match (this, other) with
            | (Inf, Inf) -> true
            | (Inf, _) -> false
            | (_, Inf) -> true
            | (Upper x, Upper y) -> x <= y
            
    member this.greatest (other: UpperBound) =
        if this.lessThanOrEqual(other) then other else this
        
    interface System.IComparable<UpperBound> with 
        member this.CompareTo other =
            match (this, other) with
            | (Inf, Inf) -> 0
            | (Inf, _) -> 1
            | (_, Inf) -> -1
            | (Upper x, Upper y) -> compare x y
        
    static member (+) (left, right) =
        match (left,right) with
        | (Inf, _) -> Inf
        | (_, Inf) -> Inf
        | (Upper x, Upper y) -> Upper (x + y)
        
    static member (-) (left, right) =
        match (left,right) with
        | (Inf, _) -> Inf
        | (_, Inf) -> Inf
        | (Upper x, Upper y) -> Upper (x - y)
                    
    static member (*) (left, right) =
        match (left,right) with
        | (Inf, _) -> Inf
        | (_, Inf) -> Inf
        | (Upper x, Upper y) -> Upper (x * y)
                    
    static member (/) (left, right) =
        match (left,right) with
        | (Inf, _) -> Inf
        | (_, Inf) -> Inf
        | (Upper x, Upper y) -> Upper (x / y)
        

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

type IntervalAnalysis(graph: AnnotatedGraph, minInt: int, maxInt: int) =
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
    
    member this.addition (z11:LowerBound , z12:UpperBound) (z21: LowerBound, z22: UpperBound) =
        let zmin = z11 + z21
        let zmax = z12 + z22
        let z1 =
            if zmin > maxInt then Lower maxInt
            elif minInt <= zmin && zmin <= maxInt then Lower zmin
            else NegInf
        let z2 =
            if zmax < minInt then Upper minInt
            elif minInt <= zmax && zmax <= maxInt then Upper zmax
            else Inf
        I(z1,z2)
    
    (*member this.multiply (left: Interval) (right: Interval) =
            match (left, right) with
            | (Bot, _) -> Bot
            | (_, Bot) -> Bot
            | (I(Lower z11, Upper z12), I(Lower z21, Upper z22)) -> 
                let zmin = min (min (z11 * z21) (z11 * z22)) (min (z12 * z21) (z12 * z22))
                let zmax = max (max (z11 * z21) (z11 * z22)) (max (z12 * z21) (z12 * z22))
                let z1 =
                    if zmin > maxInt then Lower maxInt
                    elif minInt <= zmin && zmin <= maxInt then Lower zmin
                    else NegInf
                let z2 =
                    if zmax < minInt then Upper minInt
                    elif minInt <= zmax && zmax <= maxInt then Upper zmax
                    else Inf
                I(z1,z2)*)
                    
    
    member this.arithmetic (labeling: IA) = function
        | AST.Loc(loc) ->
            labeling.[AmalgamatedLocation.fromLocation loc]
        | AST.IntLiteral(n) ->
            if n < minInt then
                I(NegInf, Upper minInt)
            elif n > maxInt then
                I(Lower maxInt, Inf)
            else
                I(Lower n, Upper n)
        | AST.ArithmeticUnary(AST.Negation, inner) -> failwith "Not yet implemented"
        | AST.ArithmeticBinary(left, op, right) -> failwith "Not yet implemented"
        
    override this.analyseEdge ((_, action, _): Edge) (labeling: IA) =
        match action with
        | Allocate(x) ->
            AmalgamatedLocation.fromDeclaration x |>
                Set.fold (fun s l -> s.Add (l, I(Lower 0, Upper 0))) labeling
        | Free(x) ->
            AmalgamatedLocation.fromDeclaration x |>
                Set.fold (fun s l -> s.Add (l, I(NegInf, Inf))) labeling
        | Assign((AST.Array(x,_), expr)) ->
            let l = AmalgamatedLocation.Array x
            let assignedInterval = this.arithmetic labeling expr
            let leastUpperBound = labeling.[l].leastUpperBound(assignedInterval)
            labeling.Add(l, leastUpperBound)
        | Assign((x, expr)) ->
            let l = AmalgamatedLocation.fromLocation x
            labeling.Add(l, this.arithmetic labeling expr)
        | AssignLiteral(strct, exprs) ->
            exprs |> List.fold
                (fun s (field, expr) -> s.Add(AmalgamatedLocation.Field(strct,field), this.arithmetic labeling expr)) labeling
        | Condition(_) -> labeling
        | Read(x) -> 
            let l = AmalgamatedLocation.fromLocation x
            labeling.Add(l, I(NegInf, Inf))
        | Write(_) -> labeling