namespace Analysis.Analyses
open Analysis
open Analysis.Analyses
open FrontEnd
open FrontEnd.ProgramGraph

type Bound =
    | NegInf
    | Inf
    | Val of int
    
    member this.lessThanOrEqual (other: Bound) =
        match (this, other) with
        | (NegInf, NegInf) -> true
        | (NegInf, _) -> true
        | (_, NegInf) -> false
        | (Inf, Inf) -> true
        | (_, Inf) -> true
        | (Inf, _) -> false
        | (Val x, Val y) -> x <= y
    
    member this.greaterThanOrEqual (other: Bound) =
        match (this, other) with
        | (NegInf, NegInf) -> true
        | (NegInf, _) -> false
        | (_, NegInf) -> true
        | (Inf, Inf) -> true
        | (Inf, _) -> true
        | (_, Inf) -> false
        | (Val x, Val y) -> x >= y
        
    member this.least (other: Bound) =
        if this.lessThanOrEqual(other) then this else other
        
    member this.greatest (other: Bound) =
        if this.greaterThanOrEqual(other) then this else other
        
type Interval =
    | Bot
    | I of Bound * Bound
    
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
    
    let minBound = Val minInt
    let maxBound = Val maxInt
    
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
    
    member this.addition (z11:Bound , z12:Bound) (z21: Bound, z22: Bound) =
        let z1 =
            match (z11, z21) with
            | (Inf, _) | (_, Inf) -> failwith "Positive infinity in lower bound addition"
            | (NegInf, _) | (_, NegInf) -> NegInf
            | (Val z11, Val z21) -> 
                let zmin = Val (z11 + z21)
                if zmin.greaterThanOrEqual(maxBound) then maxBound
                elif zmin.greaterThanOrEqual(minBound) && zmin.lessThanOrEqual(maxBound) then zmin
                else NegInf
        let z2 =
            match (z12, z22) with
            | (NegInf, _) | (_, NegInf) -> failwith "Negative infinity in lower bound addition"
            | (Inf, _) | (_, Inf) -> Inf
            | (Val z12, Val z22) -> 
                let zmax = Val (z12 + z22)
                if zmax.lessThanOrEqual(minBound) then minBound
                elif zmax.greaterThanOrEqual(minBound) && zmax.lessThanOrEqual(maxBound) then zmax
                else Inf
        I(z1,z2)
        
    member this.subtraction (z11:Bound , z12:Bound) (z21: Bound, z22: Bound) =
        let z1 =
            match (z11, z22) with
            | (Inf, _) | (_, NegInf) -> failwith "Unexpected infinity in lower bound subtraction"
            | (NegInf, _) | (_, Inf) -> NegInf
            | (Val z11, Val z22) -> 
                let zmin = Val (z11 - z22)
                if zmin.greaterThanOrEqual(maxBound) then maxBound
                elif zmin.greaterThanOrEqual(minBound) && zmin.lessThanOrEqual(maxBound) then zmin
                else NegInf
        let z2 =
            match (z12, z21) with
            | (NegInf, _) | (_, Inf) -> failwith "Negative infinity in lower bound subtraction"
            | (Inf, _) | (_, NegInf) -> Inf
            | (Val z12, Val z21) -> 
                let zmax = Val (z12 - z21)
                if zmax.lessThanOrEqual(minBound) then minBound
                elif zmax.greaterThanOrEqual(minBound) && zmax.lessThanOrEqual(maxBound) then zmax
                else Inf
        I(z1,z2)
        
        
    member this.multiplication (z11: Bound, z12: Bound) (z21: Bound, z22: Bound) =
        let product = function
            | (NegInf, NegInf) -> Inf
            | (Inf, Inf) -> Inf
            | (NegInf, Inf) | (Inf, NegInf) -> NegInf
            | (NegInf, Val x) | (Val x, NegInf) when x > 0 -> NegInf
            | (NegInf, Val x) | (Val x, NegInf) when x < 0 -> Inf
            | (NegInf, Val _) | (Val _, NegInf) -> Val 0
            | (Inf, Val x) | (Val x, Inf) when x > 0 -> Inf
            | (Inf, Val x) | (Val x, Inf) when x < 0 -> NegInf
            | (Inf, Val _) | (Val _, Inf) -> Val 0
            | (Val x, Val y) -> Val (x*y)
        
        let min a b =
            match (a,b) with
            | (NegInf, _) | (_, NegInf) -> NegInf
            | (Inf, other) | (other, Inf) -> other
            | (Val x, Val y) -> Val (if x <= y then x else y)
            
        let max a b =
            match (a,b) with
            | (NegInf, other) | (other, NegInf) -> other
            | (Inf, _) | (_, Inf) -> Inf
            | (Val x, Val y) -> Val (if x >= y then x else y)
            
        let z1 =
            let zmin: Bound = min (min (product(z11,z21)) (product(z11,z22))) (min (product(z12,z21)) (product(z12,z22)))
            if zmin.greaterThanOrEqual(maxBound) then maxBound
            elif zmin.greaterThanOrEqual(minBound) && zmin.lessThanOrEqual(maxBound) then zmin
            else NegInf
        let z2 =
            let zmax: Bound = max (max (product(z11,z21)) (product(z11,z22))) (max (product(z12,z21)) (product(z12,z22)))
            if zmax.lessThanOrEqual(minBound) then minBound
            elif zmax.greaterThanOrEqual(minBound) && zmax.lessThanOrEqual(maxBound) then zmax
            else Inf
        I(z1,z2)
        
    member this.division (z11: Bound, z12: Bound) (z21: Bound, z22: Bound) = failwith "Not yet implemented"
    
    member this.modulo (z11: Bound, z12: Bound) (z21: Bound, z22: Bound) = failwith "Not yet implemented"
                    
    
    member this.arithmetic (labeling: IA) = function
        | AST.Loc(loc) ->
            labeling.[AmalgamatedLocation.fromLocation loc]
        | AST.IntLiteral(n) ->
            if n < minInt then
                I(NegInf, minBound)
            elif n > maxInt then
                I(maxBound, Inf)
            else
                I(Val n, Val n)
        | AST.ArithmeticUnary(AST.Negation, inner) ->
            match this.arithmetic labeling inner with
            | Bot -> Bot
            | I(z1, z2) -> this.multiplication (Val -1, Val -1) (z1,z2)
            
        | AST.ArithmeticBinary(left, op, right) ->
            let l = this.arithmetic labeling left
            let r = this.arithmetic labeling right
            match (l,r) with
            | (Bot, _) -> Bot
            | (_, Bot) -> Bot
            | (I(z11, z12), I(z21, z22)) ->
                let z1 = (z11, z12)
                let z2 = (z21, z22)
                match op with
                | AST.Add -> this.addition z1 z2
                | AST.Subtract -> this.subtraction z1 z2
                | AST.Multiply -> this.multiplication z1 z2
                | AST.Divide -> this.division z1 z2
                | AST.Modulo -> this.modulo z1 z2
        
    override this.analyseEdge ((_, action, _): Edge) (labeling: IA) =
        match action with
        | Allocate(x) ->
            AmalgamatedLocation.fromDeclaration x |>
                Set.fold (fun s l -> s.Add (l, I(Val 0, Val 0))) labeling
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