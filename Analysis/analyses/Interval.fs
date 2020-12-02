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
    
    member this.intersection (other: Interval) =
        match (this, other) with
        | (I(xl,xu), I(yl,yu)) ->
            let zmin = xl.greatest(yl)
            let zmax = xu.least(yu)
            if zmin.lessThanOrEqual(zmax) then
                I(zmin, zmax)
            else
                Bot
        | _ -> Bot

type IA = Map<AmalgamatedLocation, Interval>

type IntervalAnalysis(graph: AnnotatedGraph, minInt: int, maxInt: int) =
    inherit IAnalysis<IA>()
    
    let minBound = Val minInt
    let maxBound = Val maxInt
    
    let locations =
        let (annotation, _) = graph
        annotation |> Map.toSeq |> Seq.fold (fun s (k,v) -> Set.union s (AmalgamatedLocation.fromDeclaration v)) Set.empty |> Set.ofSeq
        
    let arrayBounds =
        let (annotation, _) = graph
        annotation |> Map.fold (fun (s: Map<string, Interval>) _ v ->
            match v with
            | AST.ArrayDecl(name, size) ->
                let zmin = Val(0).greatest(minBound)
                let zmax = Val(size-1).least(maxBound)
                s.Add(name, I(zmin, zmax))
            | _ -> s) Map.empty
 
    let minOfBounds a b =
        match (a,b) with
        | (NegInf, _) | (_, NegInf) -> NegInf
        | (Inf, other) | (other, Inf) -> other
        | (Val x, Val y) -> Val (if x <= y then x else y)
                    
    let maxOfBounds a b =
        match (a,b) with
        | (NegInf, other) | (other, NegInf) -> other
        | (Inf, _) | (_, Inf) -> Inf
        | (Val x, Val y) -> Val (if x >= y then x else y)
    
    override this.name = "Interval Analysis"
        
    override this.isReverseAnalysis () = false
        
    override this.lessThanOrEqual (x: IA) (y: IA) =
        if (Map.isEmpty x) && (Map.isEmpty y) then
            true
        elif Map.isEmpty x then
            true
        elif Map.isEmpty y then
            false
        else
            locations |> Set.forall (fun loc ->
                x.[loc].lessThanOrEqual(y.[loc]))
    
    override this.leastUpperBound (x: IA) (y: IA) =
        if Map.isEmpty x then
            y
        elif Map.isEmpty y then
            x
        else
            locations |> Set.toSeq |> Seq.map (fun loc -> (loc, x.[loc].leastUpperBound(y.[loc]))) |> Map.ofSeq
        
    override this.leastElement() =
        Map.empty
        
    override this.initialElement ((annotation, _): AnnotatedGraph) =
        annotation |> Map.toSeq |>
            Seq.fold (fun s1 (_,v) ->
                Set.fold (fun s2 l ->
                    s2.Add(l,(I(NegInf, Inf)))) s1 (AmalgamatedLocation.fromDeclaration(v))) Map.empty
        
    
    member this.addition (left: Interval) (right: Interval) =
        match (left, right) with
        | (Bot, _) | (_, Bot) -> Bot
        | (I(z11,z12),I(z21,z22)) -> 
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
        
    member this.subtraction (left: Interval) (right: Interval) =
        match (left, right) with
        | (Bot, _) | (_, Bot) -> Bot
        | (I(z11,z12),I(z21,z22)) -> 
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
        
    member this.multiplication (left: Interval) (right: Interval) =
        match (left, right) with
             | (Bot, _) | (_, Bot) -> Bot
             | (I(z11,z12),I(z21,z22)) -> 
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
                 
                 let zmin = minOfBounds (minOfBounds (product(z11,z21)) (product(z11,z22))) (minOfBounds (product(z12,z21)) (product(z12,z22)))
                 let zmax = maxOfBounds (maxOfBounds (product(z11,z21)) (product(z11,z22))) (maxOfBounds (product(z12,z21)) (product(z12,z22)))
                 let z1 =
                    if zmin.greaterThanOrEqual(maxBound) then maxBound
                    elif zmin.greaterThanOrEqual(minBound) && zmin.lessThanOrEqual(maxBound) then zmin
                    else NegInf
                 let z2 =
                    if zmax.lessThanOrEqual(minBound) then minBound
                    elif zmax.greaterThanOrEqual(minBound) && zmax.lessThanOrEqual(maxBound) then zmax
                    else Inf
                 I(z1,z2)
        
    member this.division (left: Interval) (right: Interval) =
        match (left, right) with
        | (Bot, _) | (_, Bot) -> Bot
        | (I(_,_),I(z21,z22)) when z21 = Val 0 && z22 = Val 0 -> Bot
        | (I(z11,z12),I(z21,z22)) ->
            let zmin =
                match z22 with
                | Inf -> Val 0
                | Val x when x = 0 -> NegInf
                | Val x -> Val (1 / x)
                | NegInf -> failwith "Negative infinity upper bound!"
            let zmax =
                match z21 with
                | NegInf -> Val 0
                | Val x when x = 0 -> Inf
                | Val x -> Val (1 / x)
                | Inf -> failwith "Positive infinity lower bound!"
            if (z21.lessThanOrEqual(Val -1) && z22.lessThanOrEqual(Val -1)) || (z21.greaterThanOrEqual(Val 1) && z22.greaterThanOrEqual(Val 1))
            then this.multiplication (I(z11, z12)) (I(zmin, zmax))
            elif z21 = Val 0
            then this.multiplication (I(z11, z12)) (I(zmin, Val 1))
            elif z22 = Val 0
            then this.multiplication (I(z11, z12)) (I(Val -1, zmax))
            else this.multiplication (I(z11, z12)) (I(Val -1, Val 1))
            
    
    member this.modulo (left: Interval) (right: Interval) =
        match (left, right) with
        | (Bot, _) | (_, Bot) -> Bot
        | (_,I(z21,z22)) when z21 = Val 0 && z22 = Val 0 -> Bot
        | (I(z11,z12),I(z21,z22)) ->
            
            let negate = function
            | NegInf -> Inf
            | Inf -> NegInf
            | Val x -> Val (-x)

            let rec mod1 (a: Bound) (d: Bound) =
                match (a, d) with
                | (_, NegInf) -> failwith "Mod1 requires positive divisor"
                | (_, Val y) when y <= 0 -> failwith "Mod1 requires positive divisor"
                | (NegInf, y) -> negate (mod1 Inf y)
                | (Val x, y) when x < 0 -> negate (mod1 (Val (-x)) y)
                | (Val x, _) when x = 0 -> Val x
                | (Inf, Inf) -> Inf
                | (Inf, Val y) -> Val (y-1)
                | (Val x, Inf) -> Val x
                | (Val x, Val y) -> Val (min x (y-1))
                
            let (minDivisor, maxDivisor) =
                match (z21, z22) with
                | (_, NegInf) | (Inf, _) -> failwith "Invalid infinity in interval bound"
                | (NegInf, Inf) -> (Val 1, Inf)
                | (NegInf, Val y) when y < 0 -> (Val (-y), Inf)
                | (NegInf, Val _) -> (Val 1, Inf)
                | (Val x, Inf) when x > 0 -> (Val x, Inf)
                | (Val _, Inf) -> (Val 1, Inf)
                | (Val x, Val y) when x > 0 && y > 0 -> (Val x, Val y)
                | (Val x, Val y) when x < 0 && y < 0 -> (Val -y, Val -x)
                | (Val x, Val y) -> (Val 1, Val (max (abs x) (abs y)))
                
            let zmin = minOfBounds (minOfBounds (mod1 z11 minDivisor) (mod1 z11 maxDivisor)) (minOfBounds (mod1 z12 minDivisor) (mod1 z12 maxDivisor))
            let zmax = maxOfBounds (maxOfBounds (mod1 z11 minDivisor) (mod1 z11 maxDivisor)) (maxOfBounds (mod1 z12 minDivisor) (mod1 z12 maxDivisor))
            let z1 =
               if zmin.greaterThanOrEqual(maxBound) then maxBound
               elif zmin.greaterThanOrEqual(minBound) && zmin.lessThanOrEqual(maxBound) then zmin
               else NegInf
            let z2 =
               if zmax.lessThanOrEqual(minBound) then minBound
               elif zmax.greaterThanOrEqual(minBound) && zmax.lessThanOrEqual(maxBound) then zmax
               else Inf
            I(z1,z2)
    
    member this.arithmetic (labeling: IA) = function
        | AST.Loc(AST.Array(x, index)) ->
            let boundsInterval = arrayBounds.[x]
            let indexInterval = this.arithmetic labeling index
            let overlapInterval = boundsInterval.intersection(indexInterval)
            if overlapInterval = Bot then
                Bot
            else
                labeling.[Array x]
        | AST.Loc(loc) ->
            labeling.[AmalgamatedLocation.fromLocation loc]
        | AST.IntLiteral(n) ->
            if n < minInt then
                I(NegInf, minBound)
            elif n > maxInt then
                I(maxBound, Inf)
            else
                I(Val n, Val n)
        | AST.ArithmeticUnary(AST.Negative, inner) ->
            match this.arithmetic labeling inner with
            | Bot -> Bot
            | I(z1, z2) -> this.multiplication (I(Val -1, Val -1)) (I(z1,z2))
            
        | AST.ArithmeticBinary(left, op, right) ->
            let l = this.arithmetic labeling left
            let r = this.arithmetic labeling right
            match op with
            | AST.Add -> this.addition l r
            | AST.Subtract -> this.subtraction l r
            | AST.Multiply -> this.multiplication l r
            | AST.Divide -> this.division l r
            | AST.Modulo -> this.modulo l r
        
    override this.analyseEdge ((_, action, _): Edge) (labeling: IA) =
        if Map.isEmpty labeling then
            Map.empty
        else
            match action with
            | Allocate(x) ->
                AmalgamatedLocation.fromDeclaration x |>
                    Set.fold (fun s l -> s.Add (l, I(Val 0, Val 0))) labeling
            | Free(x) ->
                AmalgamatedLocation.fromDeclaration x |>
                    Set.fold (fun s l -> s.Add (l, I(NegInf, Inf))) labeling
            | Assign(AST.Array(x,index), expr) ->
                let l = AmalgamatedLocation.Array x
                let boundsInterval = arrayBounds.[x]
                let indexInterval = this.arithmetic labeling index
                let overlapInterval = boundsInterval.intersection(indexInterval)
                let assignedInterval = this.arithmetic labeling expr
                if assignedInterval = Bot || overlapInterval = Bot then
                    Map.empty
                else
                    let leastUpperBound = labeling.[l].leastUpperBound(assignedInterval)
                    labeling.Add(l, leastUpperBound)
            | Assign(x, expr) ->
                let l = AmalgamatedLocation.fromLocation x
                let assignedInterval = this.arithmetic labeling expr
                if assignedInterval = Bot then
                    Map.empty
                else
                    labeling.Add(l, this.arithmetic labeling expr)
            | AssignLiteral(strct, exprs) ->
                exprs |> List.fold
                    (fun s (field, expr) ->
                    if Map.isEmpty s then
                        Map.empty
                    else
                        let assignedInterval = this.arithmetic labeling expr
                        if assignedInterval = Bot then
                            Map.empty
                        else
                            s.Add(AmalgamatedLocation.Field(strct,field), assignedInterval)) labeling
            | Condition(_) -> labeling
            | Read(AST.Array(x, index)) ->
                let l = Array x
                let boundsInterval = arrayBounds.[x]
                let indexInterval = this.arithmetic labeling index
                let overlapInterval = boundsInterval.intersection(indexInterval)
                if overlapInterval = Bot then
                    Map.empty
                else
                    labeling.Add(l, I(NegInf, Inf))
            | Read(x) -> 
                let l = AmalgamatedLocation.fromLocation x
                labeling.Add(l, I(NegInf, Inf))

            | Write(expr) ->
                let outputInterval = this.arithmetic labeling expr
                if outputInterval = Bot then
                    Map.empty
                else
                    labeling