namespace Analysis.Analyses

open Analysis
open Analysis.Expressions
open FrontEnd
open FrontEnd.ProgramGraph

type AE = AST.ArithmeticExpr

type AvailableExpressionsAnalysis(graph: AnnotatedGraph) =
    inherit IBitVector<AE>()
    
    let allExpressions = allArithmeticExpressionsInGraph graph
    
    override this.name = "Available Expressions"
        
        override this.isReverseAnalysis() = false
        
        override this.lessThanOrEqual (x: Set<AE>) (y: Set<AE>) = Set.isSuperset x y
        
        override this.leastUpperBound (x: Set<AE>) (y: Set<AE>) = Set.intersect x y
        
        override this.leastElement() = allExpressions
        
        override this.initialElement(_: AnnotatedGraph) = Set.empty
        
        override this.killAndGen((_, action, _): Edge) =
            match action with
            | Allocate(_) ->
                (Set.empty, Set.empty)
            | Free(d) ->
                let locations = AmalgamatedLocation.fromDeclaration d
                let kill =
                    locations |> Set.fold (fun s location ->
                        Set.union s (expressionsContainingLocation allExpressions location)) Set.empty
                (kill, Set.empty)
            | Assign(AST.Array(x,index), expr) ->
                let loc = Array x
                let kill = expressionsContainingLocation allExpressions loc
                let indexExprs = nonTrivialArithmeticExpressions index
                let assignExprs = nonTrivialArithmeticExpressions expr
                let gen = expressionsNotContainingLocation (Set.union indexExprs assignExprs) loc
                (kill, gen)
            | Assign(x, expr) ->
                let loc = AmalgamatedLocation.fromLocation x
                let kill = expressionsContainingLocation allExpressions loc
                let assignExprs = nonTrivialArithmeticExpressions expr
                let gen = expressionsNotContainingLocation assignExprs loc
                (kill, gen)
            | AssignLiteral(s, exprs) ->
                List.fold (fun (kill, gen) (field, expr) ->
                    let loc = Field(s, field)
                    let addKill = expressionsContainingLocation allExpressions loc
                    let assignExprs = nonTrivialArithmeticExpressions expr
                    let addGen = expressionsNotContainingLocation assignExprs loc
                    (Set.union kill addKill, Set.union gen addGen)
                ) (Set.empty, Set.empty) exprs
            | Condition(expr) ->
                let kill = Set.empty
                let gen = nonTrivialArithmeticExpressionsInBoolean expr
                (kill, gen)
            | Read(AST.Array(x, index)) ->
                let kill = expressionsContainingLocation allExpressions (Array x)
                let gen = nonTrivialArithmeticExpressions index
                (kill, gen)
            | Read(x) ->
                let kill = expressionsContainingLocation allExpressions (AmalgamatedLocation.fromLocation x)
                let gen = Set.empty
                (kill, gen)
            | Write(expr) ->
                let kill = Set.empty
                let gen = nonTrivialArithmeticExpressions expr
                (kill, gen)