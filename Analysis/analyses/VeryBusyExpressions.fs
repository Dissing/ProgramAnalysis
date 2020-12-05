namespace Analysis.Analyses

open Analysis
open Analysis.Expressions
open FrontEnd
open FrontEnd.ProgramGraph

type VBE = AST.ArithmeticExpr

type VeryBusyExpressionsAnalysis(graph: AnnotatedGraph) =
    inherit IBitVector<VBE>()
    
    let allExpressions = allArithmeticExpressionsInGraph graph
    
    override this.name = "Very Busy Expressions"
        
        override this.isReverseAnalysis() = true
        
        override this.lessThanOrEqual (x: Set<VBE>) (y: Set<VBE>) = Set.isSuperset x y
        
        override this.leastUpperBound (x: Set<VBE>) (y: Set<VBE>) = Set.intersect x y
        
        override this.leastElement() = allExpressions
        
        override this.initialElement(_: AnnotatedGraph) = Set.empty
        
        override this.killAndGen((_, action, _): Edge) =
            match action with
            | Allocate(decl) ->
                let kill = Set.fold (fun s loc ->
                    Set.union s
                        (expressionsContainingLocation allExpressions loc)) Set.empty (AmalgamatedLocation.fromDeclaration decl)
                let gen = Set.empty
                (kill, gen)
            | Free(_) ->
                (Set.empty, Set.empty)
            | Assign(AST.Array(x,index), expr) ->
                let kill = Set.empty
                let indexExprs = nonTrivialArithmeticExpressions index
                let assignExprs = nonTrivialArithmeticExpressions expr
                let gen = (Set.union indexExprs assignExprs)
                (kill, gen)
            | Assign(x, expr) ->
                let loc = AmalgamatedLocation.fromLocation x
                let kill = expressionsContainingLocation allExpressions loc
                let gen = nonTrivialArithmeticExpressions expr
                (kill, gen)
            | RecordAssign(s, exprs) ->
                List.fold (fun (kill, gen) (field, expr) ->
                    let loc = Field(s, field)
                    let addKill = expressionsContainingLocation allExpressions loc
                    let addGen = nonTrivialArithmeticExpressions expr
                    (Set.union kill addKill, Set.union gen addGen)
                ) (Set.empty, Set.empty) exprs
            | Condition(expr) ->
                let kill = Set.empty
                let gen = nonTrivialArithmeticExpressionsInBoolean expr
                (kill, gen)
            | Read(AST.Array(_, index)) ->
                let kill = Set.empty
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