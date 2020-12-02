namespace Analysis.Analyses

open Analysis
open Analysis.Expressions
open FrontEnd
open FrontEnd.ProgramGraph
    
type FV = Set<AmalgamatedLocation>

type FaintVariableAnalysis() =
    inherit IAnalysis<FV>()
    
    override this.name = "Faint Variables"
    
    override this.isReverseAnalysis () = true
    
    override this.lessThanOrEqual (x: FV) (y: FV) = Set.isSubset x y
    
    override this.leastUpperBound (x: FV) (y: FV) = Set.union x y
    
    override this.leastElement() = Set.empty
    
    override this.initialElement (_: AnnotatedGraph) =
        Set.empty
    
    override this.analyseEdge ((_, action, _): Edge) (labeling: FV) =
        match action with
        | Allocate(_) | Free(_) -> labeling
        | Assign(AST.Array(x,index), expr) ->
            let x = Array(x)
            if labeling.Contains x then
                let fv = Set.union (arithmeticFreeVariables expr) (arithmeticFreeVariables index)
                Set.union (labeling.Remove x) fv
            else
                labeling
        | Assign(x, expr) ->
            let x = AmalgamatedLocation.fromLocation x
            if labeling.Contains x then
                let fv = arithmeticFreeVariables expr
                Set.union (labeling.Remove x) fv
            else
                labeling
        | AssignLiteral(s, exprs) ->
            let (gen, kill) =
                 List.fold
                     (fun ((gen, kill): FV * FV) (field, expr) ->
                        let fv = arithmeticFreeVariables expr
                        (Set.union gen fv, kill.Add(Field(s,field)))
                    ) (Set.empty, Set.empty) exprs
            Set.union (Set.difference labeling kill) gen
        | Condition(expr) -> Set.union labeling (booleanFreeVariables expr)
        | Read(AST.Array(x, index)) ->
            if labeling.Contains (Array(x)) then
                let fv = arithmeticFreeVariables index
                Set.union (labeling.Remove (Array(x))) fv
            else
                labeling
        | Read(x) -> labeling.Remove (AmalgamatedLocation.fromLocation x)
        | Write(expr) -> Set.union labeling (arithmeticFreeVariables expr)


