namespace Analysis.Analyses

open Analysis
open Analysis.Expressions
open FrontEnd
open FrontEnd.ProgramGraph
    
type DV = Set<AmalgamatedLocation>

type DangerousVariableAnalysis() =
    inherit IAnalysis<DV>()
    
    override this.name = "Dangerous Variables"
    
    override this.isReverseAnalysis () = false
    
    override this.lessThanOrEqual (x: DV) (y: DV) = Set.isSubset x y
    
    override this.leastUpperBound (x: DV) (y: DV) = Set.union x y
    
    override this.leastElement() = Set.empty
    
    override this.initialElement ((annotation, _): AnnotatedGraph) =
        AmalgamatedLocation.fromAnnotation annotation
    
    override this.analyseEdge ((_, action, _): Edge) (labeling: DV) =
        match action with
        | Allocate(_) | Free(_) -> labeling
        | Assign(AST.Array(x,index), expr) ->
            let fv = Set.union (arithmeticFreeVariables index) (arithmeticFreeVariables expr)
            if (Set.intersect fv labeling).IsEmpty then
                labeling
            else
                labeling.Add(Array(x))
        | Assign(x, expr) ->
            let fv = arithmeticFreeVariables expr
            if (Set.intersect fv labeling).IsEmpty then
                labeling.Remove(AmalgamatedLocation.fromLocation x)
            else
                labeling.Add(AmalgamatedLocation.fromLocation x)
        | AssignLiteral(s, exprs) ->
            let (gen, kill) =
                 List.fold
                     (fun ((gen, kill): DV * DV) (field, expr) ->
                        let fv = arithmeticFreeVariables expr
                        if (Set.intersect fv labeling).IsEmpty then
                            (gen, kill.Add(Field(s,field)))
                        else
                            (gen.Add(Field(s,field)), kill)
                    ) (Set.empty, Set.empty) exprs
            Set.union (Set.difference labeling kill) gen
        | Condition(_) -> labeling
        | Read(AST.Variable(x)) -> labeling.Remove (Variable x)
        | Read(AST.Array(x, _)) -> labeling
        | Read(AST.Field(s, f)) -> labeling.Remove (Field(s,f))
        | Write(_) -> labeling
