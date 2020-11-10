namespace Analysis

open FrontEnd
open FrontEnd.ProgramGraph
    
type DV = Set<AmalgamatedLocation>

type DangerousVariableAnalysis() =
    inherit Analysis<DV>()
    
    override this.lessThanOrEqual (x: DV) (y: DV) = Set.isSubset x y
    
    override this.leastUpperBound (x: DV) (y: DV) = Set.union x y
    
    override this.leastElement() = Set.empty
    
    override this.analyseEdge ((src, action, dst): Edge) (labeling: DV) =
        match action with
        | Allocate(_) | Free(_) -> labeling
        | Assign((AST.Array(x,index), expr)) ->
            let fv1 =  AST.freeVariables index
            let fv2 = AST.freeVariables expr
            let fv = Set.map AmalgamatedLocation.fromLocation (Set.union fv1 fv2)
            if (Set.intersect fv labeling).IsEmpty then
                labeling
            else
                labeling.Add(Array(x))
        | Assign((x, expr)) ->
            let fv = Set.map AmalgamatedLocation.fromLocation (AST.freeVariables expr)
            if (Set.intersect fv labeling).IsEmpty then
                labeling.Remove(AmalgamatedLocation.fromLocation x)
            else
                labeling.Add(AmalgamatedLocation.fromLocation x)
        | AssignLiteral(s, exprs) ->
            let (gen, kill) =
                 List.fold
                     (fun ((gen, kill): DV * DV) (field, expr) ->
                        let fv = Set.map AmalgamatedLocation.fromLocation (AST.freeVariables expr)
                        if (Set.intersect fv labeling).IsEmpty then
                            (gen, kill.Add(Field(s,field)))
                        else
                            (gen.Add(Field(s,field)), kill)
                    ) (Set.empty, Set.empty) exprs
            Set.union (Set.difference labeling kill) gen
        | Condition(_) -> labeling
        | Read(AST.Identifier(x)) -> labeling.Remove (Variable x)
        | Read(AST.Array(x, _)) -> labeling
        | Read(AST.Field(s, f)) -> labeling.Remove (Field(s,f))
        | Write(_) -> labeling
