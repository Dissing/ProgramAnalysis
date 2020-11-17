namespace Analysis.Analyses

open Analysis
open Analysis.FreeVariables
open FrontEnd
open FrontEnd.ProgramGraph

type LV = AmalgamatedLocation

type LiveVariablesAnalysis() =
    inherit IBitVector<LV>()
    
    override this.name = "Live Variables"
    
    override this.isReverseAnalysis() = true
    
    override this.lessThanOrEqual (x: Set<LV>) (y: Set<LV>) = Set.isSubset x y
    
    override this.leastUpperBound (x: Set<LV>) (y: Set<LV>) = Set.union x y
    
    override this.leastElement() = Set.empty
    
    override this.initialElement(_: AnnotatedGraph) = Set.empty
    
    override this.genAndKill((_, action, _): Edge) =
        match action with
        | Allocate(AST.Integer name) ->
            let kill = Set.empty
            let gen = Set.singleton (Variable name)
            (kill, gen)
        | Allocate(AST.ArrayDecl(name, _)) ->
            let kill = Set.empty
            let gen = Set.singleton (Array name)
            (kill, gen)
        | Allocate(AST.Struct(name, fields)) ->
            let kill = Set.empty
            let gen = List.map (fun field -> Field(name,field)) fields |> Set.ofList
            (kill, gen)
        | Free(_) -> (Set.empty, Set.empty)
        | Assign((AST.Array(x,index), expr)) ->
            let kill = Set.union (arithmeticFreeVariables index) (arithmeticFreeVariables expr)
            let gen = Set.empty
            (kill, gen)
        | Assign(x, expr) ->
            let kill = arithmeticFreeVariables expr
            let gen = Set.singleton (AmalgamatedLocation.fromLocation x)
            (kill, gen)
        | AssignLiteral(s, exprs) ->
            List.fold (fun (kill, gen) (field, expr) ->
                let singleton = Set.singleton (Field (s, field))
                let fv = arithmeticFreeVariables expr
                (Set.union kill fv, Set.union gen singleton)
            ) (Set.empty, Set.empty) exprs
        | Condition(expr) ->
            let kill = booleanFreeVariables expr
            let gen = Set.empty
            (kill, gen)
        | Read(AST.Array(x, index)) ->
            let kill = arithmeticFreeVariables index
            let gen = Set.empty
            (kill, gen)
        | Read(x) ->
            let kill = Set.empty
            let gen = Set.singleton (AmalgamatedLocation.fromLocation x)
            (kill, gen)
        | Write(expr) ->
            let kill = arithmeticFreeVariables expr
            let gen = Set.empty
            (kill, gen)