namespace Analysis.Analyses

open Analysis
open Analysis.Expressions
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
    
    override this.killAndGen((_, action, _): Edge) =
        match action with
        | Allocate(AST.VarDecl name) ->
            let kill = Set.singleton (Variable name)
            let gen = Set.empty
            (kill, gen)
        | Allocate(AST.ArrayDecl(name, _)) ->
            let kill = Set.singleton (Array name)
            let gen = Set.empty
            (kill, gen)
        | Allocate(AST.RecordDecl(name, fields)) ->
            let kill = List.map (fun field -> Field(name,field)) fields |> Set.ofList
            let gen = Set.empty
            (kill, gen)
        | Free(_) -> (Set.empty, Set.empty)
        | Assign(AST.Array(x,index), expr) ->
            let kill = Set.empty
            let gen = Set.union (arithmeticFreeVariables index) (arithmeticFreeVariables expr)
            (kill, gen)
        | Assign(x, expr) ->
            let kill = Set.singleton (AmalgamatedLocation.fromLocation x)
            let gen = arithmeticFreeVariables expr
            (kill, gen)
        | RecordAssign(s, exprs) ->
            List.fold (fun (kill, gen) (field, expr) ->
                let singleton = Set.singleton (Field (s, field))
                let fv = arithmeticFreeVariables expr
                (Set.union kill singleton, Set.union gen fv)
            ) (Set.empty, Set.empty) exprs
        | Condition(expr) ->
            let kill = Set.empty
            let gen = booleanFreeVariables expr
            (kill, gen)
        | Read(AST.Array(x, index)) ->
            let kill = Set.empty
            let gen = arithmeticFreeVariables index
            (kill, gen)
        | Read(x) ->
            let kill = Set.singleton (AmalgamatedLocation.fromLocation x)
            let gen = Set.empty
            (kill, gen)
        | Write(expr) ->
            let kill = Set.empty
            let gen = arithmeticFreeVariables expr
            (kill, gen)