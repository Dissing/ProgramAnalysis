namespace Analysis.Analyses

open System
open Analysis
open Analysis.FreeVariables
open FrontEnd
open FrontEnd.ProgramGraph

type RD = AmalgamatedLocation * Option<Node> * Node

type ReachingDefinitionsAnalysis(edges: List<Edge>) =
    inherit IBitVector<RD>()
    
    override this.name = "Reaching Definitions"
    
    override this.isReverseAnalysis() = false
    
    override this.lessThanOrEqual (x: Set<RD>) (y: Set<RD>) = Set.isSubset x y
        
    override this.leastUpperBound (x: Set<RD>) (y: Set<RD>) = Set.union x y
        
    override this.leastElement() = Set.empty
        
    override this.initialElement(_: AnnotatedGraph) = Set.empty
    
    //Yeah this is not particularly efficient, but fitting this into the general BitVector gen and kill sets is difficult.
    //One could probably do a bit better by only including edges which actually writes something to the location
    member this.exhaustiveKill(loc: AmalgamatedLocation) =
        let possibleEdges = List.map (fun (src, _, dst) -> (loc, Some(src), dst)) edges |> Set.ofList
        let undeclared = List.map (fun (_, _, dst) -> (loc, None, dst)) edges |> Set.ofList
        Set.union possibleEdges undeclared

    override this.killAndGen((src, action, dst): Edge) =
        match action with
        | Allocate(AST.Integer name) ->
            let kill = Set.empty
            let gen = Set.singleton (Variable name, None, dst)
            (kill, gen)
        | Allocate(AST.ArrayDecl(name, _)) ->
            let kill = Set.empty
            let gen = Set.singleton (Array name, None, dst)
            (kill, gen)
        | Allocate(AST.Struct(name, fields)) ->
            let kill = Set.empty
            let gen = List.map (fun field -> (Field(name,field), None, dst)) fields |> Set.ofList
            (kill, gen)
        | Free(AST.Integer name) ->
            let kill = this.exhaustiveKill (Variable name)
            let gen = Set.empty
            (kill, gen)
        | Free(AST.ArrayDecl(name, _)) ->
            let kill = this.exhaustiveKill (Array name)
            let gen = Set.empty
            (kill, gen)
        | Free(AST.Struct(name, fields)) ->
            let kill = List.map (fun field -> this.exhaustiveKill (Field(name, field))) fields |> List.reduce Set.union
            let gen = Set.empty
            (kill, gen)
        | Assign((AST.Array(x,_), _)) ->
            let kill = Set.empty
            let gen = Set.singleton (Array x, Some(src), dst)
            (kill, gen)
        | Assign(x, _) ->
            let loc = AmalgamatedLocation.fromLocation x
            let kill = this.exhaustiveKill loc
            let gen = Set.singleton (loc, Some(src), dst)
            (kill, gen)
        | AssignLiteral(name, exprs) ->
            let kill = List.map (fun (field,_) -> this.exhaustiveKill (Field(name, field))) exprs |> List.reduce Set.union
            let gen = List.map (fun (field,_) -> (Field(name, field), Some(src), dst)) exprs |> Set.ofList
            (kill, gen)
        | Condition(_) ->
            (Set.empty, Set.empty)
        | Read(AST.Array(x, _)) ->
            let kill = Set.empty
            let gen = Set.singleton (Array x, Some(src), dst)
            (kill, gen)
        | Read(x) ->
            let loc = AmalgamatedLocation.fromLocation x
            let kill = this.exhaustiveKill loc
            let gen = Set.singleton (loc, Some(src), dst)
            (kill, gen)
        | Write(_) ->
            (Set.empty, Set.empty)