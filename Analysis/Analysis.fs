namespace Analysis
open System
open FrontEnd
open FrontEnd.ProgramGraph

type IWorklist =
    
    abstract member extract: unit -> option<Node * IWorklist>
    
    abstract member insert: Node -> IWorklist
    
    abstract member name: String
    
    
[<AbstractClass>]
type IAnalysis<'L when 'L : comparison>() =
    
    abstract member name: String
    
    abstract member isReverseAnalysis: unit -> bool
    
    abstract member lessThanOrEqual: 'L -> 'L -> bool
    
    abstract member leastUpperBound: 'L -> 'L -> 'L
    
    abstract member leastElement: unit -> 'L
    
    abstract member analyseEdge: Edge -> 'L -> 'L
    
    abstract member initialElement: AnnotatedGraph -> 'L
    
    member this.analyse ((annotation, pg): AnnotatedGraph) (worklist: IWorklist) =
        
        let (nodes, edges) =
            if this.isReverseAnalysis() then
                ProgramGraph.reverse pg
            else
                pg
 
        let initialLabelling = Map.ofList (List.map (fun (q: Node) -> (q, this.leastElement())) nodes)
        
        let worklist = List.fold (fun (w: IWorklist) (q: Node) -> w.insert(q)) worklist nodes
        
        let initialLabel = this.initialElement (annotation, (nodes, edges))
        
        let initialLabelling = initialLabelling.Add(nodes.Head, initialLabel)
        
        let rec work (sol: Map<Node, 'L>) (worklist: IWorklist) (steps: int) =
            match worklist.extract() with
            | None -> (sol, steps)
            | Some((q,w)) ->
                let (sol'',w'') =
                    List.filter (fun ((src, _, _): Edge) -> src = q) edges
                    |> List.fold (fun ((sol',w'): Map<Node, 'L> * IWorklist) ((src,action,dst): Edge) ->
                            let label = this.analyseEdge (src,action,dst) (sol'.[src])
                            if not (this.lessThanOrEqual label (sol'.[dst])) then
                                (sol'.Add(dst, (this.leastUpperBound label (sol'.[dst]))),
                                 w'.insert dst)
                            else
                                (sol', w')
                        ) (sol,w)
                work sol'' w'' (steps + 1)
                
        work initialLabelling worklist 0

[<AbstractClass>]
type IBitVector<'D when 'D : comparison>() =
    inherit IAnalysis<Set<'D>>()
    
    abstract member killAndGen: Edge -> (Set<'D> * Set<'D>)
    
    override this.analyseEdge (edge: Edge) (x: Set<'D>) =
        let (killSet, genSet) = this.killAndGen edge
        Set.union (Set.difference x killSet) genSet
       

type AmalgamatedLocation =
    | Variable of AST.Ident
    | Array of AST.Ident
    | Field of AST.Ident * AST.Ident
    static member fromLocation (loc: AST.Location) =
        match loc with
        | AST.Variable(i) -> Variable(i)
        | AST.Array(i, _) -> Array(i)
        | AST.Field(s,f) -> Field(s,f)
        
    static member fromDeclaration (decl: AST.Declaration) =
        match decl with
        | AST.VarDecl i -> Set.singleton (Variable i)
        | AST.ArrayDecl (i,_) -> Set.singleton (Array i)
        | AST.RecordDecl (i, fs) -> Set.ofList fs |> Set.map (fun f -> Field(i, f))
        
    static member fromAnnotation (annotation: AST.DeclarationInfo) =
        annotation |> Map.toSeq |>
        Seq.map (fun (_,d) -> AmalgamatedLocation.fromDeclaration d) |>
        Seq.fold (fun s x -> Set.union s x) Set.empty
        
module Expressions =
    let arithmeticFreeVariables (expr: AST.ArithmeticExpr) = 
        Set.map AmalgamatedLocation.fromLocation (AST.arithmeticFreeVariables expr)
    
    let booleanFreeVariables (expr: AST.BooleanExpr) = 
        Set.map AmalgamatedLocation.fromLocation (AST.booleanFreeVariables expr)

    let rec nonTrivialArithmeticExpressions (expr: AST.ArithmeticExpr) =
        match expr with
        | AST.Loc(AST.Array(_, index)) ->
            nonTrivialArithmeticExpressions index
        | AST.Loc _ ->
            Set.empty
        | AST.IntLiteral _ ->
            Set.empty
        | AST.ArithmeticUnary(_, inner) ->
            Set.union
                (Set.singleton expr)
                (nonTrivialArithmeticExpressions inner)
        | AST.ArithmeticBinary(left, _, right) ->
            Set.union
                (Set.singleton expr)
                (Set.union
                    (nonTrivialArithmeticExpressions left)
                    (nonTrivialArithmeticExpressions right))

    let rec nonTrivialArithmeticExpressionsInBoolean (expr: AST.BooleanExpr) =
        match expr with
        | AST.BooleanLiteral _ ->
            Set.empty
        | AST.BooleanUnary (_, inner) ->
            nonTrivialArithmeticExpressionsInBoolean inner
        | AST.BooleanBinary (left, _, right) ->
            Set.union
                (nonTrivialArithmeticExpressionsInBoolean left)
                (nonTrivialArithmeticExpressionsInBoolean right)
        | AST.Comparison (left, _, right) ->
            Set.union
                (nonTrivialArithmeticExpressions left)
                (nonTrivialArithmeticExpressions right)

    let allArithmeticExpressionsInAction (action: Action) =
        match action with
        | Allocate(_) | Free(_) ->
            Set.empty
        | Assign((AST.Array(_, index)), expr) ->
            Set.union
                (nonTrivialArithmeticExpressions index)
                (nonTrivialArithmeticExpressions expr)
        | Assign(_, expr) ->
            nonTrivialArithmeticExpressions expr
        | AssignLiteral(_, exprs) ->
            exprs |> List.fold (fun s (_, expr) ->
                Set.union s (nonTrivialArithmeticExpressions expr)) Set.empty
        | Condition(expr) ->
            nonTrivialArithmeticExpressionsInBoolean expr
        | Read(AST.Array(_, index)) ->
            nonTrivialArithmeticExpressions index
        | Read(_) ->
            Set.empty
        | Write(expr) ->
            nonTrivialArithmeticExpressions expr
            
    let allArithmeticExpressionsInGraph (graph: AnnotatedGraph) =
        let (_, (_, edges)) = graph
        edges |> List.fold (fun s (_, action, _) ->
            Set.union s (allArithmeticExpressionsInAction action)) Set.empty
        
    let expressionsContainingLocation (exprs: Set<AST.ArithmeticExpr>) (location: AmalgamatedLocation) =
        exprs |> Set.filter (fun expr ->
            (arithmeticFreeVariables expr).Contains location)
        
    let expressionsNotContainingLocation (exprs: Set<AST.ArithmeticExpr>) (location: AmalgamatedLocation) =
            exprs |> Set.filter (fun expr ->
                not ((arithmeticFreeVariables expr).Contains location))