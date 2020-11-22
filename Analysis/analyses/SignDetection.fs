namespace Analysis.Analyses

open System.Net
open Analysis
open FrontEnd
open FrontEnd.AST
open FrontEnd.ProgramGraph

type Sign =
    | Plus
    | Minus
    | Zero
    

type DS = Map<AmalgamatedLocation,Set<Sign>>

type SignDetectionAnalysis(graph: AnnotatedGraph) =
    inherit IAnalysis<DS>()
    
    let locations =
        let (annotation, _) = graph
        annotation |> Map.toSeq |> Seq.fold (fun s (k,v) -> Set.union s (AmalgamatedLocation.fromDeclaration v)) Set.empty |> Set.ofSeq
    
    let rec updateMap (list: List<AmalgamatedLocation>) (map: DS) entry =
        match list with
        | [] -> map
        | head::tail -> updateMap tail (map.Add(head, entry)) entry
   
    let negateSign sign =
        match sign with
        | Sign.Zero -> Sign.Zero
        | Sign.Plus -> Sign.Minus
        | Sign.Minus -> Sign.Minus
        
    let plusSign pair = match pair with
                        | (x,y) when x = y  -> Set.ofList [x]
                        | (Sign.Zero, x) -> Set.ofList [x]
                        | (x, Sign.Zero) -> Set.ofList [x]
                        | _ -> Set.ofList [Sign.Plus; Sign.Minus; Sign.Zero]
    
    let minusSign pair = match pair with
                         | (x,y) when y = Sign.Minus  -> plusSign (x,y)
                         | (x, y) when y = Sign.Zero -> Set.ofList [x]
                         | (Sign.Plus, Sign.Plus) -> Set.ofList [Sign.Plus; Sign.Minus; Sign.Zero]
                         | (x, _) -> Set.ofList [x]
    
    let multiplySign pair = match pair with
                            | (x, y) when y = Sign.Zero || x = Sign.Zero  -> Set.ofList [Sign.Zero]
                            | (Sign.Plus, Sign.Minus) -> Set.ofList [Sign.Minus]
                            | (Sign.Minus, Sign.Plus) -> Set.ofList [Sign.Minus]
                            | _ -> Set.ofList [Sign.Plus]
    
    let divisionSign pair = match pair with
                            | (_, y) when y = Sign.Zero  -> Set.empty
                            | (x, Sign.Plus) -> Set.ofList [x]
                            | (x,Sign.Minus) -> Set.ofList [negateSign x]
                            | _ -> Set.empty // Never happens
                            
            
    let rec handlePairs pairs opr signs =
        match pairs with
        | [] -> signs
        | pair :: tail -> match opr with
                          | AST.Add -> handlePairs tail opr (Set.union signs (plusSign pair))
                          | AST.Subtract -> handlePairs tail opr (Set.union signs (minusSign pair))
                          | AST.Multiply -> handlePairs tail opr (Set.union signs (multiplySign pair))
                          | AST.Divide -> let s = divisionSign pair
                                          if Set.isEmpty s then
                                              Set.empty
                                          else
                                              handlePairs tail opr (Set.union signs s)
                          | AST.Modulo -> Set.empty
    
    let handleOpr opr signs1 signs2 =
        if Set.isEmpty signs1 || Set.isEmpty signs2 then
            Set.empty
        else
            let pairs = List.allPairs (Set.toList signs1) (Set.toList signs2)
            handlePairs pairs opr Set.empty            
        
    let rec determineArithmeticSigns (map: DS) expr =
        match expr with
        | AST.Loc l -> map.[(AmalgamatedLocation.fromLocation l)]
        | AST.IntLiteral i -> match i with
                              | 0 -> Set.empty.Add(Sign.Zero)
                              | i when i < 0  -> Set.empty.Add(Sign.Minus)
                              | _ -> Set.empty.Add(Sign.Plus)
        | AST.ArithmeticUnary (unary, expr) -> match unary with
                                               | AST.Negation -> Set.fold (fun acc ele -> acc.Add(negateSign ele)) Set.empty (determineArithmeticSigns map expr)
        | AST.ArithmeticBinary (expr1, opr, expr2) -> handleOpr opr (determineArithmeticSigns map expr1) (determineArithmeticSigns map expr2)
     
    override this.name = "Sign Detection"
    
    override this.isReverseAnalysis () = false
    
    override this.lessThanOrEqual (x: DS) (y: DS) =
        locations |> Set.forall (fun loc -> Set.isSubset x.[loc] y.[loc])
    
    override this.leastUpperBound (x: DS) (y: DS) =
        locations |> Set.toSeq |> Seq.map (fun loc -> (loc, Set.union x.[loc] y.[loc])) |> Map.ofSeq
    
    override this.leastElement() = 
        locations |> Set.toSeq |> Seq.map (fun loc -> (loc, Set.empty)) |> Map.ofSeq

    override this.initialElement ((annotation, _): AnnotatedGraph) =
        locations |> Set.toSeq |> Seq.map (fun loc -> (loc, Set.ofList [Sign.Zero; Sign.Minus; Sign.Plus])) |> Map.ofSeq
    override this.analyseEdge ((_, action, _): Edge) (labeling: DS) =
        match action with
        | Allocate(AST.Integer name) ->
            let x = Variable(name)
            labeling.Add(x, (Set.ofList [Sign.Zero]))
        | Allocate(AST.ArrayDecl (i, _)) -> 
            let x = AmalgamatedLocation.Array(i)
            labeling.Add(x, (Set.ofList [Sign.Zero]))
        | Allocate(AST.Struct (name, fields)) -> 
            let x = List.map (fun field -> AmalgamatedLocation.Field(name,field)) fields 
            updateMap x labeling (Set.ofList [Sign.Zero])
        | Free(AST.Integer name) ->
            let x = Variable(name)
            labeling.Add(x, (Set.ofList [Sign.Zero]))
        | Free(AST.ArrayDecl (i, _)) -> 
            let x = AmalgamatedLocation.Array(i)
            labeling.Add(x, (Set.ofList [Sign.Zero]))
        | Free(AST.Struct (name, fields)) -> 
            let x = List.map (fun field -> AmalgamatedLocation.Field(name,field)) fields 
            updateMap x labeling (Set.ofList [Sign.Zero])
        | Assign((AST.Array(arr,index), expr)) ->
            let x = AmalgamatedLocation.Array arr
            labeling.Add(x, (Set.union labeling.[x] (determineArithmeticSigns labeling expr))) 
        | Assign((var, expr)) ->
            let x = AmalgamatedLocation.fromLocation var
            labeling.Add(x, (determineArithmeticSigns labeling expr))
        | AssignLiteral(s, exprs) ->
            let rec updateLiterals (ds: DS) s  exps =
                match exps with
                | [] -> ds
                | (field, expr)::tail ->
                    let x = AmalgamatedLocation.Field(s, field)
                    updateLiterals (ds.Add(x, determineArithmeticSigns ds expr)) s tail
            updateLiterals labeling s exprs
        | Condition(_) -> labeling
        | Read loc ->
            let x = AmalgamatedLocation.fromLocation loc
            labeling.Add (x, Set.ofList ([Sign.Zero; Sign.Plus; Sign.Minus]))
        | Write(_) -> labeling
