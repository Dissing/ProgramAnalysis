namespace Analysis.Analyses

open System.Net
open Analysis
open FrontEnd
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
   
    let plusSign pair = match pair with
                        | (x,y) when x = y  -> Set.ofList [x]
                        | (Sign.Zero, x) -> Set.ofList [x]
                        | (x, Sign.Zero) -> Set.ofList [x]
                        | _ -> Set.ofList [Sign.Plus; Sign.Minus; Sign.Zero]
    
    let minusSign pair = match pair with
                         | (x, Sign.Minus)  -> plusSign (x,Sign.Plus)
                         | (x, Sign.Zero) -> Set.ofList [x]
                         | (Sign.Plus, Sign.Plus) -> Set.ofList [Sign.Plus; Sign.Minus; Sign.Zero]
                         | (_, _) -> Set.ofList [Sign.Minus]
                         
    let negateSign sign = minusSign (Sign.Zero, sign)
    
    let multiplySign pair = match pair with
                            | (_, Sign.Zero) | (Sign.Zero, _)  -> Set.ofList [Sign.Zero]
                            | (Sign.Plus, Sign.Minus) -> Set.ofList [Sign.Minus]
                            | (Sign.Minus, Sign.Plus) -> Set.ofList [Sign.Minus]
                            | _ -> Set.ofList [Sign.Plus]
    
    let divisionSign pair = match pair with
                            | (_, Sign.Zero) -> Set.empty
                            | (x, Sign.Plus) -> Set.ofList [x; Sign.Zero]
                            | (x,Sign.Minus) -> (negateSign x).Add Sign.Zero

                            
    let moduloSign pair = match pair with
                            | (_, Sign.Zero)  -> Set.empty
                            | (Sign.Zero, _) -> Set.ofList [Sign.Zero]
                            | (Sign.Minus, _) -> Set.ofList [Sign.Minus; Sign.Zero]
                            | _ -> Set.ofList [Sign.Zero; Sign.Plus]
            
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
                          | AST.Modulo -> let s = moduloSign pair
                                          if Set.isEmpty s then
                                              Set.empty
                                          else
                                              handlePairs tail opr (Set.union signs s)
    
    let handleOpr opr signs1 signs2 =
        if Set.isEmpty signs1 || Set.isEmpty signs2 then
            Set.empty
        else
            let pairs = List.allPairs (Set.toList signs1) (Set.toList signs2)
            handlePairs pairs opr Set.empty            
        
    let rec determineArithmeticSigns (map: DS) expr =
        match expr with
        | AST.Loc l ->
                     let x = (AmalgamatedLocation.fromLocation l)
                     match l with
                     | AST.Array (_, arrExpr) ->
                                        let arrSigns = determineArithmeticSigns map arrExpr
                                        if Set.isEmpty (Set.intersect arrSigns (Set.ofList [Sign.Plus; Sign.Zero])) then
                                            Set.empty
                                        else
                                            map.[x]
                     | _ -> map.[x]
        | AST.IntLiteral i -> match i with
                              | 0 -> Set.empty.Add(Sign.Zero)
                              | i when i < 0  -> Set.empty.Add(Sign.Minus)
                              | _ -> Set.empty.Add(Sign.Plus)
        | AST.ArithmeticUnary (unary, expr) -> match unary with
                                               | AST.Negation -> Set.fold (fun acc ele -> Set.union acc (negateSign ele)) Set.empty (determineArithmeticSigns map expr)
        | AST.ArithmeticBinary (expr1, opr, expr2) -> handleOpr opr (determineArithmeticSigns map expr1) (determineArithmeticSigns map expr2)
     
    override this.name = "Sign Detection"
    
    override this.isReverseAnalysis () = false
    
    override this.lessThanOrEqual (x: DS) (y: DS) =
        if Map.isEmpty x then
            Map.isEmpty y
        elif Map.isEmpty y then
            Map.isEmpty x
        else
            locations |> Set.forall (fun loc -> Set.isSubset x.[loc] y.[loc])
    
    override this.leastUpperBound (x: DS) (y: DS) =
        if Map.isEmpty x then
            y
        elif Map.isEmpty y then
            x
        else
            locations |> Set.toSeq |> Seq.map (fun loc -> (loc, Set.union x.[loc] y.[loc])) |> Map.ofSeq
    
    override this.leastElement() = 
        Map.empty
        //locations |> Set.toSeq |> Seq.map (fun loc -> (loc, Set.empty)) |> Map.ofSeq

    override this.initialElement ((annotation, _): AnnotatedGraph) =
        locations |> Set.toSeq |> Seq.map (fun loc -> (loc, Set.ofList [Sign.Zero; Sign.Minus; Sign.Plus])) |> Map.ofSeq
    override this.analyseEdge ((_, action, _): Edge) (labeling: DS) =
        if Map.isEmpty labeling then
            Map.empty
        else
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
                labeling.Add(x, (Set.ofList [Sign.Zero; Sign.Minus; Sign.Plus]))
            | Free(AST.ArrayDecl (i, _)) -> 
                let x = AmalgamatedLocation.Array(i)
                labeling.Add(x, (Set.ofList [Sign.Zero; Sign.Minus; Sign.Plus]))
            | Free(AST.Struct (name, fields)) -> 
                let x = List.map (fun field -> AmalgamatedLocation.Field(name,field)) fields 
                updateMap x labeling (Set.ofList [Sign.Zero; Sign.Plus; Sign.Minus])
            | Assign((AST.Array(arr,indexExpr), expr)) ->
                let x = AmalgamatedLocation.Array arr
                let index = determineArithmeticSigns labeling indexExpr
                if Set.isEmpty (Set.intersect index (Set.ofList [Sign.Plus; Sign.Zero])) then
                    Map.empty
                else
                    let signs = determineArithmeticSigns labeling expr
                    if Set.isEmpty signs then
                        Map.empty
                    else
                        labeling.Add(x, (Set.union labeling.[x] (determineArithmeticSigns labeling expr))) 
            | Assign((var, expr)) ->
                let x = AmalgamatedLocation.fromLocation var
                let signs = determineArithmeticSigns labeling expr
                if Set.isEmpty signs then
                    Map.empty
                else
                   labeling.Add(x, signs) 
            | AssignLiteral(s, exprs) ->
                let rec updateLiterals (ds: DS) s  exps =
                    match exps with
                    | [] -> ds
                    | (field, expr)::tail ->
                        let x = AmalgamatedLocation.Field(s, field)
                        let signs = determineArithmeticSigns ds expr
                        if Set.isEmpty signs then
                            Map.empty
                        else
                            updateLiterals (ds.Add(x, signs)) s tail
                updateLiterals labeling s exprs
            | Condition(_) -> labeling
            | Read (AST.Array(arr,indexExpr)) ->
                let x = AmalgamatedLocation.Array arr
                let index = determineArithmeticSigns labeling indexExpr
                if Set.isEmpty (Set.intersect index (Set.ofList [Sign.Plus; Sign.Zero])) then
                    Map.empty
                else 
                    labeling.Add (x, Set.ofList ([Sign.Zero; Sign.Plus; Sign.Minus]))
            | Read loc ->
                let x = AmalgamatedLocation.fromLocation loc
                labeling.Add (x, Set.ofList ([Sign.Zero; Sign.Plus; Sign.Minus]))
            | Write(_) -> labeling
