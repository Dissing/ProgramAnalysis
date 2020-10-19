namespace BitVector 
open System.Globalization
open AnalysisDefinition
open FrontEnd.ProgramGraph
open FrontEnd.AST

module LiveVariables =
    
    type LVVariable =
        | LiveAssign of Ident
        | LiveArray of Ident
        | LiveField of (Ident * Ident)
    
    type LV() =
        
        let rec killAction (locations: List<Location>) (state: Set<LVVariable>) =
            match locations with
            | head::tail -> match head with
                            | Identifier id -> killAction tail (state.Remove (LiveAssign(id)))
                            | Field (strct, id) -> killAction tail (state.Remove (LiveField(strct, id)))
                            | _ -> killAction tail state
            | [] -> state
            
        let rec extractLocationsArithmeticExpr (expr: ArithmeticExpr) (locations: List<Location>)=
            match expr with
            | Loc l -> match l with
                       | Location.Array (id, expr) -> extractLocationsArithmeticExpr expr (Location.Array(id, expr)::locations)
                       | _ -> l::locations
            | IntLiteral _ -> locations
            | ArithmeticUnary (_, a) -> extractLocationsArithmeticExpr a locations
            | ArithmeticBinary (a1, _, a2) -> extractLocationsArithmeticExpr a2 (extractLocationsArithmeticExpr a1 locations)
            
        let rec extractLocationsBooleanExpr (expr: BooleanExpr) (locations : List<Location>) =
            match expr with
            | BooleanLiteral _ -> locations
            | BooleanUnary (_, b) -> extractLocationsBooleanExpr b locations
            | Comparison (a1, _, a2) ->  extractLocationsArithmeticExpr a2 (extractLocationsArithmeticExpr a1 locations)
            | BooleanBinary (b1, _, b2) -> extractLocationsBooleanExpr b2 (extractLocationsBooleanExpr b1 locations)
                
        
        let rec genAction (locations : List<Location>) (state: Set<LVVariable>) =
            match locations with
            | head::tail -> match head with
                            | Identifier id -> genAction tail (state.Add (LiveAssign(id)))
                            | Location.Array (id, _) -> genAction tail (state.Add (LiveArray(id)))
                            | Field (strct, id) -> genAction tail (state.Add (LiveField(strct, id))) 
            | [] -> state
       
        
        let extractLocArray (loc : Location) =
            match loc with
            | Location.Array (id, expr) -> extractLocationsArithmeticExpr expr []
            | _ -> []
        
        let mutable states: (Set<LVVariable>)[] = [||]
        
        let mutable nodeLength = 0
        
        let performGen (state: Set<LVVariable>) (genLoc: List<Location>) (nodeIn: Node) = 
            let updatedSet = genAction genLoc state
               
            let diff = Set.difference updatedSet states.[nodeIn]
                
            if diff.Count > 0 then
               states.[nodeIn] <- Set.union states.[nodeIn] diff
               [nodeIn]
            else
                []                    
        
        let createLVString var =
            match var with
            | LiveAssign ident -> sprintf "%s" ident
            | LiveArray ident -> sprintf "%s" ident
            | LiveField (strct, ident) -> sprintf "%s.%s" strct ident
        
        let createSolutionForNode (var: Set<LVVariable>) =
            Set.fold (fun acc ele -> (createLVString ele)::acc) [] var
        
        let rec createSolutionRec (states: (Set<LVVariable>[])) (index: int) (result: List<List<string>>) =
            match index with
            | 0 -> ((createSolutionForNode states.[0])::result)
            | i -> createSolutionRec states (index-1) ((createSolutionForNode states.[i])::result)
        
        member this.getSolution() = (this :> IAlgorithm).getSolution
        
        interface IAlgorithm with
        
            member this.isReverse = true
            
            member this.initialise graph declaration =
                let nodes = fst graph
                
                states <- Array.create (nodes.Length) (Set.empty)
                nodeLength <- nodes.Length
                [nodes.Length-1]
            
            
            member this.updateAssign (nodeIn, assign, nodeOut) =
                let (loc, expr) = assign
                
                let newSet = killAction [loc] states.[nodeOut]
                let extrList = extractLocationsArithmeticExpr expr (extractLocArray loc)
                
                performGen newSet extrList nodeIn                
                    
        
            member this.updateAssignLiteral (nodeIn, assignLiteral, nodeOut) =
                let (strct, assigns) = assignLiteral
                
                let rec extractUpdates (strct: Ident) (literals: List<Ident * ArithmeticExpr>) (killLocations: List<Location>) (genLocations: List<Location>) =
                    match literals with
                    | (id, expr)::tail -> extractUpdates strct tail (Field(strct, id)::killLocations) (extractLocationsArithmeticExpr expr genLocations)
                    | [] -> (killLocations, genLocations)
                
                let (killLoc, genLoc) = extractUpdates strct assigns [] []
                
                let newSet = killAction killLoc states.[nodeOut]
                
                performGen newSet genLoc nodeIn                
                
            member this.updateCondition (nodeIn, bExpr, nodeOut) = 
                
                let genLoc = extractLocationsBooleanExpr bExpr []
                
                performGen states.[nodeOut] genLoc nodeIn                
                    
            member this.updateRead (nodeIn, read, nodeOut) =
                let newSet = killAction [read] states.[nodeOut]
                     
                performGen newSet (extractLocArray read) nodeIn                
                
        
            member this.updateWrite (nodeIn, aExpr, nodeOut) =
                
                let genLoc = extractLocationsArithmeticExpr aExpr []
                
                performGen states.[nodeOut] genLoc nodeIn                
            
            member this.updateFree (nodeIn, _, nodeOut) =
                performGen states.[nodeOut] [] nodeIn                

            member this.updateAllocate (nodeIn, allocate, nodeOut) =
                
                let rec allocateStruct (strct: Ident) (literals: List<Ident>) (set: Set<LVVariable>) =
                    match literals with
                    | [] -> set
                    | head::tail -> allocateStruct strct tail (set.Remove (LiveField(strct, head)))

                let allocateVariable (allocate: Declaration) (set: Set<LVVariable>) =
                    match allocate with
                    | Integer ident -> set.Remove (LiveAssign(ident))
                    | ArrayDecl (ident, _) -> set.Remove (LiveArray(ident))
                    | Struct (strct, idents) -> allocateStruct strct idents set

                let newSet = allocateVariable allocate states.[nodeOut]

                performGen newSet [] nodeIn 

            member this.printSolution =
                "Not implemented"
                
            member this.getSolution =
                createSolutionRec states (nodeLength-1) []
    
                

