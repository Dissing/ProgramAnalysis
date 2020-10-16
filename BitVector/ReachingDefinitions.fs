namespace BitVector 
open AnalysisDefinition
open FrontEnd.ProgramGraph
open FrontEnd.AST
open System.Text
module ReachingDefinitions =
    
    type RDVarState =
        | UnmodifiedStandard of Ident
        | UnmodifiedArray of Ident
        | UnmodifiedField of Ident * Ident
        | ModifiedStandard of Ident * Node * Node
        | ModifiedArray of Ident * Node * Node
        | ModifiedField of (Ident * Ident) * Node * Node
    
    type RD() =
        
        let shouldNotKill (location: Location) (state: RDVarState) =
            match state with
            | UnmodifiedStandard id -> match location with
                                        | Identifier iden -> if id = iden then
                                                                 false
                                                             else
                                                                 true
                                        | _ -> true
            | UnmodifiedField (s, f) -> match location with
                                        | Location.Field (strct, field) -> if s = strct && f = field then
                                                                               false
                                                                           else
                                                                               true
                                        | _ -> true
            | ModifiedStandard (id, _, _) -> match location with
                                             | Identifier iden -> if id = iden then
                                                                      false
                                                                  else
                                                                      true
                                             | _ -> true
            | ModifiedField ((s, f), _, _) -> match location with
                                              | Location.Field (strct, field) -> if s = strct && f = field then
                                                                                     false
                                                                                 else
                                                                                     true
                                              | _ -> true
            | _ -> true
           
        let genAction (states: Set<RDVarState>) (location : Location) (nodeIn : Node) (nodeOut: Node) =
            match location with
            | Identifier id -> ModifiedStandard(id, nodeIn, nodeOut)
            | Location.Array (id, _) -> ModifiedArray(id, nodeIn, nodeOut)
            | Location.Field (strct, id) -> ModifiedField((strct, id), nodeIn, nodeOut)
        
        
        let createFieldString (strct: string) (id: string) = strct + "." + id
        
        let locationToString (loc: Location) =
            match loc with
            | Identifier id -> id
            | Location.Array (id, _) -> id
            | Location.Field (strct, id) -> createFieldString strct id 
        
        let performKillGenAction (mapIn: Map<string, Set<RDVarState>>) (location : Location) (nodeIn : Node) (nodeOut: Node) =
                let lookup = locationToString location
                let inSet = mapIn.[lookup]
                let killedSet = Set.filter (shouldNotKill location) inSet
                let newSet = killedSet.Add (genAction killedSet location nodeIn nodeOut)
                 
                mapIn.Add (lookup, newSet)
            
        let rec updateMapWithDifferences (mapIn: Map<string, Set<RDVarState>>) (mapOut: Map<string, Set<RDVarState>>) (keys: List<string>) (modified: bool) =
            match keys with
            | key::tail -> let diff = Set.difference mapIn.[key] mapOut.[key]
                           if diff.Count > 0 then
                               updateMapWithDifferences mapIn (mapOut.Add (key, (Set.union mapOut.[key] diff))) tail true                           
                           else
                               updateMapWithDifferences mapIn mapOut tail modified
            | [] -> (mapOut, modified)
            
        
        let createRDString var =
            match var with
            | UnmodifiedStandard ident -> sprintf "(%s, ?, q0)" ident
            | UnmodifiedArray ident -> sprintf "(%s, ?, q0)" ident
            | UnmodifiedField (strct, ident) -> sprintf "(%s.%s, ?, q0)" strct ident
            | ModifiedStandard (ident, nin, nout) -> sprintf "(%s, q%d, q%d)" ident nin nout
            | ModifiedArray (ident, nin, nout) -> sprintf "(%s, q%d, q%d)" ident nin nout
            | ModifiedField ((strct, ident), nin, nout) -> sprintf "(%s.%s, q%d, q%d)" strct ident nin nout
            
        let rec createRDStrings (rdStrings: List<RDVarState>) (results: List<string>) =
            match rdStrings with
            | [] -> results
            | head::tail -> createRDStrings tail ((createRDString head)::results)
        
        let rec createNodeSolutionRec (keys: List<string>) (states: Map<string, Set<RDVarState>>) (results: List<string>) =
            match keys with
            | [] -> results
            | head :: tail -> createNodeSolutionRec tail states (results@( createRDStrings (Set.toList (states.[head])) []))
       
        let rec createSolutionRec (st: (Map<string, Set<RDVarState>>)[]) (index: int) (keys: List<string>) (result: List<List<string>>) =
            match index with
            | 0 -> (createNodeSolutionRec keys st.[0] [])::result
            | i -> createSolutionRec st (index-1) keys ((createNodeSolutionRec keys st.[i] [])::result)
        
         
        let mutable states: (Map<string, Set<RDVarState>>)[] = [||]
        let mutable keys: List<string> = []
        
        let mutable nodeLength: int = 0
        
        
        member this.getSolution() = (this :> IAlgorithm).getSolution
        
        interface IAlgorithm with
        
            member this.isReverse = false
            
            member this.initialise graph declaration =
                let nodes = fst graph
                
                nodeLength <- nodes.Length
                
                states <- Array.create (nodeLength) (Map.empty)
                
                let rec parseFields (id: Ident) (fields: List<FieldDeclaraction>) (map: Map<string, Set<RDVarState>>) (keys: List<string>) =
                    match fields with
                    | head :: tail -> let key = createFieldString id (snd head)
                                      parseFields id tail (map.Add (key, Set([UnmodifiedField((id, (snd head)))]))) (key::keys)
                    | [] -> (map, keys)
                let rec parseDeclaration (dList: List<Declaration>) (map: Map<string, Set<RDVarState>>) (keys: List<string>) =
                    match dList with
                    | head :: tail -> match head with
                                       | Integer id -> parseDeclaration tail (map.Add (id, Set([UnmodifiedStandard(id)]))) (id::keys)
                                       | ArrayDecl (id, _) -> parseDeclaration tail (map.Add (id, Set([UnmodifiedArray(id)]))) (id::keys)
                                       | Struct (id, fields) -> let (newMap, newKeys) = parseFields id fields map keys
                                                                parseDeclaration tail newMap newKeys
                    | [] -> (map, keys)
                                
                let (startMap, newKeys) = parseDeclaration declaration Map.empty []
                keys <- newKeys
                
                for i in 1 .. nodeLength - 1 do
                    states.[i] <- Map.ofList([for k in keys do yield (k, Set.empty)])
                
                states.[0] <- startMap
                [nodes.[0]]
            
            
            member this.updateAssign (nodeIn, assign, nodeOut) =
                let location = fst assign
                
                let newMap = performKillGenAction states.[nodeIn] location nodeIn nodeOut                
                
                let (mapUpdated, modified) = updateMapWithDifferences newMap states.[nodeOut] keys false
                
                if modified = true then
                    states.[nodeOut] <- mapUpdated
                    [nodeOut]
                else
                    []                    
                    
        
            member this.updateAssignLiteral (nodeIn, assignLiteral, nodeOut) =
                let rec recursiveKillGen (mapIn: Map<string, Set<RDVarState>>) (strct: string) (assigns: List<Ident*ArithmeticExpr>) (nodeIn: Node) (nodeOut: Node) =
                    match assigns with
                    | (id,_)::tail -> let location = Field(strct, id)
                                      let newMap = performKillGenAction mapIn location nodeIn nodeOut
                                      recursiveKillGen mapIn strct tail nodeIn nodeOut
                                                      
                    | [] -> mapIn
                
                let (strct, assigns) = assignLiteral
                let newMap = recursiveKillGen states.[nodeIn] strct assigns nodeIn nodeOut                
                
                let (mapUpdated, modified) = updateMapWithDifferences newMap states.[nodeOut] keys false
                
                if modified = true then
                    states.[nodeOut] <- mapUpdated
                    [nodeOut]
                else
                    []                    
                
            member this.updateCondition (nodeIn, _, nodeOut) = 
                let (mapUpdated, modified) = updateMapWithDifferences states.[nodeIn] states.[nodeOut] keys false
                                
                if modified = true then
                    states.[nodeOut] <- mapUpdated
                    [nodeOut]
                else
                    []
                    
            member this.updateRead (nodeIn, read, nodeOut) =
                let newMap = performKillGenAction states.[nodeIn] read nodeIn nodeOut                
                
                let (mapUpdated, modified) = updateMapWithDifferences newMap states.[nodeOut] keys false
                
                if modified = true then
                    states.[nodeOut] <- mapUpdated
                    [nodeOut]
                else
                    []               
        
            member this.updateWrite (nodeIn, _, nodeOut) =
                let (mapUpdated, modified) = updateMapWithDifferences states.[nodeIn] states.[nodeOut] keys false
                                
                if modified = true then
                    states.[nodeOut] <- mapUpdated
                    [nodeOut]
                else
                    []
            member this.printSolution =
                "Not implemented"
            
            member this.getSolution =
                createSolutionRec states (nodeLength-1) keys []
                
                
                
