namespace FrontEnd
open System.Globalization
open AnalysisDefinition
open ProgramGraph
open AST

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
            
        member this.states: (Map<string, Set<RDVarState>>)[] = [||]
        member this.keys: List<string> = []
        
        interface IAlgorithm with
        
            member this.isReverse = false
            
            member this.initialise graph declaration =
                let nodes = fst graph
                
                this.states = Array.create (nodes.Length) (Map.empty)
                
                let rec parseFields (id: Ident) (fields: List<FieldDeclaration>) (map: Map<string, Set<RDVarState>>) (keys: List<string>) =
                    match fields with
                    | head :: tail -> let key = createFieldString id (snd head)
                                      parseFields id tail (map.Add (key, Set([UnmodifiedField((id, (snd head)))]))) (key::keys)
                    | [] -> (map, keys)
                let rec parseDeclaration (dList: List<Declaration>) (map: Map<string, Set<RDVarState>>) (keys: List<string>) =
                    match dList with
                    | head :: tail -> match head with
                                       | Integer id -> parseDeclaration tail (map.Add (id, Set([UnmodifiedStandard(id)]))) (id::keys)
                                       | Array (id, _) -> parseDeclaration tail (map.Add (id, Set([UnmodifiedArray(id)]))) (id::keys)
                                       | Struct (id, fields) -> let (newMap, newKeys) = parseFields id fields map keys
                                                                parseDeclaration tail newMap newKeys
                    | [] -> (map, keys)
                
                (this.states.[0], this.keys) = parseDeclaration declaration Map.empty []
                
                [nodes.[0]]
            
            
            member this.updateAssign (nodeIn, assign, nodeOut) =
                let location = fst assign
                
                let newMap = performKillGenAction this.states.[nodeIn] location nodeIn nodeOut                
                
                let (mapUpdated, modified) = updateMapWithDifferences newMap this.states.[nodeOut] this.keys false
                
                if modified = true then
                    this.states.[nodeOut] = mapUpdated
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
                let newMap = recursiveKillGen this.states.[nodeIn] strct assigns nodeIn nodeOut                
                
                let (mapUpdated, modified) = updateMapWithDifferences newMap this.states.[nodeOut] this.keys false
                
                if modified = true then
                    this.states.[nodeOut] = mapUpdated
                    [nodeOut]
                else
                    []                    
                
            member this.updateCondition (nodeIn, _, nodeOut) = 
                let (mapUpdated, modified) = updateMapWithDifferences this.states.[nodeIn] this.states.[nodeOut] this.keys false
                                
                if modified = true then
                    this.states.[nodeOut] = mapUpdated
                    [nodeOut]
                else
                    []
                    
            member this.updateRead (nodeIn, read, nodeOut) =
                let newMap = performKillGenAction this.states.[nodeIn] read nodeIn nodeOut                
                
                let (mapUpdated, modified) = updateMapWithDifferences newMap this.states.[nodeOut] this.keys false
                
                if modified = true then
                    this.states.[nodeOut] = mapUpdated
                    [nodeOut]
                else
                    []               
        
            member this.updateWrite (nodeIn, _, nodeOut) =
                let (mapUpdated, modified) = updateMapWithDifferences this.states.[nodeIn] this.states.[nodeOut] this.keys false
                                
                if modified = true then
                    this.states.[nodeOut] = mapUpdated
                    [nodeOut]
                else
                    []
            member this.printSolution =
                "Not implemented"
    
    
