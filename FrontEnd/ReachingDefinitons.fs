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
        
        let shouldKill (state: RDVarState) (location: Location) =
            match state with
            | UnmodifiedStandard id -> match location with
                                        | Identifier iden -> if id = iden then
                                                                 true
                                                             else
                                                                 false
                                        | _ -> false
            | UnmodifiedField (s, f) -> match location with
                                        | Location.Field (strct, field) -> if s = strct && f = field then
                                                                               true
                                                                           else
                                                                               false
                                        | _ -> false
            | ModifiedStandard (id, _, _) -> match location with
                                             | Identifier iden -> if id = iden then
                                                                      true
                                                                  else
                                                                      false
                                             | _ -> false
            | ModifiedField ((s, f), _, _) -> match location with
                                              | Location.Field (strct, field) -> if s = strct && f = field then
                                                                                     true
                                                                                 else
                                                                                     false
                                              | _ -> false
            | _ -> false
            
        let rec killAction (states:List<RDVarState>) (location : Location) =
            match states with
            | head :: tail -> if (shouldKill head location) then
                                  killAction tail location
                              else
                                  head::(killAction tail location)
            | [] -> []
       
         
        let rec killList (states:List<RDVarState>) (structField: Ident) (literals : List<Field * ArithmeticExpr>) =
            match literals with
            | head :: tail -> if (shouldKill head location) then
                                  killAction tail location
                              else
                                  head::(killAction tail location)
            | [] -> states
         
        let rec stateExists (states: List<RDVarState>) (s: RDVarState) =
            match states with
            | head::tail -> if s = head then
                                    true
                                else
                                    stateExists tail s
            | [] -> false
            
        let genAction (states: List<RDVarState>) (location : Location) (nodeIn : Node) (nodeOut: Node) =
            let createAction (location : Location) (nodeIn : Node) (nodeOut: Node) = 
                    match location with
                    | Identifier id -> ModifiedStandard(id, nodeIn, nodeOut)
                    | Location.Array (id, _) -> ModifiedArray(id, nodeIn, nodeOut)
                    | Location.Field (strct, id) -> ModifiedField((strct, id), nodeIn, nodeOut)
           
            let s = createAction location nodeIn nodeOut
            
            if (stateExists states s) then
                states
            else
                s::states
                   
        let rec findDifferences (newList: List<RDVarState>) (nextState: List<RDVarState>) = 
            match newList with
            | head :: tail -> if (stateExists nextState head) then
                                    findDifferences tail nextState
                              else
                                    head::(findDifferences tail nextState)
            | [] -> nextState
       
        member this.states: List<RDVarState>[] = [||]
 
        interface IAlgorithm with
            member this.initialise graph declaration =
                let nodes = fst graph
                
                this.states = Array.create (nodes.Length) (List.empty)
                
                let rec parseFields id fields =
                    match fields with
                    | head :: tail -> UnmodifiedField((id, (snd head)))::(parseFields id tail)
                    | [] -> []
                let rec parseDeclaration dList =
                    match dList with
                    | head :: tail -> match head with
                                       | Integer id -> UnmodifiedStandard(id)::(parseDeclaration tail)
                                       | Array (id, _) -> UnmodifiedArray(id)::(parseDeclaration tail)
                                       | Struct (id, fields) -> (parseFields id fields)@(parseDeclaration tail)
                    | [] -> []
                
                this.states.[0] = parseDeclaration declaration
                
                [nodes.[0]]
            
            
            member this.updateAssign (nodeIn, assign, nodeOut) =
                let location = fst assign
                let inList = this.states.[nodeIn]
                let killedList = killAction inList location
                let newList = genAction killedList location nodeIn nodeOut
                
                let diff = findDifferences newList this.states.[nodeOut]
                
                if diff.Length = 0 then
                    []
                else
                    this.states.[nodeOut] = diff@(this.states.[nodeOut])
                    [nodeOut]
                    
                    
        
            member this.updateAssignLiteral (nodeIn, assignLiteral, nodeOut) =
                List.empty
        
            member this.updateCondition (nodeIn, _, nodeOut) = 
                let diff = findDifferences this.states.[nodeIn] this.states.[nodeOut] 
                if diff.Length = 0 then
                    []
                else
                    this.states.[nodeOut] = diff@(this.states.[nodeOut])
                    [nodeOut]
                    
            member this.updateRead (nodeIn, read, nodeOut) =
                let inList = this.states.[nodeIn]
                let killedList = killAction inList read
                let newList = genAction killedList read nodeIn nodeOut
                
                let diff = findDifferences newList this.states.[nodeOut]
                
                if diff.Length = 0 then
                    []
                else
                    this.states.[nodeOut] = diff@(this.states.[nodeOut])
                    [nodeOut]
        
            member this.updateWrite (nodeIn, _, nodeOut) =
                let diff = findDifferences this.states.[nodeIn] this.states.[nodeOut] 
                if diff.Length = 0 then
                    []
                else
                    this.states.[nodeOut] = diff@(this.states.[nodeOut])
                    [nodeOut]
        
            member this.printSolution =
                "Not implemented"
    
    
