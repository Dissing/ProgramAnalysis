namespace BitVector

open System.Globalization
open FrontEnd.AST
open FrontEnd.ProgramGraph

module AnalysisDefinition =
    type IAlgorithm =
        
        abstract member isReverse: bool
        
        abstract member initialise: graph: Graph -> declaration: List<Declaration> -> List<Node>
        //default this.initialise graph declaration =
        //    failwith "Not implemented"
        
        abstract member updateAssign: nodeIn: Node * assign: AssignExpr * nodeOut: Node -> List<Node>    
        //default this.updateAssign (nodeIn, assign, nodeOut) =
        //    failwith "Not implemented"
        
        abstract member updateAssignLiteral: nodeIn: Node * assign: AssignLiteralExpr * nodeOut: Node -> List<Node>
        //default this.updateAssignLiteral (nodeIn, assignLiteral, nodeOut) =
        //    failwith "Not implemented"
        
        abstract member updateCondition: nodeIn: Node * condition: BooleanExpr * nodeOut: Node -> List<Node>
        //default this.updateCondition (nodeIn, condition, nodeOut) =
        //    failwith "Not implemented"
        
        abstract member updateRead: nodeIn: Node * read: Location * nodeOut: Node -> List<Node>
        //default this.updateRead (nodeIn, read, nodeOut) =
        //    failwith "Not implemented"
        
        abstract member updateWrite: nodeIn: Node * write: ArithmeticExpr * nodeOut: Node -> List<Node>
        //default this.updateWrite (nodeIn, write, nodeOut) =
        //    failwith "Not implemented"
        
        abstract member printSolution: string        
        //default this.printSolution =
        //    failwith "Not implemented"
        
        abstract member getSolution: List<string>
    
    
    let getOutEdges (node : Node) (edges : List<Edge>) =
        let rec foldEdges (list: List<Edge>)  (listOut: List<Edge>) =
            match list with
            | (nodeIn, action, nodeOut) :: tail -> if nodeIn = node then
                                                       foldEdges tail ((nodeIn, action, nodeOut)::listOut)
                                                   else
                                                       foldEdges tail listOut
            | [] -> listOut
        foldEdges edges []
    
    let getInEdges (node : Node) (edges : List<Edge>) =
        let rec foldEdges (list: List<Edge>)  (listOut: List<Edge>) =
            match list with
            | (nodeIn, action, nodeOut) :: tail -> if nodeOut = node then
                                                       foldEdges tail ((nodeIn, action, nodeOut)::listOut)
                                                   else
                                                       foldEdges tail listOut
            | [] -> listOut
        foldEdges edges []
        
    let rec runEdges (edges: List<Edge>) (outNodes : List<Node>) (algorithm : IAlgorithm) =
        match edges with
        | (nodeIn, action, nodeOut) :: tail->
            match action with
             | Assign a -> runEdges tail (outNodes@(algorithm.updateAssign (nodeIn, a, nodeOut))) algorithm
             | AssignLiteral al -> runEdges tail (outNodes@(algorithm.updateAssignLiteral (nodeIn, al, nodeOut))) algorithm
             | Condition c -> runEdges tail (outNodes@(algorithm.updateCondition (nodeIn, c, nodeOut))) algorithm
             | Read r -> runEdges tail (outNodes@(algorithm.updateRead (nodeIn, r, nodeOut))) algorithm
             | Write w -> runEdges tail (outNodes@(algorithm.updateWrite (nodeIn, w, nodeOut))) algorithm
        | [] -> outNodes
        
    let rec run ((nodes, edges) : Graph) (frontier : Set<Node>) (algorithm : IAlgorithm) =
        if frontier.IsEmpty then
            algorithm
        else
            let node = Set.minElement frontier
            let outEdges = getOutEdges node edges
            let frontierRemoved = frontier.Remove node
            let newFrontier = Set.union frontierRemoved (Set.ofList(runEdges outEdges [] algorithm))
            run (nodes, edges) newFrontier algorithm
        
    let rec runReverse ((nodes, edges) : Graph) (frontier : Set<Node>) (algorithm : IAlgorithm) =
        let node = Set.minElement frontier
        let outEdges = getInEdges node edges
        let frontierRemoved = frontier.Remove node
        let newFrontier = Set.union frontierRemoved (Set.ofList(runEdges outEdges [] algorithm))
        run (nodes, edges) newFrontier algorithm
    
    let Analyse (graph : Graph) (declarations : List<Declaration>) (algorithm : IAlgorithm) =
      
      let frontier = Set.ofList (algorithm.initialise graph declarations)
    
      if algorithm.isReverse then  
          let solution = runReverse graph frontier algorithm
          solution.printSolution 
      else
          let solution = run graph frontier algorithm
          solution.printSolution 
      
      
      
      
      
      
