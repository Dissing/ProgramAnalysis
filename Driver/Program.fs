namespace Driver

open System.IO;
open Driver
open FrontEnd
open FrontEnd.AST

module Main =
    
    open ArgumentParser
    
    let parseAnalysisTarget = function
        | Some("reaching") -> Reaching
        | Some("live") -> Live
        | Some(other) -> failwithf "Error: Unrecognized analysis target: %s" other
        | None -> failwithf "Internal error: Command line option 'analysis' has no argument"
        
    let parseStage = function
            | Some("lexing") -> Lexing
            | Some("parsing") -> Parsing
            | Some("resolution") -> Resolution
            | Some("lowering") -> Lowering
            | Some(other) -> failwithf "Error: Unrecognized stage: %s" other
            | None -> failwithf "Internal error: Command line option 'stage' has no argument"
            
    let printUsage() =
        failwithf "Help not yet implemented"
        
    let frontendPipeline (config: Config) (source: SourceFile) =
        let graph = context {
            let! ast = Lexer.lex source.Content >>= Parser.parse >>= Resolution.resolve
            return EdgesFunction.runEdges ast
        }
        match graph with
        | Ok(g) -> g
        | Error(msg, _) -> failwith msg
        
        
    let analysisPipeline config (graph: ProgramGraph.AnnotatedGraph) =
        failwith "Not yet implemented"
    
    [<EntryPoint>]
    let main args =
        printfn "Hello World from F#!"
        
        (*
        let nodes = [ 0; 1; 2; 3; 4; 5;]
        let edges = [ (0, ProgramGraph.Action.Condition(AST.Comparison(AST.Loc(AST.Identifier "x"), AST.GreaterEqual, AST.IntLiteral 0)), 1)
                      (1, ProgramGraph.Action.Condition(AST.Comparison(AST.Loc(AST.Identifier "x"), AST.Greater, AST.IntLiteral 0)), 2)
                      (2, ProgramGraph.Action.Assign(AST.Identifier "x", AST.ArithmeticBinary((AST.Loc(AST.Identifier "x")), AST.Subtract, (AST.IntLiteral 1))), 1)
                      (1, ProgramGraph.Action.Condition(AST.BooleanUnary(AST.Not, AST.Comparison(AST.Loc(AST.Identifier "x"), AST.Greater, AST.IntLiteral 0))), 5)
                      
                      (0, ProgramGraph.Action.Condition(AST.BooleanUnary(AST.Not, AST.Comparison(AST.Loc(AST.Identifier "x"), AST.GreaterEqual, AST.IntLiteral 0))), 3)
                      (3, ProgramGraph.Action.Condition(AST.Comparison(AST.Loc(AST.Identifier "x"), AST.Lesser, AST.IntLiteral 0)), 4)
                      (4, ProgramGraph.Action.Assign(AST.Identifier "x", AST.ArithmeticBinary((AST.Loc(AST.Identifier "x")), AST.Add, (AST.IntLiteral 1))), 3)
                      (3, ProgramGraph.Action.Condition(AST.BooleanUnary(AST.Not, AST.Comparison(AST.Loc(AST.Identifier "x"), AST.Lesser, AST.IntLiteral 0))), 5) ]
        *)
        let nodes = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12]
        let edges = [
            (0, ProgramGraph.Action.Read(Identifier "x"), 1)
            (1, ProgramGraph.Action.Condition(Comparison(Loc(Identifier "x"), GreaterEqual, IntLiteral 0)), 2)
            (2, ProgramGraph.Action.Assign(Identifier "i", IntLiteral 0), 3)
            (3, ProgramGraph.Action.Assign(Identifier "y", IntLiteral -1), 4)
            (4, ProgramGraph.Action.Condition(Comparison(Loc(Identifier "i"), Lesser, Loc(Identifier "n"))), 6)
            (4, ProgramGraph.Action.Condition(BooleanUnary(Not, Comparison(Loc(Identifier "i"), Lesser, Loc(Identifier "n")))), 5)
            (6, ProgramGraph.Action.Condition(Comparison(Loc(Array ("A", Loc(Identifier "i"))), NotEqual, Loc(Identifier "x"))), 10)
            (10, ProgramGraph.Action.Assign(Identifier "i", ArithmeticBinary((Loc(Identifier "i")), Add, IntLiteral 1)), 4)
            (6, ProgramGraph.Action.Condition(BooleanUnary(Not, Comparison(Loc(Array ("A", Loc(Identifier "i"))), NotEqual, Loc(Identifier "x")))), 7)
            (7, ProgramGraph.Action.Assign(Array ("B", Loc(Identifier "i")), ArithmeticBinary(Loc(Array ("B", Loc(Identifier "i"))), Add, ArithmeticBinary(Loc(Array ("B", Loc(Identifier "i"))), ArithmeticBinaryOperator.Divide, IntLiteral 10))), 8)
            (8, ProgramGraph.Action.Assign(Identifier "i", ArithmeticBinary((Loc(Identifier "y")), Add, Loc(Array ("A", Loc(Identifier "i"))))), 9)
            (9, ProgramGraph.Action.Skip, 5)
            (5, ProgramGraph.Action.Write (Loc(Array ("A", Loc(Identifier "i")))), 11)
            (11, ProgramGraph.Action.Read(Identifier "x"), 1)
            (1, ProgramGraph.Action.Condition(BooleanUnary(Not, Comparison(Loc(Identifier "x"), GreaterEqual, IntLiteral 0))), 12)
        ]
        
        let topNodesSet = Set.empty.Add(0).Add(1).Add(2).Add(3).Add(4).Add(5).Add(6).Add(7).Add(8).Add(9).Add(10).Add(11).Add(12)
        let graph = (nodes, edges)
        let (tree, rpOrder) = ReversePostorder.DFST graph
        //printfn "%A" tree
        rpOrder.print()
        
        let scList = StrongComponents.StrongComps graph rpOrder
        printfn "Strong Components: %A" scList
        
        let sc_cr = ComponentRelation.ComponentRelation (scList, graph)
        printfn "Strong Relation: %A" (sc_cr.GetRelation())
        
        let (scTopNodes, scRemainder) = sc_cr.GetTopNodes topNodesSet
        printfn "Strong Top: %A" scTopNodes
        printfn "Strong Remainder: %A" scRemainder
        
        
        let ncList = NaturalLoops.NaturalLoops graph rpOrder
        printfn "Natural Components: %A" ncList
        
        let nc_cr = ComponentRelation.ComponentRelation (ncList, graph)
        printfn "Natural Relation: %A" (nc_cr.GetRelation())
        
        let (ncTopNodes, ncRemainder) = nc_cr.GetTopNodes topNodesSet
        printfn "Natural Top: %A" ncTopNodes
        printfn "Natural Remainder: %A" ncRemainder
        
        (*
        let opts =
                []
                |> addFlag "b" "benchmark" "run the benchmarking suite"
                |> addFlag "h" "help" "print this help menu"
                |> addOpt "o" "" "set output file name" "NAME"
                |> addOpt "a" "analysis" "dump the result of a specific analysis: [reaching; live]" "ANALYSIS"
                |> addOpt "s" "stage" "dump the output of a specific frontend stage: [lexing, parsing, resolution, lowering]" "STAGE"
                
        let matches =
            match parse opts (Array.toList args) with
            | Result.Ok(m) -> m
            | Result.Error(s) -> failwithf "Error: %s" s
            
        let rec iterOpts (config: Config) (opts: OptMatch list) =
            match opts with
            | [] -> config
            | o::os ->
                let config =
                    match o.Name with
                    | "b" -> { config with Benchmark = true }
                    | "h" -> { config with PrintUsage = true }
                    | "o" -> { config with OutputFile = o.Arg }
                    | "a" -> { config with AnalysisTarget = Some(parseAnalysisTarget o.Arg) }
                    | "s" -> { config with EarlyStageStop = Some(parseStage o.Arg) }
                    | other -> failwithf "Internal error: Unrecognized command line option %s" other
                iterOpts config os
            
        let config = iterOpts Config.Default matches.Opts
        
        if config.PrintUsage then
            printUsage()
        elif config.AnalysisTarget.IsSome && config.EarlyStageStop.IsSome then
            failwith "Error: The 'analysis' and 'stage' option cannot both be specified simultaneously"
        elif matches.Free.Length = 0 then
            failwith "Error: No input file specified"
        elif matches.Free.Length > 1 then
            failwith "Error: Only one input file may be specified"
        else
            let path = matches.Free.Head
            if not (File.Exists path) then failwithf "Error: Unable to open path %s" path
            let content = File.ReadAllText(path)
            let file = SourceFile(path, content)
            let graph = frontendPipeline config file
            if config.Benchmark then
                Benchmark.perform graph
            else
                analysisPipeline config graph
        *)            
        0 // return an integer exit code
