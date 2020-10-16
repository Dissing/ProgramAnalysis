namespace Driver

open System.IO;
open Driver
open FrontEnd

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
        let tokens = Lexer.lex source.Content
        let ast = Parser.parse source tokens
        failwith "Rest of pipeline not yet implemented"
        
        
    let analysisPipeline config (graph: ProgramGraph.Graph) =
        failwith "Not yet implemented"
    
    [<EntryPoint>]
    let main args =
        printfn "Hello World from F#!"
        (*
        let opts =
                []
                |> addFlag "h" "help" "print this help menu"
                |> addOpt "o" "" "set output file name" "NAME"
                |> addOpt "a" "analysis" "dump the result of a specific analysis: [reaching; live]" "ANALYSIS"
                |> addOpt "s" "stage" "dump the output of a specific frontend stage: [lexing, parsing, resolution, lowering]" "STAGE"
                
        let matches =
            match parse opts (Array.toList args) with
            | Ok(m) -> m
            | Error(s) -> failwithf "Error: %s" s
            
        let rec iterOpts (config: Config) (opts: OptMatch list) =
            match opts with
            | [] -> config
            | o::os ->
                let config =
                    match o.Name with
                    | "h" -> { config with PrintUsage = true }
                    | "o" -> { config with OutputFile = o.Arg }
                    | "a" -> { config with AnalysisTarget = Some(parseAnalysisTarget o.Arg) }
                    | "s" -> { config with EarlyStageStop = Some(parseStage o.Arg) }
                    | other -> failwithf "Internal error: Unrecognized command line option %s" other
                iterOpts config opts
            
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
            let (graph, isDone) = frontendPipeline config file
            if not isDone then
                analysisPipeline config graph
        *)
        let ast = ([], [
            AST.Statement.Assign(AST.Location.Identifier "a", AST.ArithmeticExpr.IntLiteral 10) ;
            AST.Statement.If(
                AST.Comparison ((AST.Loc (AST.Identifier "x")), AST.Greater, (AST.Loc (AST.Identifier "y"))),
                ([],[
                    AST.Statement.Assign(AST.Location.Identifier "x", AST.ArithmeticBinary ((AST.Loc (AST.Identifier "x")), AST.Subtract, (AST.IntLiteral 1)))
                    AST.Statement.Assign(AST.Location.Identifier "x", AST.ArithmeticBinary ((AST.Loc (AST.Identifier "x")), AST.Subtract, (AST.IntLiteral 1)))
                ]),
                Some ([],[
                    AST.Statement.Assign(AST.Location.Identifier "y", AST.ArithmeticBinary ((AST.Loc (AST.Identifier "y")), AST.Subtract, (AST.IntLiteral 1)))
                    AST.Statement.Assign(AST.Location.Identifier "y", AST.ArithmeticBinary ((AST.Loc (AST.Identifier "y")), AST.Subtract, (AST.IntLiteral 1)))
                ])
            ) ;
        ])
        let (nodes, edges) = EdgesFunction.runEdges ast
        printfn "%A" nodes
        printfn "%A" edges
        
                    
        0 // return an integer exit code
