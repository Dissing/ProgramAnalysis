module Driver.Benchmark

    open Analysis
    open Analysis.Analyses
    open FrontEnd.ProgramGraph
    open Analysis.Worklists

    
    let measure (graph: AnnotatedGraph) (analysis: IAnalysis<_>) (worklist: IWorklist) =
        let (_result, steps) = analysis.analyse graph worklist
        (analysis.name, worklist.name, steps)

    let perform ((decls, pg) as graph: AnnotatedGraph) =
        
        let worklists: List<IWorklist> = [
            StackWorklist.empty();
            QueueWorklist.empty();
            StrongComponentsWorklist.empty(pg);
            NaturalComponentsWorklist.empty(pg);
        ]
        let (nodes, edges) = pg
        let results = [
            List.map (measure graph (ReachingDefinitionsAnalysis(edges))) worklists
            List.map (measure graph (LiveVariablesAnalysis())) worklists
            List.map (measure graph (DangerousVariableAnalysis())) worklists
            List.map (measure graph (FaintVariableAnalysis())) worklists
        ]
        
        for analysisResults in results do
            for (analysis, worklist, steps) in analysisResults do
                printfn "%s,%s,%d" analysis worklist steps