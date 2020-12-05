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
            RoundRobinWorklist.empty(pg);
            StrongComponentsWorklist.empty(pg);
            NaturalComponentsWorklist.empty(pg);
        ]
        let reverseWorklists: List<IWorklist> = [
            StackWorklist.empty();
            QueueWorklist.empty();
            RoundRobinWorklist.empty(reverse pg);
            StrongComponentsWorklist.empty(reverse pg);
            NaturalComponentsWorklist.empty(reverse pg);
        ]
        let results = [
            List.map (measure graph (ReachingDefinitionsAnalysis(graph))) worklists
            List.map (measure graph (LiveVariablesAnalysis())) reverseWorklists
            List.map (measure graph (AvailableExpressionsAnalysis(graph))) worklists
            List.map (measure graph (VeryBusyExpressionsAnalysis(graph))) reverseWorklists
            List.map (measure graph (DangerousVariableAnalysis())) worklists
            List.map (measure graph (FaintVariableAnalysis())) reverseWorklists
            List.map (measure graph (SignDetectionAnalysis(graph))) worklists
            List.map (measure graph (IntervalAnalysis(graph, -2, 2))) worklists
        ]
        
        for analysisResults in results do
            for (analysis, worklist, steps) in analysisResults do
                printfn "%s,%s,%d" analysis worklist steps