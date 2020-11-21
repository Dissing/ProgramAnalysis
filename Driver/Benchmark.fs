module Driver.Benchmark

    open Analysis
    open FrontEnd.ProgramGraph
    open Analysis.Analyses
    open Analysis.Worklists

    let analyses: List<IAnalysis<_>> = [
        FaintVariableAnalysis();
        DangerousVariableAnalysis()
    ]
    
    let worklists = [
        StackWorklist.empty()
    ]
    
    let combine2 f xs ys = [
        for x in xs do
        for y in ys do
        yield f x y
    ]
    
    let measure (graph: AnnotatedGraph) (analysis: IAnalysis<_>) (worklist: IWorklist) =
        let (_result, steps) = analysis.analyse graph worklist
        (analysis.name, worklist.name, steps)

    let perform graph =
        let results = combine2 (measure graph) analyses worklists
        for (analysis, worklist, ops) in results do
        printfn "%s,%s,%d" analysis worklist ops