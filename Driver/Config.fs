namespace Driver

type Stage =
    | Lexing
    | Parsing
    | Resolution
    | Lowering
    
type AnalysisType =
    | Reaching
    | Live

type Config = {
    OutputFile: string option
    EarlyStageStop: Stage option
    AnalysisTarget: AnalysisType option
    PrintUsage: bool
    Benchmark: bool
}
with
    static member Default = {
        OutputFile = None
        EarlyStageStop = None
        AnalysisTarget = None
        PrintUsage = false
        Benchmark = false
    }

