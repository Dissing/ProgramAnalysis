module Analysis.Tests.Integration

open Analysis
open Analysis.Analyses
open Analysis.Worklists
open FrontEnd
open FrontEnd.ProgramGraph
open NUnit.Framework
open System.IO
open NUnit.Framework

let statisticalSource = """
int[1000] A;
{int average; int max; int std; int size} S;
int i;
S.size := 1000;
while (S.size > i) {
    int ele;
    ele := A[i];
    S.average := S.average + ele;
    if(S.max < ele)
    {
        S.max := ele;
    }
    S.std := S.std + ele*ele;
    i := i + 1;
}
S.std := (S.std - (S.average*S.average)/S.size)/(S.size-1);
S.average := S.average/S.size;
write S.average;
write S.max;
write S.std;
"""

let testAnalysis<'L when 'L : comparison> (annotatedGraph: AnnotatedGraph) (analysis: IAnalysis<'L>) =
    let (_, graph) = annotatedGraph
    let worklists = [
        StackWorklist.empty() :> IWorklist;
        QueueWorklist.empty() :> IWorklist;
        RoundRobinWorklist.empty(graph) :> IWorklist;
        StrongComponentsWorklist.empty(graph) :> IWorklist;
        NaturalComponentsWorklist.empty(graph) :> IWorklist
    ]
        
    let solutions = worklists |> List.map (fun w ->
        let (sol, _) = analysis.analyse annotatedGraph w
        sol)
        
    let referenceSolution = solutions.Head
    let areEqual = List.forall (fun sol -> sol = referenceSolution) solutions
    Assert.True(areEqual)

[<Test>]
let reachingDefinitionsIntegration () =
    let annotatedGraph = FrontEnd.compile statisticalSource
    testAnalysis annotatedGraph (ReachingDefinitionsAnalysis(annotatedGraph))
    
[<Test>]
let liveVariablesIntegration () =
    let annotatedGraph = FrontEnd.compile statisticalSource
    testAnalysis annotatedGraph (LiveVariablesAnalysis())
    
[<Test>]
let dangerousVariablesIntegration () =
    let annotatedGraph = FrontEnd.compile statisticalSource
    testAnalysis annotatedGraph (DangerousVariableAnalysis())
    
[<Test>]
let faintVariablesIntegration () =
    let annotatedGraph = FrontEnd.compile statisticalSource
    testAnalysis annotatedGraph (FaintVariableAnalysis())
    
[<Test>]
let intervalIntegration () =
    let annotatedGraph = FrontEnd.compile statisticalSource
    testAnalysis annotatedGraph (IntervalAnalysis(annotatedGraph, -2, 2))
    
[<Test>]
let signDetectionIntegration () =
    let annotatedGraph = FrontEnd.compile statisticalSource
    testAnalysis annotatedGraph (SignDetectionAnalysis(annotatedGraph))