module Analysis.Tests.SignDetection

open Analysis
open Analysis.Analyses
open Analysis.Worklists
open FrontEnd
open NUnit.Framework


[<Test>]
let faintVariables1() =
    let source = """
        int x;
        x := 42;
        x := -21;
        x := 0;
        x := -21+23;
        x := -21*23;
        x := -21/23;
        x := -21-23;
        x := -21/0;
        """
    let pg = FrontEnd.compile source
    
    let x = AmalgamatedLocation.Variable("x:1");
    let w = AmalgamatedLocation.Variable("w:4");
    
    let worklist = StackWorklist.empty()
    
    let analysis = SignDetectionAnalysis(pg)
    let (solution,_) = analysis.analyse pg worklist
    
    let expected = [
        [] //int x;
        [] // := 42;
        [] //x := -21;
        [] //x := 0;
        [] //x := -21+23;
        [] //x := -21*23;
        [] //x := -21/23;
        [] //x := -21-23;
        [] //x := -21/0;
    ]
    
    let solution = solution |> Map.toList |> List.map (fun (k,v) -> Set.toList v) |> List.rev
    Assert.That(solution, Is.EqualTo(expected))
