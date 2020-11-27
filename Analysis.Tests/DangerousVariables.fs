module Analysis.Tests.DangerousVariables

open Analysis
open Analysis.Analyses
open Analysis.Worklists
open FrontEnd
open NUnit.Framework


[<Test>]
let dangerousVariables1 () =
    let source = """
        int x;
        int y;
        int[12] A;
        y := 0;
        A[0] := 0;
        y := x * 2;
        x := 0;
        y := x * 2;
        """
    let pg = FrontEnd.compile source
    
    let x = AmalgamatedLocation.Variable("x:1");
    let y = AmalgamatedLocation.Variable("y:2");
    let A = AmalgamatedLocation.Array("A:3");
    
    let worklist = StackWorklist.empty()
    
    let analysis = DangerousVariableAnalysis()
    let (solution,_) = analysis.analyse pg worklist
    
    let expected = [
        [x;y;A]; //Initial
        [x;y;A]; //int x
        [x;y;A]; //int y
        [x;y;A]; //int[12] A
        [x;A]; //y := 0
        [x;A]; //A[0] := 0
        [x;y;A]; //y := x * 2
        [y;A]; //x := 0
        [A]; //y := x * 2
        [A]; //free A
        [A]; //free y
        [A]; //free x
    ]
    
    let solution = solution |> Map.toList |> List.map (fun (k,v) -> Set.toList v)
    Assert.That(solution, Is.EqualTo(expected))
    
    
    
