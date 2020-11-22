module Analysis.Tests.SignDetection

open Analysis
open Analysis.Analyses
open Analysis.Worklists
open FrontEnd
open NUnit.Framework


[<Test>]
let signDetection1() =
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
    
    let worklist = StackWorklist.empty()
    
    let analysis = SignDetectionAnalysis(pg)
    let (solution,_) = analysis.analyse pg worklist
    
    let expected = [
        [Set.ofList [Sign.Minus; Sign.Plus; Sign.Zero]] //start
        [Set.ofList [Sign.Zero]] //int x;
        [Set.ofList [Sign.Plus]] // := 42;
        [Set.ofList [Sign.Minus]] //x := -21;
        [Set.ofList [Sign.Zero]] //x := 0;
        [Set.ofList [Sign.Minus; Sign.Plus; Sign.Zero]] //x := -21+23;
        [Set.ofList [Sign.Minus]] //x := -21*23;
        [Set.ofList [Sign.Minus]] //x := -21/23;
        [Set.ofList [Sign.Minus]] //x := -21-23;
        [Set.empty] //x := -21/0;
        [Set.ofList [Sign.Minus; Sign.Plus; Sign.Zero]] // free x
    ]
    
    let solution = solution |> Map.toList |> List.map (fun (_,v) -> Map.toList v |> List.map (fun (_, v) -> v)) 
    Assert.That(solution, Is.EqualTo(expected))
