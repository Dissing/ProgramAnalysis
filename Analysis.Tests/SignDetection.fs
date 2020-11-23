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
    
[<Test>]
let signDetection2() =
    let source = """
        int[200] A;
        A[-1] := 24;
        A[22] := A[44]+44-12;
        A[44] := 0;
        A[2] := A[-44];
        A[3] := 5;
        A[3] := -2;
        """
    let pg = FrontEnd.compile source
    
    let x = AmalgamatedLocation.Variable("x:1");
    
    let worklist = StackWorklist.empty()
    
    let analysis = SignDetectionAnalysis(pg)
    let (solution,_) = analysis.analyse pg worklist
    
    let expected = [
        [Set.ofList [Sign.Minus; Sign.Plus; Sign.Zero]] //start
        [Set.ofList [Sign.Zero]] //int A[200];
        [Set.empty] //A[-1] := 24;
        [Set.empty] //A[22] := A[44]+44-12;
        [Set.ofList [Sign.Zero]] //A[44] := 0;
        [Set.empty] //A[2] := A[-44];
        [Set.ofList [Sign.Plus; ]]//A[3] := 5;
        [Set.ofList [Sign.Minus; Sign.Plus;]] //A[3] := -2;
        [Set.ofList [Sign.Minus; Sign.Plus; Sign.Zero]] // free x
    ]
    
    let solution = solution |> Map.toList |> List.map (fun (_,v) -> Map.toList v |> List.map (fun (_, v) -> v)) 
    Assert.That(solution, Is.EqualTo(expected))

[<Test>]
let signDetectionPlus() =
    let source = """
        int x;
        x := -24+-24;
        x := -24+0;
        x := -24+24;
        x := 0+-24;
        x := 0+0;
        x := 0+24;
        x := 24+-24;
        x := 24+0;
        x := 24+24;
        """
    let pg = FrontEnd.compile source
    
    let x = AmalgamatedLocation.Variable("x:1");
    
    let worklist = StackWorklist.empty()
    
    let analysis = SignDetectionAnalysis(pg)
    let (solution,_) = analysis.analyse pg worklist
    
    let expected = [
        [Set.ofList [Sign.Minus; Sign.Plus; Sign.Zero]] //start
        [Set.ofList [Sign.Zero]] //int x;
        [Set.ofList [Sign.Minus]] //x := -24+-24;
        [Set.ofList [Sign.Minus]] //x := -24+0;
        [Set.ofList [Sign.Minus; Sign.Plus; Sign.Zero]] //x := -24+24;
        [Set.ofList [Sign.Minus]] //x := 0+-24;
        [Set.ofList [Sign.Zero]] //x := 0+0;
        [Set.ofList [Sign.Plus]] //x := 0+24;
        [Set.ofList [Sign.Minus; Sign.Plus; Sign.Zero]] //x := 24+-24;
        [Set.ofList [Sign.Plus]] //x := 24+0;
        [Set.ofList [Sign.Plus]] //x := 24+24;
        [Set.ofList [Sign.Minus; Sign.Plus; Sign.Zero]] // free x
    ]
    
    let solution = solution |> Map.toList |> List.map (fun (_,v) -> Map.toList v |> List.map (fun (_, v) -> v)) 
    Assert.That(solution, Is.EqualTo(expected))

[<Test>]
let signDetectionMinus() =
    let source = """
        int x;
        x := -24--24;
        x := -24-0;
        x := -24-24;
        x := 0--24;
        x := 0-0;
        x := 0-24;
        x := 24--24;
        x := 24-0;
        x := 24-24;
        """
    let pg = FrontEnd.compile source
    
    let x = AmalgamatedLocation.Variable("x:1");
    
    let worklist = StackWorklist.empty()
    
    let analysis = SignDetectionAnalysis(pg)
    let (solution,_) = analysis.analyse pg worklist
    
    let expected = [
        [Set.ofList [Sign.Minus; Sign.Plus; Sign.Zero]] //start
        [Set.ofList [Sign.Zero]] //int x;
        [Set.ofList [Sign.Minus; Sign.Plus; Sign.Zero]] //x := -24--24;
        [Set.ofList [Sign.Minus]] //x := -24-0;
        [Set.ofList [Sign.Minus]] //x := -24-24;
        [Set.ofList [Sign.Plus]] //x := 0--24;
        [Set.ofList [Sign.Zero]] //x := 0-0;
        [Set.ofList [Sign.Minus]] //x := 0-24;
        [Set.ofList [Sign.Plus]] //x := 24--24;
        [Set.ofList [Sign.Plus]] //x := 24-0;
        [Set.ofList [Sign.Plus; Sign.Minus; Sign.Zero]] //x := 24-24;
        [Set.ofList [Sign.Minus; Sign.Plus; Sign.Zero]] // free x
    ]
    
    let solution = solution |> Map.toList |> List.map (fun (_,v) -> Map.toList v |> List.map (fun (_, v) -> v)) 
    Assert.That(solution, Is.EqualTo(expected))

[<Test>]
let signDetectionDivision() =
    let source = """
        int x;
        x := -24/-24;
        x := -24/0;
        x := -24/24;
        x := 0/-24;
        x := 0/0;
        x := 0/24;
        x := 24/-24;
        x := 24/0;
        x := 24/24;
        """
    let pg = FrontEnd.compile source
    
    let x = AmalgamatedLocation.Variable("x:1");
    
    let worklist = StackWorklist.empty()
    
    let analysis = SignDetectionAnalysis(pg)
    let (solution,_) = analysis.analyse pg worklist
    
    let expected = [
        [Set.ofList [Sign.Minus; Sign.Plus; Sign.Zero]] //start
        [Set.ofList [Sign.Zero]] //int x;
        [Set.ofList [Sign.Plus]] //x := -24/-24;
        [Set.empty] //x := -24/0;
        [Set.ofList [Sign.Minus]] //x := -24/24;
        [Set.ofList [Sign.Zero]] //x := 0/-24;
        [Set.empty] //x := 0/0;
        [Set.ofList [Sign.Zero]] //x := 0/24;
        [Set.ofList [Sign.Minus]] //x := 24/-24;
        [Set.empty] //x := 24/0;
        [Set.ofList [Sign.Plus]] //x := 24/24;
        [Set.ofList [Sign.Minus; Sign.Plus; Sign.Zero]] // free x
    ]
    
    let solution = solution |> Map.toList |> List.map (fun (_,v) -> Map.toList v |> List.map (fun (_, v) -> v)) 
    Assert.That(solution, Is.EqualTo(expected))

[<Test>]
let signDetectionMultiply() =
    let source = """
        int x;
        x := -24*-24;
        x := -24*0;
        x := -24*24;
        x := 0*-24;
        x := 0*0;
        x := 0*24;
        x := 24*-24;
        x := 24*0;
        x := 24*24;
        """
    let pg = FrontEnd.compile source
    
    let x = AmalgamatedLocation.Variable("x:1");
    
    let worklist = StackWorklist.empty()
    
    let analysis = SignDetectionAnalysis(pg)
    let (solution,_) = analysis.analyse pg worklist
    
    let expected = [
        [Set.ofList [Sign.Minus; Sign.Plus; Sign.Zero]] //start
        [Set.ofList [Sign.Zero]] //int x;
        [Set.ofList [Sign.Plus]] //x := -24*-24;
        [Set.ofList [Sign.Zero]] //x := -24*0;
        [Set.ofList [Sign.Minus]] //x := -24*24;
        [Set.ofList [Sign.Zero]] //x := 0*-24;
        [Set.ofList [Sign.Zero]] //x := 0*0;
        [Set.ofList [Sign.Zero]] //x := 0*24;
        [Set.ofList [Sign.Minus]] //x := 24*-24;
        [Set.ofList [Sign.Zero]] //x := 24*0;
        [Set.ofList [Sign.Plus]] //x := 24*24;
        [Set.ofList [Sign.Minus; Sign.Plus; Sign.Zero]] // free x
    ]
    
    let solution = solution |> Map.toList |> List.map (fun (_,v) -> Map.toList v |> List.map (fun (_, v) -> v)) 
    Assert.That(solution, Is.EqualTo(expected))

[<Test>]
let signDetectionModulo() =
    let source = """
        int x;
        x := -24%-24;
        x := -24%0;
        x := -24%24;
        x := 0%-24;
        x := 0%0;
        x := 0%24;
        x := 24%-24;
        x := 24%0;
        x := 24%24;
        """
    let pg = FrontEnd.compile source
    
    let x = AmalgamatedLocation.Variable("x:1");
    
    let worklist = StackWorklist.empty()
    
    let analysis = SignDetectionAnalysis(pg)
    let (solution,_) = analysis.analyse pg worklist
    
    let expected = [
        [Set.ofList [Sign.Minus; Sign.Plus; Sign.Zero]] //start
        [Set.ofList [Sign.Zero]] //int x;
        [Set.ofList [Sign.Zero; Sign.Minus]] //x := -24%-24;
        [Set.empty] //x := -24%0;
        [Set.ofList [Sign.Zero; Sign.Plus]] //x := -24%24;
        [Set.ofList [Sign.Zero]] //x := 0%-24;
        [Set.empty] //x := 0%0;
        [Set.ofList [Sign.Zero]] //x := 0%24;
        [Set.ofList [Sign.Minus; Sign.Zero]] //x := 24%-24;
        [Set.empty] //x := 24%0;
        [Set.ofList [Sign.Plus; Sign.Zero]] //x := 24%24;
        [Set.ofList [Sign.Minus; Sign.Plus; Sign.Zero]] // free x
    ]
    
    let solution = solution |> Map.toList |> List.map (fun (_,v) -> Map.toList v |> List.map (fun (_, v) -> v)) 
    Assert.That(solution, Is.EqualTo(expected))

