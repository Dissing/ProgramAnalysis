module Analysis.Tests.LiveVariables

open Analysis
open Analysis.Analyses
open Analysis.Worklists
open FrontEnd
open NUnit.Framework

let amalgamatedToString = function
    | AmalgamatedLocation.Variable(name) -> name
    | AmalgamatedLocation.Array(name) -> name
    | AmalgamatedLocation.Field(strct, field) -> strct + "." + field
let extractSolution (solution, _steps) = solution |> Map.toList |> List.map (fun (_,v) -> List.map amalgamatedToString (Set.toList v)) |> List.toArray

[<Test>]
let assignVarConst () =
    let source = "int X; X:=5;"
    
    let graph = FrontEnd.compile source
    
    let analysis = LiveVariablesAnalysis()
    let worklist = StackWorklist.empty()
    let sol = analysis.analyse graph worklist |> extractSolution
    Assert.That(sol.Length, Is.EqualTo 4)
    Assert.That(sol.[0], Is.Empty)
    Assert.That(sol.[1], Is.Empty)
    Assert.That(sol.[2], Is.Empty)
    Assert.That(sol.[3], Is.Empty)

[<Test>]
let assignVar () =
    let source = "int X; int Y; X:=Y; Y:=X;"
    
    let graph = FrontEnd.compile source
    
    let analysis = LiveVariablesAnalysis()
    let worklist = StackWorklist.empty()
    let sol = analysis.analyse graph worklist |> extractSolution
    Assert.That(sol.Length, Is.EqualTo 7)
    Assert.That(sol.[0], Is.Empty)
    Assert.That(sol.[1], Is.Empty)
    Assert.That(sol.[2], Is.EqualTo ["Y:2"])
    Assert.That(sol.[3], Is.EqualTo ["X:1"])
    Assert.That(sol.[4], Is.Empty)

[<Test>]
let assignVarSelf () =
    let source = "int X; X:=X;"
    
    let graph = FrontEnd.compile source
    
    let analysis = LiveVariablesAnalysis()
    let worklist = StackWorklist.empty()
    let sol = analysis.analyse graph worklist |> extractSolution
    Assert.That(sol.Length, Is.EqualTo 4)
    Assert.That(sol.[0], Is.Empty)
    Assert.That(sol.[1].[0], Is.EqualTo "X:1")
    Assert.That(sol.[2], Is.Empty)
    Assert.That(sol.[3], Is.Empty)
 
[<Test>]
let assignArray () =
    let source = "int[2] X; int Z; X[1]:=5; Z:= X[0];"
    
    let graph = FrontEnd.compile source
    
    let analysis = LiveVariablesAnalysis()
    let worklist = StackWorklist.empty()
    let sol = analysis.analyse graph worklist |> extractSolution
    Assert.That(sol.Length, Is.EqualTo 7)
    Assert.That(sol.[0], Is.Empty)
    Assert.That(sol.[1].[0], Is.EqualTo "X:1")
    Assert.That(sol.[2].[0], Is.EqualTo "X:1")
    Assert.That(sol.[3].[0], Is.EqualTo "X:1")
    Assert.That(sol.[4], Is.Empty)
    

[<Test>]
let assignStruct () =
    let source = "{int foo; int bar} S; S.foo:=5; S.bar := S.foo;"
    
    let graph = FrontEnd.compile source
    
    let analysis = LiveVariablesAnalysis()
    let worklist = StackWorklist.empty()
    let sol = analysis.analyse graph worklist |> extractSolution
    Assert.That(sol.Length, Is.EqualTo 5)
    Assert.That(sol.[0], Is.Empty)
    Assert.That(sol.[1], Is.Empty)
    Assert.That(sol.[2].[0], Is.EqualTo "S:1.foo")
    Assert.That(sol.[3], Is.Empty)
   
[<Test>]
let assignFullStruct () =
    let source = "{int foo; int bar} S; S:={S.bar,S.foo};S:={0,0}; S:={S.bar,S.foo};"
    
    let graph = FrontEnd.compile source
    
    let analysis = LiveVariablesAnalysis()
    let worklist = StackWorklist.empty()
    let sol = analysis.analyse graph worklist |> extractSolution
    Assert.That(sol.Length, Is.EqualTo 6)
    Assert.That(sol.[0], Is.Empty)
    Assert.True(List.contains "S:1.foo" sol.[1])
    Assert.True(List.contains "S:1.bar" sol.[1])
    Assert.That(sol.[2], Is.Empty)
    Assert.True(List.contains "S:1.foo" sol.[3])
    Assert.True(List.contains "S:1.bar" sol.[3])
    Assert.That(sol.[4], Is.Empty)
    

[<Test>]
let readNormal() =
    let source = "int x; int z; read x; z := x;"
    
    let graph = FrontEnd.compile source
    
    let analysis = LiveVariablesAnalysis()
    let worklist = StackWorklist.empty()
    let sol = analysis.analyse graph worklist |> extractSolution
    Assert.That(sol.Length, Is.EqualTo 7)
    Assert.That(sol.[2], Is.Empty)
    Assert.That(sol.[3].[0], Is.EqualTo "x:1")
    Assert.That(sol.[4], Is.Empty)

[<Test>]
let readArray() =
    let source = "int[2] x; read x[0]; x[0] := x[1];"
    
    let graph = FrontEnd.compile source
    
    let analysis = LiveVariablesAnalysis()
    let worklist = StackWorklist.empty()
    let sol = analysis.analyse graph worklist |> extractSolution
    Assert.That(sol.Length, Is.EqualTo 5)
    Assert.That(sol.[0], Is.Empty)
    Assert.That(sol.[1].[0], Is.EqualTo "x:1")
    Assert.That(sol.[2].[0], Is.EqualTo "x:1")
    Assert.That(sol.[3], Is.Empty)

[<Test>]
let readStruct() =
    let source = "{int foo; int bar} S; read S.foo; S.bar:= S.foo;"
    
    let graph = FrontEnd.compile source
    
    let analysis = LiveVariablesAnalysis()
    let worklist = StackWorklist.empty()
    let sol = analysis.analyse graph worklist |> extractSolution
    Assert.That(sol.Length, Is.EqualTo 5)
    Assert.That(sol.[0], Is.Empty)
    Assert.That(sol.[1], Is.Empty)
    Assert.That(sol.[2].[0], Is.EqualTo "S:1.foo")
    Assert.That(sol.[3], Is.Empty)

[<Test>]
let write() =
    let source = "int x; int[2] y; {int foo; int bar} S; write y[1]+5-x+S.foo;"
    
    let graph = FrontEnd.compile source
    
    let analysis = LiveVariablesAnalysis()
    let worklist = StackWorklist.empty()
    let sol = analysis.analyse graph worklist |> extractSolution
    Assert.That(sol.Length, Is.EqualTo 8)
    Assert.That(sol.[0], Is.Empty)
    Assert.True(List.contains "x:1" sol.[1])
    Assert.True(List.contains "x:1" sol.[2])
    Assert.True(List.contains "y:2" sol.[2])
    Assert.True(List.contains "S:3.foo" sol.[3])
    Assert.True(List.contains "y:2" sol.[3])
    Assert.True(List.contains "x:1" sol.[3])
    Assert.That(sol.[4], Is.Empty)


[<Test>]
let conditionIf() =
    let source = "int x; int y; if(y > 0) {x:=x;}"
    
    let graph = FrontEnd.compile source
    
    let analysis = LiveVariablesAnalysis()
    let worklist = StackWorklist.empty()
    let sol = analysis.analyse graph worklist |> extractSolution
    Assert.That(sol.Length, Is.EqualTo 7)
    Assert.That(sol.[0], Is.Empty)
    Assert.True(List.contains "x:1" sol.[1])
    Assert.False(List.contains "y:2" sol.[1])
    Assert.True(List.contains "y:2" sol.[2])
    Assert.True(List.contains "x:1" sol.[2])
    Assert.True(List.contains "x:1" sol.[3])
    Assert.False(List.contains "x:2" sol.[3])
    Assert.That(sol.[4], Is.Empty)


[<Test>]
let conditionIfElse() =
    let source = "int x; int y; if(5 > 0) {x:=y;} else{y:=x;}"
    
    let graph = FrontEnd.compile source
    
    let analysis = LiveVariablesAnalysis()
    let worklist = StackWorklist.empty()
    let sol = analysis.analyse graph worklist |> extractSolution
    Assert.That(sol.Length, Is.EqualTo 8)
    Assert.True(List.contains "x:1" sol.[2])
    Assert.True(List.contains "y:2" sol.[2])
    
    Assert.False(List.contains "x:1" sol.[3])
    Assert.True(List.contains "y:2" sol.[3])
    
    Assert.False(List.contains "y:2" sol.[5])
    Assert.True(List.contains "x:1" sol.[5])
    Assert.That(sol.[4], Is.Empty)


[<Test>]
let conditionWhile() =
    let source = "int x; while(x > 0) {x:=x-1;}"
    
    let graph = FrontEnd.compile source
    
    let analysis = LiveVariablesAnalysis()
    let worklist = StackWorklist.empty()
    let sol = analysis.analyse graph worklist |> extractSolution
    Assert.That(sol.Length, Is.EqualTo 5)
    Assert.That(sol.[0], Is.Empty)
    
    Assert.True(List.contains "x:1" sol.[1])
    Assert.True(List.contains "x:1" sol.[2])
    Assert.That(sol.[3], Is.Empty)

