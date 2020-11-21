module Analysis.Tests.ReachingDefinitions


open Analysis
open Analysis.Analyses
open Analysis.Worklists
open FrontEnd
open NUnit.Framework
open NUnit.Framework
open NUnit.Framework

let amalgamatedToString = function
    | AmalgamatedLocation.Variable(name) -> name
    | AmalgamatedLocation.Array(name) -> name
    | AmalgamatedLocation.Field(strct, field) -> strct + "." + field

let rdToString = function
    | (loc, None, dst) -> sprintf "(%s, ?, q%d)" (amalgamatedToString loc) dst
    | (loc, Some(src), dst) -> sprintf "(%s, q%d, q%d)" (amalgamatedToString loc) src dst
    
let extractSolution (solution, _steps) = solution |> Map.toList |> List.map (fun (_,v) -> List.map rdToString (Set.toList v)) |> List.toArray

[<Test>]
let assignVar () =
    let source = "int X; X:=5;"
    
    let graph = FrontEnd.compile source
    let (_, (_, edges)) = graph
    
    let analysis = ReachingDefinitionsAnalysis(edges)
    let worklist = StackWorklist.empty()
    let sol = analysis.analyse graph worklist |> extractSolution
    Assert.That(sol.Length, Is.EqualTo 4)
    Assert.That(sol.[0], Is.Empty)
    Assert.That(sol.[1].[0], Is.EqualTo "(X:1, ?, q1)")
    Assert.That(sol.[2].[0], Is.EqualTo "(X:1, q1, q2)")
    Assert.That(sol.[3], Is.Empty)
    
    
[<Test>]
let assignArray () =
    let source = "int[2] X; X[1]:=5;"
    
    let graph = FrontEnd.compile source
    let (_, (_, edges)) = graph
    
    let analysis = ReachingDefinitionsAnalysis(edges)
    let worklist = StackWorklist.empty()
    let sol = analysis.analyse graph worklist |> extractSolution
    Assert.That(sol.Length, Is.EqualTo 4)
    Assert.That(sol.[0], Is.Empty)
    Assert.That(sol.[1].[0], Is.EqualTo "(X:1, ?, q1)")
    Assert.True(List.contains "(X:1, q1, q2)" sol.[2])
    Assert.True(List.contains "(X:1, ?, q1)" sol.[2])
    Assert.That(sol.[3], Is.Empty)
    
    
[<Test>]
let assignStruct () =
    let source = "{int foo; int bar} S; S.foo:=5;"
    
    let graph = FrontEnd.compile source
    let (_, (_, edges)) = graph
    
    let analysis = ReachingDefinitionsAnalysis(edges)
    let worklist = StackWorklist.empty()
    let sol = analysis.analyse graph worklist |> extractSolution
    Assert.That(sol.Length, Is.EqualTo 4)
    Assert.That(sol.[0], Is.Empty)
    Assert.True(List.contains "(S:1.foo, ?, q1)" sol.[1])
    Assert.True(List.contains "(S:1.bar, ?, q1)" sol.[1])
    Assert.True(List.contains "(S:1.bar, ?, q1)" sol.[2])
    Assert.True(List.contains "(S:1.foo, q1, q2)" sol.[2])
    Assert.That(sol.[3], Is.Empty)

    
[<Test>]
let assignFullStruct () =
    let source = "{int foo; int bar} S; S:={5,2};"
    
    let graph = FrontEnd.compile source
    let (_, (_, edges)) = graph
    
    let analysis = ReachingDefinitionsAnalysis(edges)
    let worklist = StackWorklist.empty()
    let sol = analysis.analyse graph worklist |> extractSolution
    Assert.That(sol.Length, Is.EqualTo 4)
    
    Assert.That(sol.[0], Is.Empty)
    Assert.True(List.contains "(S:1.foo, ?, q1)" sol.[1])
    Assert.True(List.contains "(S:1.bar, ?, q1)" sol.[1])
    Assert.True(List.contains "(S:1.bar, q1, q2)" sol.[2])
    Assert.True(List.contains "(S:1.foo, q1, q2)" sol.[2])
    Assert.That(sol.[3], Is.Empty)
    
    
[<Test>]
let readNormal() =
    let source = "int x; read x;"
    
    let graph = FrontEnd.compile source
    let (_, (_, edges)) = graph
    
    let analysis = ReachingDefinitionsAnalysis(edges)
    let worklist = StackWorklist.empty()
    let sol = analysis.analyse graph worklist |> extractSolution
    Assert.That(sol.Length, Is.EqualTo 4)
    
    Assert.That(sol.[0], Is.Empty)
    Assert.That(sol.[1].[0], Is.EqualTo "(x:1, ?, q1)")
    Assert.That(sol.[2].[0], Is.EqualTo "(x:1, q1, q2)")
    Assert.That(sol.[3], Is.Empty)

[<Test>]
let readArray() =
    let source = "int[2] x; read x[0];"
    
    let graph = FrontEnd.compile source
    let (_, (_, edges)) = graph
    
    let analysis = ReachingDefinitionsAnalysis(edges)
    let worklist = StackWorklist.empty()
    let sol = analysis.analyse graph worklist |> extractSolution
    
    Assert.That(sol.Length, Is.EqualTo 4)
    Assert.That(sol.[0], Is.Empty)
    Assert.That(sol.[1].[0], Is.EqualTo "(x:1, ?, q1)")
    Assert.True(List.contains "(x:1, ?, q1)" sol.[2])
    Assert.True(List.contains "(x:1, q1, q2)" sol.[2])
    Assert.That(sol.[3], Is.Empty)
    

[<Test>]
let readStruct() =
    let source = "{int foo; int bar} S; read S.foo;"
    
    let graph = FrontEnd.compile source
    let (_, (_, edges)) = graph
    
    let analysis = ReachingDefinitionsAnalysis(edges)
    let worklist = StackWorklist.empty()
    let sol = analysis.analyse graph worklist |> extractSolution
    Assert.That(sol.Length, Is.EqualTo 4)
    Assert.That(sol.[0], Is.Empty)
    Assert.True(List.contains "(S:1.foo, ?, q1)" sol.[1])
    Assert.True(List.contains "(S:1.bar, ?, q1)" sol.[1])
    Assert.True(List.contains "(S:1.bar, ?, q1)" sol.[2])
    Assert.True(List.contains "(S:1.foo, q1, q2)" sol.[2])
    Assert.That(sol.[3], Is.Empty)

[<Test>]
let write() =
    let source = "int x; {int foo; int bar} S; write 5; write x; write S.foo;"
    
    let graph = FrontEnd.compile source
    let (_, (_, edges)) = graph
    
    let analysis = ReachingDefinitionsAnalysis(edges)
    let worklist = StackWorklist.empty()
    let sol = analysis.analyse graph worklist |> extractSolution
    Assert.That(sol.Length, Is.EqualTo 8)
    Assert.That(sol.[0], Is.Empty)
    
    Assert.True(List.contains "(x:1, ?, q1)" sol.[3])
    Assert.True(List.contains "(S:2.foo, ?, q2)" sol.[3])
    Assert.True(List.contains "(S:2.bar, ?, q2)" sol.[3])
    Assert.True(List.contains "(x:1, ?, q1)" sol.[4])
    Assert.True(List.contains "(S:2.foo, ?, q2)" sol.[4])
    Assert.True(List.contains "(S:2.bar, ?, q2)" sol.[4])
    Assert.True(List.contains "(x:1, ?, q1)" sol.[5])
    Assert.True(List.contains "(S:2.foo, ?, q2)" sol.[5])
    Assert.True(List.contains "(S:2.bar, ?, q2)" sol.[5])

[<Test>]
let conditionIf() =
    let source = "int x; if(x > 0) {x:=5;}"
    
    let graph = FrontEnd.compile source
    let (_, (_, edges)) = graph
    
    let analysis = ReachingDefinitionsAnalysis(edges)
    let worklist = StackWorklist.empty()
    let sol = analysis.analyse graph worklist |> extractSolution
    Assert.That(sol.Length, Is.EqualTo 5)
    Assert.That(sol.[0], Is.Empty)
    Assert.True(List.contains "(x:1, ?, q1)" sol.[1])
    Assert.True(List.contains "(x:1, ?, q1)" sol.[2])
    Assert.True(List.contains "(x:1, q2, q3)" sol.[3])
    Assert.That(sol.[4], Is.Empty)

[<Test>]
let conditionIfElse() =
    let source = "int x; int y; if(x > 0) {x:=5;} else{y:=2;}"
    
    let graph = FrontEnd.compile source
    let (_, (_, edges)) = graph
    
    let analysis = ReachingDefinitionsAnalysis(edges)
    let worklist = StackWorklist.empty()
    let sol = analysis.analyse graph worklist |> extractSolution
    Assert.That(sol.Length, Is.EqualTo 8)
    Assert.That(sol.[0], Is.Empty)
    Assert.True(List.contains "(x:1, ?, q1)" sol.[4])
    Assert.True(List.contains "(x:1, q3, q4)" sol.[4])
    Assert.True(List.contains "(y:2, ?, q2)" sol.[4])
    Assert.True(List.contains "(y:2, q5, q4)" sol.[4])
    
    Assert.True(List.contains "(x:1, ?, q1)" sol.[6])
    Assert.True(List.contains "(x:1, q3, q4)" sol.[6])
    Assert.That(sol.[7], Is.Empty)

[<Test>]
let conditionWhile() =
    let source = "int x; while(x > 0) {x:=x-1;}"
    
    let graph = FrontEnd.compile source
    let (_, (_, edges)) = graph
    
    let analysis = ReachingDefinitionsAnalysis(edges)
    let worklist = StackWorklist.empty()
    let sol = analysis.analyse graph worklist |> extractSolution
    Assert.That(sol.Length, Is.EqualTo 5)
    Assert.That(sol.[0], Is.Empty)
    Assert.True(List.contains "(x:1, ?, q1)" sol.[1])
    Assert.True(List.contains "(x:1, q2, q1)" sol.[1])
    Assert.True(List.contains "(x:1, ?, q1)" sol.[2])
    Assert.True(List.contains "(x:1, q2, q1)" sol.[2])
    Assert.True(List.contains "(x:1, ?, q1)" sol.[3])
    Assert.True(List.contains "(x:1, q2, q1)" sol.[3])
