module Analysis.Tests.DangerousVariables

open Analysis
open Analysis.worklists
open FrontEnd
open FrontEnd.AST
open NUnit.Framework


[<Test>]
let dangerousVariables1 () =
    let source = SourceFile("dangerousVariables1.c", """
        int x;
        int y;
        int[12] A;
        y := 0;
        A[0] := 0;
        y := x * 2;
        x := 0;
        y := x * 2;
        """)
    let ast = Lexer.lex source.Content >>= Parser.parse >>= Resolution.resolve
    let pg = EdgesFunction.runEdges (ast.unwrap())
    
    let x = AmalgamatedLocation.Variable("x:1");
    let y = AmalgamatedLocation.Variable("y:2");
    let A = AmalgamatedLocation.Array("A:3");
    
    let initials = Set.ofList [x;y;A]
    let worklist = StackWorklist.empty()
    
    let analysis = DangerousVariableAnalysis()
    let solution = analysis.analyse pg worklist initials
    
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
    
    let solution = solution |> Map.toList |> List.map (fun (k,v) -> v)
    Assert.That(solution, Is.EqualTo(expected))
    
    
    
