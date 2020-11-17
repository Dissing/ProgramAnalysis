module Analysis.Tests.FaintVariables

open Analysis
open Analysis.Analyses
open Analysis.Worklists
open FrontEnd
open FrontEnd.AST
open NUnit.Framework


[<Test>]
let faintVariables1 () =
    let source = SourceFile("faintVariables.c", """
        int x;
        int y;
        int z;
        int w;
        x := 42;
        y := x;
        z := y;
        w := x;
        write w;
        """)
    let ast = Lexer.lex source.Content >>= Parser.parse >>= Resolution.resolve
    let pg = EdgesFunction.runEdges (ast.unwrap())
    
    let x = AmalgamatedLocation.Variable("x:1");
    let w = AmalgamatedLocation.Variable("w:4");
    
    let worklist = StackWorklist.empty()
    
    let analysis = FaintVariableAnalysis()
    let (solution,_) = analysis.analyse pg worklist
    
    let expected = [
        [] //End
        [] //free x
        [] //free y
        [] //free z
        [] //free w
        [w] //write w;
        [x] //w := x;
        [x] //z := y;
        [x] //y := x;
        [] //x := 42;
        [] //int w;
        [] //int z;
        [] //int y;
        [] //int x;
    ]
    
    let solution = solution |> Map.toList |> List.map (fun (k,v) -> Set.toList v) |> List.rev
    Assert.That(solution, Is.EqualTo(expected))