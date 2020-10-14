module BitVector.Tests.ReachingDefinitions


open BitVector
open FrontEnd
open NUnit.Framework
[<Test>]
let assignVar () =
    let source = SourceFile("parseAssign.c", "int X; X:=5;")
    let (declerations, statements) = Parser.parse source (Lexer.lex source.Content)
    let graph = EdgesFunction.runEdges (declerations, statements)
    let algorithm = new BitVector.ReachingDefinitions.RD()
    let result = AnalysisDefinition.Analyse graph declerations algorithm
    let sol = algorithm.getSolution()
    
    Assert.Equals(sol.Length, 1)
    Assert.Equals(sol.[0], "q0, X, q1")
    
    
    
    
    
    
    
