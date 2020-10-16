module BitVector.Tests.ReachingDefinitions


open BitVector
open FrontEnd
open NUnit.Framework
[<Test>]
let assignVar () =
    let source = SourceFile("parseAssign.c", "int X; X:=5;")
    let (declarations, statements) = Parser.parse source (Lexer.lex source.Content)
    let graph = EdgesFunction.runEdges (declarations, statements)
    
    let algorithm = new BitVector.ReachingDefinitions.RD()
    let result = AnalysisDefinition.Analyse graph declarations algorithm
    let sol = algorithm.getSolution()
    Assert.That(sol.Length, Is.EqualTo 2)
    Assert.That(sol.[0].[0], Is.EqualTo "(X, ?, q0)")
    Assert.That(sol.[1].[0], Is.EqualTo "(X, q0, q1)")
    
    
[<Test>]
let assignArray () =
    let source = SourceFile("parseAssign.c", "int[2] X; X[1]:=5;")
    let (declarations, statements) = Parser.parse source (Lexer.lex source.Content)
    let graph = EdgesFunction.runEdges (declarations, statements)
    
    let algorithm = new BitVector.ReachingDefinitions.RD()
    let result = AnalysisDefinition.Analyse graph declarations algorithm
    let sol = algorithm.getSolution()
    Assert.That(sol.Length, Is.EqualTo 2)
    Assert.That(sol.[0].[0], Is.EqualTo "(X, ?, q0)")
    Assert.True(List.contains "(X, q0, q1)" sol.[1])
    Assert.True(List.contains "(X, ?, q0)" sol.[1])
    
    
[<Test>]
let assignStruct () =
    let source = SourceFile("parseAssign.c", "{int foo; int bar} S; S.foo:=5;")
    let (declarations, statements) = Parser.parse source (Lexer.lex source.Content)
    let graph = EdgesFunction.runEdges (declarations, statements)
    
    let algorithm = new BitVector.ReachingDefinitions.RD()
    let result = AnalysisDefinition.Analyse graph declarations algorithm
    let sol = algorithm.getSolution()
    Assert.That(sol.Length, Is.EqualTo 2)
    Assert.True(List.contains "(S.foo, ?, q0)" sol.[0])
    Assert.True(List.contains "(S.bar, ?, q0)" sol.[0])
    Assert.True(List.contains "(S.bar, ?, q0)" sol.[1])
    Assert.True(List.contains "(S.foo, q0, q1)" sol.[1])
    
[<Test>]
let assignFullStruct () =
    let source = SourceFile("parseAssign.c", "{int foo; int bar} S; S:={5,2};")
    let (declarations, statements) = Parser.parse source (Lexer.lex source.Content)
    let graph = EdgesFunction.runEdges (declarations, statements)
    
    let algorithm = new BitVector.ReachingDefinitions.RD()
    let result = AnalysisDefinition.Analyse graph declarations algorithm
    let sol = algorithm.getSolution()
    Assert.That(sol.Length, Is.EqualTo 2)
    Assert.True(List.contains "(S.foo, ?, q0)" sol.[0])
    Assert.True(List.contains "(S.bar, ?, q0)" sol.[0])
    Assert.True(List.contains "(S.bar, q0, q1)" sol.[1])
    Assert.True(List.contains "(S.foo, q0, q1)" sol.[1])
    
    
[<Test>]
let readNormal() =
    let source = SourceFile("parseAssign.c", "int x; read x;")
    let (declarations, statements) = Parser.parse source (Lexer.lex source.Content)
    let graph = EdgesFunction.runEdges (declarations, statements)
    
    let algorithm = new BitVector.ReachingDefinitions.RD()
    let result = AnalysisDefinition.Analyse graph declarations algorithm
    let sol = algorithm.getSolution()
    Assert.That(sol.Length, Is.EqualTo 2)
    Assert.True(List.contains "(x, ?, q0)" sol.[0])
    Assert.True(List.contains "(x, q0, q1)" sol.[1])

[<Test>]
let readArray() =
    let source = SourceFile("parseAssign.c", "int[2] x; read x[0];")
    let (declarations, statements) = Parser.parse source (Lexer.lex source.Content)
    let graph = EdgesFunction.runEdges (declarations, statements)
    
    let algorithm = new BitVector.ReachingDefinitions.RD()
    let result = AnalysisDefinition.Analyse graph declarations algorithm
    let sol = algorithm.getSolution()
    Assert.That(sol.Length, Is.EqualTo 2)
    Assert.True(List.contains "(x, ?, q0)" sol.[0])
    Assert.True(List.contains "(x, q0, q1)" sol.[1])
    Assert.True(List.contains "(x, ?, q0)" sol.[1])

[<Test>]
let readStruct() =
    let source = SourceFile("parseAssign.c", "{int foo; int bar} S; read S.foo;")
    let (declarations, statements) = Parser.parse source (Lexer.lex source.Content)
    let graph = EdgesFunction.runEdges (declarations, statements)
    
    let algorithm = new BitVector.ReachingDefinitions.RD()
    let result = AnalysisDefinition.Analyse graph declarations algorithm
    let sol = algorithm.getSolution()
    Assert.That(sol.Length, Is.EqualTo 2)
    Assert.True(List.contains "(S.foo, ?, q0)" sol.[0])
    Assert.True(List.contains "(S.bar, ?, q0)" sol.[0])
    Assert.True(List.contains "(S.bar, ?, q0)" sol.[1])
    Assert.True(List.contains "(S.foo, q0, q1)" sol.[1])
    
[<Test>]
let write() =
    let source = SourceFile("parseAssign.c", "int x; {int foo; int bar} S; write 5; write x; write S.foo;")
    let (declarations, statements) = Parser.parse source (Lexer.lex source.Content)
    let graph = EdgesFunction.runEdges (declarations, statements)
    
    let algorithm = new BitVector.ReachingDefinitions.RD()
    let result = AnalysisDefinition.Analyse graph declarations algorithm
    let sol = algorithm.getSolution()
    Assert.That(sol.Length, Is.EqualTo 4)
    Assert.True(List.contains "(x, ?, q0)" sol.[0])
    Assert.True(List.contains "(S.foo, ?, q0)" sol.[0])
    Assert.True(List.contains "(S.bar, ?, q0)" sol.[0])
    Assert.True(List.contains "(x, ?, q0)" sol.[1])
    Assert.True(List.contains "(S.foo, ?, q0)" sol.[1])
    Assert.True(List.contains "(S.bar, ?, q0)" sol.[1])
    Assert.True(List.contains "(x, ?, q0)" sol.[2])
    Assert.True(List.contains "(S.foo, ?, q0)" sol.[2])
    Assert.True(List.contains "(S.bar, ?, q0)" sol.[2])
    Assert.True(List.contains "(x, ?, q0)" sol.[3])
    Assert.True(List.contains "(S.foo, ?, q0)" sol.[3])
    Assert.True(List.contains "(S.bar, ?, q0)" sol.[3])

let conditionIf() =
    let source = SourceFile("parseAssign.c", "int x; if(x > 0) {x:=5;}")
    let (declarations, statements) = Parser.parse source (Lexer.lex source.Content)
    let graph = EdgesFunction.runEdges (declarations, statements)
    
    let algorithm = new BitVector.ReachingDefinitions.RD()
    let result = AnalysisDefinition.Analyse graph declarations algorithm
    let sol = algorithm.getSolution()
    Assert.That(sol.Length, Is.EqualTo 3)
    Assert.True(List.contains "(x, ?, q0)" sol.[0])
    Assert.True(List.contains "(x, ?, q0)" sol.[1])
    Assert.True(List.contains "(x, ?, q0)" sol.[2])
    Assert.True(List.contains "(x, q1, q2)" sol.[2])
let conditionIfElse() =
    let source = SourceFile("parseAssign.c", "int x; int; y; if(x > 0) {x:=5;} else{y:=2;}")
    let (declarations, statements) = Parser.parse source (Lexer.lex source.Content)
    let graph = EdgesFunction.runEdges (declarations, statements)
    
    let algorithm = new BitVector.ReachingDefinitions.RD()
    let result = AnalysisDefinition.Analyse graph declarations algorithm
    let sol = algorithm.getSolution()
    Assert.That(sol.Length, Is.EqualTo 4)
    Assert.True(List.contains "(x, ?, q0)" sol.[0])
    Assert.True(List.contains "(y, ?, q0)" sol.[0])
    Assert.True(List.contains "(x, ?, q0)" sol.[3])
    Assert.True(List.contains "(x, q1, q3)" sol.[3])
    Assert.True(List.contains "(y, ?, q0)" sol.[3])
    Assert.True(List.contains "(y, q2, q3)" sol.[3])
let conditionWhile() =
    let source = SourceFile("parseAssign.c", "int x; while(x > 0) {x:=x-1;}")
    let (declarations, statements) = Parser.parse source (Lexer.lex source.Content)
    let graph = EdgesFunction.runEdges (declarations, statements)
    
    let algorithm = new BitVector.ReachingDefinitions.RD()
    let result = AnalysisDefinition.Analyse graph declarations algorithm
    let sol = algorithm.getSolution()
    Assert.That(sol.Length, Is.EqualTo 3)
    Assert.True(List.contains "(x, ?, q0)" sol.[0])
    Assert.True(List.contains "(y, ?, q0)" sol.[0])
    Assert.True(List.contains "(x, ?, q0)" sol.[2])
    Assert.True(List.contains "(x, q1, q0)" sol.[2])
