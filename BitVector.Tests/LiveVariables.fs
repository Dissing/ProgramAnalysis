module BitVector.Tests.LiveVariables

open BitVector
open FrontEnd
open NUnit.Framework
[<Test>]
let assignVarConst () =
    let source = SourceFile("parseAssign.c", "int X; X:=5;")
    let (declarations, statements) = Parser.parse source (Lexer.lex source.Content)
    let graph = EdgesFunction.runEdges (declarations, statements)
    
    let algorithm = new BitVector.LiveVariables.LV()
    let result = AnalysisDefinition.Analyse graph declarations algorithm
    let sol = algorithm.getSolution()
    Assert.That(sol.Length, Is.EqualTo 2)
    Assert.That(sol.[0], Is.Empty)
    Assert.That(sol.[1], Is.Empty)
    
[<Test>]
let assignVar () =
    let source = SourceFile("parseAssign.c", "int X; int Y; X:=Y; Y:=X;")
    let (declarations, statements) = Parser.parse source (Lexer.lex source.Content)
    let graph = EdgesFunction.runEdges (declarations, statements)
    
    let algorithm = new BitVector.LiveVariables.LV()
    let result = AnalysisDefinition.Analyse graph declarations algorithm
    let sol = algorithm.getSolution()
    Assert.That(sol.Length, Is.EqualTo 3)
    Assert.That(sol.[0].[0], Is.EqualTo "Y")
    Assert.That(sol.[1].[0], Is.EqualTo "X")
    Assert.That(sol.[2], Is.Empty)
    
[<Test>]
let assignVarSelf () =
    let source = SourceFile("parseAssign.c", "int X; X:=X;")
    let (declarations, statements) = Parser.parse source (Lexer.lex source.Content)
    let graph = EdgesFunction.runEdges (declarations, statements)
    
    let algorithm = new BitVector.LiveVariables.LV()
    let result = AnalysisDefinition.Analyse graph declarations algorithm
    let sol = algorithm.getSolution()
    Assert.That(sol.Length, Is.EqualTo 2)
    Assert.That(sol.[0].[0], Is.EqualTo "X")
    Assert.That(sol.[1], Is.Empty)
    
[<Test>]
let assignArray () =
    let source = SourceFile("parseAssign.c", "int[2] X; int Z; X[1]:=5; Z:= X[0];")
    let (declarations, statements) = Parser.parse source (Lexer.lex source.Content)
    let graph = EdgesFunction.runEdges (declarations, statements)
    
    let algorithm = new BitVector.LiveVariables.LV()
    let result = AnalysisDefinition.Analyse graph declarations algorithm
    let sol = algorithm.getSolution()
    Assert.That(sol.Length, Is.EqualTo 3)
    Assert.That(sol.[0].[0], Is.EqualTo "X")
    Assert.That(sol.[1].[0], Is.EqualTo "X")
    Assert.That(sol.[2], Is.Empty)
    
    
[<Test>]
let assignStruct () =
    let source = SourceFile("parseAssign.c", "{int foo; int bar} S; S.foo:=5; S.bar := S.foo;")
    let (declarations, statements) = Parser.parse source (Lexer.lex source.Content)
    let graph = EdgesFunction.runEdges (declarations, statements)
    
    let algorithm = new BitVector.LiveVariables.LV()
    let result = AnalysisDefinition.Analyse graph declarations algorithm
    let sol = algorithm.getSolution()
    Assert.That(sol.Length, Is.EqualTo 3)
    Assert.That(sol.[0], Is.Empty)
    Assert.That(sol.[1].[0], Is.EqualTo "S.foo")
    Assert.That(sol.[2], Is.Empty)
    
[<Test>]
let assignFullStruct () =
    let source = SourceFile("parseAssign.c", "{int foo; int bar} S; S:={S.bar,S.foo};S:={0,0}; S:={S.bar,S.foo};")
    let (declarations, statements) = Parser.parse source (Lexer.lex source.Content)
    let graph = EdgesFunction.runEdges (declarations, statements)
    
    let algorithm = new BitVector.LiveVariables.LV()
    let algorithm = new BitVector.ReachingDefinitions.RD()
    let result = AnalysisDefinition.Analyse graph declarations algorithm
    let sol = algorithm.getSolution()
    Assert.That(sol.Length, Is.EqualTo 4)
    Assert.True(List.contains "S.foo" sol.[0])
    Assert.True(List.contains "S.bar" sol.[0])
    Assert.That(sol.[1], Is.Empty)
    Assert.True(List.contains "S.foo" sol.[2])
    Assert.True(List.contains "S.bar" sol.[2])
    Assert.That(sol.[3], Is.Empty)
    
    
[<Test>]
let readNormal() =
    let source = SourceFile("parseAssign.c", "int x; int z; read x; z := x;")
    let (declarations, statements) = Parser.parse source (Lexer.lex source.Content)
    let graph = EdgesFunction.runEdges (declarations, statements)
    
    let algorithm = new BitVector.LiveVariables.LV()
    let result = AnalysisDefinition.Analyse graph declarations algorithm
    let sol = algorithm.getSolution()
    Assert.That(sol.Length, Is.EqualTo 3)
    Assert.That(sol.[0], Is.Empty)
    Assert.That(sol.[1].[0], Is.EqualTo "x")
    Assert.That(sol.[2], Is.Empty)

[<Test>]
let readArray() =
    let source = SourceFile("parseAssign.c", "int[2] x; read x[0]; x[0] := x[1];")
    let (declarations, statements) = Parser.parse source (Lexer.lex source.Content)
    let graph = EdgesFunction.runEdges (declarations, statements)
    
    let algorithm = new BitVector.LiveVariables.LV()
    let result = AnalysisDefinition.Analyse graph declarations algorithm
    let sol = algorithm.getSolution()
    Assert.That(sol.Length, Is.EqualTo 3)
    Assert.That(sol.[0].[0], Is.EqualTo "x")
    Assert.That(sol.[1].[0], Is.EqualTo "x")
    Assert.That(sol.[2], Is.Empty)

[<Test>]
let readStruct() =
    let source = SourceFile("parseAssign.c", "{int foo; int bar} S; read S.foo; S.bar:= S.foo;")
    let (declarations, statements) = Parser.parse source (Lexer.lex source.Content)
    let graph = EdgesFunction.runEdges (declarations, statements)
    
    let algorithm = new BitVector.LiveVariables.LV()
    let result = AnalysisDefinition.Analyse graph declarations algorithm
    let sol = algorithm.getSolution()
    Assert.That(sol.Length, Is.EqualTo 3)
    Assert.That(sol.[0], Is.Empty)
    Assert.That(sol.[1].[0], Is.EqualTo "S.foo")
    Assert.That(sol.[2], Is.Empty)
    
[<Test>]
let write() =
    let source = SourceFile("parseAssign.c", "int x; int[2] y; {int foo; int bar} S; write y[1]+5-x+S.foo;")
    let (declarations, statements) = Parser.parse source (Lexer.lex source.Content)
    let graph = EdgesFunction.runEdges (declarations, statements)
    
    let algorithm = new BitVector.LiveVariables.LV()
    let result = AnalysisDefinition.Analyse graph declarations algorithm
    let sol = algorithm.getSolution()
    Assert.That(sol.Length, Is.EqualTo 2)
    Assert.True(List.contains "S.foo" sol.[0])
    Assert.True(List.contains "y" sol.[0])
    Assert.True(List.contains "x" sol.[0])
    Assert.That(sol.[1], Is.Empty)


[<Test>]
let conditionIf() =
    let source = SourceFile("parseAssign.c", "int x; int y; if(y > 0) {x:=x;}")
    let (declarations, statements) = Parser.parse source (Lexer.lex source.Content)
    let graph = EdgesFunction.runEdges (declarations, statements)
    
    let algorithm = new BitVector.LiveVariables.LV()
    let result = AnalysisDefinition.Analyse graph declarations algorithm
    let sol = algorithm.getSolution()
    Assert.That(sol.Length, Is.EqualTo 3)
    Assert.True(List.contains "y" sol.[0])
    Assert.True(List.contains "x" sol.[0])
    Assert.True(List.contains "x" sol.[1])
    Assert.That(sol.[2], Is.Empty)

[<Test>]
let conditionIfElse() =
    let source = SourceFile("parseAssign.c", "int x; int y; if(5 > 0) {x:=y;} else{y:=x;}")
    let (declarations, statements) = Parser.parse source (Lexer.lex source.Content)
    let graph = EdgesFunction.runEdges (declarations, statements)
    
    let algorithm = new BitVector.LiveVariables.LV()
    let result = AnalysisDefinition.Analyse graph declarations algorithm
    let sol = algorithm.getSolution()
    Assert.That(sol.Length, Is.EqualTo 4)
    Assert.True(List.contains "x" sol.[0])
    Assert.True(List.contains "y" sol.[0])
    Assert.True(List.contains "y" sol.[1])
    Assert.True(List.contains "x" sol.[2])
    Assert.That(sol.[3], Is.Empty)

[<Test>]
let conditionWhile() =
    let source = SourceFile("parseAssign.c", "int x; while(x > 0) {x:=x-1;}")
    let (declarations, statements) = Parser.parse source (Lexer.lex source.Content)
    let graph = EdgesFunction.runEdges (declarations, statements)
    
    let algorithm = new BitVector.LiveVariables.LV()
    let result = AnalysisDefinition.Analyse graph declarations algorithm
    let sol = algorithm.getSolution()
    Assert.That(sol.Length, Is.EqualTo 3)
    Assert.True(List.contains "x" sol.[0])
    Assert.True(List.contains "x" sol.[1])
    Assert.That(sol.[2], Is.Empty)

