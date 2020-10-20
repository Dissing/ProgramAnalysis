module BitVector.Tests.ReachingDefinitions


open BitVector
open FrontEnd
open NUnit.Framework
[<Test>]
let assignVar () =
    let source = SourceFile("parseAssign.c", "int X; X:=5;")
    let (declarations, statements) = match Lexer.lex source.Content >>= Parser.parse >>= Resolution.resolve with
                                     | Ok ast -> ast
                                     | Error (s, _) -> failwith s
    
    let (_, graph) = EdgesFunction.runEdges (declarations, statements)
    
    let algorithm = new BitVector.ReachingDefinitions.RD()
    let result = AnalysisDefinition.Analyse graph declarations algorithm
    let sol = algorithm.getSolution()
    Assert.That(sol.Length, Is.EqualTo 4)
    Assert.That(sol.[0].[0], Is.EqualTo "(X:1, ?, q0)")
    Assert.That(sol.[1].[0], Is.EqualTo "(X:1, ?, q0)")
    Assert.That(sol.[2].[0], Is.EqualTo "(X:1, q1, q2)")
    Assert.That(sol.[3], Is.Empty)
    
    
[<Test>]
let assignArray () =
    let source = SourceFile("parseAssign.c", "int[2] X; X[1]:=5;")
    let (declarations, statements) = match Lexer.lex source.Content >>= Parser.parse >>= Resolution.resolve with
                                     | Ok ast -> ast
                                     | Error (s, _) -> failwith s
    
    let (_, graph) = EdgesFunction.runEdges (declarations, statements)
    
    let algorithm = new BitVector.ReachingDefinitions.RD()
    let result = AnalysisDefinition.Analyse graph declarations algorithm
    let sol = algorithm.getSolution()
    Assert.That(sol.Length, Is.EqualTo 4)
    Assert.That(sol.[0].[0], Is.EqualTo "(X:1, ?, q0)")
    Assert.That(sol.[1].[0], Is.EqualTo "(X:1, ?, q0)")
    Assert.True(List.contains "(X:1, q1, q2)" sol.[2])
    Assert.True(List.contains "(X:1, ?, q0)" sol.[2])
    Assert.That(sol.[3], Is.Empty)
    
    
[<Test>]
let assignStruct () =
    let source = SourceFile("parseAssign.c", "{int foo; int bar} S; S.foo:=5;")
    let (declarations, statements) = match Lexer.lex source.Content >>= Parser.parse >>= Resolution.resolve with
                                     | Ok ast -> ast
                                     | Error (s, _) -> failwith s
    
    let (_, graph) = EdgesFunction.runEdges (declarations, statements)
    
    let algorithm = new BitVector.ReachingDefinitions.RD()
    let result = AnalysisDefinition.Analyse graph declarations algorithm
    let sol = algorithm.getSolution()
    Assert.That(sol.Length, Is.EqualTo 4)
    Assert.True(List.contains "(S:1.foo, ?, q0)" sol.[0])
    Assert.True(List.contains "(S:1.bar, ?, q0)" sol.[0])
    Assert.True(List.contains "(S:1.foo, ?, q0)" sol.[1])
    Assert.True(List.contains "(S:1.bar, ?, q0)" sol.[1])
    Assert.True(List.contains "(S:1.bar, ?, q0)" sol.[2])
    Assert.True(List.contains "(S:1.foo, q1, q2)" sol.[2])
    Assert.That(sol.[3], Is.Empty)

    
[<Test>]
let assignFullStruct () =
    let source = SourceFile("parseAssign.c", "{int foo; int bar} S; S:={5,2};")
    let (declarations, statements) = match Lexer.lex source.Content >>= Parser.parse >>= Resolution.resolve with
                                     | Ok ast -> ast
                                     | Error (s, _) -> failwith s
    
    let (_, graph) = EdgesFunction.runEdges (declarations, statements)
    
    let algorithm = new BitVector.ReachingDefinitions.RD()
    let result = AnalysisDefinition.Analyse graph declarations algorithm
    let sol = algorithm.getSolution()
    Assert.That(sol.Length, Is.EqualTo 4)
    
    Assert.True(List.contains "(S:1.foo, ?, q0)" sol.[0])
    Assert.True(List.contains "(S:1.bar, ?, q0)" sol.[0])
    Assert.True(List.contains "(S:1.foo, ?, q0)" sol.[1])
    Assert.True(List.contains "(S:1.bar, ?, q0)" sol.[1])
    Assert.True(List.contains "(S:1.bar, q1, q2)" sol.[2])
    Assert.True(List.contains "(S:1.foo, q1, q2)" sol.[2])
    Assert.That(sol.[3], Is.Empty)
    
    
[<Test>]
let readNormal() =
    let source = SourceFile("parseAssign.c", "int x; read x;")
    let (declarations, statements) = match Lexer.lex source.Content >>= Parser.parse >>= Resolution.resolve with
                                     | Ok ast -> ast
                                     | Error (s, _) -> failwith s
    
    let (_, graph) = EdgesFunction.runEdges (declarations, statements)
    
    let algorithm = new BitVector.ReachingDefinitions.RD()
    let result = AnalysisDefinition.Analyse graph declarations algorithm
    let sol = algorithm.getSolution()
    Assert.That(sol.Length, Is.EqualTo 4)
    Assert.That(sol.[0].[0], Is.EqualTo "(x:1, ?, q0)")
    Assert.That(sol.[1].[0], Is.EqualTo "(x:1, ?, q0)")
    Assert.That(sol.[2].[0], Is.EqualTo "(x:1, q1, q2)")
    Assert.That(sol.[3], Is.Empty)

[<Test>]
let readArray() =
    let source = SourceFile("parseAssign.c", "int[2] x; read x[0];")
    let (declarations, statements) = match Lexer.lex source.Content >>= Parser.parse >>= Resolution.resolve with
                                     | Ok ast -> ast
                                     | Error (s, _) -> failwith s
    
    let (_, graph) = EdgesFunction.runEdges (declarations, statements)
    
    let algorithm = new BitVector.ReachingDefinitions.RD()
    let result = AnalysisDefinition.Analyse graph declarations algorithm
    let sol = algorithm.getSolution()
    
    Assert.That(sol.Length, Is.EqualTo 4)
    Assert.That(sol.[0].[0], Is.EqualTo "(x:1, ?, q0)")
    Assert.That(sol.[1].[0], Is.EqualTo "(x:1, ?, q0)")
    Assert.That(sol.[2].[0], Is.EqualTo "(x:1, q1, q2)")
    Assert.True(List.contains "(x:1, q1, q2)" sol.[2])
    Assert.That(sol.[3], Is.Empty)
    

[<Test>]
let readStruct() =
    let source = SourceFile("parseAssign.c", "{int foo; int bar} S; read S.foo;")
    let (declarations, statements) = match Lexer.lex source.Content >>= Parser.parse >>= Resolution.resolve with
                                     | Ok ast -> ast
                                     | Error (s, _) -> failwith s
    
    let (_, graph) = EdgesFunction.runEdges (declarations, statements)
    
    let algorithm = new BitVector.ReachingDefinitions.RD()
    let result = AnalysisDefinition.Analyse graph declarations algorithm
    let sol = algorithm.getSolution()
    Assert.That(sol.Length, Is.EqualTo 4)
    Assert.True(List.contains "(S:1.foo, ?, q0)" sol.[0])
    Assert.True(List.contains "(S:1.bar, ?, q0)" sol.[0])
    Assert.True(List.contains "(S:1.foo, ?, q0)" sol.[1])
    Assert.True(List.contains "(S:1.bar, ?, q0)" sol.[1])
    Assert.True(List.contains "(S:1.bar, ?, q0)" sol.[2])
    Assert.True(List.contains "(S:1.foo, q1, q2)" sol.[2])
    Assert.That(sol.[3], Is.Empty)

[<Test>]
let write() =
    let source = SourceFile("parseAssign.c", "int x; {int foo; int bar} S; write 5; write x; write S.foo;")
    let (declarations, statements) = match Lexer.lex source.Content >>= Parser.parse >>= Resolution.resolve with
                                     | Ok ast -> ast
                                     | Error (s, _) -> failwith s
    
    let (_, graph) = EdgesFunction.runEdges (declarations, statements)
    
    let algorithm = new BitVector.ReachingDefinitions.RD()
    let result = AnalysisDefinition.Analyse graph declarations algorithm
    let sol = algorithm.getSolution()
    Assert.That(sol.Length, Is.EqualTo 8)
    Assert.True(List.contains "(x:1, ?, q0)" sol.[0])
    Assert.True(List.contains "(S:2.foo, ?, q0)" sol.[0])
    Assert.True(List.contains "(S:2.bar, ?, q0)" sol.[0])
    
    Assert.True(List.contains "(x:1, ?, q0)" sol.[3])
    Assert.True(List.contains "(S:2.foo, ?, q0)" sol.[3])
    Assert.True(List.contains "(S:2.bar, ?, q0)" sol.[3])
    Assert.True(List.contains "(x:1, ?, q0)" sol.[4])
    Assert.True(List.contains "(S:2.foo, ?, q0)" sol.[4])
    Assert.True(List.contains "(S:2.bar, ?, q0)" sol.[4])
    Assert.True(List.contains "(x:1, ?, q0)" sol.[5])
    Assert.True(List.contains "(S:2.foo, ?, q0)" sol.[5])
    Assert.True(List.contains "(S:2.bar, ?, q0)" sol.[5])

[<Test>]
let conditionIf() =
    let source = SourceFile("parseAssign.c", "int x; if(x > 0) {x:=5;}")
    let (declarations, statements) = match Lexer.lex source.Content >>= Parser.parse >>= Resolution.resolve with
                                     | Ok ast -> ast
                                     | Error (s, _) -> failwith s
    
    let (_, graph) = EdgesFunction.runEdges (declarations, statements)
    
    let algorithm = new BitVector.ReachingDefinitions.RD()
    let result = AnalysisDefinition.Analyse graph declarations algorithm
    let sol = algorithm.getSolution()
    Assert.That(sol.Length, Is.EqualTo 5)
    Assert.True(List.contains "(x:1, ?, q0)" sol.[0])
    Assert.True(List.contains "(x:1, ?, q0)" sol.[1])
    Assert.True(List.contains "(x:1, ?, q0)" sol.[2])
    Assert.True(List.contains "(x:1, q2, q3)" sol.[3])
    Assert.That(sol.[4], Is.Empty)

[<Test>]
let conditionIfElse() =
    let source = SourceFile("parseAssign.c", "int x; int y; if(x > 0) {x:=5;} else{y:=2;}")
    let (declarations, statements) = match Lexer.lex source.Content >>= Parser.parse >>= Resolution.resolve with
                                     | Ok ast -> ast
                                     | Error (s, _) -> failwith s
    
    let (_, graph) = EdgesFunction.runEdges (declarations, statements)
    
    let algorithm = new BitVector.ReachingDefinitions.RD()
    let result = AnalysisDefinition.Analyse graph declarations algorithm
    let sol = algorithm.getSolution()
    Assert.That(sol.Length, Is.EqualTo 8)
    Assert.True(List.contains "(x:1, ?, q0)" sol.[0])
    Assert.True(List.contains "(y:2, ?, q0)" sol.[0])
    Assert.True(List.contains "(x:1, ?, q0)" sol.[4])
    Assert.True(List.contains "(x:1, q3, q4)" sol.[4])
    Assert.True(List.contains "(y:2, ?, q0)" sol.[4])
    Assert.True(List.contains "(y:2, q5, q4)" sol.[4])

[<Test>]
let conditionWhile() =
    let source = SourceFile("parseAssign.c", "int x; while(x > 0) {x:=x-1;}")
    let (declarations, statements) = match Lexer.lex source.Content >>= Parser.parse >>= Resolution.resolve with
                                     | Ok ast -> ast
                                     | Error (s, _) -> failwith s
    
    let (_, graph) = EdgesFunction.runEdges (declarations, statements)
    
    let algorithm = new BitVector.ReachingDefinitions.RD()
    let result = AnalysisDefinition.Analyse graph declarations algorithm
    let sol = algorithm.getSolution()
    Assert.That(sol.Length, Is.EqualTo 5)
    Assert.True(List.contains "(x:1, ?, q0)" sol.[0])
    Assert.True(List.contains "(x:1, ?, q0)" sol.[1])
    Assert.True(List.contains "(x:1, q2, q1)" sol.[1])
    Assert.True(List.contains "(x:1, ?, q0)" sol.[2])
    Assert.True(List.contains "(x:1, q2, q1)" sol.[2])
    Assert.True(List.contains "(x:1, ?, q0)" sol.[3])
    Assert.True(List.contains "(x:1, q2, q1)" sol.[3])
