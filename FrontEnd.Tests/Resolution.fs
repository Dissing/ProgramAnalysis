module FrontEnd.Tests.Resolution

open FrontEnd
open FrontEnd.AST
open NUnit.Framework

[<Test>]
let resolveIntDecl() =
    let source = SourceFile("resolveIntDecl.c", "int x;\nint averylongandwindynameforpuretestingpurposes;")
    let ast = Lexer.lex source.Content >>= Parser.parse >>= Resolution.resolve
    
    let d1 = Integer "x:1"
    let d2 = Integer "averylongandwindynameforpuretestingpurposes:2"
    
    let expected: Context<Declaration list * Statement list> = Ok([d1; d2],[])
    Assert.That(ast, Is.EqualTo(expected))
    
[<Test>]
let resolveArrayDecl() =
    let source = SourceFile("resolveArrayDecl.c", "int[12] x;\nint[10] y;")
    let ast = Lexer.lex source.Content >>= Parser.parse >>= Resolution.resolve
    
    let d1 = ArrayDecl("x:1", 12)
    let d2 = ArrayDecl("y:2", 10)
    
    let expected: Context<Declaration list * Statement list> = Ok([d1; d2],[])
    Assert.That(ast, Is.EqualTo(expected))
    
[<Test>]
let resolveStructDecl() =
    let source = SourceFile("resolveStructDecl.c", "{int x; int y; int z } A; {int x } B;")
    let ast = Lexer.lex source.Content >>= Parser.parse >>= Resolution.resolve
    
    let da = Struct ("A:1", [("int", "x"); ("int", "y"); ("int", "z")])
    let db = Struct ("B:2", [("int", "x")])
    
    let expected: Context<Declaration list * Statement list> = Ok([da; db],[])
    Assert.That(ast, Is.EqualTo(expected))
    

[<Test>]
let resolveInts() =
    let source = SourceFile("resolveWhile.c", "int x; x := 12; int y; y := 42;")
    let ast = Lexer.lex source.Content >>= Parser.parse >>= Resolution.resolve
        
    let decls = [Integer "x:1"; Integer "y:2"]
    let stmts = [Assign(Identifier "x:1", IntLiteral 12); Assign(Identifier "y:2", IntLiteral 42)]
    
    let expected: Context<Declaration list * Statement list> = Ok(decls, stmts)
    Assert.That(ast, Is.EqualTo(expected))

[<Test>]
let resolveArithmeticExpr() =
    let source = SourceFile("resolveArithmetic.c", "int x; int y; int[10] A; {int z} B; x := x + y * A[x+y] - B.z;")
    let ast = Lexer.lex source.Content >>= Parser.parse >>= Resolution.resolve
        
    let decls = [Integer "x:1"; Integer "y:2"; ArrayDecl("A:3", 10); Struct("B:4",[("int","z")])]
    
    let idx_expr = ArithmeticBinary(Loc(Identifier "x:1"), Add, Loc(Identifier "y:2"))
    let e1 = ArithmeticBinary(Loc(Identifier "y:2"), Multiply, Loc(Array("A:3",idx_expr)))
    let e2 = ArithmeticBinary(Loc(Identifier "x:1"), Add, e1)
    let e3 = ArithmeticBinary(e2, Subtract, Loc(Field("B:4","z")))
    let stmts = [Assign(Identifier "x:1", e3)]
    
    let expected: Context<Declaration list * Statement list> = Ok(decls, stmts)
    Assert.That(ast, Is.EqualTo(expected))
    
[<Test>]
let resolveBooleanExpr() =
    let source = SourceFile("resolveBoolean.c", "int x; int y; while (x > y) {}")
    let ast = Lexer.lex source.Content >>= Parser.parse >>= Resolution.resolve
        
    let decls = [Integer "x:1"; Integer "y:2"]
    
    let e = Comparison(Loc(Identifier "x:1"), Greater, Loc(Identifier("y:2")))
    let stmts = [While(e,([],[]))]
    
    let expected: Context<Declaration list * Statement list> = Ok(decls, stmts)
    Assert.That(ast, Is.EqualTo(expected))
    
[<Test>]
let resolveShadowing() =
    let source = SourceFile("resolveShadowing.c", "int x; x := x+1; while (true) {int x; x := x+2;} x := x+3;")
    let ast = Lexer.lex source.Content >>= Parser.parse >>= Resolution.resolve
 
    let preAssign =  Assign(Identifier "x:1", ArithmeticBinary(Loc(Identifier "x:1"), Add, IntLiteral 1))
    let loopBlock = (
                        [Integer "x:2"],
                        [Assign(Identifier "x:2", ArithmeticBinary(Loc(Identifier "x:2"), Add, IntLiteral 2))]
                    )       
    let postAssign =  Assign(Identifier "x:1", ArithmeticBinary(Loc(Identifier "x:1"), Add, IntLiteral 3))
    let stmts = [preAssign; While(BooleanLiteral(true),loopBlock); postAssign]
    let expected: Context<Declaration list * Statement list> = Ok([Integer "x:1"], stmts)
    Assert.That(ast, Is.EqualTo(expected))
    
[<Test>]
let parseStructLiteral() =
    let source = SourceFile("parseStructLiteral.c", "int x;\n{int x} foo;\nfoo := {x};")
    let ast = Lexer.lex source.Content >>= Parser.parse >>= Resolution.resolve
    
    let decls = [Integer "x:1"; Struct("foo:2", [("int", "x")])]
    let stmts = [StructAssign("foo:2", [("x", Loc(Identifier "x:1"))])]
    
    let expected: Context<Declaration list * Statement list> = Ok(decls,stmts)
    
    Assert.That(ast, Is.EqualTo(expected))