module FrontEnd.Tests.Resolution

open FrontEnd
open FrontEnd.AST
open NUnit.Framework

[<Test>]
let resolveIntDecl() =
    let source = SourceFile("resolveIntDecl.c", "int x;\nint averylongandwindynameforpuretestingpurposes;")
    let ast = Lexer.lex source.Content >>= Parser.parse >>= Resolution.resolve
    
    let n1 = "x:1"
    let n2 = "averylongandwindynameforpuretestingpurposes:2"
    
    let d1 = VarDecl n1
    let d2 = VarDecl n2
    
    let decls = [(n1, d1); (n2, d2)]
    let stmts = [Allocate(d1); Allocate(d2); Free(d2); Free(d1)]
    
    let expected: Context<DeclarationInfo * Statement list> = Ok(Map.ofList decls, stmts)
    Assert.That(ast, Is.EqualTo(expected))
    
[<Test>]
let resolveArrayDecl() =
    let source = SourceFile("resolveArrayDecl.c", "int[12] x;\nint[10] y;")
    let ast = Lexer.lex source.Content >>= Parser.parse >>= Resolution.resolve
    
    let n1 = "x:1"
    let n2 = "y:2"
    
    let d1 = ArrayDecl(n1, 12)
    let d2 = ArrayDecl(n2, 10)
    
    let decls = [(n1,d1); (n2,d2)]
    let stmts = [Allocate(d1); Allocate(d2); Free(d2); Free(d1)]
    
    let expected: Context<DeclarationInfo * Statement list> = Ok(Map.ofList decls, stmts)
    Assert.That(ast, Is.EqualTo(expected))
    
[<Test>]
let resolveStructDecl() =
    let source = SourceFile("resolveStructDecl.c", "{int x; int y; int z } A; {int x } B;")
    let ast = Lexer.lex source.Content >>= Parser.parse >>= Resolution.resolve
    
    let na = "A:1"
    let nb = "B:2"
    
    let da = RecordDecl (na, ["x"; "y"; "z"])
    let db = RecordDecl (nb, ["x"])
    
    let decls = [(na, da); (nb, db)]
    let stmts = [Allocate(da); Allocate(db); Free(db); Free(da)]
    
    let expected: Context<DeclarationInfo * Statement list> = Ok(Map.ofList decls, stmts)
    Assert.That(ast, Is.EqualTo(expected))
    

[<Test>]
let resolveInts() =
    let source = SourceFile("resolveWhile.c", "int x; int y; x := 12; y := 42;")
    let ast = Lexer.lex source.Content >>= Parser.parse >>= Resolution.resolve
    
    let n1 = "x:1"
    let n2 = "y:2"
    
    let d1 = VarDecl n1
    let d2 = VarDecl n2
        
    let decls = [(n1, d1); (n2, d2)]
    let stmts = [Allocate(d1); Allocate(d2); Assign(Variable n1, IntLiteral 12); Assign(Variable n2, IntLiteral 42); Free(d2); Free(d1)]
    
    let expected: Context<DeclarationInfo * Statement list> = Ok(Map.ofList decls, stmts)
    Assert.That(ast, Is.EqualTo(expected))

[<Test>]
let resolveArithmeticExpr() =
    let source = SourceFile("resolveArithmetic.c", "int x; int y; int[10] A; {int z} B; x := x + y * A[x+y] - B.z;")
    let ast = Lexer.lex source.Content >>= Parser.parse >>= Resolution.resolve
    
    let n1 = "x:1"
    let n2 = "y:2"
    let n3 = "A:3"
    let n4 = "B:4"
    
    let d1 = VarDecl n1
    let d2 = VarDecl n2
    let d3 = ArrayDecl(n3, 10)
    let d4 = RecordDecl(n4,["z"])
        
    let decls = [(n1, d1); (n2, d2); (n3, d3); (n4, d4)]
    
    let idx_expr = ArithmeticBinary(Loc(Variable n1), Add, Loc(Variable n2))
    let e1 = ArithmeticBinary(Loc(Variable n2), Multiply, Loc(Array(n3,idx_expr)))
    let e2 = ArithmeticBinary(Loc(Variable n1), Add, e1)
    let e3 = ArithmeticBinary(e2, Subtract, Loc(Field(n4, "z")))
    let stmts = [Allocate(d1); Allocate(d2); Allocate(d3); Allocate(d4); Assign(Variable n1, e3); Free(d4); Free(d3); Free(d2); Free(d1)]
    
    let expected: Context<DeclarationInfo * Statement list> = Ok(Map.ofList decls, stmts)
    Assert.That(ast, Is.EqualTo(expected))
    
[<Test>]
let resolveBooleanExpr() =
    let source = SourceFile("resolveBoolean.c", "int x; int y; while (x > y) {}")
    let ast = Lexer.lex source.Content >>= Parser.parse >>= Resolution.resolve
    
    let n1 = "x:1"
    let n2 = "y:2"
    
    let d1 = VarDecl n1
    let d2 = VarDecl n2
        
    let decls = [(n1,d1); (n2,d2)]
    
    let e = Comparison(Loc(Variable n1), Greater, Loc(Variable(n2)))
    let stmts = [Allocate(d1); Allocate(d2); While(e,[]); Free(d2); Free(d1)]
    
    let expected: Context<DeclarationInfo * Statement list> = Ok(Map.ofList decls, stmts)
    Assert.That(ast, Is.EqualTo(expected))
    
[<Test>]
let resolveShadowing() =
    let source = SourceFile("resolveShadowing.c", "int x; x := x+1; while (true) {int x; x := x+2;} x := x+3;")
    let ast = Lexer.lex source.Content >>= Parser.parse >>= Resolution.resolve
    
    let n1 = "x:1"
    let n2 = "x:2"
    
    let d1 = VarDecl n1
    let d2 = VarDecl n2
 
    let preAssign =  Assign(Variable n1, ArithmeticBinary(Loc(Variable n1), Add, IntLiteral 1))
    let loopBlock = [Allocate(d2); Assign(Variable n2, ArithmeticBinary(Loc(Variable n2), Add, IntLiteral 2)); Free(d2)]
    let postAssign =  Assign(Variable n1, ArithmeticBinary(Loc(Variable n1), Add, IntLiteral 3))
    let decls = [(n1, d1); (n2, d2)]
    let stmts = [Allocate(d1); preAssign; While(BooleanLiteral(true), loopBlock); postAssign; Free(d1)]
    let expected: Context<DeclarationInfo * Statement list> = Ok(Map.ofList decls, stmts)
    Assert.That(ast, Is.EqualTo(expected))
    
[<Test>]
let resolveStructLiteral() =
    let source = SourceFile("parseStructLiteral.c", "int x;\n{int x} foo;\nfoo := {x};")
    let ast = Lexer.lex source.Content >>= Parser.parse >>= Resolution.resolve
    
    let n1 = "x:1"
    let n2 = "foo:2"
    
    let d1 = VarDecl n1
    let d2 = RecordDecl(n2, ["x"])
    
    let decls = [(n1, d1); (n2, d2)]
    let stmts = [Allocate(d1); Allocate(d2); RecordAssign(n2, [("x", Loc(Variable n1))]); Free(d2); Free(d1)]
    
    let expected: Context<DeclarationInfo * Statement list> = Ok(Map.ofList decls,stmts)
    
    Assert.That(ast, Is.EqualTo(expected))