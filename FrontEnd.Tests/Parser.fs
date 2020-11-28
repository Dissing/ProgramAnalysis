module FrontEnd.Tests.Parser

open FrontEnd
open FrontEnd.AST
open NUnit.Framework

[<Test>]
let parseReadWrite () =
    let source = SourceFile("parseReadWrite.c", "read hello;\nwrite world;")
    let ast = Lexer.lex source.Content >>= Parser.parse
    let expected: Context<Statement list> = Ok([
        Read(Identifier("hello"))
        Write(Loc(Identifier("world")))
    ])
    Assert.That(ast, Is.EqualTo(expected))
    
[<Test>]
let parseArithmetic1 () =
    let source = SourceFile("parseArithmetic.c", "z := x + 1;")
    let ast = Lexer.lex source.Content >>= Parser.parse
    
    let expected: Context<Statement list> = Ok([
        Assign(Identifier "z", ArithmeticBinary (Loc (Identifier "x"), Add, IntLiteral(1)))
    ])
    Assert.That(ast, Is.EqualTo(expected))
    
[<Test>]
let parseArithmetic2 () =
    let source = SourceFile("parseArithmetic.c", "z := a + -b * (c + d);")
    let ast = Lexer.lex source.Content >>= Parser.parse
    
    let e1 = ArithmeticBinary (Loc (Identifier "c"), Add, Loc (Identifier "d"))
    let e2 = ArithmeticUnary  (Negative, Loc (Identifier "b"))
    let e3 = ArithmeticBinary (e2, Multiply, e1)
    let e4 = ArithmeticBinary (Loc (Identifier "a"), Add, e3)
    
    let expected: Context<Statement list> = Ok([
        Assign(Identifier "z", e4)
    ])
    Assert.That(ast, Is.EqualTo(expected))
    
[<Test>]
let parseArithmetic3 () =
    let source = SourceFile("parseArithmetic.c", "z := -1+x*2/(13-A[0]);")
    let ast = Lexer.lex source.Content >>= Parser.parse
    
    let e1 = ArithmeticUnary (Negative, (IntLiteral 1))
    let e2 = ArithmeticBinary ((Loc (Identifier "x")), Multiply, (IntLiteral 2))
    let e3 = ArithmeticBinary ((IntLiteral 13), Subtract, (Loc (Array ("A", IntLiteral 0))))
    let e4 = ArithmeticBinary (e2, Divide, e3)
    let e5 = ArithmeticBinary (e1, Add, e4)
    
    let expected: Context<Statement list> = Ok([
        Assign(Identifier "z", e5)
    ])
    Assert.That(ast, Is.EqualTo(expected))
    
[<Test>]
let parseIf () =
    let source = SourceFile("parseIf.c", "if (true) { x := 0; }")
    let ast = Lexer.lex source.Content >>= Parser.parse
    
    let thenBlock = [Assign(Identifier "x", IntLiteral 0)]

    let expected: Context<Statement list> = Ok([
        If(BooleanLiteral true, thenBlock, None)
    ])
    Assert.That(ast, Is.EqualTo(expected))
    
[<Test>]
let parseIfElse () =
    let source = SourceFile("parseIfElse.c", "if (true)\n{ x := 0; }\nelse { z := 0; }")
    let ast = Lexer.lex source.Content >>= Parser.parse
    
    let thenBlock = [Assign(Identifier "x", IntLiteral 0)]
    let elseBlock = [Assign(Identifier "z", IntLiteral 0)]

    let expected: Context<Statement list> = Ok([
        If(BooleanLiteral true, thenBlock, Some(elseBlock))
    ])
    Assert.That(ast, Is.EqualTo(expected))
    
[<Test>]
let parseWhile () =
    let source = SourceFile("parseWhile.c", "while (true) { x := 0; }")
    let ast = Lexer.lex source.Content >>= Parser.parse
    
    let loopBlock = [Assign(Identifier "x", IntLiteral 0)]

    let expected: Context<Statement list> = Ok([
        While(BooleanLiteral true, loopBlock)
    ])
    Assert.That(ast, Is.EqualTo(expected))
    
[<Test>]
let parseAndOr () =
    let source = SourceFile("parseBoolean.c", "if (true || false && !(true || false)) { x := 0; }")
    let tokens = Lexer.lex source.Content
    printfn "%A" tokens
    let ast = tokens >>= Parser.parse
    
    let e1 = BooleanBinary (BooleanLiteral true, Or, BooleanLiteral false)
    let e2 = BooleanUnary (Not, e1)
    let e3 = BooleanBinary (BooleanLiteral false, And, e2)
    let e4 = BooleanBinary (BooleanLiteral true, Or, e3)
    
    let thenBlock = [Assign(Identifier "x", IntLiteral 0)]

    let expected: Context<Statement list> = Ok([
        If(e4, thenBlock, None)
    ])
    Assert.That(ast, Is.EqualTo(expected))
    
[<Test>]
let parseComparisons () =
    let source = SourceFile("parseComparisons.c", "if (0 > 0 && a < b && A[0] == A[1] && 1+2 != 4) { x := 0; }")
    let ast = Lexer.lex source.Content >>= Parser.parse
    
    let e1 = Comparison (IntLiteral 0, ComparisonOperator.Greater, IntLiteral 0)
    let e2 = Comparison (Loc (Identifier "a"), ComparisonOperator.Lesser, Loc (Identifier "b"))
    let e3 = Comparison (Loc (Array("A", IntLiteral 0)), ComparisonOperator.Equal, Loc (Array("A", IntLiteral 1)))
    let e4 = Comparison (ArithmeticBinary(IntLiteral 1, ArithmeticBinaryOperator.Add, IntLiteral 2) , ComparisonOperator.NotEqual, IntLiteral 4)
    
    let e5 = BooleanBinary(e1, And, e2)
    let e6 = BooleanBinary(e5, And, e3)
    let e7 = BooleanBinary(e6, And, e4)
    
    let thenBlock = [Assign(Identifier "x", IntLiteral 0)]

    let expected: Context<Statement list> = Ok([
        If(e7, thenBlock, None)
    ])
    Assert.That(ast, Is.EqualTo(expected))
    
[<Test>]
let parseIntDecl() =
    let source = SourceFile("parseIntDecl.c", "int x;\nint averylongandwindynameforpuretestingpurposes;")
    let ast = Lexer.lex source.Content >>= Parser.parse
    
    let d1 = Allocate(Integer "x")
    let d2 = Allocate(Integer "averylongandwindynameforpuretestingpurposes")
    
    let expected: Context<Statement list> = Ok( [d1; d2])
    Assert.That(ast, Is.EqualTo(expected))

[<Test>]
let parseArrayDecl() =
    let source = SourceFile("parseArrayDecl.c", "int[0] x;\nint[10] y;\nint[128] foobar;\nint[2147483647] z;")
    let ast = Lexer.lex source.Content >>= Parser.parse
    
    let d1 = Allocate(ArrayDecl("x", 0))
    let d2 = Allocate(ArrayDecl("y", 10))
    let d3 = Allocate(ArrayDecl("foobar", 128))
    let d4 = Allocate(ArrayDecl("z", 2147483647))
    
    let expected: Context<Statement list> = Ok([d1; d2; d3; d4])
    Assert.That(ast, Is.EqualTo(expected))
    
[<Test>]
let parseStructLiteral() =
    let source = SourceFile("parseStructLiteral.c", "foo := {2+2, A[0], strct.fld};")
    let ast = Lexer.lex source.Content >>= Parser.parse
    
    
    let a1 = ArithmeticBinary((IntLiteral 2), Add, (IntLiteral 2))
    let a2 = Loc( Array("A", IntLiteral 0))
    let a3 = Loc( Field("strct", "fld"))
    
    let assign = StructAssign("foo", [("", a1); ("", a2); ("", a3)])
    
    let expected: Context<Statement list> = Ok([assign])
    
    Assert.That(ast, Is.EqualTo(expected))
    
[<Test>]
let parseStructDecl() =
    let source = SourceFile("parseStructDecl.c", "int x; {int x; int y; int z } strct; int y;")
    let ast = Lexer.lex source.Content >>= Parser.parse
    
    let di1 = Allocate(Integer "x")
    let di2 = Allocate(Integer "y")
    
    let ds = Allocate(Struct("strct", ["x";"y";"z"]))
    
    let expected: Context<Statement list> = Ok( [di1; ds; di2])
    
    Assert.That(ast, Is.EqualTo(expected))
 
    
[<Test>]
let parseFieldAccess() =
    let source = SourceFile("parseFieldAccess.c", "x := strct.fld + 1; if (x.fst > y.snd) { x := 0; }")
    let ast = Lexer.lex source.Content >>= Parser.parse
    let assign = Assign ((Identifier "x"), ArithmeticBinary(Loc (Field("strct", "fld")), Add, IntLiteral 1))
    let condition = Comparison (Loc (Field("x","fst")), Greater, Loc (Field("y", "snd")))
    let thenBlock = [Assign(Identifier "x", IntLiteral 0)]
    let expected: Context<Statement list> = Ok([
        assign; If(condition, thenBlock, None)
    ])
    
    Assert.That(ast, Is.EqualTo(expected))
    
[<Test>]
let parseModuloProgram() =
    let source = SourceFile("parseModulo.c", """
                            int x;
                            int y;
                            read x;
                            read y;
                            while (x > y) {
                              x := x - y;
                            }
                            write x;
                            """)
    let ast = Lexer.lex source.Content >>= Parser.parse
    
    let declX = Allocate(Integer "x")
    let declY = Allocate(Integer "y")
    
    let readX = Read(Identifier "x")
    let readY = Read(Identifier "y")
    let condition = Comparison(Loc(Identifier "x"), Greater, Loc(Identifier "y"))
    let assign = Assign(Identifier "x", ArithmeticBinary(Loc (Identifier "x"), Subtract, Loc (Identifier "y")))
    let block = [assign]
    let while_ = While(condition, block)
    let writeX = Write (Loc (Identifier "x"))
    let stmts = [declX; declY; readX; readY; while_; writeX]
    let expected: Context<Statement list> = Ok( stmts)
    
    Assert.That(ast, Is.EqualTo(expected))
