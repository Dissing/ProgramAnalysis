module FrontEnd.Tests.Parser

open FrontEnd
open FrontEnd.AST
open NUnit.Framework

[<Test>]
let parseReadWrite () =
    let source = SourceFile("parseReadWrite.c", "read hello;\nwrite world;")
    let ast = Lexer.lex source.Content >>= Parser.parse
    let expected: Context<Declaration list * Statement list> = Ok([],[
        AST.Read(AST.Identifier("hello"))
        AST.Write(AST.Loc(AST.Identifier("world")))
    ])
    Assert.That(ast, Is.EqualTo(expected))
    
[<Test>]
let parseArithmetic1 () =
    let source = SourceFile("parseArithmetic.c", "z := x + 1;")
    let ast = Lexer.lex source.Content >>= Parser.parse
    
    let expected: Context<Declaration list * Statement list> = Ok([],[
        AST.Assign(AST.Identifier "z", AST.ArithmeticBinary (AST.Loc (AST.Identifier "x"), AST.Add, AST.IntLiteral(1)))
    ])
    Assert.That(ast, Is.EqualTo(expected))
    
[<Test>]
let parseArithmetic2 () =
    let source = SourceFile("parseArithmetic.c", "z := a + -b * (c + d);")
    let ast = Lexer.lex source.Content >>= Parser.parse
    
    let e1 = AST.ArithmeticBinary (AST.Loc (AST.Identifier "c"), AST.Add, AST.Loc (AST.Identifier "d"))
    let e2 = AST.ArithmeticUnary  (AST.Negation, AST.Loc (AST.Identifier "b"))
    let e3 = AST.ArithmeticBinary (e2, AST.Multiply, e1)
    let e4 = AST.ArithmeticBinary (AST.Loc (AST.Identifier "a"), AST.Add, e3)
    
    let expected: Context<Declaration list * Statement list> = Ok([],[
        AST.Assign(AST.Identifier "z", e4)
    ])
    Assert.That(ast, Is.EqualTo(expected))
    
[<Test>]
let parseArithmetic3 () =
    let source = SourceFile("parseArithmetic.c", "z := -1+x*2/(13-A[0]);")
    let ast = Lexer.lex source.Content >>= Parser.parse
    
    let e1 = AST.ArithmeticUnary (AST.Negation, (AST.IntLiteral 1))
    let e2 = AST.ArithmeticBinary ((AST.Loc (AST.Identifier "x")), AST.Multiply, (AST.IntLiteral 2))
    let e3 = AST.ArithmeticBinary ((AST.IntLiteral 13), AST.Subtract, (AST.Loc (AST.Array ("A", AST.IntLiteral 0))))
    let e4 = AST.ArithmeticBinary (e2, AST.Divide, e3)
    let e5 = AST.ArithmeticBinary (e1, AST.Add, e4)
    
    let expected: Context<Declaration list * Statement list> = Ok([],[
        AST.Assign(AST.Identifier "z", e5)
    ])
    Assert.That(ast, Is.EqualTo(expected))
    
[<Test>]
let parseIf () =
    let source = SourceFile("parseIf.c", "if (true) { x := 0; }")
    let ast = Lexer.lex source.Content >>= Parser.parse
    
    let then_block = ([], [AST.Assign(AST.Identifier "x", AST.IntLiteral 0)])

    let expected: Context<Declaration list * Statement list> = Ok([],[
        AST.If(AST.BooleanLiteral true, then_block, None)
    ])
    Assert.That(ast, Is.EqualTo(expected))
    
[<Test>]
let parseIfElse () =
    let source = SourceFile("parseIfElse.c", "if (true)\n{ x := 0; }\nelse { z := 0; }")
    let ast = Lexer.lex source.Content >>= Parser.parse
    
    let then_block = ([], [AST.Assign(AST.Identifier "x", AST.IntLiteral 0)])
    let else_block = ([], [AST.Assign(AST.Identifier "z", AST.IntLiteral 0)])

    let expected: Context<Declaration list * Statement list> = Ok([],[
        AST.If(AST.BooleanLiteral true, then_block, Some(else_block))
    ])
    Assert.That(ast, Is.EqualTo(expected))
    
[<Test>]
let parseWhile () =
    let source = SourceFile("parseWhile.c", "while (true) { x := 0; }")
    let ast = Lexer.lex source.Content >>= Parser.parse
    
    let then_block = ([], [AST.Assign(AST.Identifier "x", AST.IntLiteral 0)])

    let expected: Context<Declaration list * Statement list> = Ok([],[
        AST.While(AST.BooleanLiteral true, then_block)
    ])
    Assert.That(ast, Is.EqualTo(expected))
    
[<Test>]
let parseAndOr () =
    let source = SourceFile("parseBoolean.c", "if (true || false && !(true || false)) { x := 0; }")
    let tokens = Lexer.lex source.Content
    printfn "%A" tokens
    let ast = tokens >>= Parser.parse
    
    let e1 = AST.BooleanBinary (AST.BooleanLiteral true, AST.Or, AST.BooleanLiteral false)
    let e2 = AST.BooleanUnary (AST.Not, e1)
    let e3 = AST.BooleanBinary (AST.BooleanLiteral false, AST.And, e2)
    let e4 = AST.BooleanBinary (AST.BooleanLiteral true, AST.Or, e3)
    
    let then_block = ([], [AST.Assign(AST.Identifier "x", AST.IntLiteral 0)])

    let expected: Context<Declaration list * Statement list> = Ok([],[
        AST.If(e4, then_block, None)
    ])
    Assert.That(ast, Is.EqualTo(expected))
    
[<Test>]
let parseComparisons () =
    let source = SourceFile("parseComparisons.c", "if (0 > 0 && a < b && A[0] == A[1] && 1+2 != 4) { x := 0; }")
    let ast = Lexer.lex source.Content >>= Parser.parse
    
    let e1 = AST.Comparison (AST.IntLiteral 0, AST.ComparisonOperator.Greater, AST.IntLiteral 0)
    let e2 = AST.Comparison (AST.Loc (AST.Identifier "a"), AST.ComparisonOperator.Lesser, AST.Loc (AST.Identifier "b"))
    let e3 = AST.Comparison (AST.Loc (AST.Array("A", AST.IntLiteral 0)), AST.ComparisonOperator.Equal, AST.Loc (AST.Array("A", AST.IntLiteral 1)))
    let e4 = AST.Comparison (AST.ArithmeticBinary(AST.IntLiteral 1, AST.ArithmeticBinaryOperator.Add, AST.IntLiteral 2) , AST.ComparisonOperator.NotEqual, AST.IntLiteral 4)
    
    let e5 = AST.BooleanBinary(e1, AST.And, e2)
    let e6 = AST.BooleanBinary(e5, AST.And, e3)
    let e7 = AST.BooleanBinary(e6, AST.And, e4)
    
    let then_block = ([], [AST.Assign(AST.Identifier "x", AST.IntLiteral 0)])

    let expected: Context<Declaration list * Statement list> = Ok([],[
        AST.If(e7, then_block, None)
    ])
    Assert.That(ast, Is.EqualTo(expected))
    
[<Test>]
let parseIntDecl() =
    let source = SourceFile("parseIntDecl.c", "int x;\nint averylongandwindynameforpuretestingpurposes;")
    let ast = Lexer.lex source.Content >>= Parser.parse
    
    let d1 = AST.Integer "x"
    let d2 = AST.Integer "averylongandwindynameforpuretestingpurposes"
    
    let expected: Context<Declaration list * Statement list> = Ok([d1; d2],[])
    Assert.That(ast, Is.EqualTo(expected))

[<Test>]
let parseArrayDecl() =
    let source = SourceFile("parseArrayDecl.c", "int[0] x;\nint[10] y;\nint[128] foobar;\nint[2147483647] z;")
    let ast = Lexer.lex source.Content >>= Parser.parse
    
    let d1 = AST.ArrayDecl("x", 0)
    let d2 = AST.ArrayDecl("y", 10)
    let d3 = AST.ArrayDecl("foobar", 128)
    let d4 = AST.ArrayDecl("z", 2147483647)
    
    let expected: Context<Declaration list * Statement list> = Ok([d1; d2; d3; d4],[])
    Assert.That(ast, Is.EqualTo(expected))
    
[<Test>]
let parseStructLiteral() =
    let source = SourceFile("parseStructLiteral.c", "foo := {2+2, A[0], strct.fld};")
    let ast = Lexer.lex source.Content >>= Parser.parse
    
    
    let a1 = AST.ArithmeticBinary((AST.IntLiteral 2), AST.Add, (AST.IntLiteral 2))
    let a2 = AST.Loc( AST.Array("A", AST.IntLiteral 0))
    let a3 = AST.Loc( AST.Field("strct", "fld"))
    
    let assign = AST.StructAssign("foo", [("", a1); ("", a2); ("", a3)])
    
    let expected: Context<Declaration list * Statement list> = Ok([],[assign])
    
    Assert.That(ast, Is.EqualTo(expected))
    
[<Test>]
let parseStructDecl() =
    let source = SourceFile("parseStructDecl.c", "int x; {int x; int y; int z } strct; int y;")
    let ast = Lexer.lex source.Content >>= Parser.parse
    
    let di1 = AST.Integer "x"
    let di2 = AST.Integer "y"
    
    let fx = ("int", "x")
    let fy = ("int", "y")
    let fz = ("int", "z")
    
    let ds = AST.Struct ("strct", [fx;fy;fz])
    
    let expected: Context<Declaration list * Statement list> = Ok([di1; ds; di2],[])
    
    Assert.That(ast, Is.EqualTo(expected))
 
    
[<Test>]
let parseFieldAccess() =
    let source = SourceFile("parseFieldAccess.c", "x := strct.fld + 1; if (x.fst > y.snd) { x := 0; }")
    let ast = Lexer.lex source.Content >>= Parser.parse
    let assign = AST.Assign ((AST.Identifier "x"), AST.ArithmeticBinary(AST.Loc (AST.Field("strct", "fld")), AST.Add, AST.IntLiteral 1))
    let condition = AST.Comparison (AST.Loc (AST.Field("x","fst")), AST.Greater, AST.Loc (AST.Field("y", "snd")))
    let then_block = ([], [AST.Assign(AST.Identifier "x", AST.IntLiteral 0)])
    let expected: Context<Declaration list * Statement list> = Ok([],[
        assign; AST.If(condition, then_block, None)
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
    
    let declX = AST.Integer "x"
    let declY = AST.Integer "y"
    let decls = [declX; declY]
    
    let readX = AST.Read(AST.Identifier "x")
    let readY = AST.Read(AST.Identifier "y")
    let condition = AST.Comparison(AST.Loc(AST.Identifier "x"), AST.Greater, AST.Loc(AST.Identifier "y"))
    let assign = AST.Assign(AST.Identifier "x", AST.ArithmeticBinary(AST.Loc (AST.Identifier "x"), AST.Subtract, AST.Loc (AST.Identifier "y")))
    let block = AST.Block([], [assign])
    let while_ = AST.While(condition, block)
    let writeX = AST.Write (AST.Loc (AST.Identifier "x"))
    let stmts = [readX; readY; while_; writeX]
    let expected: Context<Declaration list * Statement list> = Ok(decls, stmts)
    
    Assert.That(ast, Is.EqualTo(expected))
