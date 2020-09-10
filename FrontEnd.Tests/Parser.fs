module FrontEnd.Tests.Parser

open FrontEnd
open NUnit.Framework

[<Test>]
let parseReadWrite () =
    let source = SourceFile("parseReadWrite.c", "read hello;\nwrite world;")
    let ast = Parser.parse source (Lexer.lex source.Content)
    let expected = ([],[
        AST.Read(AST.Identifier("hello"))
        AST.Write(AST.Loc(AST.Identifier("world")))
    ])
    Assert.That(ast, Is.EqualTo(expected))
    
[<Test>]
let parseArithmetic1 () =
    let source = SourceFile("parseArithmetic.c", "z := x + 1;")
    let ast = Parser.parse source (Lexer.lex source.Content)
    
    let expected = ([],[
        AST.Assign(AST.Identifier "z", AST.ArithmeticBinary (AST.Loc (AST.Identifier "x"), AST.Add, AST.Literal(1)))
    ])
    Assert.That(ast, Is.EqualTo(expected))
    
[<Test>]
let parseArithmetic2 () =
    let source = SourceFile("parseArithmetic.c", "z := a + -b * (c + d);")
    let ast = Parser.parse source (Lexer.lex source.Content)
    
    let e1 = AST.ArithmeticBinary (AST.Loc (AST.Identifier "c"), AST.Add, AST.Loc (AST.Identifier "d"))
    let e2 = AST.ArithmeticUnary  (AST.Negation, AST.Loc (AST.Identifier "b"))
    let e3 = AST.ArithmeticBinary (e2, AST.Multiply, e1)
    let e4 = AST.ArithmeticBinary (AST.Loc (AST.Identifier "a"), AST.Add, e3)
    
    let expected = ([],[
        AST.Assign(AST.Identifier "z", e4)
    ])
    Assert.That(ast, Is.EqualTo(expected))
    
[<Test>]
let parseArithmetic3 () =
    let source = SourceFile("parseArithmetic.c", "z := -1+x*2/(13-A[0]);")
    let ast = Parser.parse source (Lexer.lex source.Content)
    
    let e1 = AST.ArithmeticUnary (AST.Negation, (AST.Literal 1))
    let e2 = AST.ArithmeticBinary ((AST.Loc (AST.Identifier "x")), AST.Multiply, (AST.Literal 2))
    let e3 = AST.ArithmeticBinary ((AST.Literal 13), AST.Subtract, (AST.Loc (AST.Array ("A", AST.Literal 0))))
    let e4 = AST.ArithmeticBinary (e2, AST.Divide, e3)
    let e5 = AST.ArithmeticBinary (e1, AST.Add, e4)
    
    let expected = ([],[
        AST.Assign(AST.Identifier "z", e5)
    ])
    Assert.That(ast, Is.EqualTo(expected))
