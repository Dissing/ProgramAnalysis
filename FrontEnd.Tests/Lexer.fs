module FrontEnd.Tests.Lexer

open FrontEnd
open NUnit.Framework

[<Test>]
let lexReadWrite () =
    let code = "read hello;\nwrite world;"
    let expected = [
      (READ, {From = 0; To = 3});
      (IDENTIFIER("hello"), {From = 5; To = 9})
      (SEMI_COLON, {From = 10; To = 10});
      (WRITE, {From = 12; To = 16});
      (IDENTIFIER("world"), {From = 18; To = 22});
      (SEMI_COLON, {From = 23; To = 23});
    ]
    let tokens = Lexer.lex code
    Assert.That(List.toArray tokens, Is.EqualTo(List.toArray expected))

[<Test>]
let lexArithmetic () =
    let code = "z := 1+x*2/(13-A[0]);"
    let expected = [
      (IDENTIFIER("z"), {From = 0; To = 0});
      (ASSIGN, {From = 2; To = 3});
      (INTEGER(1), {From = 5; To = 5});
      (PLUS, {From = 6; To = 6});
      (IDENTIFIER("x"), {From = 7; To = 7});
      (MULTIPLICATION, {From = 8; To = 8});
      (INTEGER(2), {From = 9; To = 9});
      (DIVISION, {From = 10; To = 10});
      (LEFT_PAREN, {From = 11; To = 11});
      (INTEGER(13), {From = 12; To = 13});
      (MINUS, {From = 14; To = 14});
      (IDENTIFIER("A"), {From = 15; To = 15});
      (LEFT_SQUARE, {From = 16; To = 16});
      (INTEGER(0), {From = 17; To = 17});
      (RIGHT_SQUARE, {From = 18; To = 18});
      (RIGHT_PAREN, {From = 19; To = 19});
      (SEMI_COLON, {From = 20; To = 20});
    ]
    let tokens = Lexer.lex code
    Assert.That(List.toArray tokens, Is.EqualTo(List.toArray expected))
    
[<Test>]
let lexKeywords () =
    let code = "read write int if else while"
    let expected = [
      (READ, {From = 0; To = 3});
      (WRITE, {From = 5; To = 9});
      (INT, {From = 11; To = 13});
      (IF, {From = 15; To = 16});
      (ELSE, {From = 18; To = 21});
      (WHILE, {From = 23; To = 27});
    ]
    let tokens = Lexer.lex code
    Assert.That(List.toArray tokens, Is.EqualTo(List.toArray expected))