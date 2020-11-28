module FrontEnd.Tests.Printer

open FrontEnd;
open FrontEnd.ProgramGraph;
open FrontEnd.AST;
open NUnit.Framework



[<Test>]
let PrinterBool () =
    let statement = (BooleanBinary ((BooleanBinary (BooleanUnary (Not, BooleanLiteral true), Or, BooleanLiteral true)), And, (BooleanBinary (BooleanLiteral true, Or, BooleanUnary (Not, BooleanLiteral false)))))

    let expectedRes = "(!True || True) && (True || !False)"
    let (res, _) = FrontEnd.Printer.BooleanPrinter "" statement

    Assert.That(res, Is.EqualTo(expectedRes))
    
[<Test>]
let PrinterArith () =
    let statement = (ArithmeticBinary (ArithmeticBinary (ArithmeticUnary(Negative, Loc (Identifier "Var")), Subtract, (ArithmeticUnary(Negative, Loc (Field ("R", "fst"))))), Divide, (IntLiteral 10)))

    let expectedRes = "((-Var) - (-R.fst)) / 10"
    let (res, _) = FrontEnd.Printer.ArithmeticPrinter "" statement

    Assert.That(res, Is.EqualTo(expectedRes))
    
[<Test>]
let PrinterBoolArith () =
    let statement = (Comparison (ArithmeticBinary (ArithmeticUnary(Negative, Loc (Identifier "Var")), Subtract, (ArithmeticUnary(Negative, Loc (Field ("R", "fst"))))), GreaterEqual, (IntLiteral 10)))

    let expectedRes = "((-Var) - (-R.fst)) >= 10"
    let (res, _) = FrontEnd.Printer.BooleanPrinter "" statement

    Assert.That(res, Is.EqualTo(expectedRes))
    

