module Analysis.Tests.VeryBusyExpressions

open Analysis
open Analysis.Analyses
open Analysis.Worklists
open FrontEnd
open FrontEnd.AST
open NUnit.Framework


[<Test>]
let busyExpressions1 () =
    let source = """
        int a;
        int b;
        int x;
        int y;
        if (a > b) {
          x := b-a;
          y := a-b;
        }
        else {
          y := b-a;
          x := a-b;
        }
        """
    let graph = FrontEnd.compile source
    
    let na = "a:1"
    let nb = "b:2"
    let nx = "x:3"
    
    let a = Loc (Variable na)
    let b = Loc (Variable nb)
    let x = Loc (Variable nx)
    
    let worklist = StackWorklist.empty()
    
    let analysis = VeryBusyExpressionsAnalysis(graph)
    let (solution,_) = analysis.analyse graph worklist
    
    let expected = [
        []; //int a
        []; //int b
        [ArithmeticBinary(a, Subtract, b); ArithmeticBinary(b, Subtract, a)]; //int x
        [ArithmeticBinary(a, Subtract, b); ArithmeticBinary(b, Subtract, a)]; //int y
        [ArithmeticBinary(a, Subtract, b); ArithmeticBinary(b, Subtract, a)]; //a > b
        [ArithmeticBinary(a, Subtract, b); ArithmeticBinary(b, Subtract, a)]; //x := b - a
        [ArithmeticBinary(a, Subtract, b)]; //y := a - b
        []; //Free y
        [ArithmeticBinary(a, Subtract, b); ArithmeticBinary(b, Subtract, a)]; //y := b - a
        [ArithmeticBinary(a, Subtract, b)]; //x := a - b
        []; //Free x
        []; //Free b
        []; //Free a
        []; //End
    ]
    
    let solution = solution |> Map.toList |> List.map (fun (_,v) -> Set.toList v)
    Assert.That(solution, Is.EqualTo(expected))
    
[<Test>]
let busyExpressions2 () =
    let source = """
        int[12] A;
        int i;
        int j;
        A[i+1] := j+1;
        i := A[j+2];
        """
    let graph = FrontEnd.compile source
    
    let nA = "A:1"
    let ni = "i:2"
    let nj = "j:3"
    
    let A = Loc (Variable nA)
    let i = Loc (Variable ni)
    let j = Loc (Variable nj)
    
    let worklist = StackWorklist.empty()
    
    let analysis = VeryBusyExpressionsAnalysis(graph)
    let (solution,_) = analysis.analyse graph worklist
    
    let expected = [
        []; //int[12] A
        []; //int i
        [ArithmeticBinary(i, Add, IntLiteral 1)]; //int j
        [ArithmeticBinary(i, Add, IntLiteral 1); ArithmeticBinary(j, Add, IntLiteral 1); ArithmeticBinary(j, Add, IntLiteral 2)]; //A[i+1] = j+1
        [ArithmeticBinary(j, Add, IntLiteral 2)]; //i = A[j+2]
        []; //Free j
        []; //Free i
        []; //Free A
        []; //End
    ]
    
    let solution = solution |> Map.toList |> List.map (fun (_,v) -> Set.toList v)
    Assert.That(solution, Is.EqualTo(expected))
    
