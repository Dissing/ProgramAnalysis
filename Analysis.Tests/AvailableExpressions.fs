module Analysis.Tests.AvailableExpressions

open Analysis
open Analysis.Analyses
open Analysis.Worklists
open FrontEnd
open FrontEnd.AST
open NUnit.Framework


[<Test>]
let availableExpressions1 () =
    let source = """
        int a;
        int b;
        int x;
        int y;
        x := a + b;
        y := a * x;
        while (y > a+b) {
          a := a + 1;
          x := a + b;
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
    
    let analysis = AvailableExpressionsAnalysis(graph)
    let (solution,_) = analysis.analyse graph worklist
    
    let expected = [
        []; //Initial
        []; //int a
        []; //int b
        []; //int x
        []; //int y
        [ArithmeticBinary(a, Add, b)]; //x := a + b
        [ArithmeticBinary(a, Add, b)]; //y := a * x
        [ArithmeticBinary(a, Add, b)]; //y > a+b
        []; //a := a + 1
        [ArithmeticBinary(a, Add, b)]; //x := a + b
        [ArithmeticBinary(a, Add, b)]; //Free y
        [ArithmeticBinary(a, Add, b)]; //Free x
        []; //Free b
        []; //Free a
    ]
    
    let solution = solution |> Map.toList |> List.map (fun (_,v) -> Set.toList v)
    Assert.That(solution, Is.EqualTo(expected))
    
[<Test>]
let availableExpressions2 () =
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
    
    let analysis = AvailableExpressionsAnalysis(graph)
    let (solution,_) = analysis.analyse graph worklist
    
    let expected = [
        []; //Initial
        []; //int[12] A
        []; //int i
        []; //int j
        [ArithmeticBinary(i, Add, IntLiteral 1); ArithmeticBinary(j, Add, IntLiteral 1)]; //A[i+1] = j+1
        [ArithmeticBinary(j, Add, IntLiteral 1); ArithmeticBinary(j, Add, IntLiteral 2)]; //i = A[j+2]
        []; //Free j
        []; //Free i
        []; //Free A
    ]
    
    let solution = solution |> Map.toList |> List.map (fun (_,v) -> Set.toList v)
    Assert.That(solution, Is.EqualTo(expected))
    
