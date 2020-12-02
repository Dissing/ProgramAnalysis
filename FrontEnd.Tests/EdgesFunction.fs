module FrontEnd.Tests.EdgesFunction

open NUnit.Framework
open FrontEnd
open FrontEnd.AST
open FrontEnd.ProgramGraph
open NUnit.Framework




[<Test>]
let EdgesIfThen () =
    let stmts = [
        Statement.Assign(Location.Variable "a", ArithmeticExpr.IntLiteral 10) ;
        Statement.If(
            Comparison ((Loc (Variable "x")), Greater, (Loc (Variable "y"))),
            [
                Statement.Assign(Location.Variable "x", ArithmeticBinary ((Loc (Variable "x")), Subtract, (IntLiteral 1)))
            ],
            None
        ) ;
        Statement.Assign(Location.Variable "b", ArithmeticExpr.IntLiteral 10)
    ]
        
    let expectedNodes = [0;1;2;3;4]
    let expectedEdges = [
        (0, Action.Assign (Variable "a", IntLiteral 10), 1);
        (1, Action.Condition (Comparison (Loc (Variable "x"), Greater, Loc (Variable "y"))), 2);
        (2, Action.Assign (Variable "x", ArithmeticBinary (Loc (Variable "x"), Subtract, IntLiteral 1)), 3);
        (1, Action.Condition (BooleanUnary (Not, Comparison (Loc (Variable "x"), Greater, Loc (Variable "y")))), 3);
        (3, Action.Assign (Variable "b", IntLiteral 10), 4)
    ]  

    let (_, (nodes, edges)) = EdgesFunction.runEdges (Map.empty, stmts)
   
    Assert.That(List.toArray nodes, Is.EqualTo(List.toArray expectedNodes))
    Assert.That(List.toArray edges, Is.EqualTo(List.toArray expectedEdges))

[<Test>]
let EdgesInnerIfThen () =
    let stmts =
         [ Statement.Assign(Location.Variable "y", IntLiteral 1) ;
           Statement.While
               (Comparison((Loc(Variable "x")), Greater, IntLiteral 0),
                 [ Statement.If
                     (Comparison((Loc(Variable "x")), Greater, IntLiteral 0),
                       [ Statement.Assign
                           (Location.Variable "x", ArithmeticBinary(Loc(Variable "x"), Subtract, IntLiteral 1)) ],
                       None) ]) ;
           Statement.Assign(Location.Variable "b", ArithmeticExpr.IntLiteral 10) ]

    let expectedNodes = [ 0; 1; 2; 3; 4; 5 ]
    let expectedEdges =
        [ (0, Action.Assign(Variable "y", IntLiteral 1), 1)
          (1, Action.Condition(Comparison(Loc(Variable "x"), Greater, IntLiteral 0)), 2)
          (2, Action.Condition(Comparison(Loc(Variable "x"), Greater, IntLiteral 0)), 3)
          (3, Action.Assign(Variable "x", ArithmeticBinary(Loc(Variable "x"), Subtract, IntLiteral 1)), 1)
          (2, Action.Condition(BooleanUnary(Not, Comparison(Loc(Variable "x"), Greater, IntLiteral 0))), 1)
          (1, Action.Condition(BooleanUnary(Not, Comparison(Loc(Variable "x"), Greater, IntLiteral 0))), 4)
          (4, Action.Assign(Variable "b", IntLiteral 10), 5) ]

    let (_, (nodes, edges)) = EdgesFunction.runEdges (Map.empty, stmts)
    Assert.That(List.toArray nodes, Is.EqualTo(List.toArray expectedNodes))
    Assert.That(List.toArray edges, Is.EqualTo(List.toArray expectedEdges))

[<Test>]
let EdgesIfThenElse () =
    let stmts =
         [ Statement.Assign(Location.Variable "a", ArithmeticExpr.IntLiteral 10)
           Statement.If
               (Comparison((Loc(Variable "x")), Greater, (Loc(Variable "y"))),
                 [ Statement.Assign
                     (Location.Variable "x", ArithmeticBinary((Loc(Variable "x")), Subtract, (IntLiteral 1))) ],
                 Some
                     [ Statement.Assign
                         (Location.Variable "y", ArithmeticBinary((Loc(Variable "y")), Subtract, (IntLiteral 1))) ])
           Statement.Assign(Location.Variable "b", ArithmeticExpr.IntLiteral 10) ]

    let expectedNodes = [ 0; 1; 2; 3; 4; 5 ]
    let expectedEdges =
        [ (0, Action.Assign(Variable "a", IntLiteral 10), 1)
          (1, Action.Condition(Comparison(Loc(Variable "x"), Greater, Loc(Variable "y"))), 2)
          (2, Action.Assign(Variable "x", ArithmeticBinary(Loc(Variable "x"), Subtract, IntLiteral 1)), 3)
          (1, Action.Condition(BooleanUnary(Not, Comparison(Loc(Variable "x"), Greater, Loc(Variable "y")))), 4)
          (4, Action.Assign(Variable "y", ArithmeticBinary(Loc(Variable "y"), Subtract, IntLiteral 1)), 3)
          (3, Action.Assign(Variable "b", IntLiteral 10), 5) ]

    let (_, (nodes, edges)) = EdgesFunction.runEdges (Map.empty, stmts)
    Assert.That(List.toArray nodes, Is.EqualTo(List.toArray expectedNodes))
    Assert.That(List.toArray edges, Is.EqualTo(List.toArray expectedEdges))

[<Test>]
let EdgesInnerIfThenElse () =
    let stmts =
         [ Statement.Assign(Location.Variable "y", ArithmeticExpr.IntLiteral 1)
           Statement.While
               (Comparison((Loc(Variable "x")), Greater, IntLiteral 0),
                 [ Statement.Assign(Location.Variable "a", ArithmeticExpr.IntLiteral 10)
                   Statement.If
                       (Comparison((Loc(Variable "x")), Greater, (Loc(Variable "y"))),
                         [ Statement.Assign
                             (Location.Variable "x", ArithmeticBinary((Loc(Variable "x")), Subtract, (IntLiteral 1))) ],
                         Some
                             [ Statement.Assign
                                 (Location.Variable "y",
                                  ArithmeticBinary((Loc(Variable "y")), Subtract, (IntLiteral 1))) ])
                   Statement.Assign(Location.Variable "b", ArithmeticExpr.IntLiteral 10) ])
           Statement.Assign(Location.Variable "c", ArithmeticExpr.IntLiteral 10) ]

    let expectedNodes = [ 0; 1; 2; 3; 4; 5; 6; 7; 8 ]
    let expectedEdges =
        [ (0, Action.Assign(Variable "y", IntLiteral 1), 1)
          (1, Action.Condition(Comparison(Loc(Variable "x"), Greater, IntLiteral 0)), 2)
          (2, Action.Assign(Variable "a", IntLiteral 10), 3)
          (3, Action.Condition(Comparison(Loc(Variable "x"), Greater, Loc(Variable "y"))), 4)
          (4, Action.Assign(Variable "x", ArithmeticBinary(Loc(Variable "x"), Subtract, IntLiteral 1)), 5)
          (3, Action.Condition(BooleanUnary(Not, Comparison(Loc(Variable "x"), Greater, Loc(Variable "y")))), 6)
          (6, Action.Assign(Variable "y", ArithmeticBinary(Loc(Variable "y"), Subtract, IntLiteral 1)), 5)
          (5, Action.Assign(Variable "b", IntLiteral 10), 1)
          (1, Condition(BooleanUnary(Not, Comparison(Loc(Variable "x"), Greater, IntLiteral 0))), 7)
          (7, Action.Assign(Variable "c", IntLiteral 10), 8) ]

    let (_, (nodes, edges)) = EdgesFunction.runEdges (Map.empty, stmts)
    Assert.That(List.toArray nodes, Is.EqualTo(List.toArray expectedNodes))
    Assert.That(List.toArray edges, Is.EqualTo(List.toArray expectedEdges))


[<Test>]
let EdgesWhile () =
    let stmts =
         [ Statement.Assign(Location.Variable "a", ArithmeticExpr.IntLiteral 10)
           Statement.Assign(Location.Variable "y", ArithmeticExpr.IntLiteral 1)
           Statement.While
               (Comparison((Loc(Variable "x")), Greater, IntLiteral 0),
                 [ Statement.Assign
                     (Location.Variable "y", ArithmeticBinary(Loc(Variable "x"), Multiply, Loc(Variable "y")))
                   Statement.Assign
                       (Location.Variable "x", ArithmeticBinary(Loc(Variable "x"), Subtract, IntLiteral 1)) ])
           Statement.Assign(Location.Variable "b", ArithmeticExpr.IntLiteral 10) ]

    let expectedNodes = [ 0; 1; 2; 3; 4; 5; 6 ]
    let expectedEdges =
        [ (0, Action.Assign(Variable "a", IntLiteral 10), 1)
          (1, Action.Assign(Variable "y", IntLiteral 1), 2)
          (2, Action.Condition(Comparison(Loc(Variable "x"), Greater, IntLiteral 0)), 3)
          (3, Action.Assign(Variable "y", ArithmeticBinary(Loc(Variable "x"), Multiply, (Loc(Variable "y")))), 4)
          (4, Action.Assign(Variable "x", ArithmeticBinary(Loc(Variable "x"), Subtract, IntLiteral 1)), 2)
          (2, Action.Condition(BooleanUnary(Not, Comparison(Loc(Variable "x"), Greater, IntLiteral 0))), 5)
          (5, Action.Assign(Variable "b", IntLiteral 10), 6) ]

    let (_, (nodes, edges)) = EdgesFunction.runEdges (Map.empty, stmts)
    Assert.That(List.toArray nodes, Is.EqualTo(List.toArray expectedNodes))
    Assert.That(List.toArray edges, Is.EqualTo(List.toArray expectedEdges))

[<Test>]
let EdgesInnerWhile () =
    let stmts =
         [ Statement.Assign(Location.Variable "y", ArithmeticExpr.IntLiteral 1)
           Statement.If
               (Comparison(Loc(Variable "x"), Greater, Loc(Variable "y")),
                 [ Statement.Assign
                     (Location.Variable "x", ArithmeticBinary((Loc(Variable "x")), Subtract, (IntLiteral 1))) ],
                 Some
                     [ Statement.While
                         (Comparison(Loc(Variable "y"), GreaterEqual, Loc(Variable "x")),
                           [ Statement.Assign
                               (Location.Variable "y",
                                ArithmeticBinary((Loc(Variable "y")), Subtract, (IntLiteral 1))) ]) ])
           Statement.Assign(Location.Variable "b", ArithmeticExpr.IntLiteral 10) ]

    let expectedNodes = [ 0; 1; 2; 3; 4; 5; 6 ]
    let expectedEdges =
        [ (0, Action.Assign(Variable "y", IntLiteral 1), 1)
          (1, Action.Condition(Comparison(Loc(Variable "x"), Greater, Loc(Variable "y"))), 2)
          (2, Action.Assign(Variable "x", ArithmeticBinary((Loc(Variable "x")), Subtract, (IntLiteral 1))), 3)
          (1, Action.Condition(BooleanUnary(Not, Comparison(Loc(Variable "x"), Greater, Loc(Variable "y")))), 4)
          (4, Action.Condition(Comparison(Loc(Variable "y"), GreaterEqual, Loc(Variable "x"))), 5)
          (5, Action.Assign(Variable "y", ArithmeticBinary((Loc(Variable "y")), Subtract, (IntLiteral 1))), 4)
          (4, Action.Condition(BooleanUnary(Not, Comparison(Loc(Variable "y"), GreaterEqual, Loc(Variable "x")))), 3)
          (3, Action.Assign(Variable "b", IntLiteral 10), 6) ]

    let (_, (nodes, edges)) = EdgesFunction.runEdges (Map.empty, stmts)
    Assert.That(List.toArray nodes, Is.EqualTo(List.toArray expectedNodes))
    Assert.That(List.toArray edges, Is.EqualTo(List.toArray expectedEdges))
    
    (*let ast = ([], [
        Statement.Assign(Location.Identifier "y", ArithmeticExpr.IntLiteral 1)
        Statement.While (
            Comparison ((Loc (Identifier "x")), Greater, IntLiteral 0),
            ([],[
                Statement.Assign(Location.Identifier "a", ArithmeticExpr.IntLiteral 10);
                Statement.If(
                    Comparison ((Loc (Identifier "x")), Greater, (Loc (Identifier "y"))),
                    ([],[
                        Statement.Assign(Location.Identifier "x", ArithmeticBinary ((Loc (Identifier "x")), Subtract, (IntLiteral 1)))
                    ]),
                    Some ([],[
                        Statement.Assign(Location.Identifier "y", ArithmeticBinary ((Loc (Identifier "y")), Subtract, (IntLiteral 1)))
                    ])
                );
                Statement.Assign(Location.Identifier "b", ArithmeticExpr.IntLiteral 10)
            ])
        ) ;
        Statement.Assign(Location.Identifier "c", ArithmeticExpr.IntLiteral 10)
    ])

    let expectedNodes = [0;1;2;3;4;5;6;7;8]
    let expectedEdges = [
        (0, Action.Assign (Identifier "y", IntLiteral 1), 1);
        (1, Action.Condition (Comparison (Loc (Identifier "x"), Greater, IntLiteral 0)), 2);
        (2, Action.Assign (Identifier "a", IntLiteral 10), 3);
        (3, Action.Condition (Comparison (Loc (Identifier "x"), Greater, (Loc (Identifier "y")))), 4)
        (4, Action.Assign (Identifier "x", ArithmeticBinary (Loc (Identifier "x"), Subtract, IntLiteral 1)), 5);
        (3, Action.Condition (BooleanUnary (Not, (Comparison (Loc (Identifier "x"), Greater, (Loc (Identifier "y")))))), 6)
        (6, Action.Assign (Identifier "y", ArithmeticBinary (Loc (Identifier "y"), Subtract, IntLiteral 1)), 5);
        (5, Action.Assign (Identifier "b", IntLiteral 10), 1);
        (1, Action.Condition (BooleanUnary (Not, (Comparison (Loc (Identifier "x"), Greater, IntLiteral 0)))), 7)
        (7, Action.Assign (Identifier "c", IntLiteral 10), 8)
    ]

    let (nodes, edges) = EdgesFunction.runEdges ast
    Assert.That(List.toArray nodes, Is.EqualTo(List.toArray expectedNodes)) //Would rather test if they contain the same
    Assert.That(List.toArray edges, Is.EqualTo(List.toArray expectedEdges))
    *)