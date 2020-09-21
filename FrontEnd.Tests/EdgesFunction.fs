module FrontEnd.Tests.EdgesFunction

open FrontEnd;
open FrontEnd.ProgramGraph;
open FrontEnd.AST;
open NUnit.Framework



(*
[<Test>]
let EdgesIfThen () =
    let ast = ([], [
        Statement.Assign(Location.Identifier "a", ArithmeticExpr.IntLiteral 10) ;
        Statement.If(
            Comparison ((Loc (Identifier "x")), Greater, (Loc (Identifier "y"))),
            ([],[
                Statement.Assign(Location.Identifier "x", ArithmeticBinary ((Loc (Identifier "x")), Subtract, (IntLiteral 1)))
            ]),
            None
        ) ;
        Statement.Assign(Location.Identifier "b", ArithmeticExpr.IntLiteral 10)
    ])
    
    let expectedNodes = [0;1;2;3]
    let expectedEdges = [
        (0, Action.Assign (Identifier "a", IntLiteral 10), 1);
        (1, Action.Condition (Comparison (Loc (Identifier "x"), Greater, Loc (Identifier "y"))), 2);
        (2, Action.Assign (Identifier "x", ArithmeticBinary (Loc (Identifier "x"), Subtract, IntLiteral 1)), 3);
        (1, Action.Condition (BooleanUnary (Not, Comparison (Loc (Identifier "x"), Greater, Loc (Identifier "y")))), 3);
        (3, Action.Assign (Identifier "b", IntLiteral 10), 4)
    ]

    let (nodes, edges) = EdgesFunction.runEdges ast
    Assert.That(List.toArray nodes, Is.EqualTo(List.toArray expectedNodes)) //Would rather test if they contain the same
    Assert.That(List.toArray edges, Is.EqualTo(List.toArray expectedEdges))

[<Test>]
let EdgesWhile () =
    let ast = ([], [
        Statement.Assign(Location.Identifier "a", ArithmeticExpr.IntLiteral 10)
        Statement.Assign(Location.Identifier "y", ArithmeticExpr.IntLiteral 1)
        Statement.While (
            Comparison ((Loc (Identifier "x")), Greater, IntLiteral 0),
            ([],[
                Statement.Assign(Location.Identifier "y", ArithmeticBinary ((Loc (Identifier "x")), Multiply, (Loc (Identifier "y"))))
                Statement.Assign(Location.Identifier "x", ArithmeticBinary ((Loc (Identifier "x")), Subtract, (IntLiteral 1)))
            ])
        )
        Statement.Assign(Location.Identifier "b", ArithmeticExpr.IntLiteral 10)
    ])
    
    let expectedNodes = [0;1;2;3;4;5;6]
    let expectedEdges = [
        (0, Action.Assign (Identifier "a", IntLiteral 10), 1);
        (1, Action.Assign (Identifier "y", IntLiteral 1), 2);
        (2, Action.Condition (Comparison (Loc (Identifier "x"), Greater, IntLiteral 0)), 3);
        (3, Action.Assign (Identifier "x", ArithmeticBinary (Loc (Identifier "x"), Multiply, (Loc (Identifier "y")))), 4);
        (4, Action.Assign (Identifier "x", ArithmeticBinary (Loc (Identifier "x"), Multiply, (Loc (Identifier "y")))), 2);
        (4, Action.Assign (Identifier "b", IntLiteral 10), 5)
    ]

    let (nodes, edges) = EdgesFunction.runEdges ast
    Assert.That(List.toArray nodes, Is.EqualTo(List.toArray expectedNodes)) //Would rather test if they contain the same
    Assert.That(List.toArray edges, Is.EqualTo(List.toArray expectedEdges))

[<Test>]
let EdgesWhileIfThenElse () =
    let ast = ([], [
        Statement.Assign(Location.Identifier "y", ArithmeticExpr.IntLiteral 1)
        Statement.While (
            Comparison ((Loc (Identifier "x")), Greater, IntLiteral 0),
            ([],[
                Statement.If(
                    Comparison ((Loc (Identifier "x")), Greater, (Loc (Identifier "y"))),
                    ([],[
                        Statement.Assign(Location.Identifier "x", ArithmeticBinary ((Loc (Identifier "x")), Subtract, (IntLiteral 1)))
                    ]),
                    Some ([],[
                        Statement.Assign(Location.Identifier "y", ArithmeticBinary ((Loc (Identifier "y")), Subtract, (IntLiteral 1)))
                    ])
                )
            ])
        ) ;
        Statement.Assign(Location.Identifier "b", ArithmeticExpr.IntLiteral 10)
    ])
    
    let expectedNodes = [0;1;2;3;4;5;6]
    let expectedEdges = [
        (0, Action.Assign (Identifier "y", IntLiteral 1), 1);
        (1, Action.Condition (Comparison (Loc (Identifier "x"), Greater, IntLiteral 0)), 2);
        (2, Action.Condition (Comparison (Loc (Identifier "x"), Greater, (Loc (Identifier "y")))), 3);
        (3, Action.Assign (Identifier "x", ArithmeticBinary (Loc (Identifier "x"), Subtract, (IntLiteral 1))), 1);
        (2, Action.Condition (BooleanUnary (Not, (Comparison (Loc (Identifier "x"), Greater, (Loc (Identifier "y")))))), 4);
        (4, Action.Assign (Identifier "y", ArithmeticBinary (Loc (Identifier "y"), Subtract, (IntLiteral 1))), 1);
        (1, Action.Condition (BooleanUnary (Not, (Comparison (Loc (Identifier "x"), Greater, IntLiteral 0)))), 5);
        (5, Action.Assign (Identifier "b", IntLiteral 10), 6)
    ]
    
    let (nodes, edges) = EdgesFunction.runEdges ast
    Assert.That(List.toArray nodes, Is.EqualTo(List.toArray expectedNodes)) //Would rather test if they contain the same
    Assert.That(List.toArray edges, Is.EqualTo(List.toArray expectedEdges))

[<Test>]
let EdgesWhileIfThenElseExtended () =
    let ast = ([], [
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
    
    let expectedNodes = [0;1;2;3;4;5;6]
    let expectedEdges = [
        (0, Action.Assign (Identifier "y", IntLiteral 1), 1);
        (1, Action.Condition (Comparison (Loc (Identifier "x"), Greater, IntLiteral 0)), 2);
        (2, Action.Assign (Identifier "a", IntLiteral 10), 3);
        (3, Action.Condition (Comparison (Loc (Identifier "x"), Greater, (Loc (Identifier "y")))), 4)
        
        
        (3, Action.Condition (BooleanUnary (Not, (Comparison (Loc (Identifier "x"), Greater, (Loc (Identifier "y")))))), 6);
        (4, Action.Assign (Identifier "y", ArithmeticBinary (Loc (Identifier "y"), Subtract, (IntLiteral 1))), 1);
        (1, Action.Condition (BooleanUnary (Not, (Comparison (Loc (Identifier "x"), Greater, IntLiteral 0)))), 5);
        (5, Action.Assign (Identifier "b", IntLiteral 10), 6)
    ]
    
    let (nodes, edges) = EdgesFunction.runEdges ast
    Assert.That(List.toArray nodes, Is.EqualTo(List.toArray expectedNodes)) //Would rather test if they contain the same
    Assert.That(List.toArray edges, Is.EqualTo(List.toArray expectedEdges))
*)

let main =
    //Printer Test
    let (res, _) = FrontEnd.Printer.BooleanPrinter "" (BooleanBinary ((BooleanBinary (BooleanUnary (Not, BooleanLiteral true), Or, BooleanLiteral true)), And, (BooleanBinary (BooleanLiteral true, Or, BooleanUnary (Not, BooleanLiteral false)))))
    printfn "%s" res
    
    
    //If-Then Test
    //let ast = ([], [
    //    Statement.Assign(Location.Identifier "a", ArithmeticExpr.IntLiteral 10) ;
    //    Statement.If(
    //        Comparison ((Loc (Identifier "x")), Greater, (Loc (Identifier "y"))),
    //        ([],[
    //            Statement.Assign(Location.Identifier "x", ArithmeticBinary ((Loc (Identifier "x")), Subtract, (IntLiteral 1)))
    //        ]),
    //        None
    //    ) ;
    //    Statement.Assign(Location.Identifier "b", ArithmeticExpr.IntLiteral 10)
    //])
    
    //Inner If-Then Test
    //let ast = ([], [
    //    Statement.Assign(Location.Identifier "y", IntLiteral 1) ;
    //    Statement.While (
    //        Comparison ((Loc (Identifier "x")), Greater, IntLiteral 0),
    //        ([],[
    //            Statement.If(
    //                Comparison ((Loc (Identifier "x")), Greater, IntLiteral 0),
    //                ([],[
    //                    Statement.Assign(Location.Identifier "x", ArithmeticBinary (Loc (Identifier "x"), Subtract, IntLiteral 1))
    //                    ]),
    //                None
    //                ) ;
    //        ])
    //    ) ;
    //    Statement.Assign(Location.Identifier "b", ArithmeticExpr.IntLiteral 10)
    //])
    
    //If-Then-Else Test
    //let ast = ([], [
    //    Statement.Assign(Location.Identifier "a", ArithmeticExpr.IntLiteral 10) ;
    //    Statement.If(
    //        Comparison ((Loc (Identifier "x")), Greater, (Loc (Identifier "y"))),
    //        ([],[
    //            Statement.Assign(Location.Identifier "x", ArithmeticBinary ((Loc (Identifier "x")), Subtract, (IntLiteral 1)))
    //        ]),
    //        Some ([],[
    //            Statement.Assign(Location.Identifier "y", ArithmeticBinary ((Loc (Identifier "y")), Subtract, (IntLiteral 1)))
    //        ])
    //    ) ;
    //    Statement.Assign(Location.Identifier "b", ArithmeticExpr.IntLiteral 10)
    //])
    
    //Inner If-Then-Else Test
    //let ast = ([], [
    //    Statement.Assign(Location.Identifier "y", ArithmeticExpr.IntLiteral 1) ;
    //    Statement.While (
    //        Comparison ((Loc (Identifier "x")), Greater, IntLiteral 0),
    //        ([],[
    //            Statement.If(
    //                Comparison ((Loc (Identifier "x")), Greater, (Loc (Identifier "y"))),
    //                ([],[
    //                    Statement.Assign(Location.Identifier "x", ArithmeticBinary ((Loc (Identifier "x")), Subtract, (IntLiteral 1)))
    //                    ]),
    //                Some ([],[
    //                    Statement.Assign(Location.Identifier "y", ArithmeticBinary ((Loc (Identifier "y")), Subtract, (IntLiteral 1)))
    //                    ])
    //                ) ;
    //        ])
    //    ) ;
    //    Statement.Assign(Location.Identifier "b", ArithmeticExpr.IntLiteral 10)
    //])
    
    //While Test
    //let ast = ([], [
    //    Statement.Assign(Location.Identifier "a", ArithmeticExpr.IntLiteral 10) ;
    //    Statement.Assign(Location.Identifier "y", ArithmeticExpr.IntLiteral 1) ;
    //    Statement.While (
    //        Comparison ((Loc (Identifier "x")), Greater, IntLiteral 0),
    //        ([],[
    //            Statement.Assign(Location.Identifier "y", ArithmeticBinary ((Loc (Identifier "x")), Multiply, (Loc (Identifier "y")))) ;
    //            Statement.Assign(Location.Identifier "x", ArithmeticBinary ((Loc (Identifier "x")), Subtract, (IntLiteral 1)))
    //        ])
    //    ) ;
    //    Statement.Assign(Location.Identifier "b", ArithmeticExpr.IntLiteral 10)
    //])
    
    //Inner While Test
    //let ast = ([], [
    //    Statement.Assign(Location.Identifier "y", ArithmeticExpr.IntLiteral 1)
    //    Statement.If(
    //                Comparison (Loc (Identifier "x"), Greater, Loc (Identifier "y")),
    //                ([],[
    //                    Statement.Assign(Location.Identifier "x", ArithmeticBinary ((Loc (Identifier "x")), Subtract, (IntLiteral 1)))
    //                    ]),
    //                Some ([],[
    //                    Statement.While (
    //                        Comparison (Loc (Identifier "y"), GreaterEqual, Loc (Identifier "x")),
    //                        ([],[
    //                            Statement.Assign(Location.Identifier "y", ArithmeticBinary ((Loc (Identifier "y")), Subtract, (IntLiteral 1)))
    //                        ])
    //                    ) ;
    //                    ])
    //                ) ;
    //    Statement.Assign(Location.Identifier "b", ArithmeticExpr.IntLiteral 10)
    //])
    
    
    //Very Busy Expressions Test
    //let ast = ([], [
    //    Statement.Assign(Location.Identifier "y", ArithmeticExpr.IntLiteral 1)
    //    Statement.While (
    //        Comparison ((Loc (Identifier "x")), Greater, IntLiteral 0),
    //        ([],[
    //            Statement.Assign(Location.Identifier "a", ArithmeticExpr.IntLiteral 10);
    //            Statement.If(
    //                Comparison ((Loc (Identifier "x")), Greater, (Loc (Identifier "y"))),
    //                ([],[
    //                    Statement.Assign(Location.Identifier "x", ArithmeticBinary ((Loc (Identifier "x")), Subtract, (IntLiteral 1)))
    //                ]),
    //                Some ([],[
    //                    Statement.Assign(Location.Identifier "y", ArithmeticBinary ((Loc (Identifier "y")), Subtract, (IntLiteral 1)))
    //                    Statement.Assign(Location.Identifier "b", ArithmeticExpr.IntLiteral 10)
    //                ])
    //            );
    //            Statement.Assign(Location.Identifier "c", ArithmeticExpr.IntLiteral 10)
    //        ])
    //    ) ;
    //    Statement.Assign(Location.Identifier "d", ArithmeticExpr.IntLiteral 10)
    //])
    
    //let ast = ([], [
    //    Statement.Assign(Location.Identifier "x", IntLiteral 0) ;
    //    Statement.Assign(Location.Identifier "y", IntLiteral 0) ;
    //    Statement.Assign(Location.Identifier "i", IntLiteral 0) ;
    //    Statement.While (
    //        Comparison (Loc (Identifier "i"), Lesser, Loc (Identifier "n")),
    //        ([],[
    //            Statement.If(
    //                Comparison (Loc (Location.Array ("A", Loc (Identifier "i"))), Greater, IntLiteral 0),
    //                ([],[
    //                    Statement.Assign(Location.Identifier "x", ArithmeticBinary (Loc (Identifier "x"), Add, Loc (Location.Array ("A", Loc (Identifier "i"))))) ;
    //                    Statement.Assign(Location.Identifier "i", ArithmeticBinary ((Loc (Identifier "i")), Add, IntLiteral 1))
    //                ]),
    //                Some ([],[
    //                    Statement.Assign(Location.Identifier "y", ArithmeticBinary (Loc (Identifier "y"), Add, Loc (Location.Array ("A", Loc (Identifier "i"))))) ;
    //                    Statement.Assign(Location.Identifier "i", ArithmeticBinary ((Loc (Identifier "i")), Add, IntLiteral 1))
    //                ])
    //            )
    //        ])
    //    )
    //])
    
    //EdgesFunction.runEdges ast
    