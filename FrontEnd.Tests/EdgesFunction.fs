﻿module FrontEnd.Tests.EdgesFunction

open FrontEnd.EdgesFunction;
open FrontEnd.AST;

let main =
    let ast = ([], [
        Statement.Assign(Location.Identifier "a", ArithmeticExpr.IntLiteral 10) ;
        Statement.Assign(Location.Identifier "y", ArithmeticExpr.IntLiteral 1) ;
        Statement.While (
            Comparison ((Loc (Identifier "x")), Greater, IntLiteral 0),
            ([],[
                Statement.Assign(Location.Identifier "y", ArithmeticBinary ((Loc (Identifier "x")), Multiply, (Loc (Identifier "y"))))
                Statement.Assign(Location.Identifier "x", ArithmeticBinary ((Loc (Identifier "x")), Subtract, (IntLiteral 1)))
            ])
        )
    ])
       
    runEdges ast 