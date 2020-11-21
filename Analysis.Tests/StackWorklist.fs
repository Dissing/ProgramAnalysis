module Analysis.Tests.StackWorklist

open Analysis
open Analysis.Worklists
open NUnit.Framework
open Tests.Utils


[<Test>]
let stackWorklist() =
    let w = StackWorklist.empty() :> IWorklist
    let w = insertElements w [1;2;3;4;5;6]
    let w = extractElements w [6;5;4]
    let w = insertElements w [7;8;9]
    let w = extractElements w [9;8;7;3;2;1]
    Assert.That(w.extract(), Is.EqualTo(None))