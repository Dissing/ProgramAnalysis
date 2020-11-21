module Analysis.Tests.QueueWorklist

open Analysis
open Analysis.Worklists
open NUnit.Framework
open Tests.Utils


[<Test>]
let queueWorklist() =
    let w = QueueWorklist.empty() :> IWorklist
    let w = insertElements w [1;2;3;4;5;6]
    let w = extractElements w [1;2;3]
    let w = insertElements w [7;8;9]
    let w = extractElements w [4;5;6;7;8;9]
    Assert.That(w.extract(), Is.EqualTo(None))