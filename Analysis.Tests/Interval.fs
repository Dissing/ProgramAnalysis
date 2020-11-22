module Analysis.Tests.Interval

open Analysis
open Analysis.Analyses
open Analysis.Worklists
open FrontEnd
open FrontEnd.AST
open NUnit.Framework
open NUnit.Framework


[<Test>]
let intervalLessThanOrEqual () =
    let x = I(Lower -2, Upper 2)
    let y = I(Lower 0, Upper 1)
    let z = I(NegInf, Upper 1)
    let w = I(Lower 0, Inf)
    let q = I(NegInf, Inf)
    let r = I(Lower 0, Upper 0)
    let s = Bot
    
    
    Assert.IsTrue(x.lessThanOrEqual(x))
    Assert.IsFalse(x.lessThanOrEqual(y))
    Assert.IsFalse(x.lessThanOrEqual(z))
    Assert.IsFalse(x.lessThanOrEqual(w))
    Assert.IsTrue(x.lessThanOrEqual(q))
    Assert.IsFalse(x.lessThanOrEqual(r))
    Assert.IsFalse(x.lessThanOrEqual(s))
    
    Assert.IsTrue(y.lessThanOrEqual(x))
    Assert.IsTrue(y.lessThanOrEqual(y))
    Assert.IsTrue(y.lessThanOrEqual(z))
    Assert.IsTrue(y.lessThanOrEqual(w))
    Assert.IsTrue(y.lessThanOrEqual(q))
    Assert.IsFalse(y.lessThanOrEqual(r))
    Assert.IsFalse(y.lessThanOrEqual(s))
    
    Assert.IsFalse(z.lessThanOrEqual(x))
    Assert.IsFalse(z.lessThanOrEqual(y))
    Assert.IsTrue(z.lessThanOrEqual(z))
    Assert.IsFalse(z.lessThanOrEqual(w))
    Assert.IsTrue(z.lessThanOrEqual(q))
    Assert.IsFalse(z.lessThanOrEqual(r))
    Assert.IsFalse(z.lessThanOrEqual(s))
    
    Assert.IsFalse(w.lessThanOrEqual(x))
    Assert.IsFalse(w.lessThanOrEqual(y))
    Assert.IsFalse(w.lessThanOrEqual(z))
    Assert.IsTrue(w.lessThanOrEqual(w))
    Assert.IsTrue(w.lessThanOrEqual(q))
    Assert.IsFalse(w.lessThanOrEqual(r))
    Assert.IsFalse(w.lessThanOrEqual(s))
    
    Assert.IsFalse(q.lessThanOrEqual(x))
    Assert.IsFalse(q.lessThanOrEqual(y))
    Assert.IsFalse(q.lessThanOrEqual(z))
    Assert.IsFalse(q.lessThanOrEqual(w))
    Assert.IsTrue(q.lessThanOrEqual(q))
    Assert.IsFalse(q.lessThanOrEqual(r))
    Assert.IsFalse(q.lessThanOrEqual(s))
    
    Assert.IsTrue(r.lessThanOrEqual(x))
    Assert.IsTrue(r.lessThanOrEqual(y))
    Assert.IsTrue(r.lessThanOrEqual(z))
    Assert.IsTrue(r.lessThanOrEqual(w))
    Assert.IsTrue(r.lessThanOrEqual(q))
    Assert.IsTrue(r.lessThanOrEqual(r))
    Assert.IsFalse(r.lessThanOrEqual(s))
    
    Assert.IsTrue(s.lessThanOrEqual(x))
    Assert.IsTrue(s.lessThanOrEqual(y))
    Assert.IsTrue(s.lessThanOrEqual(z))
    Assert.IsTrue(s.lessThanOrEqual(w))
    Assert.IsTrue(s.lessThanOrEqual(q))
    Assert.IsTrue(s.lessThanOrEqual(r))
    Assert.IsTrue(s.lessThanOrEqual(s))

[<Test>]
let intervalLeastUpperBound () =
    let x = I(Lower -2, Upper 2)
    let y = I(Lower 0, Upper 1)
    let z = I(NegInf, Upper 1)
    let w = I(Lower 0, Inf)
    let q = I(NegInf, Inf)
    let r = I(Lower 0, Upper 0)
    let s = Bot
    
    Assert.That(x.leastUpperBound(x), Is.EqualTo(I(Lower -2, Upper 2)))
    Assert.That(x.leastUpperBound(y), Is.EqualTo(I(Lower -2, Upper 2)))
    Assert.That(x.leastUpperBound(z), Is.EqualTo(I(NegInf, Upper 2)))
    Assert.That(x.leastUpperBound(w), Is.EqualTo(I(Lower -2, Inf)))
    Assert.That(x.leastUpperBound(q), Is.EqualTo(I(NegInf, Inf)))
    Assert.That(x.leastUpperBound(r), Is.EqualTo(I(Lower -2, Upper 2)))
    Assert.That(x.leastUpperBound(s), Is.EqualTo(I(Lower -2, Upper 2)))
    
    Assert.That(y.leastUpperBound(x), Is.EqualTo(I(Lower -2, Upper 2)))
    Assert.That(y.leastUpperBound(y), Is.EqualTo(I(Lower 0, Upper 1)))
    Assert.That(y.leastUpperBound(z), Is.EqualTo(I(NegInf, Upper 1)))
    Assert.That(y.leastUpperBound(w), Is.EqualTo(I(Lower 0, Inf)))
    Assert.That(y.leastUpperBound(q), Is.EqualTo(I(NegInf, Inf)))
    Assert.That(y.leastUpperBound(r), Is.EqualTo(I(Lower 0, Upper 1)))
    Assert.That(y.leastUpperBound(s), Is.EqualTo(I(Lower 0, Upper 1)))
    
    Assert.That(z.leastUpperBound(x), Is.EqualTo(I(NegInf, Upper 2)))
    Assert.That(z.leastUpperBound(y), Is.EqualTo(I(NegInf, Upper 1)))
    Assert.That(z.leastUpperBound(z), Is.EqualTo(I(NegInf, Upper 1)))
    Assert.That(z.leastUpperBound(w), Is.EqualTo(I(NegInf, Inf)))
    Assert.That(z.leastUpperBound(q), Is.EqualTo(I(NegInf, Inf)))
    Assert.That(z.leastUpperBound(r), Is.EqualTo(I(NegInf, Upper 1)))
    Assert.That(z.leastUpperBound(s), Is.EqualTo(I(NegInf, Upper 1)))
    
    Assert.That(w.leastUpperBound(x), Is.EqualTo(I(Lower -2, Inf)))
    Assert.That(w.leastUpperBound(y), Is.EqualTo(I(Lower 0, Inf)))
    Assert.That(w.leastUpperBound(z), Is.EqualTo(I(NegInf, Inf)))
    Assert.That(w.leastUpperBound(w), Is.EqualTo(I(Lower 0, Inf)))
    Assert.That(w.leastUpperBound(q), Is.EqualTo(I(NegInf, Inf)))
    Assert.That(w.leastUpperBound(r), Is.EqualTo(I(Lower 0, Inf)))
    Assert.That(w.leastUpperBound(s), Is.EqualTo(I(Lower 0, Inf)))
    
    Assert.That(q.leastUpperBound(x), Is.EqualTo(I(NegInf, Inf)))
    Assert.That(q.leastUpperBound(y), Is.EqualTo(I(NegInf, Inf)))
    Assert.That(q.leastUpperBound(z), Is.EqualTo(I(NegInf, Inf)))
    Assert.That(q.leastUpperBound(w), Is.EqualTo(I(NegInf, Inf)))
    Assert.That(q.leastUpperBound(q), Is.EqualTo(I(NegInf, Inf)))
    Assert.That(q.leastUpperBound(r), Is.EqualTo(I(NegInf, Inf)))
    Assert.That(q.leastUpperBound(s), Is.EqualTo(I(NegInf, Inf)))
    
    Assert.That(r.leastUpperBound(x), Is.EqualTo(I(Lower -2, Upper 2)))
    Assert.That(r.leastUpperBound(y), Is.EqualTo(I(Lower 0, Upper 1)))
    Assert.That(r.leastUpperBound(z), Is.EqualTo(I(NegInf, Upper 1)))
    Assert.That(r.leastUpperBound(w), Is.EqualTo(I(Lower 0, Inf)))
    Assert.That(r.leastUpperBound(q), Is.EqualTo(I(NegInf, Inf)))
    Assert.That(r.leastUpperBound(r), Is.EqualTo(I(Lower 0, Upper 0)))
    Assert.That(r.leastUpperBound(s), Is.EqualTo(I(Lower 0, Upper 0)))
    
    Assert.That(s.leastUpperBound(x), Is.EqualTo(I(Lower -2, Upper 2)))
    Assert.That(s.leastUpperBound(y), Is.EqualTo(I(Lower 0, Upper 1)))
    Assert.That(s.leastUpperBound(z), Is.EqualTo(I(NegInf, Upper 1)))
    Assert.That(s.leastUpperBound(w), Is.EqualTo(I(Lower 0, Inf)))
    Assert.That(s.leastUpperBound(q), Is.EqualTo(I(NegInf, Inf)))
    Assert.That(s.leastUpperBound(r), Is.EqualTo(I(Lower 0, Upper 0)))
    Assert.That(s.leastUpperBound(s), Is.EqualTo(Bot))
