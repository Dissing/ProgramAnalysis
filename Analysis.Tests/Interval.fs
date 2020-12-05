module Analysis.Tests.Interval

open Analysis
open Analysis.Analyses
open Analysis.Worklists
open FrontEnd
open NUnit.Framework
open NUnit.Framework


[<Test>]
let intervalLessThanOrEqual () =
    let x = I(Val -2, Val 2)
    let y = I(Val 0, Val 1)
    let z = I(NegInf, Val 1)
    let w = I(Val 0, Inf)
    let q = I(NegInf, Inf)
    let r = I(Val 0, Val 0)
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
    let x = I(Val -2, Val 2)
    let y = I(Val 0, Val 1)
    let z = I(NegInf, Val 1)
    let w = I(Val 0, Inf)
    let q = I(NegInf, Inf)
    let r = I(Val 0, Val 0)
    let s = Bot
    
    Assert.That(x.leastUpperBound(x), Is.EqualTo(I(Val -2, Val 2)))
    Assert.That(x.leastUpperBound(y), Is.EqualTo(I(Val -2, Val 2)))
    Assert.That(x.leastUpperBound(z), Is.EqualTo(I(NegInf, Val 2)))
    Assert.That(x.leastUpperBound(w), Is.EqualTo(I(Val -2, Inf)))
    Assert.That(x.leastUpperBound(q), Is.EqualTo(I(NegInf, Inf)))
    Assert.That(x.leastUpperBound(r), Is.EqualTo(I(Val -2, Val 2)))
    Assert.That(x.leastUpperBound(s), Is.EqualTo(I(Val -2, Val 2)))
    
    Assert.That(y.leastUpperBound(x), Is.EqualTo(I(Val -2, Val 2)))
    Assert.That(y.leastUpperBound(y), Is.EqualTo(I(Val 0, Val 1)))
    Assert.That(y.leastUpperBound(z), Is.EqualTo(I(NegInf, Val 1)))
    Assert.That(y.leastUpperBound(w), Is.EqualTo(I(Val 0, Inf)))
    Assert.That(y.leastUpperBound(q), Is.EqualTo(I(NegInf, Inf)))
    Assert.That(y.leastUpperBound(r), Is.EqualTo(I(Val 0, Val 1)))
    Assert.That(y.leastUpperBound(s), Is.EqualTo(I(Val 0, Val 1)))
    
    Assert.That(z.leastUpperBound(x), Is.EqualTo(I(NegInf, Val 2)))
    Assert.That(z.leastUpperBound(y), Is.EqualTo(I(NegInf, Val 1)))
    Assert.That(z.leastUpperBound(z), Is.EqualTo(I(NegInf, Val 1)))
    Assert.That(z.leastUpperBound(w), Is.EqualTo(I(NegInf, Inf)))
    Assert.That(z.leastUpperBound(q), Is.EqualTo(I(NegInf, Inf)))
    Assert.That(z.leastUpperBound(r), Is.EqualTo(I(NegInf, Val 1)))
    Assert.That(z.leastUpperBound(s), Is.EqualTo(I(NegInf, Val 1)))
    
    Assert.That(w.leastUpperBound(x), Is.EqualTo(I(Val -2, Inf)))
    Assert.That(w.leastUpperBound(y), Is.EqualTo(I(Val 0, Inf)))
    Assert.That(w.leastUpperBound(z), Is.EqualTo(I(NegInf, Inf)))
    Assert.That(w.leastUpperBound(w), Is.EqualTo(I(Val 0, Inf)))
    Assert.That(w.leastUpperBound(q), Is.EqualTo(I(NegInf, Inf)))
    Assert.That(w.leastUpperBound(r), Is.EqualTo(I(Val 0, Inf)))
    Assert.That(w.leastUpperBound(s), Is.EqualTo(I(Val 0, Inf)))
    
    Assert.That(q.leastUpperBound(x), Is.EqualTo(I(NegInf, Inf)))
    Assert.That(q.leastUpperBound(y), Is.EqualTo(I(NegInf, Inf)))
    Assert.That(q.leastUpperBound(z), Is.EqualTo(I(NegInf, Inf)))
    Assert.That(q.leastUpperBound(w), Is.EqualTo(I(NegInf, Inf)))
    Assert.That(q.leastUpperBound(q), Is.EqualTo(I(NegInf, Inf)))
    Assert.That(q.leastUpperBound(r), Is.EqualTo(I(NegInf, Inf)))
    Assert.That(q.leastUpperBound(s), Is.EqualTo(I(NegInf, Inf)))
    
    Assert.That(r.leastUpperBound(x), Is.EqualTo(I(Val -2, Val 2)))
    Assert.That(r.leastUpperBound(y), Is.EqualTo(I(Val 0, Val 1)))
    Assert.That(r.leastUpperBound(z), Is.EqualTo(I(NegInf, Val 1)))
    Assert.That(r.leastUpperBound(w), Is.EqualTo(I(Val 0, Inf)))
    Assert.That(r.leastUpperBound(q), Is.EqualTo(I(NegInf, Inf)))
    Assert.That(r.leastUpperBound(r), Is.EqualTo(I(Val 0, Val 0)))
    Assert.That(r.leastUpperBound(s), Is.EqualTo(I(Val 0, Val 0)))
    
    Assert.That(s.leastUpperBound(x), Is.EqualTo(I(Val -2, Val 2)))
    Assert.That(s.leastUpperBound(y), Is.EqualTo(I(Val 0, Val 1)))
    Assert.That(s.leastUpperBound(z), Is.EqualTo(I(NegInf, Val 1)))
    Assert.That(s.leastUpperBound(w), Is.EqualTo(I(Val 0, Inf)))
    Assert.That(s.leastUpperBound(q), Is.EqualTo(I(NegInf, Inf)))
    Assert.That(s.leastUpperBound(r), Is.EqualTo(I(Val 0, Val 0)))
    Assert.That(s.leastUpperBound(s), Is.EqualTo(Bot))

[<Test>]
let intervalAddition () =
    let x = I(Val -2, Val 2)
    let y = I(Val 0, Val 1)
    let z = I(NegInf, Val 1)
    let w = I(Val 0, Inf)
    let q = I(NegInf, Inf)
    let r = I(Val 0, Val 0)
    
    let analysis = IntervalAnalysis((Map.empty, ([],[])), -2, 2)
    
    Assert.That(analysis.addition x x, Is.EqualTo(I(NegInf, Inf)))
    Assert.That(analysis.addition x y, Is.EqualTo(I(Val -2, Inf)))
    Assert.That(analysis.addition x z, Is.EqualTo(I(NegInf, Inf)))
    Assert.That(analysis.addition x w, Is.EqualTo(I(Val -2, Inf)))
    Assert.That(analysis.addition x q, Is.EqualTo(I(NegInf, Inf)))
    Assert.That(analysis.addition x r, Is.EqualTo(I(Val -2, Val 2)))
    
    Assert.That(analysis.addition y x, Is.EqualTo(I(Val -2, Inf)))
    Assert.That(analysis.addition y y, Is.EqualTo(I(Val 0, Val 2)))
    Assert.That(analysis.addition y z, Is.EqualTo(I(NegInf, Val 2)))
    Assert.That(analysis.addition y w, Is.EqualTo(I(Val 0, Inf)))
    Assert.That(analysis.addition y q, Is.EqualTo(I(NegInf, Inf)))
    Assert.That(analysis.addition y r, Is.EqualTo(I(Val 0, Val 1)))
    
    Assert.That(analysis.addition z x, Is.EqualTo(I(NegInf, Inf)))
    Assert.That(analysis.addition z y, Is.EqualTo(I(NegInf, Val 2)))
    Assert.That(analysis.addition z z, Is.EqualTo(I(NegInf, Val 2)))
    Assert.That(analysis.addition z w, Is.EqualTo(I(NegInf, Inf)))
    Assert.That(analysis.addition z q, Is.EqualTo(I(NegInf, Inf)))
    Assert.That(analysis.addition z r, Is.EqualTo(I(NegInf, Val 1)))
    
    Assert.That(analysis.addition w x, Is.EqualTo(I(Val -2, Inf)))
    Assert.That(analysis.addition w y, Is.EqualTo(I(Val 0, Inf)))
    Assert.That(analysis.addition w z, Is.EqualTo(I(NegInf, Inf)))
    Assert.That(analysis.addition w w, Is.EqualTo(I(Val 0, Inf)))
    Assert.That(analysis.addition w q, Is.EqualTo(I(NegInf, Inf)))
    Assert.That(analysis.addition w r, Is.EqualTo(I(Val 0, Inf)))
    
    Assert.That(analysis.addition q x, Is.EqualTo(I(NegInf, Inf)))
    Assert.That(analysis.addition q y, Is.EqualTo(I(NegInf, Inf)))
    Assert.That(analysis.addition q z, Is.EqualTo(I(NegInf, Inf)))
    Assert.That(analysis.addition q w, Is.EqualTo(I(NegInf, Inf)))
    Assert.That(analysis.addition q q, Is.EqualTo(I(NegInf, Inf)))
    Assert.That(analysis.addition q r, Is.EqualTo(I(NegInf, Inf)))
    
    Assert.That(analysis.addition r x, Is.EqualTo(I(Val -2, Val 2)))
    Assert.That(analysis.addition r y, Is.EqualTo(I(Val 0, Val 1)))
    Assert.That(analysis.addition r z, Is.EqualTo(I(NegInf, Val 1)))
    Assert.That(analysis.addition r w, Is.EqualTo(I(Val 0, Inf)))
    Assert.That(analysis.addition r q, Is.EqualTo(I(NegInf, Inf)))
    Assert.That(analysis.addition r r, Is.EqualTo(I(Val 0, Val 0)))

[<Test>]
let intervalSubtraction () =
    let x = I(Val -2, Val 2)
    let y = I(Val 0, Val 1)
    let z = I(NegInf, Val 1)
    let w = I(Val 0, Inf)
    let q = I(NegInf, Inf)
    let r = I(Val 0, Val 0)
    
    let analysis = IntervalAnalysis((Map.empty, ([],[])), -2, 2)
    
    Assert.That(analysis.subtraction x x, Is.EqualTo(I(NegInf, Inf)))
    Assert.That(analysis.subtraction x y, Is.EqualTo(I(NegInf, Val 2)))
    Assert.That(analysis.subtraction x z, Is.EqualTo(I(NegInf, Inf)))
    Assert.That(analysis.subtraction x w, Is.EqualTo(I(NegInf, Val 2)))
    Assert.That(analysis.subtraction x q, Is.EqualTo(I(NegInf, Inf)))
    Assert.That(analysis.subtraction x r, Is.EqualTo(I(Val -2, Val 2)))
    
    Assert.That(analysis.subtraction y x, Is.EqualTo(I(Val -2, Inf)))
    Assert.That(analysis.subtraction y y, Is.EqualTo(I(Val -1, Val 1)))
    Assert.That(analysis.subtraction y z, Is.EqualTo(I(Val -1, Inf)))
    Assert.That(analysis.subtraction y w, Is.EqualTo(I(NegInf, Val 1)))
    Assert.That(analysis.subtraction y q, Is.EqualTo(I(NegInf, Inf)))
    Assert.That(analysis.subtraction y r, Is.EqualTo(I(Val 0, Val 1)))
    
    Assert.That(analysis.subtraction z x, Is.EqualTo(I(NegInf, Inf)))
    Assert.That(analysis.subtraction z y, Is.EqualTo(I(NegInf, Val 1)))
    Assert.That(analysis.subtraction z z, Is.EqualTo(I(NegInf, Inf)))
    Assert.That(analysis.subtraction z w, Is.EqualTo(I(NegInf, Val 1)))
    Assert.That(analysis.subtraction z q, Is.EqualTo(I(NegInf, Inf)))
    Assert.That(analysis.subtraction z r, Is.EqualTo(I(NegInf, Val 1)))
    
    Assert.That(analysis.subtraction w x, Is.EqualTo(I(Val -2, Inf)))
    Assert.That(analysis.subtraction w y, Is.EqualTo(I(Val -1, Inf)))
    Assert.That(analysis.subtraction w z, Is.EqualTo(I(Val -1, Inf)))
    Assert.That(analysis.subtraction w w, Is.EqualTo(I(NegInf, Inf)))
    Assert.That(analysis.subtraction w q, Is.EqualTo(I(NegInf, Inf)))
    Assert.That(analysis.subtraction w r, Is.EqualTo(I(Val 0, Inf)))
    
    Assert.That(analysis.subtraction q x, Is.EqualTo(I(NegInf, Inf)))
    Assert.That(analysis.subtraction q y, Is.EqualTo(I(NegInf, Inf)))
    Assert.That(analysis.subtraction q z, Is.EqualTo(I(NegInf, Inf)))
    Assert.That(analysis.subtraction q w, Is.EqualTo(I(NegInf, Inf)))
    Assert.That(analysis.subtraction q q, Is.EqualTo(I(NegInf, Inf)))
    Assert.That(analysis.subtraction q r, Is.EqualTo(I(NegInf, Inf)))
    
    Assert.That(analysis.subtraction r x, Is.EqualTo(I(Val -2, Val 2)))
    Assert.That(analysis.subtraction r y, Is.EqualTo(I(Val -1, Val 0)))
    Assert.That(analysis.subtraction r z, Is.EqualTo(I(Val -1, Inf)))
    Assert.That(analysis.subtraction r w, Is.EqualTo(I(NegInf, Val 0)))
    Assert.That(analysis.subtraction r q, Is.EqualTo(I(NegInf, Inf)))
    Assert.That(analysis.subtraction r r, Is.EqualTo(I(Val 0, Val 0)))

[<Test>]
let intervalMultiplication () =
    let x = I(Val -2, Val 2)
    let y = I(Val 0, Val 1)
    let z = I(NegInf, Val 1)
    let w = I(Val 0, Inf)
    let q = I(NegInf, Inf)
    let r = I(Val 0, Val 0)
    
    let analysis = IntervalAnalysis((Map.empty, ([],[])), -2, 2)
    
    Assert.That(analysis.multiplication x x, Is.EqualTo(I(NegInf, Inf)))
    Assert.That(analysis.multiplication x y, Is.EqualTo(I(Val -2, Val 2)))
    Assert.That(analysis.multiplication x z, Is.EqualTo(I(NegInf, Inf)))
    Assert.That(analysis.multiplication x w, Is.EqualTo(I(NegInf, Inf)))
    Assert.That(analysis.multiplication x q, Is.EqualTo(I(NegInf, Inf)))
    Assert.That(analysis.multiplication x r, Is.EqualTo(I(Val 0, Val 0)))
    
    Assert.That(analysis.multiplication y x, Is.EqualTo(I(Val -2, Val 2)))
    Assert.That(analysis.multiplication y y, Is.EqualTo(I(Val 0, Val 1)))
    Assert.That(analysis.multiplication y z, Is.EqualTo(I(NegInf, Val 1)))
    Assert.That(analysis.multiplication y w, Is.EqualTo(I(Val 0, Inf)))
    Assert.That(analysis.multiplication y q, Is.EqualTo(I(NegInf, Inf)))
    Assert.That(analysis.multiplication y r, Is.EqualTo(I(Val 0, Val 0)))
    
    Assert.That(analysis.multiplication z x, Is.EqualTo(I(NegInf, Inf)))
    Assert.That(analysis.multiplication z y, Is.EqualTo(I(NegInf, Val 1)))
    Assert.That(analysis.multiplication z z, Is.EqualTo(I(NegInf, Inf)))
    Assert.That(analysis.multiplication z w, Is.EqualTo(I(NegInf, Inf)))
    Assert.That(analysis.multiplication z q, Is.EqualTo(I(NegInf, Inf)))
    Assert.That(analysis.multiplication z r, Is.EqualTo(I(Val 0, Val 0)))
    
    Assert.That(analysis.multiplication w x, Is.EqualTo(I(NegInf, Inf)))
    Assert.That(analysis.multiplication w y, Is.EqualTo(I(Val 0, Inf)))
    Assert.That(analysis.multiplication w z, Is.EqualTo(I(NegInf, Inf)))
    Assert.That(analysis.multiplication w w, Is.EqualTo(I(Val 0, Inf)))
    Assert.That(analysis.multiplication w q, Is.EqualTo(I(NegInf, Inf)))
    Assert.That(analysis.multiplication w r, Is.EqualTo(I(Val 0, Val 0)))
    
    Assert.That(analysis.multiplication q x, Is.EqualTo(I(NegInf, Inf)))
    Assert.That(analysis.multiplication q y, Is.EqualTo(I(NegInf, Inf)))
    Assert.That(analysis.multiplication q z, Is.EqualTo(I(NegInf, Inf)))
    Assert.That(analysis.multiplication q w, Is.EqualTo(I(NegInf, Inf)))
    Assert.That(analysis.multiplication q q, Is.EqualTo(I(NegInf, Inf)))
    Assert.That(analysis.multiplication q r, Is.EqualTo(I(Val 0, Val 0)))
    
    Assert.That(analysis.multiplication r x, Is.EqualTo(I(Val 0, Val 0)))
    Assert.That(analysis.multiplication r y, Is.EqualTo(I(Val 0, Val 0)))
    Assert.That(analysis.multiplication r z, Is.EqualTo(I(Val 0, Val 0)))
    Assert.That(analysis.multiplication r w, Is.EqualTo(I(Val 0, Val 0)))
    Assert.That(analysis.multiplication r q, Is.EqualTo(I(Val 0, Val 0)))
    Assert.That(analysis.multiplication r r, Is.EqualTo(I(Val 0, Val 0)))
    
[<Test>]
let intervalDivision () =
    let x = I(Val -2, Val 2)
    let y = I(Val 0, Val 1)
    let z = I(NegInf, Val 1)
    let w = I(Val 0, Inf)
    let q = I(NegInf, Inf)
    let r = I(Val 0, Val 0)
    
    let analysis = IntervalAnalysis((Map.empty, ([],[])), -2, 2)
    
    Assert.That(analysis.division x x, Is.EqualTo(I(Val -2, Val 2)))
    Assert.That(analysis.division x y, Is.EqualTo(I(Val -2, Val 2)))
    Assert.That(analysis.division x z, Is.EqualTo(I(Val -2, Val 2)))
    Assert.That(analysis.division x w, Is.EqualTo(I(Val -2, Val 2)))
    Assert.That(analysis.division x q, Is.EqualTo(I(Val -2, Val 2)))
    Assert.That(analysis.division x r, Is.EqualTo(Bot))
    
    Assert.That(analysis.division y x, Is.EqualTo(I(Val -1, Val 1)))
    Assert.That(analysis.division y y, Is.EqualTo(I(Val 0, Val 1)))
    Assert.That(analysis.division y z, Is.EqualTo(I(Val -1, Val 1)))
    Assert.That(analysis.division y w, Is.EqualTo(I(Val 0, Val 1)))
    Assert.That(analysis.division y q, Is.EqualTo(I(Val -1, Val 1)))
    Assert.That(analysis.division y r, Is.EqualTo(Bot))
    
    Assert.That(analysis.division z x, Is.EqualTo(I(NegInf, Inf)))
    Assert.That(analysis.division z y, Is.EqualTo(I(NegInf, Val 1)))
    Assert.That(analysis.division z z, Is.EqualTo(I(NegInf, Inf)))
    Assert.That(analysis.division z w, Is.EqualTo(I(NegInf, Val 1)))
    Assert.That(analysis.division z q, Is.EqualTo(I(NegInf, Inf)))
    Assert.That(analysis.division z r, Is.EqualTo(Bot))
    
    Assert.That(analysis.division w x, Is.EqualTo(I(NegInf, Inf)))
    Assert.That(analysis.division w y, Is.EqualTo(I(Val 0, Inf)))
    Assert.That(analysis.division w z, Is.EqualTo(I(NegInf, Inf)))
    Assert.That(analysis.division w w, Is.EqualTo(I(Val 0, Inf)))
    Assert.That(analysis.division w q, Is.EqualTo(I(NegInf, Inf)))
    Assert.That(analysis.division w r, Is.EqualTo(Bot))
    
    Assert.That(analysis.division q x, Is.EqualTo(I(NegInf, Inf)))
    Assert.That(analysis.division q y, Is.EqualTo(I(NegInf, Inf)))
    Assert.That(analysis.division q z, Is.EqualTo(I(NegInf, Inf)))
    Assert.That(analysis.division q w, Is.EqualTo(I(NegInf, Inf)))
    Assert.That(analysis.division q q, Is.EqualTo(I(NegInf, Inf)))
    Assert.That(analysis.division q r, Is.EqualTo(Bot))
    
    Assert.That(analysis.division r x, Is.EqualTo(I(Val 0, Val 0)))
    Assert.That(analysis.division r y, Is.EqualTo(I(Val 0, Val 0)))
    Assert.That(analysis.division r z, Is.EqualTo(I(Val 0, Val 0)))
    Assert.That(analysis.division r w, Is.EqualTo(I(Val 0, Val 0)))
    Assert.That(analysis.division r q, Is.EqualTo(I(Val 0, Val 0)))
    Assert.That(analysis.division r r, Is.EqualTo(Bot))
    
    let analysis = IntervalAnalysis((Map.empty, ([],[])), -10, 10)
    let a = I(Val 0, Val 7)
    let b = I(Val -7, Val 0)
    let c = I(Val 3, Val 4)
    let d = I(Val -4, Val -3)
    let e = I(Val -4, Val 4)
    Assert.That(analysis.division a c, Is.EqualTo(I(Val 0, Val 2)))
    Assert.That(analysis.division b c, Is.EqualTo(I(Val -2, Val 0)))
    Assert.That(analysis.division a d, Is.EqualTo(I(Val -2, Val 0)))
    Assert.That(analysis.division b d, Is.EqualTo(I(Val 0, Val 2)))
    Assert.That(analysis.division a e, Is.EqualTo(I(Val -7, Val 7)))
    Assert.That(analysis.division b e, Is.EqualTo(I(Val -7, Val 7)))
    
[<Test>]
let intervalModulo () =
    let x = I(Val -2, Val 2)
    let y = I(Val 0, Val 1)
    let z = I(NegInf, Val 1)
    let w = I(Val 0, Inf)
    let q = I(NegInf, Inf)
    let r = I(Val 0, Val 0)
    let u = I(Val 1, Val 2)
    let v = I(Val -2, Val -1)
    
    let analysis = IntervalAnalysis((Map.empty, ([],[])), -2, 2)
    
    Assert.That(analysis.modulo x x, Is.EqualTo(I(Val -1, Val 1)))
    Assert.That(analysis.modulo x y, Is.EqualTo(I(Val 0, Val 0)))
    Assert.That(analysis.modulo x z, Is.EqualTo(I(Val -2, Val 2)))
    Assert.That(analysis.modulo x w, Is.EqualTo(I(Val -2, Val 2)))
    Assert.That(analysis.modulo x q, Is.EqualTo(I(Val -2, Val 2)))
    Assert.That(analysis.modulo x r, Is.EqualTo(Bot))
    Assert.That(analysis.modulo x u, Is.EqualTo(I(Val -1, Val 1)))
    Assert.That(analysis.modulo x v, Is.EqualTo(I(Val -1, Val 1)))
    
    Assert.That(analysis.modulo y x, Is.EqualTo(I(Val 0, Val 1)))
    Assert.That(analysis.modulo y y, Is.EqualTo(I(Val 0, Val 0)))
    Assert.That(analysis.modulo y z, Is.EqualTo(I(Val 0, Val 1)))
    Assert.That(analysis.modulo y w, Is.EqualTo(I(Val 0, Val 1)))
    Assert.That(analysis.modulo y q, Is.EqualTo(I(Val 0, Val 1)))
    Assert.That(analysis.modulo y r, Is.EqualTo(Bot))
    Assert.That(analysis.modulo y u, Is.EqualTo(I(Val 0, Val 1)))
    Assert.That(analysis.modulo y v, Is.EqualTo(I(Val 0, Val 1)))
    
    Assert.That(analysis.modulo z x, Is.EqualTo(I(Val -1, Val 1)))
    Assert.That(analysis.modulo z y, Is.EqualTo(I(Val 0, Val 0)))
    Assert.That(analysis.modulo z z, Is.EqualTo(I(NegInf, Val 1)))
    Assert.That(analysis.modulo z w, Is.EqualTo(I(NegInf, Val 1)))
    Assert.That(analysis.modulo z q, Is.EqualTo(I(NegInf, Val 1)))
    Assert.That(analysis.modulo z r, Is.EqualTo(Bot))
    Assert.That(analysis.modulo z u, Is.EqualTo(I(Val -1, Val 1)))
    Assert.That(analysis.modulo z v, Is.EqualTo(I(Val -1, Val 1)))
    
    Assert.That(analysis.modulo w x, Is.EqualTo(I(Val 0, Val 1)))
    Assert.That(analysis.modulo w y, Is.EqualTo(I(Val 0, Val 0)))
    Assert.That(analysis.modulo w z, Is.EqualTo(I(Val 0, Inf)))
    Assert.That(analysis.modulo w w, Is.EqualTo(I(Val 0, Inf)))
    Assert.That(analysis.modulo w q, Is.EqualTo(I(Val 0, Inf)))
    Assert.That(analysis.modulo w r, Is.EqualTo(Bot))
    Assert.That(analysis.modulo w u, Is.EqualTo(I(Val 0, Val 1)))
    Assert.That(analysis.modulo w v, Is.EqualTo(I(Val 0, Val 1)))
    
    Assert.That(analysis.modulo q x, Is.EqualTo(I(Val -1, Val 1)))
    Assert.That(analysis.modulo q y, Is.EqualTo(I(Val 0, Val 0)))
    Assert.That(analysis.modulo q z, Is.EqualTo(I(NegInf, Inf)))
    Assert.That(analysis.modulo q w, Is.EqualTo(I(NegInf, Inf)))
    Assert.That(analysis.modulo q q, Is.EqualTo(I(NegInf, Inf)))
    Assert.That(analysis.modulo q r, Is.EqualTo(Bot))
    Assert.That(analysis.modulo q u, Is.EqualTo(I(Val -1, Val 1)))
    Assert.That(analysis.modulo q v, Is.EqualTo(I(Val -1, Val 1)))
    
    Assert.That(analysis.modulo r x, Is.EqualTo(I(Val 0, Val 0)))
    Assert.That(analysis.modulo r y, Is.EqualTo(I(Val 0, Val 0)))
    Assert.That(analysis.modulo r z, Is.EqualTo(I(Val 0, Val 0)))
    Assert.That(analysis.modulo r w, Is.EqualTo(I(Val 0, Val 0)))
    Assert.That(analysis.modulo r q, Is.EqualTo(I(Val 0, Val 0)))
    Assert.That(analysis.modulo r r, Is.EqualTo(Bot))
    Assert.That(analysis.modulo r u, Is.EqualTo(I(Val 0, Val 0)))
    Assert.That(analysis.modulo r v, Is.EqualTo(I(Val 0, Val 0)))
    
    Assert.That(analysis.modulo u x, Is.EqualTo(I(Val 0, Val 1)))
    Assert.That(analysis.modulo u y, Is.EqualTo(I(Val 0, Val 0)))
    Assert.That(analysis.modulo u z, Is.EqualTo(I(Val 0, Val 2)))
    Assert.That(analysis.modulo u w, Is.EqualTo(I(Val 0, Val 2)))
    Assert.That(analysis.modulo u q, Is.EqualTo(I(Val 0, Val 2)))
    Assert.That(analysis.modulo u r, Is.EqualTo(Bot))
    Assert.That(analysis.modulo u u, Is.EqualTo(I(Val 0, Val 1)))
    Assert.That(analysis.modulo u v, Is.EqualTo(I(Val 0, Val 1)))
    
    Assert.That(analysis.modulo v x, Is.EqualTo(I(Val -1, Val 0)))
    Assert.That(analysis.modulo v y, Is.EqualTo(I(Val 0, Val 0)))
    Assert.That(analysis.modulo v z, Is.EqualTo(I(Val -2, Val 0)))
    Assert.That(analysis.modulo v w, Is.EqualTo(I(Val -2, Val 0)))
    Assert.That(analysis.modulo v q, Is.EqualTo(I(Val -2, Val 0)))
    Assert.That(analysis.modulo v r, Is.EqualTo(Bot))
    Assert.That(analysis.modulo v u, Is.EqualTo(I(Val -1, Val 0)))
    Assert.That(analysis.modulo v v, Is.EqualTo(I(Val -1, Val 0)))
    
    let analysis = IntervalAnalysis((Map.empty, ([],[])), -10, 10)
    let a = I(Val 0, Val 7)
    let b = I(Val -7, Val 0)
    let c = I(Val 3, Val 4)
    let d = I(Val -4, Val -3)
    let e = I(Val -4, Val 4)
    Assert.That(analysis.modulo a c, Is.EqualTo(I(Val 0, Val 3)))
    Assert.That(analysis.modulo b c, Is.EqualTo(I(Val -3, Val 0)))
    Assert.That(analysis.modulo a d, Is.EqualTo(I(Val 0, Val 3)))
    Assert.That(analysis.modulo b d, Is.EqualTo(I(Val -3, Val 0)))
    Assert.That(analysis.modulo a e, Is.EqualTo(I(Val 0, Val 3)))
    Assert.That(analysis.modulo b e, Is.EqualTo(I(Val -3, Val 0)))

[<Test>]
let intervalDivisionByZero () =
    let source = """
        int x;
        int y;
        x := 2;
        y := 0;
        if (true) {
          x := x / y;
          write x;
        } else {
          x := y / x;
        }
        """
    let graph = FrontEnd.compile source
    let worklist = StackWorklist.empty()
    let analysis = IntervalAnalysis(graph, -2, 2)
    let (solution,_) = analysis.analyse graph worklist
    
    let x = Variable "x:1"
    let y = Variable "y:2"
    
    let expected = [
        [(x, I(NegInf, Inf)); (y, I(NegInf, Inf))]; //0: Initial
        [(x, I(Val 0, Val 0)); (y, I(NegInf, Inf))]; //1: int x;
        [(x, I(Val 0, Val 0)); (y, I(Val 0, Val 0))]; //2: int y;
        [(x, I(Val 2, Val 2)); (y, I(Val 0, Val 0))]; //3: x := 2
        [(x, I(Val 2, Val 2)); (y, I(Val 0, Val 0))]; //4: y := 0
        [(x, I(Val 2, Val 2)); (y, I(Val 0, Val 0))]; //5: true
        []; //6: x := x / y
        [(x, I(Val 0, Val 0)); (y, I(Val 0, Val 0))]; //7: x := y / x and write x
        [(x, I(Val 2, Val 2)); (y, I(Val 0, Val 0))]; //8: !true
        [(x, I(Val 0, Val 0)); (y, I(NegInf, Inf))]; //free y;
        [(x, I(NegInf, Inf)); (y, I(NegInf, Inf))]; //free x;
    ]
    
    
    let solution = solution |> Map.toList |> List.map (fun (_,v) -> Map.toList v)
    Assert.That(solution, Is.EqualTo(expected))

[<Test>]
let intervalAccessArrayOutOfBounds () =
    let source = """
        int x;
        int[1] A;
        read x;
        write A[x];
        write A[2];
        """
    let graph = FrontEnd.compile source
    let worklist = StackWorklist.empty()
    let analysis = IntervalAnalysis(graph, -2, 2)
    let (solution,_) = analysis.analyse graph worklist
    
    let x = Variable "x:1"
    let a = Array "A:2"
    
    let expected = [
        [(x, I(NegInf, Inf)); (a, I(NegInf, Inf))]; //0: Initial
        [(x, I(Val 0, Val 0)); (a, I(NegInf, Inf))]; //1: int x;
        [(x, I(Val 0, Val 0)); (a, I(Val 0, Val 0))]; //2: int[1] A;
        [(x, I(NegInf, Inf)); (a, I(Val 0, Val 0))]; //3: read x;
        [(x, I(NegInf, Inf)); (a, I(Val 0, Val 0))]; //4: write A[x];
        []; //5: write A[2];
        []; //6: free A;
        []; //7: free x;
    ]
    
    let solution = solution |> Map.toList |> List.map (fun (_,v) -> Map.toList v)
    Assert.That(solution, Is.EqualTo(expected))

[<Test>]
let intervalReadArrayOutOfBounds () =
    let source = """
        int x;
        int[1] A;
        read x;
        read A[x];
        read A[2];
        """
    let graph = FrontEnd.compile source
    let worklist = StackWorklist.empty()
    let analysis = IntervalAnalysis(graph, -2, 2)
    let (solution,_) = analysis.analyse graph worklist
    
    let x = Variable "x:1"
    let a = Array "A:2"
    
    let expected = [
        [(x, I(NegInf, Inf)); (a, I(NegInf, Inf))]; //0: Initial
        [(x, I(Val 0, Val 0)); (a, I(NegInf, Inf))]; //1: int x;
        [(x, I(Val 0, Val 0)); (a, I(Val 0, Val 0))]; //2: int[1] A;
        [(x, I(NegInf, Inf)); (a, I(Val 0, Val 0))]; //3: read x;
        [(x, I(NegInf, Inf)); (a, I(NegInf, Inf))]; //4: read A[x];
        []; //5: read A[2];
        []; //6: free A;
        []; //7: free x;
    ]
    
    let solution = solution |> Map.toList |> List.map (fun (_,v) -> Map.toList v)
    Assert.That(solution, Is.EqualTo(expected))
    
[<Test>]
let intervalAssignArrayOutOfBounds () =
    let source = """
        int x;
        int[1] A;
        read x;
        A[x] := 2;
        A[2] := 2;
        """
    let graph = FrontEnd.compile source
    let worklist = StackWorklist.empty()
    let analysis = IntervalAnalysis(graph, -2, 2)
    let (solution,_) = analysis.analyse graph worklist
    
    let x = Variable "x:1"
    let a = Array "A:2"
    
    let expected = [
        [(x, I(NegInf, Inf)); (a, I(NegInf, Inf))]; //0: Initial
        [(x, I(Val 0, Val 0)); (a, I(NegInf, Inf))]; //1: int x;
        [(x, I(Val 0, Val 0)); (a, I(Val 0, Val 0))]; //2: int[1] A;
        [(x, I(NegInf, Inf)); (a, I(Val 0, Val 0))]; //3: read x;
        [(x, I(NegInf, Inf)); (a, I(Val 0, Val 2))]; //4: read A[x];
        []; //5: read A[2];
        []; //6: free A;
        []; //7: free x;
    ]
    
    let solution = solution |> Map.toList |> List.map (fun (_,v) -> Map.toList v)
    Assert.That(solution, Is.EqualTo(expected))

[<Test>]
let intervalAssignRecord () =
    let source = """
        {int x; int y} R;
        R := {1,3};
        R := {R.x,R.y/0};
        """
    let graph = FrontEnd.compile source
    let worklist = StackWorklist.empty()
    let analysis = IntervalAnalysis(graph, -2, 2)
    let (solution,_) = analysis.analyse graph worklist
    
    let x = Field ("R:1","x")
    let y = Field ("R:1","y")
    
    let expected = [
        [(x, I(NegInf, Inf)); (y, I(NegInf, Inf))]; //0: Initial
        [(x, I(Val 0, Val 0)); (y, I(Val 0, Val 0))]; //1: {int x; int y} R;
        [(x, I(Val 1, Val 1)); (y, I(Val 2, Inf))]; //2: R := {1,3};
        []; //3: R := {R.x, R.y/0};
        []; //4: free A;
    ]
    
    let solution = solution |> Map.toList |> List.map (fun (_,v) -> Map.toList v)
    Assert.That(solution, Is.EqualTo(expected))