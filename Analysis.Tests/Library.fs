namespace Analysis.Tests


module Utils =

    open Analysis
    open FrontEnd
    open NUnit.Framework

    let insertElements (w: IWorklist) (elems: List<ProgramGraph.Node>): IWorklist = 
        List.fold (fun (s: IWorklist) i -> s.insert(i)) w elems
    
    let extractElements (w: IWorklist) (elems: List<ProgramGraph.Node>): IWorklist =
        List.fold (fun (s: IWorklist) i ->
            match s.extract() with
            | Some((q,w)) -> Assert.That(q, Is.EqualTo(i))
                             w
            | None -> Assert.Fail()
                      w
            ) w elems

