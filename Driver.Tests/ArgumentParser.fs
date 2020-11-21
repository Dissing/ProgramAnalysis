module Driver.Tests

open NUnit.Framework
open Driver.ArgumentParser
    
let unwrap = function
    | Ok x -> x
    | Error s -> failwithf "Tried to unwrap Error %s" s

[<Test>]
let flags() =
    let shortArgs = ["-a"; "-c"; "-b"]
    let longArgs = ["--A"; "--C"; "--B"]
    let mixArgs = ["-a"; "--C"; "-b"]
    
    let opts =
        []
        |> addFlag "a" "A" ""
        |> addFlag "b" "B" ""
        |> addFlag "c" "C" ""
   
    let optA = { Name = "a"; Arg = None }
    let optB = { Name = "b"; Arg = None }
    let optC = { Name = "c"; Arg = None }
    
    let expected = [optA; optC; optB]
    
    let shortMatches = parse opts shortArgs
    Assert.That(unwrap shortMatches, Is.EqualTo({Opts = expected; Free = []}))
    
    let longMatches = parse opts longArgs
    Assert.That(unwrap longMatches, Is.EqualTo({Opts = expected; Free = []}))
        
    let mixMatches = parse opts mixArgs
    Assert.That(unwrap mixMatches, Is.EqualTo({Opts = expected; Free = []}))


[<Test>]
let opts() =
    let shortArgs = ["-a=foo"; "-c=bar"; "-b=spam"]
    let longArgs = ["--A=foo"; "--C=bar"; "--B=spam"]
    let mixArgs = ["-a=foo"; "--C=bar"; "-b=spam"]
    
    let opts =
        []
        |> addOpt "a" "A" "" "ARG"
        |> addOpt "b" "B" "" "ARG"
        |> addOpt "c" "C" "" "ARG"
        
    let optA = { Name = "a"; Arg = Some("foo") }
    let optB = { Name = "b"; Arg = Some("spam") }
    let optC = { Name = "c"; Arg = Some("bar") }
    let expected = [optA; optC; optB]
    
    let shortMatches = parse opts shortArgs
    Assert.That(unwrap shortMatches, Is.EqualTo({Opts = expected; Free = []}))
        
    let longMatches = parse opts longArgs
    Assert.That(unwrap longMatches, Is.EqualTo({Opts = expected; Free = []}))
            
    let mixMatches = parse opts mixArgs
    Assert.That(unwrap mixMatches, Is.EqualTo({Opts = expected; Free = []}))

[<Test>]
let flagsAndOpts() =
    let args = ["-a=foo"; "--C"; "--B=bar"; "-d"]
    
    let opts =
        []
        |> addOpt "a" "A" "" "ARG"
        |> addOpt "b" "B" "" "ARG"
        |> addFlag "c" "C" ""
        |> addFlag "d" "D" ""
        
    let optA = { Name = "a"; Arg = Some("foo") }
    let optB = { Name = "b"; Arg = Some("bar") }
    let optC = { Name = "c"; Arg = None }
    let optD = { Name = "d"; Arg = None }
    let expected = [optA; optC; optB; optD]
    
    let matches = parse opts args
    Assert.That(unwrap matches, Is.EqualTo({Opts = expected; Free = []}))
    
[<Test>]
let freeArgs() =
    let args = ["hello"; "world"]
    
    let opts =
        []
        |> addOpt "a" "A" "" "ARG"
        
    let matches = parse opts args
    Assert.That(unwrap matches, Is.EqualTo({Opts = []; Free = ["hello"; "world"]}))
    
[<Test>]
let floatingFreeArgs() =
    let args = ["-p"; "hello.c"; "-o=lexer.txt";"--stage=lexer"]
    
    let opts =
        []
        |> addFlag "p" "P" ""
        |> addOpt "o" "" "" "ARG"
        |> addOpt "s" "stage" "" "ARG"
    
    let optP = { Name = "p"; Arg = None }
    let optO = { Name = "o"; Arg = Some("lexer.txt") }
    let optS = { Name = "s"; Arg = Some("lexer") }
        
    let matches = parse opts args
    Assert.That(unwrap matches, Is.EqualTo({Opts = [optP; optO; optS]; Free = ["hello.c"]}))