namespace Driver

module ArgumentParser =

    type Opt = {
        Short: string
        Long: string
        Desc: string
        Hint: string
        HasArg: bool
    }

    type Opts = Opt list
    
    type OptMatch = {
        Name: string
        Arg: string option
    }
        
    type Matches = {
        Opts: OptMatch list
        Free: string list
    }

    let addFlag (short: string) (long: string) (desc: string) (opts: Opts) =
        let opt = {
            Short = short;
            Long = long;
            Desc = desc;
            Hint = "";
            HasArg = false;
        }
        opt::opts
        
    let addOpt (short: string) (long: string) (desc: string) (hint: string) (opts: Opts) =
        let opt = {
            Short = short;
            Long = long;
            Desc = desc;
            Hint = hint;
            HasArg = true;
        }
        opt::opts
        
    let parse (opts: Opts) (args: string list): Result<Matches, string> =
        
        let shortFlags = Map.ofList <| List.map (fun o -> ("-" + o.Short, o.Short)) opts
        let longFlags = Map.ofList <| List.map (fun o -> ("--" + o.Long, o.Short)) opts
        
        let optsMap = Map.ofList <| List.map (fun o -> (o.Short, o)) opts
        
        let rec iter optMatches free (args: string list): Result<(OptMatch list) * (string list),string> =
            match args with
            | x::xs ->
                let split = x.Split('=')
                let tryShort = shortFlags.TryFind split.[0]
                let tryLong = longFlags.TryFind split.[0]
                match (tryShort, tryLong) with
                | (Some(n),None) | (None, Some(n)) ->
                    let opt = optsMap.Item n
                    let arg =
                        if opt.HasArg then
                            if split.Length = 2 then
                                Ok(Some(split.[1]))
                            else
                                Error(sprintf "Option %s must have an argument: -%s=%s!" n opt.Short opt.Hint)
                        else
                            Ok(None)
                    match arg with
                    | Ok(arg) ->
                        let optMatch = {
                            Name = n
                            Arg = arg
                        }
                        iter (optMatch::optMatches) free xs
                    | Error(s) -> Error(s)
                | (None, None) -> 
                    iter optMatches (x::free) xs
                | other ->
                    failwithf "Internal command line argument parsing fault: %A" other
            | [] ->
                Ok((List.rev optMatches, List.rev free))
         
        if args.Length > 1 then
            match iter [] [] args.Tail with
            | Ok((optMatches, free)) -> Ok({ Opts = optMatches; Free = free })
            | Error(s) -> Error(s)
        else
            Ok({ Opts = []; Free = [] })
 