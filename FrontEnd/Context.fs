namespace FrontEnd

type Context<'state> =
        | Ok of 'state
        | Error of string * int

type ContextBuilder() =
        member __.Return(a) = Ok(a)
        member __.ReturnFrom(a) = a

        member __.Bind(m, f) =
            match m with
            | Ok (s) -> f s
            | Error (msg, i) -> Error(msg, i)
            
        member __.map(m, f) =
            match m with
            | Ok(s) -> Ok(f s)
            | Error (msg, i) -> Error(msg, i)
            
        member __.unwrap(m) =
            match m with
            | Ok(s) -> s
            | Error (msg, i) -> failwithf "Unwrapped error %s at %d" msg i
            
[<AutoOpen>]
module ContextBuilderImpl =
    let context = ContextBuilder()
    let inline (>>=) m f = context.Bind(m,f)