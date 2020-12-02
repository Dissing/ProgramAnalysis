namespace FrontEnd

type Context<'state> =
        | Ok of 'state
        | Error of string * Span
        
        member __.unwrap() =
            match __ with
            | Ok(s) -> s
            | Error (msg, span) -> failwithf "Unwrapped error %s at [%d:%d]" msg span.From span.To

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
            | Error (msg, span) -> failwithf "Unwrapped error %s at [%d:%d]" msg span.From span.To
            
[<AutoOpen>]
module ContextBuilderImpl =
    let context = ContextBuilder()
    let inline (>>=) m f = context.Bind(m,f)