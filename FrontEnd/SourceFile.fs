namespace FrontEnd

type SourceFile(path: string, content: string) =
    member this.Path = path
    member this.Content = content
    
    member this.Lines =
        let rec helper content acc =
            failwith "Not yet implemented"
        List.rev (helper content [])