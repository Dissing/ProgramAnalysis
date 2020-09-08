namespace FrontEnd

open System.IO;

type SourceFile(path: string, content: string) =
    member this.Path = path
    member this.Content = content
    
    member this.computeLineBreaks content =
            let rec helper content acc =
                failwith "Not yet implemented"
            List.rev (helper content [])
    member this.Lines = this.computeLineBreaks content
    new(path) =
        if not (File.Exists path) then failwithf "Unable to open file %s" path
        let content = File.ReadAllText(path)
        SourceFile(path, content)