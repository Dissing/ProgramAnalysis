module FrontEnd.FrontEnd

    let compile (program: string) =
        let ast = program |> Lexer.lex >>= Parser.parse >>= Resolution.resolve
        let pg = EdgesFunction.runEdges (ast.unwrap())
        pg