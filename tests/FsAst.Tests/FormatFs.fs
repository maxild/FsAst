[<AutoOpen>]
module FormatFs

    open Fantomas

    let formatAst ast =
        let fileName = "/tmp.fsx"
        let cfg = { FormatConfig.FormatConfig.Default with StrictMode = true; PageWidth = 120 } // do not format comments
        let emptySource = "" |> SourceOrigin.SourceString |> Some
        CodeFormatter.FormatASTAsync(ast, fileName, [], emptySource, cfg) |> Async.RunSynchronously

