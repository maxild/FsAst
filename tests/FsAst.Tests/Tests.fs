module Tests

open FSharp.Compiler.Ast
open FSharp.Compiler.Range
open FsAst
open Xunit

[<Fact>]
let ``create basic class`` () =
    let ty =
        SynModuleDecl.CreateObjectType (
            SynComponentInfoRcd.Create (Ident.CreateLong "Triangle"),
            [   SynMemberDefn.CreateImplicitCtor()
                SynMemberDefn.CreateMember
                    { SynBindingRcd.Null with
                          Pattern = SynPatRcd.CreateLongIdent(LongIdentWithDots.CreateString "x.Points", [])
                          Expr = SynExpr.CreateConst(SynConst.Int32 3) }
            ]
        )

    let mdl = "BasicClass"

    let parseTree =
        ParsedInput.CreateImplFile(
            ParsedImplFileInputRcd.CreateFs(mdl)
                .AddModule(
                    SynModuleOrNamespaceRcd.CreateModule(Ident.CreateLong mdl)
                        .AddDeclaration(ty)
                )
        )

    let source = formatAst parseTree

    // TODO: Uncommet and wrap type in a module or namespace
//    let expected = """module Hello

// type Triangle() =
//    member x.Points = 3
//"""
    let expected = """type Triangle() =
    member x.Points = 3
"""

    // TODO: check fantomas tests
    Assert.Equal(expected, source)

[<Fact>]
let ``create cli enum`` () =
    let typ =
        SynModuleDecl.CreateSimpleType (
            { SynComponentInfoRcd.Create (Ident.CreateLong "CXErrorCode") with
                XmlDoc = PreXmlDoc.Create [ " enum uint32" ]
            },
            SynTypeDefnSimpleReprEnumRcd.Create(
                [   SynEnumCaseRcd.Create(Ident.Create "CXError_Success", SynConst.UInt32 0u)
                    SynEnumCaseRcd.Create(Ident.Create "CXError_Failure", SynConst.UInt32 1u)
                    SynEnumCaseRcd.Create(Ident.Create "CXError_Crashed", SynConst.UInt32 2u)
                    SynEnumCaseRcd.Create(Ident.Create "CXError_InvalidArguments", SynConst.UInt32 3u)
                    SynEnumCaseRcd.Create(Ident.Create "CXError_ASTReadError", SynConst.UInt32 4u)
                ]
            )
            |> SynTypeDefnSimpleReprRcd.Enum
        )

    let mdl = "BasicEnum"

    let parseTree =
        ParsedInput.CreateImplFile(
            ParsedImplFileInputRcd.CreateFs(mdl)
                .AddModule(
                    SynModuleOrNamespaceRcd.CreateModule(Ident.CreateLong mdl)
                        .AddDeclaration(typ)
                )
        )

    let source = formatAst parseTree

//    let expected = """module Enum

//    /// enum uint32
//    type CXErrorCode =
//        | CXError_Success = 0u
//        | CXError_Failure = 1u
//        | CXError_Crashed = 2u
//        | CXError_InvalidArguments = 3u
//        | CXError_ASTReadError = 4u
//"""

    let expected = """/// enum uint32
type CXErrorCode =
    | CXError_Success = 0u
    | CXError_Failure = 1u
    | CXError_Crashed = 2u
    | CXError_InvalidArguments = 3u
    | CXError_ASTReadError = 4u
"""

    Assert.Equal(expected, source)

[<Fact>]
let ``create basic PInvoke`` () =
    let opn = SynModuleDecl.CreateOpen (LongIdentWithDots.CreateString "System.Runtime.InteropServices")

    let at : SynAttribute =
        {   TypeName = LongIdentWithDots.CreateString "DllImport"
            ArgExpr =
                SynExpr.CreateParen(
                    SynExpr.CreateTuple(
                        false,
                        [   SynExpr.CreateConstString "blas.dll"
                            SynExpr.CreateApp(
                                SynExpr.CreateAppInfix(
                                    SynExpr.CreateIdentString "op_Equality",
                                    SynExpr.CreateIdentString "EntryPoint"),
                                SynExpr.CreateConstString "dgemm_"
                            )
                        ]
                    )
                )
            Target = None
            AppliesToGetterAndSetter = false
            Range = range.Zero
        }

    let args =
        [   "char", "transa"
            "char", "transb"
            "int", "m"
            "int", "n"
            "int", "k"
            "double", "alpha"
            "double", "a"
            "int", "lda"
            "double", "b"
            "int", "ldb"
            "double", "beta"
            "double", "c"
            "int", "ldc"
        ]
        |> List.map (fun (typ, name) ->
            SynPatRcd.CreateAttrib(
                SynPatRcd.CreateTyped(
                    SynPatRcd.CreateNamed(Ident.Create name, SynPatRcd.CreateWild),
                    SynType.CreateApp(SynType.CreateLongIdent(LongIdentWithDots.CreateString "nativeptr"),
                        [SynType.CreateApp(SynType.CreateLongIdent(LongIdentWithDots.CreateString typ), [])])
                ),
                []
            )
        )

    let dgemm =
        SynModuleDecl.CreateLet(
            { SynBindingRcd.Let with
                Pattern = SynPatRcd.CreateLongIdent(LongIdentWithDots.CreateString "dgemm_", [SynPatRcd.CreateTuple(false, args)])
                ReturnInfo = SynBindingReturnInfoRcd.Create(SynType.CreateApp(SynType.CreateUnit, [])) |> Some
                Attributes = [ { Attributes = [at]
                                 Range = range.Zero }]
            } |> List.singleton
     )

    let mdl = "BasicPInvoke"

    let parseTree =
        ParsedInput.CreateImplFile(
            ParsedImplFileInputRcd.CreateFs(mdl)
                .AddModule(
                    SynModuleOrNamespaceRcd.CreateModule(Ident.CreateLong mdl)
                        .AddDeclaration(opn)
                        .AddDeclaration(dgemm)
                )
        )

    let source = formatAst parseTree

    let expected = """open System.Runtime.InteropServices

[<DllImport("blas.dll", EntryPoint = "dgemm_")>]
extern unit dgemm_(nativeptr<char> transa, nativeptr<char> transb, nativeptr<int> m, nativeptr<int> n, nativeptr<int> k, nativeptr<double> alpha, nativeptr<double> a, nativeptr<int> lda, nativeptr<double> b, nativeptr<int> ldb, nativeptr<double> beta, nativeptr<double> c, nativeptr<int> ldc)
"""
    Assert.Equal(expected, source)
