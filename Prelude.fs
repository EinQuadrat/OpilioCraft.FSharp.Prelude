namespace OpilioCraft.FSharp.Prelude

[<AutoOpen>]
module Prelude =
    let getAllUnionCases discrUnionType =
        Reflection.FSharpType.GetUnionCases(discrUnionType)
        |> Seq.map (fun case -> case.Name)
        |> Set.ofSeq
