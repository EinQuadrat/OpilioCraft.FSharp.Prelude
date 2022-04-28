namespace OpilioCraft.FSharp.Prelude

[<AutoOpen>]
module Prelude =
    /// <summary>
    /// Get union cases as string sequence.
    /// </summary>
    /// <param name="discrUnionType">DU type to be analyzed.</param>
    let getAllUnionCases discrUnionType =
        Reflection.FSharpType.GetUnionCases(discrUnionType)
        |> Seq.map (fun case -> case.Name)
        |> Set.ofSeq
