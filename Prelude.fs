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

    /// <summary>
    /// Check if a value is null. Useful e.g. in context of JSON parsing.
    /// </summary>
    let inline isNull value = value = Unchecked.defaultof<_>

    /// <summary>
    /// Check if a value is not null. Useful e.g. in context of JSON parsing.
    /// </summary>
    let inline isNotNull value = not (isNull value)
