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

    /// <summary>
    /// Simplifies verification of critical conditions.
    /// </summary>
    let inline ( -||- ) condition exn = if not condition then raise exn

    /// <summary>
    /// Allows to apply a side effect without changing the value
    /// </summary>
    /// <param name="x">The value.</param>
    /// <param name="f">The side effect to apply.</param>
    let inline ( |>! ) x f = ignore(f x); x
