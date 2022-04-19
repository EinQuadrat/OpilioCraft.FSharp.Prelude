namespace OpilioCraft.FSharp.Prelude

// simplify Result handling
[<RequireQualifiedAccessAttribute>]
module Result =
    let toOption = function
        | Ok result -> Some result
        | Error _ -> None

    let tee action = function
        | Ok value as result -> action value ; result
        | result -> result
