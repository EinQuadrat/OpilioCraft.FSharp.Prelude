namespace OpilioCraft.FSharp.Prelude

// simplify Option handling
[<RequireQualifiedAccessAttribute>]
module Option =
    let ifNone action maybe =
        if Option.isNone maybe then action ()
        maybe
