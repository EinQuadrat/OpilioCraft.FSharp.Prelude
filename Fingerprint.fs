namespace OpilioCraft.FSharp.Prelude

type Fingerprint = string

type QualifiedFingerprint =
    | Full of Fingerprint
    | Partly of Fingerprint
    | Derived of Fingerprint
    | Unknown

    member x.Value =
        match x with
        | Full x | Partly x | Derived x -> x
        | Unknown -> invalidOp $"[{nameof QualifiedFingerprint}] cannot extract value of unknown fingerprint"

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Fingerprint =
    open System.IO
    open System.Security.Cryptography
    open System.Text.RegularExpressions

    // Algorithm used
    let private hashingAlgorithm = SHA256.Create();

    // Helper functions
    let private readStream (stream : FileStream) =
        seq {
            let mutable currentByte = 0

            let moveNext () =
                currentByte <- stream.ReadByte()
                currentByte >= 0

            while moveNext () do
                yield byte currentByte
        }

    let private readBytes filename length =
        use stream = File.OpenRead(filename)
        readStream stream |> Seq.truncate length |> Seq.toArray

    let private convertBytesToString (bytes : byte[]) =
        bytes |> Array.fold (fun resultString b -> resultString + b.ToString("x2")) ""
    
    // Fingerprint calculations
    let calculatePartlyFingerprint filename =
        hashingAlgorithm.ComputeHash(readBytes filename 1024)

    let calculateFingerprint filename =
        use stream = new FileStream(filename, FileMode.Open, FileAccess.Read, FileShare.ReadWrite, 16 * 1024 * 1024)
        hashingAlgorithm.ComputeHash(stream)

    // Converter functions
    let fingerprintAsString = calculateFingerprint >> convertBytesToString
    let partlyFingerprintAsString = calculatePartlyFingerprint >> convertBytesToString

    // Typed result
    let getFullFingerprint filename = filename |> fingerprintAsString |> QualifiedFingerprint.Full
    let getPartlyFingerprint filename = filename |> partlyFingerprintAsString |> QualifiedFingerprint.Partly

    // Guess fingerprint from filename
    let private _fingerprintRegex = Regex(@"^(.+)#([0-9a-z]{64})$", RegexOptions.Compiled)

    let tryGuessFingerprint (filename : string) : Fingerprint option =
        let matchResult =
            filename
            |> Path.GetFileNameWithoutExtension
            |> _fingerprintRegex.Match

        if matchResult.Success then
            Some <| matchResult.Groups.[2].Value
        else
            None

    let getPlainFilename (filename : string) : string =
        match filename.LastIndexOf('#') with
        | -1 -> filename
        | found -> filename.Substring(0, found)

    // High-level API
    type Strategy =
        | Calculate = 0
        | GuessFirst = 1

    let tryParseStrategy (input : string) : Strategy option =
        match System.Enum.TryParse<Strategy>(input, true) with
        | true, value -> Some value
        | _ -> None

    let getFingerprint (strategy : Strategy) (filename : string) =
        match strategy with
        | Strategy.GuessFirst -> tryGuessFingerprint filename
        | _ -> None

        |> Option.map QualifiedFingerprint.Derived
        |> Option.defaultValue (getFullFingerprint filename)
