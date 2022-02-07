namespace OpilioCraft.FSharp.Prelude

open System
open System.IO
open System.Text.Json
open System.Text.Json.Serialization

open Microsoft.FSharp.Reflection

// exceptions
exception IncompleteSetupException of MissingFile:string
    with override x.Message = $"incomplete user settings; missing file is: {x.MissingFile}"
exception InvalidUserSettingsException of File:string * ErrorMessage:string
    with override x.Message = $"invalid user settings; file {x.File} contains error: {x.ErrorMessage}"

// json converters
type EnumUnionConverter<'T> () =
    inherit JsonConverter<'T> ()

    let cases = FSharpType.GetUnionCases(typeof<'T>) |> Array.map (fun case -> case.Name, case) |> Map.ofArray

    override _.Read (reader : byref<Utf8JsonReader>, _: Type, _: JsonSerializerOptions) =
        let rawCase = reader.GetString()

        try
            let casePrototype = cases.[rawCase]
            FSharpValue.MakeUnion(casePrototype, args = [| |]) :?> 'T
        with
        | :? System.Collections.Generic.KeyNotFoundException -> failwith $"[{nameof(EnumUnionConverter)}] \"{rawCase}\" is not a valid case for {typeof<'T>.Name}"

    override _.Write (writer: Utf8JsonWriter, value: 'T, _: JsonSerializerOptions) =
        writer.WriteStringValue(value.ToString())


module UserSettingsHelper =
    let (-||-) condition exn = if not condition then raise exn

    // load user settings on demand
    let lazyLoad<'T> jsonFilename jsonOptions =
        lazy (
            File.Exists(jsonFilename) -||- IncompleteSetupException(MissingFile = jsonFilename)

            try
                let settingsAsJson = File.ReadAllText(jsonFilename)
                JsonSerializer.Deserialize<'T>(settingsAsJson, options = jsonOptions)
            with
            | exn -> raise <| InvalidUserSettingsException(File = jsonFilename, ErrorMessage = exn.Message)
        )

module Verify =
    let raiseIfNone jsonFile message =
        Option.orElseWith (fun () -> raise <| InvalidUserSettingsException(File = jsonFile, ErrorMessage = message))

    let isValidDirectory dir =
        match System.IO.Directory.Exists(dir) with
        | true -> Some dir
        | false -> None
