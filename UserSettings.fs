namespace OpilioCraft.FSharp.Prelude

open System
open System.IO
open System.Text.Json
open System.Text.Json.Serialization

open Microsoft.FSharp.Reflection

// exceptions
exception IncompleteSetupException of MissingFile:string
    with override x.Message = $"incomplete user settings; missing file: {x.MissingFile}"

exception InvalidUserSettingsException of File:string * ErrorMessage:string
    with override x.Message = $"invalid user settings; file {x.File} contains error: {x.ErrorMessage}"

exception IncompatibleVersionException of Type:Type * Expected:Version * Found:Version
    with override x.Message = $"version of settings file is not supported\n{x.Type.FullName}: expected {x.Expected}, found {x.Found}"

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


module UserSettings =
    // load user settings
    let loadWithOptions<'T> jsonFilename jsonOptions =
        File.Exists(jsonFilename) -||- IncompleteSetupException(MissingFile = jsonFilename)

        try
            let settingsAsJson = File.ReadAllText(jsonFilename)
            JsonSerializer.Deserialize<'T>(settingsAsJson, options = jsonOptions)
        with
        | exn -> raise <| InvalidUserSettingsException(File = jsonFilename, ErrorMessage = exn.Message)

    let load<'T> jsonFilename = loadWithOptions<'T> jsonFilename (JsonSerializerOptions.Default)

    // load user settings on demand
    let lazyLoadWithOptionsAndVerify<'T> jsonFilename jsonOptions (verifier : 'T -> 'T) = lazy ( loadWithOptions<'T> jsonFilename jsonOptions |> verifier )
    let lazyLoadWithOptions<'T> jsonFilename jsonOptions = lazyLoadWithOptionsAndVerify<'T> jsonFilename jsonOptions id

    let lazyLoadAndVerify<'T> jsonFilename (verifier : 'T -> 'T) = lazyLoadWithOptionsAndVerify<'T> jsonFilename (JsonSerializerOptions.Default) verifier
    let lazyLoad<'T> jsonFilename = lazyLoadAndVerify<'T> jsonFilename id

    // save
    let saveWithOptions<'T> jsonFile jsonOptions settings =
        let json = JsonSerializer.Serialize<'T>(settings, options = jsonOptions) in
        IO.saveGuard jsonFile <| fun uri -> File.WriteAllText(uri, json)

    let save<'T> jsonFile settings = saveWithOptions<'T> jsonFile (JsonSerializerOptions.Default)

    // supportive functions
    let tryGetProperty name settings =
        settings.GetType().GetProperty(name)
        |> Option.ofObj
        |> Option.map (fun prop -> prop.GetValue(box settings))
        |> Option.bind (function | :? 'r as typedValue -> Some typedValue | _ -> None)

    let hasProperty name settings =
        settings.GetType().GetProperty(name)
        |> Option.ofObj
        |> Option.isSome

module Verify =
    [<Literal>]
    let VersionPropertyName = "Version"

    let tryGetVersion settings : Version option =
        UserSettings.tryGetProperty VersionPropertyName settings

    let isVersion (expectedVersion : Version) settings =
        tryGetVersion settings
        |> function
            | None ->
                raise <| IncompatibleVersionException(Type = settings.GetType(), Expected = expectedVersion, Found = Version())
            | Some foundVersion when foundVersion.CompareTo(expectedVersion) <> 0 ->
                raise <| IncompatibleVersionException(Type = settings.GetType(), Expected = expectedVersion, Found = foundVersion)
            | _ -> settings

    let raiseIfNone jsonFile message =
        Option.orElseWith (fun _ -> raise <| InvalidUserSettingsException(File = jsonFile, ErrorMessage = message))

    let isValidDirectory path =
        match Directory.Exists(path) with
        | true -> Some path
        | false -> None

    let isValidFile path =
        match File.Exists(path) with
        | true -> Some path
        | false -> None
