﻿namespace OpilioCraft.FSharp.Prelude

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
    with override x.Message = $"version of settings file is not supported\n{x.Type.Name}: expected {x.Expected}, found {x.Found}"

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

    // load user settings
    let load<'T> jsonFilename jsonOptions =
        File.Exists(jsonFilename) -||- IncompleteSetupException(MissingFile = jsonFilename)

        try
            let settingsAsJson = File.ReadAllText(jsonFilename)
            JsonSerializer.Deserialize<'T>(settingsAsJson, options = jsonOptions)
        with
        | exn -> raise <| InvalidUserSettingsException(File = jsonFilename, ErrorMessage = exn.Message)

    // load user settings on demand
    let lazyLoad<'T> jsonFilename jsonOptions = lazy ( load<'T> jsonFilename jsonOptions )

    // save
    let save<'T> jsonFile jsonOptions settings =
        let json = JsonSerializer.Serialize<'T>(settings, options = jsonOptions) in
        IO.saveGuard jsonFile <| fun uri -> File.WriteAllText(uri, json)

module Verify =
    [<Literal>]
    let VersionPropertyName = "Version"

    let tryGetVersion settings =
        settings.GetType().GetProperty(VersionPropertyName, typeof<Version>)
        |> Option.ofObj
        |> Option.bind (fun versionProperty -> versionProperty.GetValue(settings) :?> Version |> Some)

    let raiseIfNone jsonFile message =
        Option.orElseWith (fun () -> raise <| InvalidUserSettingsException(File = jsonFile, ErrorMessage = message))

    let isVersion (expectedVersion : Version) settings =
        tryGetVersion settings
        |> function
            | None ->
                raise <| IncompatibleVersionException(Type = settings.GetType(), Expected = expectedVersion, Found = Version())
            | Some foundVersion when foundVersion.CompareTo(expectedVersion) <> 0 ->
                raise <| IncompatibleVersionException(Type = settings.GetType(), Expected = expectedVersion, Found = foundVersion)
            | _ -> settings

    let isValidDirectory dir =
        match System.IO.Directory.Exists(dir) with
        | true -> Some dir
        | false -> None