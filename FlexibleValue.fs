namespace OpilioCraft.FSharp.Prelude

[<RequireQualifiedAccess>]
type FlexibleValue =
    // primitive types
    | Boolean of bool
    | Numeral of int
    | Float of float
    | Decimal of decimal
    | String of string
    // object types
    | Date of System.DateOnly
    | Time of System.TimeOnly
    | DateTime of System.DateTime
    | TimeSpan of System.TimeSpan

    with
        static member inline Wrap (incoming : 'a) : FlexibleValue =
            match box incoming with
            | :? FlexibleValue as flexValue -> flexValue // prevent nexted values
            | :? bool as boolValue -> Boolean boolValue
            | :? int as intValue -> Numeral intValue
            | :? float as floatValue -> Float floatValue
            | :? decimal as decimalValue -> Decimal decimalValue
            | :? string as stringValue -> String stringValue
            | :? System.DateOnly as dateValue -> Date dateValue
            | :? System.TimeOnly as timeValue -> Time timeValue
            | :? System.DateTime as datetimeValue -> DateTime datetimeValue
            | :? System.TimeSpan as timespanValue -> TimeSpan timespanValue
            | _ -> invalidArg "value" $"{nameof(FlexibleValue)} does not support incoming type {incoming.GetType().FullName}"

        override x.ToString () =
            match x with
            | FlexibleValue.Boolean boolValue -> boolValue.ToString()
            | FlexibleValue.Numeral numValue -> numValue.ToString()
            | FlexibleValue.Float numValue -> numValue.ToString()
            | FlexibleValue.Decimal numValue -> numValue.ToString()
            | FlexibleValue.String stringValue -> stringValue
            | FlexibleValue.Date dateValue -> dateValue.ToString("yyyy-MM-dd")
            | FlexibleValue.Time timeValue -> timeValue.ToString("hh:mm:ss")
            | FlexibleValue.DateTime datetimeValue -> datetimeValue.ToString("yyyy-MM-ddThh:mm:ss")
            | FlexibleValue.TimeSpan timespanValue -> timespanValue.ToString("yyyy-MM-ddThh:mm:ss")
