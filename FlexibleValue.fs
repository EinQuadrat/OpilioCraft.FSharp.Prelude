namespace OpilioCraft.FSharp.Prelude

[<RequireQualifiedAccess>]
type FlexibleValue =
    | Boolean of bool
    | String of string
    | Numeral of int
    | Float of float
    | Decimal of decimal
    | DateTime of System.DateTime
    | Date of System.DateOnly
    | Time of System.TimeOnly
    | TimeSpan of System.TimeSpan

    with
        static member inline Wrap (incoming : 'a) : FlexibleValue =
            match box incoming with
            | :? bool as boolValue -> Boolean boolValue
            | :? string as stringValue -> String stringValue
            | :? int as intValue -> Numeral intValue
            | :? float as floatValue -> Float floatValue
            | :? decimal as decimalValue -> Decimal decimalValue
            | :? System.DateOnly as dateValue -> Date dateValue
            | :? System.TimeOnly as timeValue -> Time timeValue
            | :? System.DateTime as datetimeValue -> DateTime datetimeValue
            | :? System.TimeSpan as timespanValue -> TimeSpan timespanValue
            | _ -> invalidArg "value" $"{nameof(FlexibleValue)} does not support incoming type {incoming.GetType().Name}"
