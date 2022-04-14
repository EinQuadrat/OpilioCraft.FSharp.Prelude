namespace OpilioCraft.FSharp.Prelude

exception FlexibleValueException of ErrorMsg:string
    with override x.ToString () = $"[FlexibleValue] {x.ErrorMsg}"

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
        
        static member inline TryWrap (incoming : 'a) : FlexibleValue option =
            match box incoming with
            | :? FlexibleValue as flexValue       -> Some flexValue // prevent nesting
            | :? System.Boolean as boolValue      -> Some (Boolean boolValue)
            | :? System.Int32 as intValue         -> Some (Numeral intValue)
            | :? System.Double as floatValue      -> Some (Float floatValue)
            | :? System.Decimal as decimalValue   -> Some (Decimal decimalValue)
            | :? System.String as stringValue     -> Some (String stringValue)
            | :? System.DateOnly as dateValue     -> Some (Date dateValue)
            | :? System.TimeOnly as timeValue     -> Some (Time timeValue)
            | :? System.DateTime as datetimeValue -> Some (DateTime datetimeValue)
            | :? System.TimeSpan as timespanValue -> Some (TimeSpan timespanValue)
            | _ -> None

        static member inline Wrap (incoming : 'a) : FlexibleValue =
            FlexibleValue.TryWrap incoming
            |> Option.defaultWith (raise <| FlexibleValueException $"unsupported support incoming type: {incoming.GetType().FullName}")

        member x.TryCompareTo value : int option =
            let compareFlexibleValues (fv1 : FlexibleValue) (fv2 : FlexibleValue) : int option =
                match (fv1, fv2) with
                    | FlexibleValue.Boolean a, FlexibleValue.Boolean b -> a.CompareTo(b) |> Some
                    | FlexibleValue.Numeral a, FlexibleValue.Numeral b -> a.CompareTo(b) |> Some
                    | FlexibleValue.Decimal a, FlexibleValue.Decimal b -> a.CompareTo(b) |> Some
                    | FlexibleValue.String a, FlexibleValue.String b -> System.String.Compare(a, b) |> Some
                    | FlexibleValue.Date a, FlexibleValue.Date b -> a.CompareTo(b) |> Some
                    | FlexibleValue.Time a, FlexibleValue.Time b -> a.CompareTo(b) |> Some
                    | FlexibleValue.DateTime a, FlexibleValue.DateTime b -> a.CompareTo(b) |> Some
                    | _ -> None // comparison is not supported

            match box value with
                | :? FlexibleValue as flexValue -> compareFlexibleValues x flexValue
                | other ->
                    try
                        FlexibleValue.TryWrap other
                        |> Option.bind (fun value -> compareFlexibleValues x value)
                    with
                        | _ -> None
