module Equipments.MCF88.Comm

open System
open System.IO
open Helpers


// === Measures

[<RequireQualifiedAccess>]
type TPRhMeasure =
    { Timestamp : DateTime
      Temperature : decimal 
      Humidity : decimal
      Pressure : decimal }

[<RequireQualifiedAccess>]
type TPRhLuxVocMeasure =
    { TPRh : TPRhMeasure 
      Luminance : decimal 
      Voc : decimal }

[<RequireQualifiedAccess>]
type TPRhLuxVocCo2Measure =
    { TPRhLuxVoc : TPRhLuxVocMeasure
      Co2 : decimal }

[<RequireQualifiedAccess>]
type AnalogDataMeasure =
    | NoData
    | Error
    | MilliAmp4to20 of decimal
    | Volt0to10 of decimal
    | Volt0to5 of decimal




let private decodeTimestamp (binReader : BinaryReader) =
    let time = binReader.ReadInt32()
    let year = 2000 + ((time >>> 25) &&& 0x7F)
    let month = (time >>> 21) &&& 0x0F
    let day = (time >>> 16) &&& 0x1F
    let hour = (time >>> 11) &&& 0x1F
    let minutes = (time >>> 5) &&& 0x3F
    let seconds = (time &&& 0x1F) * 2
    DateTime(year, month, day, hour, minutes, seconds)


let decodeAnalogDataMeasure (binReader : BinaryReader) =
    let measure = binReader.ReadInt16()
    let hasError = (measure &&& 0x1000s) <> 0s
    let dataType = (measure &&& 0x6000s) >>> 13
    let data = measure &&& 0xFFFs

    match hasError, data with
    | true, 0s -> AnalogDataMeasure.NoData
    | true, _ -> AnalogDataMeasure.Error
    | false, _ -> match dataType with 
                  | 0s -> (((decimal)data/ 4095.0m) * 16.0m + 4.0m) |> AnalogDataMeasure.MilliAmp4to20 
                  | 1s -> (decimal)data |> AnalogDataMeasure.Volt0to10
                  | 2s -> (decimal)data |> AnalogDataMeasure.Volt0to5
                  | _ -> AnalogDataMeasure.Error


let private decodeTPRhMeasure (binReader : BinaryReader) =
    let time = binReader |> decodeTimestamp
    let temperature = (binReader.ReadInt16() |> decimal) / 100.0m
    let humidity = (binReader.ReadByte() |> decimal) / 2.0m
    let pressure = (BitConverter.ToInt32(Array.append (binReader.ReadBytes(3)) [| 0uy |], 0) |> decimal) / 100.0m
    { TPRhMeasure.Timestamp = time
      TPRhMeasure.Temperature = temperature
      TPRhMeasure.Humidity = humidity
      TPRhMeasure.Pressure = pressure }


let private decodeTPRhLuxVocMeasure (binReader : BinaryReader) =
    let tprh = binReader |> decodeTPRhMeasure
    let lux = binReader.ReadInt16() |> decimal
    let voc = binReader.ReadInt16() |> decimal

    { TPRhLuxVocMeasure.TPRh = tprh
      TPRhLuxVocMeasure.Luminance = lux
      TPRhLuxVocMeasure.Voc = voc }


let private decodeTPRhLuxVocCo2Measure (binReader : BinaryReader) =
    let tprhlv = binReader |> decodeTPRhLuxVocMeasure
    let co2 = binReader.ReadInt16() |> decimal

    { TPRhLuxVocCo2Measure.TPRhLuxVoc = tprhlv
      TPRhLuxVocCo2Measure.Co2 = co2 }


// === Messages

[<RequireQualifiedAccess>]
type TemperaturePressureHumidity =
    { Measure1 : TPRhMeasure
      Measure2 : TPRhMeasure
      Measure3 : TPRhMeasure
      Battery : decimal option }

[<RequireQualifiedAccess>]
type TemperaturePressureHumidityLuxVoc =
    { Measure1 : TPRhLuxVocMeasure
      Measure2 : TPRhLuxVocMeasure
      Battery : decimal option }

[<RequireQualifiedAccess>]
type TPRhLuxVocCo2 =
    { Measure1 : TPRhLuxVocCo2Measure
      Measure2 : TPRhLuxVocCo2Measure
      Battery : decimal option }

[<RequireQualifiedAccess>]
type AnalogData =
    { Timestamp : DateTime
      Measure1 : AnalogDataMeasure
      Measure2 : AnalogDataMeasure
      Measure3 : AnalogDataMeasure
      Measure4 : AnalogDataMeasure
      Battery : decimal option }


let private parseAnalogData (binReader : BinaryReader) =
    let time = binReader |> decodeTimestamp
    let measure1 = binReader |> decodeAnalogDataMeasure
    let measure2 = binReader |> decodeAnalogDataMeasure
    let measure3 = binReader |> decodeAnalogDataMeasure
    let measure4 = binReader |> decodeAnalogDataMeasure

    let batteryLevel = if binReader |> isEof then None
                       else binReader.ReadByte() |> decimal |> Some

    { AnalogData.Timestamp = time 
      AnalogData.Measure1 = measure1
      AnalogData.Measure2 = measure2
      AnalogData.Measure3 = measure3
      AnalogData.Measure4 = measure4
      AnalogData.Battery = batteryLevel }



let private parseTemperaturePressureHumidity (binReader : BinaryReader) =
    let measure1 = binReader |> decodeTPRhMeasure
    let measure2 = binReader |> decodeTPRhMeasure
    let measure3 = binReader |> decodeTPRhMeasure

    let batteryLevel = if binReader |> isEof then None
                       else binReader.ReadByte() |> decimal |> Some
    { TemperaturePressureHumidity.Measure1 = measure1
      TemperaturePressureHumidity.Measure2 = measure2
      TemperaturePressureHumidity.Measure3 = measure3
      TemperaturePressureHumidity.Battery = batteryLevel }



let private parseTemperaturePressureHumidityLuxVoc (binReader : BinaryReader) =
    let measure1 = binReader |> decodeTPRhLuxVocMeasure
    let measure2 = binReader |> decodeTPRhLuxVocMeasure
    let batteryLevel = if binReader |> isEof then None
                       else binReader.ReadByte() |> decimal |> Some

    { TemperaturePressureHumidityLuxVoc.Measure1 = measure1
      TemperaturePressureHumidityLuxVoc.Measure2 = measure2
      TemperaturePressureHumidityLuxVoc.Battery = batteryLevel }



let private parseTPRhLuxVocCo2 (binReader : BinaryReader) =
    let measure1 = binReader |> decodeTPRhLuxVocCo2Measure
    let measure2 = binReader |> decodeTPRhLuxVocCo2Measure
    let batteryLevel = if binReader |> isEof then None
                       else binReader.ReadByte() |> decimal |> Some

    { TPRhLuxVocCo2.Measure1 = measure1
      TPRhLuxVocCo2.Measure2 = measure2
      TPRhLuxVocCo2.Battery = batteryLevel }


// Main decoder

type UplinkMessage =
    | TimeSyncRequest
    | TemperaturePressureHumidity of TemperaturePressureHumidity
    | Uart
    | Power
    | InputOutput
    | ReportData
    | TemperaturePressureHumidityLuxVoc of TemperaturePressureHumidityLuxVoc
    | AnalogData of AnalogData
    | TPRhLuxVocCo2 of TPRhLuxVocCo2
    | Unknown

let private toBinaryPayload (payload : string) = 
    payload |> Seq.splitInto (payload.Length / 2)
            |> Seq.map (fun c2 -> Byte.Parse(String(c2), Globalization.NumberStyles.HexNumber))
            |> Array.ofSeq

let decodeUplinkMessage (payload : string) =
    let binPayload = payload |> toBinaryPayload

    use memStream = new MemoryStream(binPayload)
    use binReader = new BinaryReader(memStream)

    let upLinkId = binReader.ReadByte()
    match upLinkId with
    | 0x01uy -> UplinkMessage.TimeSyncRequest
    | 0x04uy -> binReader |> parseTemperaturePressureHumidity |> TemperaturePressureHumidity
    | 0x05uy -> UplinkMessage.Uart
    | 0x09uy -> UplinkMessage.Power
    | 0x0Auy -> UplinkMessage.InputOutput
    | 0x0Buy -> UplinkMessage.ReportData
    | 0x0Cuy -> binReader |> parseTemperaturePressureHumidityLuxVoc |> TemperaturePressureHumidityLuxVoc
    | 0x0Duy -> binReader |> parseAnalogData |> AnalogData
    | 0x0Euy -> binReader |> parseTPRhLuxVocCo2 |> TPRhLuxVocCo2
    | _ -> UplinkMessage.Unknown
