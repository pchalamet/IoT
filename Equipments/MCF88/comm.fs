module Equipments.MCF88.Comm

open System
open System.IO
open Helpers


   
[<RequireQualifiedAccess>]
type BatteryLevel = 
    | NoData
    | Percent of decimal


[<RequireQualifiedAccess>]
type TemperaturePressureHumidityMeasure =
    { Timestamp : DateTime
      Temperature : decimal 
      Humidity : decimal
      Pressure : decimal }

[<RequireQualifiedAccess>]
type TemperaturePressureHumidity =
    { Measure1 : TemperaturePressureHumidityMeasure
      Measure2 : TemperaturePressureHumidityMeasure
      Measure3 : TemperaturePressureHumidityMeasure
      BatteryLevel : BatteryLevel }



[<RequireQualifiedAccess>]
type AnalogDataMeasure =
    | NoData
    | Error
    | MilliAmp4_20 of decimal
    | Volt0_10 of decimal
    | Volt0_5 of decimal

[<RequireQualifiedAccess>]
type AnalogData =
    { Timestamp : DateTime
      Measure1 : AnalogDataMeasure
      Measure2 : AnalogDataMeasure
      Measure3 : AnalogDataMeasure
      Measure4 : AnalogDataMeasure
      BatteryLevel : BatteryLevel }





let private toTimestamp (time : int32) =
    let year = 2000 + ((time >>> 25) &&& 0x7F)
    let month = (time >>> 21) &&& 0x0F
    let day = (time >>> 16) &&& 0x1F
    let hour = (time >>> 11) &&& 0x1F
    let minutes = (time >>> 5) &&& 0x3F
    let seconds = (time &&& 0x1F) * 2
    DateTime(year, month, day, hour, minutes, seconds)


let private toBatteryLevel (b : byte) =
    b |> decimal |> BatteryLevel.Percent



type UplinkMessage =
    | TimeSyncRequest
    | TemperaturePressureHumidity of TemperaturePressureHumidity
    | Uart
    | Power
    | InputOutput
    | ReportData
    | TemperaturePressureHumidityLuxVoc
    | AnalogData of AnalogData
    | TPRhLuxVocCo2
    | Unknown


let private parseTemperaturePressureHumidityMeasure (binReader : BinaryReader) =
    let time = binReader.ReadInt32() |> toTimestamp
    let temperature = (binReader.ReadInt16() |> decimal) / 100.0m
    let humidity = (binReader.ReadByte() |> decimal) / 2.0m
    let pressure = (BitConverter.ToInt32(Array.append (binReader.ReadBytes(3)) [| 0uy |], 0) |> decimal) / 100.0m
    { TemperaturePressureHumidityMeasure.Timestamp = time
      TemperaturePressureHumidityMeasure.Temperature = temperature
      TemperaturePressureHumidityMeasure.Humidity = humidity
      TemperaturePressureHumidityMeasure.Pressure = pressure }

let private parseTemperaturePressureHumidity (binReader : BinaryReader) =
    let measure1 = binReader |> parseTemperaturePressureHumidityMeasure
    let measure2 = binReader |> parseTemperaturePressureHumidityMeasure
    let measure3 = binReader |> parseTemperaturePressureHumidityMeasure

    let batteryLevel = if binReader |> isEof then BatteryLevel.NoData
                       else binReader.ReadByte() |> toBatteryLevel
    { TemperaturePressureHumidity.Measure1 = measure1
      TemperaturePressureHumidity.Measure2 = measure2
      TemperaturePressureHumidity.Measure3 = measure3
      TemperaturePressureHumidity.BatteryLevel = batteryLevel }




let toTemperaturePressureHumidityMeasure (s : int16) =
    let hasError = (s &&& 0x1000s) <> 0s
    let dataType = (s &&& 0x6000s) >>> 13
    let data = s &&& 0xFFFs

    match hasError, data with
    | true, 0s -> AnalogDataMeasure.NoData
    | true, _ -> AnalogDataMeasure.Error
    | false, _ -> match dataType with 
                  | 0s -> (((decimal)data/ 4095.0m) * 16.0m + 4.0m) |> AnalogDataMeasure.MilliAmp4_20 
                  | 1s -> (decimal)data |> AnalogDataMeasure.Volt0_10
                  | 2s -> (decimal)data |> AnalogDataMeasure.Volt0_5
                  | _ -> AnalogDataMeasure.Error

let private parseAnalogData (binReader : BinaryReader) =
    let time = binReader.ReadInt32() |> toTimestamp
    let measure1 = binReader.ReadInt16() |> toTemperaturePressureHumidityMeasure
    let measure2 = binReader.ReadInt16() |> toTemperaturePressureHumidityMeasure
    let measure3 = binReader.ReadInt16() |> toTemperaturePressureHumidityMeasure
    let measure4 = binReader.ReadInt16() |> toTemperaturePressureHumidityMeasure

    let batteryLevel = if binReader |> isEof then BatteryLevel.NoData
                       else binReader.ReadByte() |> toBatteryLevel

    { AnalogData.Timestamp = time 
      AnalogData.Measure1 = measure1
      AnalogData.Measure2 = measure2
      AnalogData.Measure3 = measure3
      AnalogData.Measure4 = measure4
      AnalogData.BatteryLevel = batteryLevel }


let toBinaryPayload (payload : string) = 
    payload |> Seq.splitInto (payload.Length / 2)
            |> Seq.map (fun c2 -> String(c2))
            |> Seq.map (fun s -> Byte.Parse(s, Globalization.NumberStyles.HexNumber))
            |> Array.ofSeq

let parseUplinkMessage (payload : string) =
    let binPayload = payload |> toBinaryPayload

    use memStream = new MemoryStream(binPayload)
    use binReader = new BinaryReader(memStream)

    let upLinkId = binReader.ReadByte()

    match upLinkId with
    | 0x01uy -> UplinkMessage.TimeSyncRequest
    | 0x04uy -> parseTemperaturePressureHumidity binReader |> UplinkMessage.TemperaturePressureHumidity
    | 0x05uy -> UplinkMessage.Uart
    | 0x09uy -> UplinkMessage.Power
    | 0x0Auy -> UplinkMessage.InputOutput
    | 0x0Buy -> UplinkMessage.ReportData
    | 0x0Cuy -> UplinkMessage.TemperaturePressureHumidityLuxVoc
    | 0x0Duy -> parseAnalogData binReader |> UplinkMessage.AnalogData
    | 0x0Euy -> UplinkMessage.TPRhLuxVocCo2
    | _ -> UplinkMessage.Unknown
