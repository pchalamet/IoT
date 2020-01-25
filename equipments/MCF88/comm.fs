module Equipments.Mcf88.Comm

open System
open System.IO
open Helpers


[<RequireQualifiedAccess>]
type ChannelData =
    | NoData
    | Error
    | MilliAmp4_20 of float
    | Volt0_10 of float
    | Volt0_5 of float
   
[<RequireQualifiedAccess>]
type BatteryLevel = 
    | NoData
    | Percent of int



type TemperaturePressureHumidity =
    { Timestamp : DateTime
      Temperature : float 
      Humidity : float
      Pressure : float 
      BatteryLevel : BatteryLevel }

type AnalogData =
    { Timestamp : DateTime
      Data1 : ChannelData
      Data2 : ChannelData
      Data3 : ChannelData
      Data4 : ChannelData
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
    b |> int |> BatteryLevel.Percent


let toChannelData (s : int16) =
    let hasError = (s &&& 0x1000s) <> 0s
    let dataType = (s &&& 0x6000s) >>> 13
    let data = s &&& 0xFFFs

    match hasError, data with
    | true, 0s -> ChannelData.NoData
    | true, _ -> ChannelData.Error
    | false, _ -> match dataType with 
                  | 0s -> (((float)data/ 4095.0) * 16.0 + 4.0) |> ChannelData.MilliAmp4_20 
                  | 1s -> (float)data |> ChannelData.Volt0_10
                  | 2s -> (float)data |> ChannelData.Volt0_5
                  | _ -> ChannelData.Error

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



let private parseTemperaturePressureHumidity (binReader : BinaryReader) =
    let time = binReader.ReadInt32() |> toTimestamp
    let temperature = (binReader.ReadInt16() |> float) / 100.0
    let humidity = (binReader.ReadByte() |> float) / 2.0
    let pressure = (BitConverter.ToInt32(Array.append (binReader.ReadBytes(3)) [| 0uy |], 0) |> float) / 100.0
    let batteryLevel = if binReader |> isEof then BatteryLevel.NoData
                       else binReader.ReadByte() |> toBatteryLevel
    { TemperaturePressureHumidity.Timestamp = time
      TemperaturePressureHumidity.Temperature = temperature
      TemperaturePressureHumidity.Humidity = humidity
      TemperaturePressureHumidity.Pressure = pressure
      TemperaturePressureHumidity.BatteryLevel = batteryLevel }


let private parseAnalogData (binReader : BinaryReader) =
    let time = binReader.ReadInt32() |> toTimestamp
    let data1 = binReader.ReadInt16() |> toChannelData
    let data2 = binReader.ReadInt16() |> toChannelData
    let data3 = binReader.ReadInt16() |> toChannelData
    let data4 = binReader.ReadInt16() |> toChannelData

    let batteryLevel = if binReader |> isEof then BatteryLevel.NoData
                       else binReader.ReadByte() |> toBatteryLevel

    { AnalogData.Timestamp = time 
      AnalogData.Data1 = data1
      AnalogData.Data2 = data2
      AnalogData.Data3 = data3
      AnalogData.Data4 = data4
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
