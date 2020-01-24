module Equipments.Mcf88.Modem

open System
open System.IO
open EnumHelpers

[<RequireQualifiedAccess>]
type UpLinkId =
    | TimeSyncRequest = 0x01uy
    | TemperaturePressureHumidity = 0x04uy
    | Uart = 0x05uy
    | Power = 0x09uy
    | InputOutput = 0x0Auy
    | ReportData = 0x0Buy
    | TemperaturePressureHumidityLuxVoc = 0x0Cuy
    | AnalogData = 0x0Duy
    | TPRhLuxVocCo2 = 0x0Euy

[<RequireQualifiedAccess>]
type BatteryLevel = 
    | NoData
    | Level of byte

[<RequireQualifiedAccess>]
type BatteryInfo =
    { Level : BatteryLevel }

[<RequireQualifiedAccess>]
type DataType = 
    | MilliAmp4_20 = 0
    | Volt0_10 = 1
    | Volt0_5 = 2

[<RequireQualifiedAccess>]
type ChannelData =
    | NoData
    | Error
    | MilliAmp4_20 of float
    | Volt0_10 of float
    | Volt0_5 of float
   

let private toProductId (b : byte array) =
    if b.Length <> 4 then failwithf "Invalid ProductId buffer length"
    b |> string

let private toUpLinkId (b : byte) =
    if Enum.IsDefined(typeof<UpLinkId>, b) |> not then failwithf "Invalid UpLinkId value %A" b
    let res : UpLinkId = b |> LanguagePrimitives.EnumOfValue
    res

let private toDataType (s : int16) =
    if Enum.IsDefined(typeof<UpLinkId>, s) |> not then failwithf "Invalid DataType value %A" s
    let res : DataType = s |> int |> LanguagePrimitives.EnumOfValue
    res

let private toBatteryLevel (b : byte) =
    b |> BatteryLevel.Level

let toChannelData (s : int16) =
    let hasError = (s &&& 0x1000s) <> 0s
    let dataType = (s &&& 0x6000s) >>> 13 |> toDataType
    let data = s &&& 0xFFFs

    match hasError, data with
    | true, 0s -> ChannelData.NoData
    | true, _ -> ChannelData.Error
    | false, _ -> match dataType with 
                  | DataType.MilliAmp4_20 -> (((float)data/ 4095.0) * 16.0 + 4.0) |> ChannelData.MilliAmp4_20 
                  | DataType.Volt0_10 -> (float)data |> ChannelData.Volt0_10
                  | DataType.Volt0_5 -> (float)data |> ChannelData.Volt0_5
                  | _ -> ChannelData.Error

type AnalogDataFrame =
    { Data : Map<string, int16>
      BatteryInfo : BatteryInfo }

type Data =
    | AnalogData of AnalogDataFrame
    | Unsupported

type Frame =
    { ProductId : string
      Timestamp : int32
      Data : Data }

let private parseAnalogData (binReader : BinaryReader) =
    let data0 = binReader.ReadInt16()
    let data1 = binReader.ReadInt16()
    let data2 = binReader.ReadInt16()
    let data3 = binReader.ReadInt16()
    let data = [ ("data0", data0)
                 ("data1", data1)
                 ("data2", data2)
                 ("data3", data3)] |> Map
    let batteryLevel = binReader.ReadByte() |> toBatteryLevel
    let batteryInfo = { BatteryInfo.Level = batteryLevel }
    
    { AnalogDataFrame.Data = data
      AnalogDataFrame.BatteryInfo = batteryInfo } 


let ParseFrame (frame : byte array) =
    use memStream = new MemoryStream(frame)
    use binReader = new BinaryReader(memStream)

    let productId = binReader.ReadBytes(4) |> toProductId
    let upLinkId = binReader.ReadByte() |> toUpLinkId
    binReader.ReadByte() |> ignore
    let timestamp = binReader.ReadInt32()

    match upLinkId with
    | UpLinkId.AnalogData -> parseAnalogData binReader |> Data.AnalogData
    | _ -> Data.Unsupported
