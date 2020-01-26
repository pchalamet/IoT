// payload decoder for MCF88 IO devices:
// MCF-LW06485
// MCF-LW06VMC
// MCF-LW06232
// MCF-LW06420
// MCF-LW06010
// MCF-LW06424
// MCF-LW06420D
// MCF-LW06010D
// MCF-LW06424D
// MCF-LW06KIO
// MCF-LW12PLG
// MCF-LW12MET
// MCF-LW13IO
// MCF-LW13MIO
// MCF-LW12TERWP
// MCF-LW12TERPM
// MCF-LW12TER
// MCF-LW12VOC
// MCF-LW12CO2
// MCF-LWWS00
// MCF-LWWS01
// MCF-LW06DAVK
// MCF-LW06DAVKP

module Devices.MCF88.MCFLW
open System
open System.IO
open Devices.Payload

// === Measures
module Measures =
    type TPRh =
        { Timestamp : DateTime
          Temperature : decimal 
          Humidity : decimal
          Pressure : decimal }

    type TPRhLuxVoc =
        { TPRh : TPRh
          Luminance : decimal 
          Voc : decimal }

    type TPRhLuxVocCo2 =
        { TPRhLuxVoc : TPRhLuxVoc
          Co2 : decimal }

    type AnalogData =
        | NoData
        | Error
        | MilliAmp4to20 of decimal
        | Volt0to10 of decimal
        | Volt0to5 of decimal


    let decodeTimestamp (binReader : BinaryReader) =
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
        | true, 0s -> NoData
        | true, _ -> Error
        | false, _ -> match dataType with 
                      | 0s -> (((decimal)data/ 4095.0m) * 16.0m + 4.0m) |> MilliAmp4to20 
                      | 1s -> (decimal)data |> Volt0to10
                      | 2s -> (decimal)data |> Volt0to5
                      | _ -> Error


    let decodeTPRhMeasure (binReader : BinaryReader) =
        let time = binReader |> decodeTimestamp
        let temperature = (binReader.ReadInt16() |> decimal) / 100.0m
        let humidity = (binReader.ReadByte() |> decimal) / 2.0m
        let pressure = (BitConverter.ToInt32(Array.append (binReader.ReadBytes(3)) [| 0uy |], 0) |> decimal) / 100.0m

        { TPRh.Timestamp = time
          TPRh.Temperature = temperature
          TPRh.Humidity = humidity
          TPRh.Pressure = pressure }


    let decodeTPRhLuxVocMeasure (binReader : BinaryReader) =
        let tprh = binReader |> decodeTPRhMeasure
        let lux = binReader.ReadInt16() |> decimal
        let voc = binReader.ReadInt16() |> decimal

        { TPRhLuxVoc.TPRh = tprh
          TPRhLuxVoc.Luminance = lux
          TPRhLuxVoc.Voc = voc }


    let decodeTPRhLuxVocCo2Measure (binReader : BinaryReader) =
        let tprhlv = binReader |> decodeTPRhLuxVocMeasure
        let co2 = binReader.ReadInt16() |> decimal

        { TPRhLuxVocCo2.TPRhLuxVoc = tprhlv
          TPRhLuxVocCo2.Co2 = co2 }


// === Messages
module Messages =
    type TemperaturePressureHumidity =
        { Measure1 : Measures.TPRh
          Measure2 : Measures.TPRh
          Measure3 : Measures.TPRh
          Battery : decimal option }

    type TemperaturePressureHumidityLuxVoc =
        { Measure1 : Measures.TPRhLuxVoc
          Measure2 : Measures.TPRhLuxVoc
          Battery : decimal option }

    type TPRhLuxVocCo2 =
        { Measure1 : Measures.TPRhLuxVocCo2
          Measure2 : Measures.TPRhLuxVocCo2
          Battery : decimal option }

    type AnalogData =
        { Timestamp : DateTime
          Measure1 : Measures.AnalogData
          Measure2 : Measures.AnalogData
          Measure3 : Measures.AnalogData
          Measure4 : Measures.AnalogData
          Battery : decimal option }


    let parseAnalogData (binReader : BinaryReader) =
        let time = binReader |> Measures.decodeTimestamp
        let measure1 = binReader |> Measures.decodeAnalogDataMeasure
        let measure2 = binReader |> Measures.decodeAnalogDataMeasure
        let measure3 = binReader |> Measures.decodeAnalogDataMeasure
        let measure4 = binReader |> Measures.decodeAnalogDataMeasure
        let batteryLevel = if binReader |> isEof then None
                            else binReader.ReadByte() |> decimal |> Some

        { AnalogData.Timestamp = time 
          AnalogData.Measure1 = measure1
          AnalogData.Measure2 = measure2
          AnalogData.Measure3 = measure3
          AnalogData.Measure4 = measure4
          AnalogData.Battery = batteryLevel }



    let parseTemperaturePressureHumidity (binReader : BinaryReader) =
        let measure1 = binReader |> Measures.decodeTPRhMeasure
        let measure2 = binReader |> Measures.decodeTPRhMeasure
        let measure3 = binReader |> Measures.decodeTPRhMeasure
        let batteryLevel = if binReader |> isEof then None
                            else binReader.ReadByte() |> decimal |> Some

        { TemperaturePressureHumidity.Measure1 = measure1
          TemperaturePressureHumidity.Measure2 = measure2
          TemperaturePressureHumidity.Measure3 = measure3
          TemperaturePressureHumidity.Battery = batteryLevel }



    let parseTemperaturePressureHumidityLuxVoc (binReader : BinaryReader) =
        let measure1 = binReader |> Measures.decodeTPRhLuxVocMeasure
        let measure2 = binReader |> Measures.decodeTPRhLuxVocMeasure
        let batteryLevel = if binReader |> isEof then None
                           else binReader.ReadByte() |> decimal |> Some

        { TemperaturePressureHumidityLuxVoc.Measure1 = measure1
          TemperaturePressureHumidityLuxVoc.Measure2 = measure2
          TemperaturePressureHumidityLuxVoc.Battery = batteryLevel }



    let parseTPRhLuxVocCo2 (binReader : BinaryReader) =
        let measure1 = binReader |> Measures.decodeTPRhLuxVocCo2Measure
        let measure2 = binReader |> Measures.decodeTPRhLuxVocCo2Measure
        let batteryLevel = if binReader |> isEof then None
                           else binReader.ReadByte() |> decimal |> Some

        { TPRhLuxVocCo2.Measure1 = measure1
          TPRhLuxVocCo2.Measure2 = measure2
          TPRhLuxVocCo2.Battery = batteryLevel }


// === Main decoder
type UplinkMessage =
    | TimeSyncRequest
    | TemperaturePressureHumidity of Messages.TemperaturePressureHumidity
    | Uart
    | Power
    | InputOutput
    | ReportData
    | TemperaturePressureHumidityLuxVoc of Messages.TemperaturePressureHumidityLuxVoc
    | AnalogData of Messages.AnalogData
    | TPRhLuxVocCo2 of Messages.TPRhLuxVocCo2
    | Unknown

let DecodeUplinkMessage (payload : string) =
    let binPayload = payload |> toBinaryPayload
    use memStream = new MemoryStream(binPayload)
    use binReader = new BinaryReader(memStream)

    let upLinkId = binReader.ReadByte()
    match upLinkId with
    | 0x01uy -> TimeSyncRequest
    | 0x04uy -> binReader |> Messages.parseTemperaturePressureHumidity |> TemperaturePressureHumidity
    | 0x05uy -> Uart
    | 0x09uy -> Power
    | 0x0Auy -> InputOutput
    | 0x0Buy -> ReportData
    | 0x0Cuy -> binReader |> Messages.parseTemperaturePressureHumidityLuxVoc |> TemperaturePressureHumidityLuxVoc
    | 0x0Duy -> binReader |> Messages.parseAnalogData |> AnalogData
    | 0x0Euy -> binReader |> Messages.parseTPRhLuxVocCo2 |> TPRhLuxVocCo2
    | _ -> Unknown
