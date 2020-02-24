module Test.Devices.MCF88.MCFLW

open System
open NUnit.Framework
open FsUnit

open Devices.MCF88.MCFLW
open Devices.MCF88.MCFLW.Measures
open Devices.MCF88.MCFLW.Messages

[<Test>]
let TestTemperatureHumidityPressure () =
    let expected = { Measure1 = { Timestamp = DateTime(2016, 9, 23, 15, 54, 56)
                                  Temperature = 27.40
                                  Humidity = 35.5
                                  Pressure = 1004.48 }
                     Measure2 = { Timestamp = DateTime(2016, 9, 23, 15, 54, 58)
                                  Temperature = 27.37
                                  Humidity = 33.5
                                  Pressure = 1004.48 }
                     Measure3 = { Timestamp = DateTime(2016, 9, 23, 15, 55, 00)
                                  Temperature = 27.38
                                  Humidity = 33.0
                                  Pressure = 1004.45 }
                     Battery = None } |> TemperaturePressureHumidity

    let payload = "04dc7e3721b40a47608801dd7e3721b10a43608801e07e3721b20a425d8801"
    let message = DecodeUplinkMessage payload

    message |> should equal expected

[<Test>]
let TestAnalogData() =
    let expected = { Timestamp = new DateTime(2020, 2, 19, 17, 44, 20)
                     Measure1 = MilliAmp4to20 20.0
                     Measure2 = MilliAmp4to20 4.0429792429792428
                     Measure3 = NoData
                     Measure4 = NoData
                     Battery = Some 93.0 } |> AnalogData 

    let payload = "0d018a8d5328ff0f0b00001000105d"
    let message = DecodeUplinkMessage payload

    message |> should equal expected

[<Test>]
let TestAnalogDataWithoutBattery() =
    let expected = { Timestamp = new DateTime(2020, 2, 19, 17, 44, 20)
                     Measure1 = MilliAmp4to20 20.0
                     Measure2 = MilliAmp4to20 4.0429792429792428
                     Measure3 = NoData
                     Measure4 = NoData
                     Battery = None } |> AnalogData 

    let payload = "0d018a8d5328ff0f0b0000100010"
    let message = DecodeUplinkMessage payload

    message |> should equal expected
