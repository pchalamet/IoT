module Test.Equipments.MCF88.Comm

open System
open NUnit.Framework
open FsUnit

open Equipments.MCF88.Comm
open Equipments.MCF88.Comm.Measures
open Equipments.MCF88.Comm.Messages

[<Test>]
let TestTemperatureHumidityPressure () =
    let expected = { Measure1 = { Timestamp = DateTime(2016, 9, 23, 15, 54, 56)
                                  Temperature = 27.40m
                                  Humidity = 35.5m
                                  Pressure = 1004.48m }
                     Measure2 = { Timestamp = DateTime(2016, 9, 23, 15, 54, 58)
                                  Temperature = 27.37m
                                  Humidity = 33.5m
                                  Pressure = 1004.48m }
                     Measure3 = { Timestamp = DateTime(2016, 9, 23, 15, 55, 00)
                                  Temperature = 27.38m
                                  Humidity = 33m
                                  Pressure = 1004.45m }
                     Battery = None } |> TemperaturePressureHumidity

    let payload = "04dc7e3721b40a47608801dd7e3721b10a43608801e07e3721b20a425d8801"
    let message = DecodeUplinkMessage payload

    message |> should equal expected
