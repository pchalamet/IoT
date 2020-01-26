module Equipments.Tests

open System
open NUnit.Framework
open FsUnit

open Equipments.MCF88.Comm


[<Test>]
let TestTemperatureHumidityPressure () =
    let expected = { TemperaturePressureHumidity.Measure1 = { TPRhMeasure.Timestamp = DateTime(2016, 9, 23, 15, 54, 56)
                                                              TPRhMeasure.Temperature = 27.40m
                                                              TPRhMeasure.Humidity = 35.5m
                                                              TPRhMeasure.Pressure = 1004.48m }
                     TemperaturePressureHumidity.Measure2 = { TPRhMeasure.Timestamp = DateTime(2016, 9, 23, 15, 54, 58)
                                                              TPRhMeasure.Temperature = 27.37m
                                                              TPRhMeasure.Humidity = 33.5m
                                                              TPRhMeasure.Pressure = 1004.48m }
                     TemperaturePressureHumidity.Measure3 = { TPRhMeasure.Timestamp = DateTime(2016, 9, 23, 15, 55, 00)
                                                              TPRhMeasure.Temperature = 27.38m
                                                              TPRhMeasure.Humidity = 33m
                                                              TPRhMeasure.Pressure = 1004.45m }
                     TemperaturePressureHumidity.Battery = None } |> UplinkMessage.TemperaturePressureHumidity

    let payload = "04dc7e3721b40a47608801dd7e3721b10a43608801e07e3721b20a425d8801"
    let message = decodeUplinkMessage payload

    message |> should equal expected
