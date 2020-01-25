module Equipments.Tests

open System
open NUnit.Framework
open FsUnit

open Equipments.Mcf88.Comm


[<Test>]
let TestTemperatureHumidityPressure () =
    let expected = { TemperaturePressureHumidity.Measure1 = { TemperaturePressureHumidityMeasure.Timestamp = DateTime(2016, 9, 23, 15, 54, 56)
                                                              TemperaturePressureHumidityMeasure.Temperature = 27.40m
                                                              TemperaturePressureHumidityMeasure.Humidity = 35.5m
                                                              TemperaturePressureHumidityMeasure.Pressure = 1004.48m }
                     TemperaturePressureHumidity.Measure2 = { TemperaturePressureHumidityMeasure.Timestamp = DateTime(2016, 9, 23, 15, 54, 58)
                                                              TemperaturePressureHumidityMeasure.Temperature = 27.37m
                                                              TemperaturePressureHumidityMeasure.Humidity = 33.5m
                                                              TemperaturePressureHumidityMeasure.Pressure = 1004.48m }
                     TemperaturePressureHumidity.Measure3 = { TemperaturePressureHumidityMeasure.Timestamp = DateTime(2016, 9, 23, 15, 55, 00)
                                                              TemperaturePressureHumidityMeasure.Temperature = 27.38m
                                                              TemperaturePressureHumidityMeasure.Humidity = 33m
                                                              TemperaturePressureHumidityMeasure.Pressure = 1004.45m }
                     TemperaturePressureHumidity.BatteryLevel = BatteryLevel.NoData } |> UplinkMessage.TemperaturePressureHumidity

    let payload = "04dc7e3721b40a47608801dd7e3721b10a43608801e07e3721b20a425d8801"
    let message = parseUplinkMessage payload

    message |> should equal expected
