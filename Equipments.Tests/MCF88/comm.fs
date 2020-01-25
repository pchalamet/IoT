module Equipments.Tests

open NUnit.Framework
open Equipments.Mcf88.Comm


[<Test>]
let TestTemperatureHumidityPressure () =
    let payload = "04dc7e3721b40a47608801dd7e3721b10a43608801e07e3721b20a425d8801"
    let message = parseUplinkMessage payload
    match message with
    | TemperaturePressureHumidity tph -> ()
    | _ -> failwith "Invalid message"
