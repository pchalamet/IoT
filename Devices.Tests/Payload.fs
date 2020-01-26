module Tests.Devices.Payload

open System
open FsUnit
open NUnit.Framework
open Devices.Payload


[<Test>]
let ``Check payload conversion is ok`` () =
    let expected = [| 0x04uy; 0xDCuy; 0x7Euy; 0x37uy; 0xFFuy |]

    let payload = "04dc7e37ff"
    let binPayload = payload |> toBinaryPayload

    binPayload |> should equal expected

[<Test>]
let ``Payload length must be even`` () =
    let payload = "04dc7e37a"

    (fun () -> payload |> toBinaryPayload |> ignore) |> should throw typeof<ArgumentException>

