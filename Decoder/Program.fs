// Learn more about F# at http://fsharp.org

open System
open System.IO
open System.Text.Json
open System.Text.Json.Serialization

open Devices.MCF88.MCFLW


let usage() =
    printfn "Decoder <payload>"

[<EntryPoint>]
let main argv =
    for payload in argv do
        let msg = Devices.MCF88.MCFLW.DecodeUplinkMessage payload
        printfn "%s" payload
        printfn "%A" msg
    0
