// Learn more about F# at http://fsharp.org

open System
open System.Text.Json
open System.Text.Json.Serialization

let usage() =
    printfn "Decoder <payload>"

let serialize o =
    let options = JsonSerializerOptions()
    options.WriteIndented <- true
    options.Converters.Add(JsonFSharpConverter())
    JsonSerializer.Serialize(o, options)


let decode s =
    let msg = Devices.MCF88.MCFLW.DecodeUplinkMessage s
    printfn "%A" msg
    printfn "%s" (msg |> serialize)

[<EntryPoint>]
let main argv =
    for s in argv do
        printfn ">>> %s" s
        decode s

    0


