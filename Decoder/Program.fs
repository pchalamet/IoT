// Learn more about F# at http://fsharp.org

open System
open System.IO
open System.Text.Json
open System.Text.Json.Serialization

open Devices.MCF88.MCFLW


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
    let content = System.IO.File.ReadAllLines(argv.[0])
    for s in content do 
        let msg = Devices.MCF88.MCFLW.DecodeUplinkMessage s
        match msg with 
        | UplinkMessage.AnalogData m -> printfn "%A;%A" m.Timestamp m.Battery
        | _ -> printfn "Unknown data"


    //for s in argv do
    //    printfn ">>> %s" s
    //    decode s

    0


