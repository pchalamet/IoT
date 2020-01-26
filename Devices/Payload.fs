module Devices.Payload

open System
open System.IO


let toBinaryPayload (payload : string) = 
    if payload.Length % 2 <> 0 then invalidArg "payload" "Invalid payload size"

    payload |> Seq.splitInto (payload.Length / 2)
            |> Seq.map (fun c2 -> Byte.Parse(String(c2), Globalization.NumberStyles.HexNumber))
            |> Array.ofSeq


let isEof (br : BinaryReader) =
    br.BaseStream.Position >= br.BaseStream.Length
