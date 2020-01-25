module Helpers

open System.IO


let isEof (br : BinaryReader) =
    br.BaseStream.Position >= br.BaseStream.Length
