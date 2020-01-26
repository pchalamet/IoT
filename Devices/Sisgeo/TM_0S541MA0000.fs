module Devices.Sisgeo.TM_0S541MA0000
open System

type Configuration =
    { A : float
      B : float
      C : float
      D : float
      Length : float }


let transform (config : Configuration) (input : float) =
    let sinAlpha = (config.A * input ** 3.0) + (config.B * input ** 2.0) + (config.C * input) + config.D
    let angleDegree = 180.0 * Math.Asin(sinAlpha) / Math.PI
    let displacement = Math.Asin(sinAlpha)  * config.Length
    ()
