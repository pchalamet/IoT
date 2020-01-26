module Devices.Sisgeo.CM0D313SA5000
open System

type Configuration =
    { A : float
      B : float
      C : float
      D : float }


let transform (config : Configuration) (input : float) =
    let output = (config.A * input ** 3.0) + (config.B * input ** 2.0) + (config.C * input) + config.D
    output
