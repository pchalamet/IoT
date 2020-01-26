module VWP3000
open System

type Configuration =
    { A : float
      B : float
      Th : float }


let transform (config : Configuration) (pressureMilliAmps : float) (temperatureMilliAmps : float) (P0 : float) (T0 : float) =
    let C = -config.A * P0 * P0 - config.B * P0
    let pressureHz = ((3500.0 - 1850.0) / (20.0 - 4.0)) * (pressureMilliAmps - 4.0) + 1850.0
    let digit = (pressureHz * pressureHz) / 1000.0
    let currentTemperature = ((80.0 + 20.0) / (20.0 - 4.0)) * (temperatureMilliAmps- 4.0) - 20.0

    let currentPressure = config.A * digit * digit + config.B * digit + C + config.Th * (currentTemperature - T0)
    currentPressure
