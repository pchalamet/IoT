module VWP3000
open System

type Configuration =
    { A : decimal
      B : decimal
      Th : decimal }


let compute (config : Configuration) (pressureMilliAmps : decimal) (temperatureMilliAmps : decimal) (P0 : decimal) (T0 : decimal) =
    let C = -config.A * P0 * P0 - config.B * P0
    let pressureHz = ((3500m - 1850m) / (20m - 4m)) * (pressureMilliAmps - 4m) + 1850m
    let digit = (pressureHz * pressureHz) / 1000m
    let currentTemperature = ((80m + 20m) / (20m - 4m)) * (temperatureMilliAmps- 4m) - 20m

    let currentPressure = config.A * digit * digit + config.B * digit + C + config.Th * (currentTemperature - T0)
    currentPressure
