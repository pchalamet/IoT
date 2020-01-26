module VWP3000

type Configuration =
    { A : decimal
      B : decimal
      C : decimal
      Th : decimal
      T0 : decimal }


let compute (config : Configuration) (pressureMilliAmps : decimal) (temperatureMilliAmps : decimal) =
    let currentPressureHz = ((3500m - 1850m) / (20m - 4m)) * (pressureMilliAmps - 4m) + 1850m
    let currentTemperature = ((80m + 20m) / (20m - 4m)) * (temperatureMilliAmps- 4m) - 20m

    let currentPressure = config.A * currentPressureHz*currentPressureHz + config.B * currentPressureHz + config.C + config.Th * (currentTemperature - config.T0)
    currentPressure


