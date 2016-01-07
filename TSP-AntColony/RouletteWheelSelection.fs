namespace AntColonyOptimization

open AntColonyOptimization.RequiredTypes
open AntColonyOptimization.Settings

module Selectors = 
    let RouletteWheelSelection p =
        let r = rnd.NextDouble()
        let c = p|> Array.mapi (fun idx elem -> p.[..idx] |> Array.sum) 
        let cc= c |> Array.tryFindIndex (fun elem -> r <= elem) 
        cc