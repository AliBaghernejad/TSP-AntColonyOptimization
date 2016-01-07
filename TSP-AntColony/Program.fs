// Ant Colony Optimization for Traveling Salesman Problem (TSP)
// ACO was proposed in 1991 by M.Dorigo
// By Ali Baghernejad
// www.Baghernejad.ir

module AntColonyOptimization.Main

open System
open AntColonyOptimization.RequiredTypes
open AntColonyOptimization
open AntColonyOptimization.Helpers

let costFunc (r: Route) (distanceMatrix:float [][]) = 
    // Calculate cost value for specified route
    let mutable cost =  0.0
    // Add first city to the end of route array to create a tour.
    let route =  [| r.[0] |] |> Array.append r
    for i=0 to route.Length-2 do
        cost <- cost + distanceMatrix.[route.[i]].[route.[i+1]] 
    // Return cost value
    cost

// ---------- Initialization-----------

printfn "Initializing ant colony optimization..."

let distanceMatrix = distanceGenerator Settings.citiesCount
let heuristicMatrix = etaGenerator distanceMatrix
let pheromonMatrix = tauGenerator Settings.citiesCount
let antsPopulation = antGenerator Settings.antCount
let bestAnt = {Route=[||]; Cost= infinity}
let bestSolutions = Array.zeroCreate<Ant> Settings.iterationSize

// ----------Main processing loop-----------

for i = 0 to Settings.iterationSize-1 do
    for j = 0 to Settings.antCount-1 do
        // For each ant in ants population first of all select a random city.
        // and add it to ant's route array.
        
        antsPopulation.[j].Route <- [| rnd.Next(Settings.citiesCount) |]

        for k = 1 to Settings.citiesCount-1 do
            // In this step ant need to select next city. 
            // to do this he need to calculate p value for neighbour cities

            // Select last visited city.
            let lastVisitedCity = antsPopulation.[j].Route.[(Array.length antsPopulation.[j].Route)-1]

            // Calculate p value for neighbour cities
            let a = Array.map (fun elem -> pown elem Settings.alpha) pheromonMatrix.[lastVisitedCity]
            let b = Array.map (fun elem -> pown elem Settings.beta) heuristicMatrix.[lastVisitedCity]
            let p = Array.map2 (fun elem1 elem2 -> elem1 * elem2) a b           
          
            // Set p=0 for visited cities (this is because visited cities should not select again)
            Array.iter (fun elem -> p.[elem] <- 0.0) (antsPopulation.[j].Route)
 
            // Normalization 
            let pSum = Array.sum p
            Array.iteri (fun idx elem -> p.[idx] <- elem / pSum) p
            
            // Select next city
            let nextCity = Selectors.RouletteWheelSelection p 

            // Add selected city to the and of route.
            antsPopulation.[j].Route <- Array.append antsPopulation.[j].Route [|nextCity.Value|]           
    
        // Evaluate cost for curent ant
        antsPopulation.[j].Cost <- (costFunc antsPopulation.[j].Route distanceMatrix)
        
        // Save best ant
        if antsPopulation.[j].Cost < bestAnt.Cost then 
            bestAnt.Cost <- antsPopulation.[j].Cost
            bestAnt.Route <- antsPopulation.[j].Route

    // Update pheromone
    for l=0 to Settings.antCount-1 do
        // Add first city to the end of route array to create a tour.
        let route =  [| antsPopulation.[l].Route.[0] |] |> Array.append antsPopulation.[l].Route

        for m=0 to Settings.citiesCount-1 do            
            let cityA, cityB = route.[m], route.[m+1]

            // add some value of pheromone for cityA to cityB
            // less cost = more pheromone 
            pheromonMatrix.[cityA].[cityB] <- pheromonMatrix.[cityA].[cityB] + Settings.pir / (antsPopulation.[l].Cost )

    // Evaporation
    Array.iteri (fun idxi elem -> 
        Array.iteri (fun idxj e -> pheromonMatrix.[idxi].[idxj] <- (1.0-Settings.pdr)*e) elem) pheromonMatrix
    
    // save best ant
    bestSolutions.[i] <- {Route=bestAnt.Route; Cost=bestAnt.Cost}

    // Display iteration information
    printfn "Best cost in iteration %d is %A" i bestSolutions.[i].Cost
let f = 0;    



