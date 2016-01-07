namespace AntColonyOptimization

 // Type Definition
module RequiredTypes= 

    let rnd = System.Random()    // Random generator func
    type Route = int []
    [<Measure>] type km          // kilometre unit of measure
    type Ant = {mutable Route : Route; mutable Cost : float }


