(* Paclet Info File *)

(* created 2015/06/17*)

Paclet[
    Name -> "TravelDirectionsClient",
    Version -> "0.0.17",
    MathematicaVersion -> "10.2+",
    Loading -> Automatic,
    Extensions -> 
        {
            {"Kernel",
                Symbols -> {"System`TravelDirections", "System`TravelDirectionsData",
                    "System`TravelDistance", "System`TravelTime", "System`TravelMethod"},
                Root -> "Kernel",
                Context -> {"TravelDirectionsClient`", "TravelDirectionsClientLoader`"}
            }
        }
]

