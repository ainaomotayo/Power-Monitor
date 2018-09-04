(* Paclet Info File *)

(* created 2014/10/21*)

Paclet[
    Name -> "Interpreter",
Version -> "1.2.149.0",
    MathematicaVersion -> "10.1+",
    Description -> "Interpretation of strings",
    Creator -> "Riccardo Di Virgilio <riccardod@wolfram.com>, Carlo Barbieri <carlob@wolfram.com>, Chiara Basile <chiarab@wolfram.com>",
    Loading -> Automatic,
    Extensions -> {
        {
            "Kernel", 
            Context -> {"InterpreterLoader`", "Interpreter`"}, 
            Symbols -> {
                "System`Interpreter", 
                "System`Restricted", 
                "System`DelimitedSequence", 
                "System`GeoLocation", 
                "System`RepeatingElement",
                "System`CompoundElement",
                "System`AnySubset",
                "System`$InterpreterTypes",
                "System`ImportOptions"
            }
        },
        {"JLink"},
        {"Resource", Root -> ".", Resources -> {"MetaData"}}
    }
]