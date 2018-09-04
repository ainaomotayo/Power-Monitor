(* All loading of the paclet's Wolfram Language code should go through this file. *)

(* Developer maintains this list of symbols.
   autoloadSymbols must agree with the symbols listed in the Kernel extension in the PacletInfo.m file.
*)

Interpreter`Private`autoloadSymbols = {
    "System`Interpreter", 
    "System`Restricted", 
    "System`DelimitedSequence", 
    "System`GeoLocation", 
    "System`RepeatingElement",
    "System`CompoundElement",
    "System`AnySubset",
    "System`$InterpreterTypes"
};

Map[
    (Unprotect[#];ClearAll[#]) &, Join[
        Interpreter`Private`autoloadSymbols, {
            "Interpreter`*",
            "Interpreter`PackageScope`*",
            "Interpreter`*`PackagePrivate`*"
        }
    ]
];

PacletManager`Package`loadWolframLanguageCode[
    "Interpreter", 
    "Interpreter`", 
    DirectoryName[$InputFileName], 
    "Primitives.m",
    "AutoUpdate" -> True, 
    "ForceMX" -> False, 
    "Lock" -> False,
    "AutoloadSymbols" -> Interpreter`Private`autoloadSymbols, 
    "SymbolsToProtect" -> Interpreter`Private`autoloadSymbols ~ Complement ~ {"System`$InterpreterTypes"},
    "HiddenImports" -> {
        (* "DataDropClient`", uncomment this when datadrop type will be inside interpreter *)
        "Security`", 
        "URLUtilities`", 
        "JLink`"
    }
]