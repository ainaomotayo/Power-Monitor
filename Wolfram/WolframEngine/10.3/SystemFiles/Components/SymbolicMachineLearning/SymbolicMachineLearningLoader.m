(*
All loading of the paclet's Wolfram Language code should go through this file. 
*)

SymbolicMachineLearning`Private`autoloadSymbols = {
	"System`FindDistribution",
	"System`FindFormula"
};

SymbolicMachineLearning`Private`symsToProtect = {};

PacletManager`Package`loadWolframLanguageCode[
	"SymbolicMachineLearning", "SymbolicMachineLearning`",
	DirectoryName[$InputFileName], "FindDistribution.m",
	"AutoUpdate" -> True,
	"ForceMX" -> TrueQ[SymbolicMachineLearning`$ForceMX],
	"Lock" -> False,
	"AutoloadSymbols" -> SymbolicMachineLearning`Private`autoloadSymbols,
	"HiddenImports" -> {"PacletManager`", "Developer`", "GeneralUtilities`"},
	"SymbolsToProtect" -> SymbolicMachineLearning`Private`symsToProtect
]

