(* Paclet Info File *)

(* created 2015/02/15*)

Paclet[
	Name -> "SymbolicMachineLearning",
	Version -> "0.1.0",
	MathematicaVersion -> "10+",
	Description -> "FindSimpleFit and FindSimpleDistribution function",
	Creator -> "Giorgia Fortuna, <giorgiaf@wolfram.com>",
	Loading -> Automatic,
	Extensions -> {
		{
			"Kernel", 
			Symbols -> {
				"System`FindDistribution",
				"System`FindFormula"
				},
			Context -> {
				"SymbolicMachineLearningLoader`", 
				"SymbolicMachineLearning`"
			}
		}
	(*, Root-> "Kernel" (*to specify where the .m files are*) *)
	}
]
