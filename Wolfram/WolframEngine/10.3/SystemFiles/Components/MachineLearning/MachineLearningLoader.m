(* All loading of the paclet's Wolfram Language code should go through this file. *)

(* Developer maintains this list of symbols.
   autoloadSymbols must agree with the symbols listed in the Kernel extension in the PacletInfo.m file.
*)
MachineLearning`Private`autoloadSymbols = {
			"System`Classify", 
			"System`ClassifierFunction", 
			"System`ClassifierMeasurements",
			"System`ClassifierMeasurementsObject",
			"System`ClassifierInformation",
			"System`ClassPriors",
			"System`IndeterminateThreshold",
			
			"System`Predict",
			"System`PredictorFunction",
			"System`PredictorMeasurements",
			"System`PredictorMeasurementsObject",
			"System`PredictorInformation",
			
			"System`UtilityFunction",
			"System`ValidationSet",
			"System`FeatureTypes",
			"System`FeatureNames",
			
			"System`DimensionReduction",
			"System`DimensionReduce",
			"System`DimensionReducerFunction",
			
			"System`ImageIdentify",
			"System`ImageInstanceQ",
			"System`SpecificityGoal",
			"System`RecognitionThreshold",
			"System`RecognitionPrior",
			
			"System`DistanceMatrix",
			"System`ConformationRules"
}


MachineLearning`Private`symsToProtect =
    Hold[Complement[
            Select[Names["MachineLearning`*"] ~Join~ Names["MachineLearning`PackageScope`*"],
                ToExpression[#, InputForm, Function[{sym}, Length[DownValues[sym]] > 0 || Length[SubValues[sym]] > 0, HoldFirst]] &
            ] ~Join~ MachineLearning`Private`autoloadSymbols
            ,{
            	
               "MachineLearning`PackageScope`Evaluation",
               "MachineLearning`PackageScope`PredictorEvaluation",
               "MachineLearning`PackageScope`ClassifierEvaluation",
               "MachineLearning`PackageScope`BuiltInFunction"
           }
        ]
    ]


PacletManager`Package`loadWolframLanguageCode["MachineLearning", "MachineLearning`", DirectoryName[$InputFileName], "ClassifyPredict.m",
         "AutoUpdate" -> True,
         "ForceMX" -> TrueQ[MachineLearning`$ForceMX], "Lock" -> False,
         "AutoloadSymbols" -> MachineLearning`Private`autoloadSymbols,
         "HiddenImports" -> {"PacletManager`", "Developer`", "GeneralUtilities`"},
         "SymbolsToProtect" -> MachineLearning`Private`symsToProtect
]