(* Paclet Info File *)

(* created 2015/02/17*)

Paclet[
	Name -> "NaturalLanguageProcessing",
	Version -> "1.0",
	MathematicaVersion -> "10+",
	Description -> "Natural Language Processing Utilities",
	Creator -> "Gopal Sarma <gopals@wolfram.com>",
	Loading -> Automatic,
	Extensions -> {
		{
		"Resource", 
		Root -> "Resources", 
		Resources -> {
			"Misc",
			"Pluralize",
			"SentenceBoundaries",
			"WLTagger"
			}
		}, 
		{
			"Kernel", 
			Context -> {"NaturalLanguageProcessingLoader`", "NaturalLanguageProcessing`"},
			Symbols -> {
				"System`RemoveDiacritics",
				"System`TextWords",
				"System`TextSentences",     
				"System`WordStem",
				"System`WordCount",
				"System`WordCounts",
				"System`DeleteStopwords",
				"System`CharacterCounts",
				"System`LanguageIdentify",
				"System`LetterCounts",
				
				"System`TextPosition",
				"System`TextCases",
				"System`Containing",
				"System`Pluralize",
				(*"System`TextSegmentation",*)
				"System`TextElement",
				"System`TextStructure"
			}
		}
	}
]
