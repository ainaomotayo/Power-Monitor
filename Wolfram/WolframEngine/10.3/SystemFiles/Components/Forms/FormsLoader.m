(* All loading of the paclet's Wolfram Language code should go through this file. *)

(* Developer maintains this list of symbols.
   autoloadSymbols must agree with the symbols listed in the Kernel extension in the PacletInfo.m file.
*)

Forms`Private`autoloadSymbols = {
	"System`FormObject", 
	"System`FormTheme", 
	"System`PageTheme" ,
	"System`FormFunction",
	"System`FormLayoutFunction", 
	"System`AppearanceRules", 
	"System`APIFunction",
	"System`AutoSubmitting"(*,
	"System`QuestionObject",
	"System`QuestionSequence",
	"System`AnswerObject",
	"System`AssessmentFunction",
	"System`AssessmentAction"*)
}

Map[
    (Unprotect[#];ClearAll[#]) &, Join[
        Forms`Private`autoloadSymbols, {
            "Forms`*",
            "Forms`PackageScope`*",
            "Forms`*`PackagePrivate`*"
        }
    ]
]

PacletManager`Package`loadWolframLanguageCode[
	"Forms", 
	"Forms`", 
	DirectoryName[$InputFileName], 
	"Primitives.m",
	"AutoUpdate" -> True,
    "ForceMX" -> False, 
    "Lock" -> False,
	"AutoloadSymbols" -> Forms`Private`autoloadSymbols,
	"HiddenImports" -> {"GeneralUtilities`", "Interpreter`"}
]