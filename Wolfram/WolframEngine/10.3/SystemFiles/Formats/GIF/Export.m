(* ::Package:: *)

Begin["System`ConvertersDump`"]


ImportExport`RegisterExport["GIF",
	ExportGIFElements[##]&,
	"Options" -> {"Background", "TransparentColor", "FrameRate", "AnimationRepetitions", "TransitionEffect", "UserInputFlag"},
	"DefaultElement" -> "Frames",
	"Sources" -> {"GIF.exe", "RDPStruct.exe"},
	"BinaryFormat" -> True
]


End[]
