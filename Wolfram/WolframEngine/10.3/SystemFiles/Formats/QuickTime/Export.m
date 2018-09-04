(* ::Package:: *)

If[ StringMatchQ[$SystemID, "MacOSX*"] || StringMatchQ[$SystemID, "Windows*"],

	(****************** QuickTime for Windows & OSX ******************)
    ImportExport`RegisterExport[
    	"QuickTime",
		System`Convert`MovieDump`ExportQuickTime,
		"Sources" -> {"Convert`CommonGraphics`", "QuickTime.exe", "Convert`QuickTime`"},
		"FunctionChannels" -> {"FileNames"},
		"SystemID" -> ("Windows*" | "Mac*"),
		"BinaryFormat" -> True
		(* no "DefaultElement" explicitly.  Converter handles default element parsing *)
	]
	(* No RegisterExport[] for all other platforms. *)
]
