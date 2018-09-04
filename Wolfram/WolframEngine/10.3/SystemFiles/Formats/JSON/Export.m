(* ::Package:: *)

Begin["System`Convert`JSONDump`"]

ImportExport`RegisterExport["JSON",
	exportJSON,
	"FunctionChannels" -> {"Streams"},
    "Sources" -> {"Convert`JSON`"}
]

End[]
