(* ::Package:: *)

ImportExport`RegisterExport[
	"SDF",
	System`Convert`MolDump`ExportSDF,
	"Sources"->ImportExport`DefaultSources["Mol"],
	"FunctionChannels" -> {"FileNames"}
]
