(* ::Package:: *)

ImportExport`RegisterExport[
	"TSV",
	System`Convert`TableDump`ExportTSV,
	"Sources" -> ImportExport`DefaultSources["Table"],
	"FunctionChannels" -> {"Streams"},
	"DefaultElement" -> "Data"
]
