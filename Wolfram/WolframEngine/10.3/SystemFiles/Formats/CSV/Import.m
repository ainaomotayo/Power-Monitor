(* ::Package:: *)

Begin["System`Convert`TableDump`"]


ImportExport`RegisterImport[
  "CSV",
  ImportCSV,
  {
    "Data" :> GetData,
	"Grid" :> GetGrid
  },
  "Sources" -> ImportExport`DefaultSources["Table"],
  "FunctionChannels" -> {"FileNames"},
  "AvailableElements" -> {"Data", "Grid"},
  "DefaultElement" -> "Data"
]


End[]
