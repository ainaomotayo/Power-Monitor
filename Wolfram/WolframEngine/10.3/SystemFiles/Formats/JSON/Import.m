(* ::Package:: *)

Begin["System`Convert`JSONDump`"]


ImportExport`RegisterImport[
  "JSON",
  importJSON,
  "AvailableElements" -> {"Data"},
  "DefaultElement" -> "Data",
  "FunctionChannels" -> {"Streams"},
  "Sources" -> {"Convert`JSON`"}
]


End[]
