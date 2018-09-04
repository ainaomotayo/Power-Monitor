(* ::Package:: *)

ImportExport`RegisterExport[
  "CSV",
  System`Convert`TableDump`ExportCSV,
  "Sources" -> ImportExport`DefaultSources["Table"],
  "FunctionChannels" -> {"Streams"},
  "DefaultElement" -> "Data"
]
