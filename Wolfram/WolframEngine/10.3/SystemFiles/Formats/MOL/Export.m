(* ::Package:: *)

ImportExport`RegisterExport[
  "MOL",
  System`Convert`MolDump`ExportMOL,
  "FunctionChannels" -> {"Streams"},
  "Sources" -> ImportExport`DefaultSources["Mol"]
]
