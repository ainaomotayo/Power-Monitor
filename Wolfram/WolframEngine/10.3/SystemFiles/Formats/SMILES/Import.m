(* ::Package:: *)

ImportExport`RegisterImport[
  "SMILES",
  System`Convert`SMILESDump`ImportSMILES,
  "FunctionChannels" -> {"Streams"},
  "AvailableElements" -> {"EdgeRules", "EdgeTypes", "FormalCharges", "VertexTypes"}
  (* "DefaultElement" -> explicitly not included here *)
]
