(* ::Package:: *)

Begin["System`Convert`MolDump`"]


ImportExport`RegisterImport[
  "MOL",
  ImportMol,
  {
	"Graphics3D":> ImportExport`MoleculePlot3D ,
	"StructureDiagram" :> ImportExport`StructureDiagram,
	Automatic :> MolToDefault
  },
  "FunctionChannels" -> {"Streams"},
  "AvailableElements" -> {"EdgeRules", "EdgeTypes", "FormalCharges", "Graphics3D", "Header",
			"MassNumbers", "StructureDiagram", "VertexCoordinates", "VertexTypes"},
  "DefaultElement" -> Automatic,
  "Sources" -> ImportExport`DefaultSources["Mol"]
]


End[]

