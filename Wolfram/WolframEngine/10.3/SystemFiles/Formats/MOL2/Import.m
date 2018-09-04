(* ::Package:: *)

Begin["System`Convert`MOL2Dump`"]


ImportExport`RegisterImport[
  "MOL2",
  ImportMol2,
  {
	"Graphics3D":> MoleculePlot3DList,
	"StructureDiagram" :> StructureDiagramList,
	Automatic :> MOL2ToDefault
  },
  "FunctionChannels" -> {"Streams"},
  "AvailableElements" -> {"EdgeRules", "EdgeTypes", "Graphics3D", "PartialCharges",
			"ResidueAtoms", "ResidueCharges", "ResidueCoordinates", "Residues",
			"Sequence", "StructureDiagram", "VertexCoordinates", "VertexTypes"},
  "DefaultElement" -> Automatic
]


End[]
