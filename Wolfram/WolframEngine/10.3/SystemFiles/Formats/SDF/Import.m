(* ::Package:: *)

Begin["System`Convert`MolDump`"]


ImportExport`RegisterImport[
 "SDF",
 ImportSDF,
 {
	"Graphics3D":> MoleculePlot3DList,
	"StructureDiagram" :> StructureDiagramList,
	Automatic :> SDFToDefault
 },
 "Sources"->ImportExport`DefaultSources["Mol"],
 "FunctionChannels" -> {"Streams"},
 "AvailableElements" -> {"EdgeRules", "EdgeTypes", "FormalCharges",
 				"Graphics3D", "Header", "MassNumbers", "Metadata",
 				"StructureDiagram", "VertexCoordinates", "VertexTypes"},
 "DefaultElement" -> Automatic
]


End[]
