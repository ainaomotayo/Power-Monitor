(* ::Package:: *)

Begin["System`Convert`XYZDump`"]


ImportExport`RegisterImport["XYZ",
  ImportXYZ,
  {"Graphics3D":> ImportExport`MoleculePlot3D },
  "FunctionChannels" -> {"Streams"},
  "AvailableElements" -> {"Graphics3D", "VertexCoordinates", "VertexTypes"},
  "DefaultElement" -> "Graphics3D"
]


End[]
