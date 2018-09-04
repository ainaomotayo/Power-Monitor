(* ::Package:: *)

Begin["System`Convert`DXFDump`"]


ImportExport`RegisterImport[
 "DXF",
 ImportDXF,
 {
   "Graphics3D" -> DXFToGraphics,

   "GraphicsComplex" -> DXFToGraphicsComplex,

   "PolygonObjects"-> DXFToObjs["PolygonObjects"],
   "LineObjects"-> DXFToObjs["LineObjects"],
   "PointObjects"-> DXFToObjs["PointObjects"],

   "PolygonData"-> DXFToData["PolygonData"],
   "LineData"-> DXFToData["LineData"],
   "PointData"-> DXFToData["PointData"],
   "VertexData"-> DXFToData["VertexData"],
   "VertexColors"-> DXFToData["VertexColors"],
   "PlotRange" -> DXFToPlotRange,
   "ViewPoint" -> DXFToViewPoint
 },
 "Sources" -> {"Convert`Common3D`", "Convert`DXF`"},
 "FunctionChannels" -> {"Streams"},
 "AvailableElements" -> {"Graphics3D", "GraphicsComplex", "LineData", "LineObjects",
		"PlotRange", "PointData", "PointObjects", "PolygonData",
		"PolygonObjects", "VertexColors", "VertexData", "ViewPoint"},
 "DefaultElement" -> "Graphics3D",
 "Options" -> {"PlotRange", "ViewPoint"}
]


End[]
