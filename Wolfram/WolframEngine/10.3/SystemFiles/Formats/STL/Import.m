(* ::Package:: *)

Begin["System`Convert`STLDump`"]


ImportExport`RegisterImport[
	"STL",
	{
		"MeshRegion"			:> CreateMeshRegion,
		"Graphics3D"			:> CreateGraphics,
		"GraphicsComplex" 	  :> CreateGraphicsComplex,
		"VertexData"			:> CreateVertexData,
		"PolygonData"		   :> CreatePolygonData,
		"PolygonObjects"		:> CreatePolygonObjects,
		"BinaryFormat"      	:> CreateBinaryFormat,
		"Comments"			  :> CreateName,
		"PolygonCount"		  :> CreateTriangleCount,
		"FacetNormals"		  :> CreateFacetNormals,
        "VerticalAxis"          :> CreateVerticalAxis,
		"CoordinateTransform"   :> CreateCoordinateTransform,
		"BoundaryMeshRegion"    :> CreateBoundaryMeshRegion,
		"Summary"        	   :> CreateSummary,
		CreateMeshRegion
	},
	"AvailableElements" -> {"BinaryFormat", 
		"MeshRegion", "Graphics3D", "GraphicsComplex", 
		"PolygonData", "PolygonObjects", "VertexData", "VerticalAxis",
        "Comments", "PolygonCount", "FacetNormals", "BoundaryMeshRegion", 
		"CoordinateTransform", "Summary"
	  },
	"BinaryFormat" -> True,
	"DefaultElement" -> "MeshRegion",
	"FunctionChannels" -> {"Streams"},
	"Options" -> {"BinaryFormat", "VerticalAxis"},
	"Sources" -> ImportExport`DefaultSources[{"Common3D", "STL"}]
]



End[]
