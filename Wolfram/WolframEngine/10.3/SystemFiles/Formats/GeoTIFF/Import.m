(* ::Package:: *)

Begin["System`Convert`GDALDump`"]


ImportExport`RegisterImport[
  "GeoTIFF",
 { (* Raw *)
	("Image"|"Graphics"|"Data"|"ElevationRange") :> ImportGISRasterRaw["GeoTIFF"],
	"Elements" :> GDALGetElements["GeoTIFF"],
	Alternatives[ "DataFormat", "CornerCoordinates", "ColorInterpretation",
			 "SpatialRange"] :> ImportGISRasterLayerMetadata["GeoTIFF"],
	ImportGISRasterMetadata["GeoTIFF"]
 },
 { (* Post Process *)

	"Data" :> GISRasterData,
	"DataFormat" -> GISElementsFromCSI["DataFormat"],
	"RasterSize" -> GISElementsFromCSI["RasterSize"],
	"ColorSpace" -> GISElementsFromCSI["ColorInterpretation"],
	"ElevationRange"->getRasterElevationRange,
	"Graphics" :> GISRasterGraphics,
	"SpatialRange" :> GISRasterSRange,
	"SpatialResolution" :> GISRasterSResolution,
	"SemimajorAxis" :> GISGetSemiMajorAxis,
    "SemiminorAxis" :> GISGetSemiMinorAxis,
	"InverseFlattening" :> GISGetInverseFlattening,
	"LinearUnits"  :> GISGetLinearUnits,
	"CoordinateSystemInformation":>GISGetCoordSysInfo,
	"CoordinateSystem":> GISCoordinateSysName,
	"ProjectionName":>GISProjectionName,
	"Datum":>GISGetDatum,
	"Projection":>GISProjectionParameters,
	"CentralScaleFactor" :>getSubElement["CentralScaleFactor"],
    "StandardParallels"  :>getSubElement["StandardParallels"],
    "ReferenceModel"  :>getSubElement["ReferenceModel"],
    "Centering"  :>getSubElement["Centering"],
    "GridOrigin":>getSubElement["GridOrigin"],
    "Image" :> GISRasterImage
  },
 "Sources" -> ImportExport`DefaultSources["GDAL"],
 "AvailableElements" -> {"Centering", "CentralScaleFactor", "ColorSpace", "CoordinateSystem", 
			"CoordinateSystemInformation", "Data", "DataFormat", "Datum", 
			"ElevationRange", "Graphics", "GridOrigin", "Image", "InverseFlattening", 
			"LinearUnits", "Projection", "ProjectionName", "RasterSize", 
			"ReferenceModel", "SemimajorAxis", "SemiminorAxis", "SpatialRange", 
			"SpatialResolution", "StandardParallels"},
 "DefaultElement" -> "Image",
 "FunctionChannels" -> {"FileNames","Directories"},
 "BinaryFormat" -> True
]


End[]
