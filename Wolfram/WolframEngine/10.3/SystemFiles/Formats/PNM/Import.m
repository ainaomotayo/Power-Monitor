(* ::Package:: *)

Begin["System`Convert`PNMDump`"]


ImportExport`RegisterImport[
	"PNM",
	ImportPNM["PNM"],
	{
		"Graphics" -> ImportPNMGraphics,
		"Image" -> ImportPNMImage,
		"BitDepth" -> ImportPNMBitDepth,
		"GrayLevels" -> ImportPNMGrayLevels,
		"RGBColorArray" -> ImportPNMRGBColorArray
	},
	"AvailableElements" -> {"BitDepth", "ColorSpace", "Data",
			"DataType", "Graphics", "GrayLevels", "Image", "ImageSize",
			"RGBColorArray"},
	"DefaultElement" -> "Image",
	"FunctionChannels"  -> {"Streams"},
	"Sources" -> ImportExport`DefaultSources["PNM"],
	"Options" -> {"DataType", "BitDepth", "ColorSpace", "ImageSize"},
	"BinaryFormat" -> True
]


End[]
