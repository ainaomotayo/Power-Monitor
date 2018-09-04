(* ::Package:: *)

Begin["System`ConvertersDump`"]


ImportExport`RegisterImport[
	"SCT",
	System`Convert`SCTDump`ImportSCT,
	{
		"Data" -> ElementsToRasterData,
		"Graphics" -> ElementsToRaster,
		"Image" -> (GraphicToImage[ElementsToRaster[##]]&),
		"RGBColorArray" -> ElementsToColorData[RGBColor, Heads -> True],
		"GrayLevels" :> ElementsToColorData[GrayLevel, Heads -> False]
	},
	"FunctionChannels" -> {"Streams"},
	"AvailableElements" -> {"BitDepth", "ColorSpace", "Data", "DataType", "Graphics",
			"GrayLevels", "Image", "ImageResolution", "ImageSize", "RGBColorArray"},
	"DefaultElement" -> "Image",
	"Options" -> {"ColorSpace", "ImageSize"},
	"BinaryFormat" -> True
]


End[]
