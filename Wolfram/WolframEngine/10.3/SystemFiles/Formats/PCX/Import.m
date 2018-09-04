(* ::Package:: *)

Begin["System`ConvertersDump`"]


ImportExport`RegisterImport[
   "PCX",
   System`Convert`PCXDump`ImportPCX,
   {
	"Data" -> ElementsToRasterData,
	"Graphics" -> ElementsToRaster,
	"Image" -> (GraphicToImage[ElementsToRaster[##]]&),
	"RGBColorArray" -> ElementsToColorData[RGBColor, Heads -> True],
	"GrayLevels" :> ElementsToColorData[GrayLevel, Heads -> False]
   },
   "FunctionChannels" -> {"Streams"},
   "AvailableElements" -> {"BitDepth", "ColorMap", "ColorSpace", "Data", "DataType",
			"Graphics", "GrayLevels", "Image", "ImageSize", "RawData",
			"RGBColorArray"},
   "DefaultElement" -> "Image",
   "Options" -> {"DataType", "BitDepth", "ColorSpace", "ImageSize"},
   "BinaryFormat" -> True
]


End[]
