(* ::Package:: *)

Begin["System`ConvertersDump`"]


ImportExport`RegisterImport[
  "PXR",
  System`Convert`PXRDump`ImportPXR,
  {
	"Data" -> ElementsToRasterData,
	"Graphics" -> ElementsToRaster,
	"Image" -> (GraphicToImage[System`ConvertersDump`ElementsToRaster[##]]&),
	"RGBColorArray" -> ElementsToColorData[RGBColor, Heads -> True],
	"GrayLevels" :> ElementsToColorData[GrayLevel, Heads -> False]
  },
  "FunctionChannels" -> {"Streams"},
  "AvailableElements" -> {"BitDepth", "ColorSpace", "Data", "DataType", "Graphics", "GrayLevels", "Image", "ImageSize", "RGBColorArray"},
  "DefaultElement" -> "Image",
  "Options" -> {"ColorSpace", "ImageSize"},
  "BinaryFormat" -> True
]


End[]
