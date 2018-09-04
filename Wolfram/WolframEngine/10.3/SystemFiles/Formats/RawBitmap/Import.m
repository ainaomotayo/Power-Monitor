(* ::Package:: *)

Begin["System`ConvertersDump`"]


ImportExport`RegisterImport[
 "RawBitmap",
 System`Convert`BitmapDump`ImportRawBitmap,
 {
	"Graphics" -> ElementsToRaster,
	"Image" -> (GraphicToImage[ElementsToRaster[##]]&),
	"Data" -> ElementsToRasterData,
	"RGBColorArray" -> ElementsToColorData[RGBColor, Heads -> True],
	"GrayLevels" :> ElementsToColorData[GrayLevel, Heads -> False]
 },
 "Sources" -> ImportExport`DefaultSources["Bitmap"],
 "AvailableElements" -> {"BitDepth", "ColorSpace", "Data", "DataType", "Graphics", "GrayLevels", "Image", "ImageSize", "RGBColorArray"},
 "DefaultElement" -> "Image",
 "Options" -> {"DataType", "BitDepth", "ColorSpace", "ImageSize"},
 "BinaryFormat" -> True
]


End[]
