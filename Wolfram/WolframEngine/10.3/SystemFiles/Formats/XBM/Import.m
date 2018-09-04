(* ::Package:: *)

Begin["System`ConvertersDump`"]


ImportExport`RegisterImport["XBM",
{
	System`Convert`BitmapDump`ImportXBitmap
},
{
	"Graphics" -> ElementsToRaster,
	"Image" -> (GraphicToImage[ElementsToRaster[##]]&),
	"Data" -> ElementsToRasterData,
	"RGBColorArray" -> ElementsToColorData[RGBColor, Heads -> True],
	"GrayLevels" :> ElementsToColorData[GrayLevel, Heads -> False]
},
	"Sources" -> ImportExport`DefaultSources["Bitmap"],
	"FunctionChannels" -> {"Streams"},
	"AvailableElements" -> {"BitDepth", "ColorSpace", "Data", "Data", "DataType", "Graphics", "GrayLevels", "Image", "ImageSize", "RGBColorArray"},
	"DefaultElement" -> "Image",
	"Options" -> {"DataType", "BitDepth", "ColorSpace", "ImageSize"}
]


End[]

