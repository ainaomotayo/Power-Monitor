(* ::Package:: *)

Begin["System`Convert`ICODump`"]


ImportExport`RegisterImport["ICO",
  ImportICO,
  {
    "GrayLevels" 	:> ICONElementsToGrayLevelsArray,
	"RGBColorArray" :> ICONElementsToRGBColorArray,
	"GraphicsList" 	:> ICONElementsToGraphics,
	"ImageList" 	:> ICOElementsToImages,
	"Graphics" 		:> (ICONElementsToGraphics[##][[1]]&),
	"Image" 		:> (ICOElementsToImages[##][[1]]&)
  },
  "FunctionChannels" -> {"Streams"},
  "AvailableElements" -> {"BitDepth", "ColorSpace", "Data", "Graphics", "GraphicsList",
						"GrayLevels", "Image", "ImageList", "ImageSize", "RGBColorArray"},
  "DefaultElement" -> "ImageList",
  "Sources" -> ImportExport`DefaultSources["ICO"],  
  "Options" -> {"BitDepth", "ColorSpace", "ImageSize"},
  "BinaryFormat" -> True
]


End[]
