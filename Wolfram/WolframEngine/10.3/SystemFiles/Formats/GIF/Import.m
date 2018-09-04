(* ::Package:: *)

Begin["System`ConvertersDump`"]


ImportExport`RegisterImport[
"GIF",
{
	"ImageSize"|"DataType"|"BitDepth"|"ColorSpace" :> GIFDefaultHeader,
	"Background"|"GlobalColorMap" :> GIFHeader,
	"DisplayDurations"|"ColorMap"|"TransitionEffect"|"UserInputFlag"|"ImageCount"|"Comments"|"AnimationRepetitions" :> GIFFrameElements,
	{"Frames", nums:(_Integer?Positive|{__Integer?Positive})} :> (GIFFixColorMapAlphaChannel[iImportGIFElement[nums][##]]&),
	(GIFFixColorMapAlphaChannel[iImportGIFElements[##]]&)
},
{
	"ImageList" -> GIFFramesToImageList[All],
	{"ImageList", n_Integer?Positive} :> GIFFramesToImageList[n],
	"Image" -> GIFFramesToImageList[1],
	"GraphicsList" :> GIFFramesToGraphics,
	"Graphics" -> GIFFramesToGraphic[1],
	"RawData" :> FrameElement[All, "RawData"],
	"Data" -> FramesToData,
	"RGBColorArray" -> FramesToColorData[RGBColor, Heads -> True],
	"GrayLevels" -> FramesToColorData[GrayLevel, Heads -> False],
	"Animation" -> GIFFramesToAnimation,
	Automatic -> GIFFramesToDefault
},
  "AvailableElements" -> {"Animation", "AnimationRepetitions", "Background", "BitDepth",
  	 	"ColorMap", "ColorSpace", "Comments", "Data", "DataType", "DisplayDurations", "Frames",
    	"GlobalColorMap", "Graphics", "GraphicsList", "GrayLevels", "Image", "ImageCount", 
        "ImageList", "ImageOffset", "ImageSize", "RawData", "RGBColorArray", "TransitionEffect",
        "TransparentColor", "UserInputFlag"},
  "DefaultElement" -> Automatic,
  "Sources" -> {"GIF.exe", "RDPStruct.exe"},
  "Options" -> {"Background", "ImageSize", "ColorSpace", "DisplayDurations", "AnimationRepetitions", "TransitionEffect", "UserInputFlag"},
  "BinaryFormat" -> True
]



End[]
