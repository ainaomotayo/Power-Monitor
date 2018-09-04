(* ::Package:: *)

Begin["System`Convert`ICNSDump`"]


ImportExport`RegisterImport[
  "ICNS",
  {
   "Elements" :> ImportICNSElements,
   ImportICNS
  },
  {
    "BitDepth"      :> ImportICNSBitDepth,
    "ColorSpace"    :> ImportICNSColorSpace,
    "Graphics"      :> ImportICNSGraphics,
    "GraphicsList"  :> ImportICNSGraphicsList,
    "Data"          :> ImportICNSData,
    "Image"         :> ImportICNSImage,
    "ImageSize"     :> ImportICNSImageSize,
    "RGBColorArray" :> ImportICNSRGBColorArray,
    "GrayLevels"    :> ImportICNSGrayLevels
  },
  "FunctionChannels" -> {"Streams"},
  "AvailableElements" -> {"BitDepth","ColorSpace","Data","Graphics","GraphicsList","GrayLevels","Image","ImageList","ImageSize","RGBColorArray"},
  "DefaultElement"-> "ImageList",
  "Options" -> {"BitDepth", "ColorSpace", "ImageSize"},
  "BinaryFormat"->True
]


End[]
