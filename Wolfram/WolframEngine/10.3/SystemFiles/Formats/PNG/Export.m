(* ::Package:: *)

Begin["System`ConvertersDump`"]


ImportExport`RegisterExport[
    "PNG",
    System`Convert`CommonGraphicsDump`ExportElementsToRasterFormat["PNG", ##]&,
    "Sources" -> {"Convert`CommonGraphics`"},
	"DefaultElement" -> Automatic,
	"Options" -> {"BitDepth", "ColorSpace", "Comments"},
	"BinaryFormat" -> True
]


End[]
