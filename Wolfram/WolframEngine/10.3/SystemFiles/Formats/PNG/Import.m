(* ::Package:: *)

Begin["System`Convert`CommonGraphicsDump`"]


ImportExport`RegisterImport["PNG",
	{
		"Thumbnail" | {"Thumbnail", s:(_Integer|_Symbol)} :> GetThumbnailElement["PNG", s],      
		"Graphics" 					  :> GetGraphicsElement["PNG"],
		"Image" 						 :> GetImageElement["PNG"],
		"Data" 					  	:> GetDataElement["PNG", All],
		"BitDepth" 				  	:> GetImageMetaData["PNG", "BitDepth", All],
		"ColorProfileData" 		  	:> GetImageMetaData["PNG", "ColorProfileData", All],
		"ColorSpace" 			  	  :> GetImageMetaData["PNG", "ColorSpace", All],
		"ImageSize" 				 	:> GetImageMetaData["PNG", "ImageSize", All],
		"Comments" 				  	:> GetImageMetaData["PNG", "Comments", All],
		"ColorMap"				   	:> GetRawDataAndColorMapElements["PNG", All, "Element" -> "ColorMap"],
		"RawData"						:> GetRawDataAndColorMapElements["PNG", All, "Element" -> "RawData"],
		"RGBColorArray" 			 	:> GetRGBColorArrayElement["PNG", All],
		"GrayLevels" 					:> GetGrayLevelsElement["PNG", All],
		"Elements"					   :> GetListOfElements["PNG"],
		GetListOfElements["PNG"]
	},
	"Sources" -> {"JLink`", "Convert`CommonGraphics`"},
	"AvailableElements" ->
		{
			"ColorSpace", "BitDepth", "ColorMap", "ColorProfileData", "Comments", "Data",
			"Graphics", "GrayLevels", "Image", "ImageSize", "RawData", "RGBColorArray",
			"Thumbnail"
		},
	"DefaultElement" -> "Image",
	"Options" -> {"BitDepth", "ColorSpace", "ImageSize"},
	"BinaryFormat" -> True
]

End[]
