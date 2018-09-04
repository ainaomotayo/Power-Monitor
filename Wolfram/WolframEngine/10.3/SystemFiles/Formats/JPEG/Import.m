(* ::Package:: *)

Begin["System`Convert`CommonGraphicsDump`"]

ImportExport`RegisterImport["JPEG",
	{
		"Graphics" 					 :> GetGraphicsElement["JPEG"],
		"Image" 						:> GetImageElement["JPEG"],
		"Data" 						 :> GetDataElement["JPEG", All],
		"ImageWithExif" 				:> GetImageElementWithExif["JPEG"],
		"ImageNoExif" 				  :> GetImageElementNoExif["JPEG"],
		"Exif"					      :> GetExifInformation["JPEG"],
		"BitDepth" 					 :> GetImageMetaData["JPEG", "BitDepth", All],
		"ColorProfileData"			  :> GetImageMetaData["JPEG", "ColorProfileData", All],
	    "ColorSpace" 				   :> GetImageMetaData["JPEG", "ColorSpace", All],
		"ImageSize" 					:> GetImageMetaData["JPEG", "ImageSize", All],
		"ColorMap"|"RawData" 		   :> GetRawDataAndColorMapElements["JPEG", All],
		"RGBColorArray" 				:> GetRGBColorArrayElement["JPEG", All],
		"GrayLevels" 				   :> GetGrayLevelsElement["JPEG", All],
		"Thumbnail" | {"Thumbnail", s:(_Integer|_Symbol)}  :> GetThumbnailElement["JPEG", s],    

Alternatives		[
			"FocalLength",
			"CameraTopOrientation",
			"Manufacturer",
			"Model",
			"Exposure",
			"Date",
			"Aperture",
			"ISOSpeed" 
		] 								:> System`Convert`ExifDump`getExifElements,

		"Elements"				      :> GetListOfElements["JPEG"],
		GetListOfElements["JPEG"]
	},
	"Sources" -> {"JLink`", "Convert`Exif`", "Convert`CommonGraphics`"},
	"AvailableElements" ->
		{
			"FocalLength", "Model", "Exposure", "ISOSpeed", "ColorSpace",
			"Aperture", "BitDepth", "CameraTopOrientation", "ColorMap", "ColorProfileData",
			"Data", "Date", "Exif", "Graphics", "GrayLevels", "Image", 
			"ImageSize", "ImageWithExif", "ImageNoExif", "Manufacturer", "RawData",
			"RGBColorArray",
			"Thumbnail"
		},
	"DefaultElement" -> "Image",
	"Options" -> {"BitDepth", "ColorSpace", "ImageSize"},
	"BinaryFormat" -> True
]


End[]
