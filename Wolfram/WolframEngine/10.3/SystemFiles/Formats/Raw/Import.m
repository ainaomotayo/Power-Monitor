(* ::Package:: *)

Begin["System`Convert`CommonGraphicsDump`"]


ImportExport`RegisterImport["Raw",
	{
		"Graphics" 						:> GetGraphicsElement["Raw"],
		"Image" 						   :> GetImageElement["Raw"],
        "Thumbnail" | {"Thumbnail", s:(_Integer|_Symbol)}  :> GetThumbnailElement["Raw", s],
		"RawImage" 						:> GetRawImageElement["Raw"],
		"Data" 							:> GetDataElement["Raw", All],
		"Author" 						  :> GetImageMetaData["Raw", "Author", All],
		"ImageDescription" 				:> GetImageMetaData["Raw", "ImageDescription", All],
		"Exif" 							:> GetImageMetaData["Raw", "Exif", All],
		"BitDepth" 						:> GetImageMetaData["Raw", "BitDepth", All],
		"FilterPattern" 				   :> GetImageMetaData["Raw", "FilterPattern", All],
		"ColorProfileData" 				:> GetImageMetaData["Raw", "ColorProfileData", All],
		"ColorSpace" 					  :> GetImageMetaData["Raw", "ColorSpace", All],
		"FlashUsed"						:> GetImageMetaData["Raw", "FlashUsed", All],
		"FocalLength" 					 :> GetImageMetaData["Raw", "FocalLength", All],
		"CameraTopOrientation" 			:> GetImageMetaData["Raw", "CameraTopOrientation", All],
		"Make"		 					:> GetImageMetaData["Raw", "Make", All],
		"Model"				 		   :> GetImageMetaData["Raw", "Model", All],
		"ExposureTime"			 		:> GetImageMetaData["Raw", "ExposureTime", All],
		"Date"				 			:> GetImageMetaData["Raw", "Date", All],
		"Aperture"			 			:> GetImageMetaData["Raw", "Aperture", All],
		"ISOSpeedRatings"	 			 :> GetImageMetaData["Raw", "ISOSpeedRatings", All],
		"ImageSize" 					   :> GetImageMetaData["Raw", "ImageSize", All],
		"RawData" 				 		:> GetRawDataAndColorMapElements["Raw", All],
		"Elements"				 		:> GetListOfElements["Raw"],
		GetListOfElements["Raw"]
	},
	"Sources" -> {"JLink`", "Convert`Exif`", "Convert`CommonGraphics`"},
	"AvailableElements" ->
		{
			"Author", "Aperture", "BitDepth", "CameraTopOrientation", "ColorProfileData",
			"ColorSpace", "Data", "Date", "Exif", "ExposureTime", "FilterPattern", "FlashUsed", "FocalLength",
			"Graphics", "Image", "ImageDescription",
			"ImageSize", "ISOSpeedRatings", "Make", "Model", "RawData", "RawImage",
            "Thumbnail"
		},
	"DefaultElement" -> "Image",
	"Options" -> {"BitDepth", "ColorSpace", "ImageSize"},
	"BinaryFormat" -> True
]

End[]
