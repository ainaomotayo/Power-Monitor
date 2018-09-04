(* ::Package:: *)

Begin["System`Convert`CommonGraphicsDump`"]

ImportExport`RegisterImport["TIFF",
	{	(* uses new Image`ImageRead code *)
		"Animation"													:> GetAnimationElement["TIFF"],
		"Graphics"													 :> GetGraphicsElement["TIFF"],
		"Image"													    :> GetImageElement["TIFF"],
		"ImageCount"												   :> GetImageCountElement["TIFF"],      
		"Author"|{"Author", All|"All"}								:> GetImageMetaData["TIFF","Author", All],
		"BitDepth"|{"BitDepth", All|"All"}							:> GetImageMetaData["TIFF","BitDepth", All],
		"CameraTopOrientation"|{"CameraTopOrientation", All|"All"}	:> GetImageMetaData["TIFF","CameraTopOrientation", All],
		"ColorMap"|{"ColorMap", All|"All"}							:> GetRawDataAndColorMapElements["TIFF", All],
		"ColorProfileData"|{"ColorProfile", All|"All"}				:> GetImageMetaData["TIFF","ColorProfileData", All],
		"ColorSpace"|{"ColorSpace", All|"All"}						:> GetImageMetaData["TIFF","ColorSpace", All],
		"Comments"|{"Comments", All|"All"}							:> GetImageMetaData["TIFF","Comments", All],
		"CopyrightNotice"|{"CopyrightNotice", All|"All"}			  :> GetImageMetaData["TIFF","CopyrightNotice", All],
		"Data"|{"Data", All|"All"}									:> GetDataElement["TIFF", All],
		"Device"|{"Device", All|"All"}								:> GetImageMetaData["TIFF","Device", All],
		"DeviceManufacturer"|{"DeviceManufacturer", All|"All"}		:> GetImageMetaData["TIFF","DeviceManufacturer", All],
		"GraphicsList"|{"GraphicsList", All|"All"}					:> GetGraphicsListElement["TIFF", All],
		"GrayLevels"|{"GrayLevels", All|"All"}						:> GetGrayLevelsElement["TIFF", All],
		"Image3D"|{"Image3D", All|"All"}							:> GetImage3DElement["TIFF", All],
		"ImageCreationDate"|{"ImageCreationDate", All|"All"}		:> GetImageMetaData["TIFF","ImageCreationDate", All],
		"ImageEncoding"|{"ImageEncoding", All|"All"}				:> GetImageMetaData["TIFF","ImageEncoding", All],
		"ImageList"|{"ImageList", All|"All"}						:> GetImageListElement["TIFF", All],
		"ImageResolution"|{"ImageResolution", All|"All"}			:> GetImageMetaData["TIFF","ImageResolution", All],
		"ImageSize"|{"ImageSize", All|"All"}						:> GetImageMetaData["TIFF","ImageSize", All],
		"RawData"|{"RawData", All|"All"}							:> GetRawDataAndColorMapElements["TIFF", All],
		"RGBColorArray"|{"RGBColorArray", All|"All"}				:> GetRGBColorArrayElement["TIFF", All],
		"RowsPerStrip"|{"RowsPerStrip", All|"All"}					:> GetImageMetaData["TIFF", "RowsPerStrip", All],
		"TileSize"|{"TileSize", All|"All"}							:> GetImageMetaData["TIFF", "TileSize", All],
        "Thumbnail" 												:> GetThumbnailElement["TIFF"],
		{"Thumbnail", f:(_Integer|_Symbol)}           				:> GetThumbnailElement["TIFF", f],
		"ThumbnailList"												:> GetThumbnailListElement["TIFF"], 
		{"ThumbnailList", f:(_Integer|_List|All|"All")} 			:> GetThumbnailListElement["TIFF", f],
		{"ThumbnailList", f:(_Integer|_List|All|"All"), s:(_Integer|_Symbol)} :> GetThumbnailListElement["TIFF", f, s],  

		{"Author", f:(_Integer|_List)}								:> GetImageMetaData["TIFF","Author", f],
		{"BitDepth", f:(_Integer|_List)}							:> GetImageMetaData["TIFF","BitDepth", f],
		{"CameraTopOrientation", f:(_Integer|_List)}				:> GetImageMetaData["TIFF","CameraTopOrientation", f],
		{"ColorMap", f:(_Integer|_List)}							:> GetRawDataAndColorMapElements["TIFF", f],
		{"ColorProfile", f:(_Integer|_List)}						:> GetImageMetaData["TIFF","ColorProfile", f],
		{"ColorSpace", f:(_Integer|_List)}							:> GetImageMetaData["TIFF","ColorSpace", f],
		{"Comments", f:(_Integer|_List)}							:> GetImageMetaData["TIFF","Comments", f],
		{"CopyrightNotice", f:(_Integer|_List)}						:> GetImageMetaData["TIFF","CopyrightNotice", f],
		{"Data", f:(_Integer|_List)}								:> GetDataElement["TIFF", f],
		{"Device", f:(_Integer|_List)}								:> GetImageMetaData["TIFF","Device", f],
		{"DeviceManufacturer", f:(_Integer|_List)}					:> GetImageMetaData["TIFF","DeviceManufacturer", f],
		{"GraphicsList", f:(_Integer|_List)}						:> GetGraphicsListElement["TIFF", f],
		{"GrayLevels", f:(_Integer|_List)}							:> GetGrayLevelsElement["TIFF", f],
		{"ImageCreationDate", f:(_Integer|_List)}					:> GetImageMetaData["TIFF","ImageCreationDate", f],
		{"ImageEncoding", f:(_Integer|_List)}						:> GetImageMetaData["TIFF","ImageEncoding", f],
		{"ImageList", f:(_Integer|_List)}							:> GetImageListElement["TIFF", f],
		{"Image3D", f:(_Integer|_List)}								:> GetImage3DElement["TIFF", f],
		{"ImageResolution", f:(_Integer|_List)}						:> GetImageMetaData["TIFF","ImageResolution", f],
		{"ImageSize", f:(_Integer|_List)}							:> GetImageMetaData["TIFF","ImageSize", f],
		{"RawData", f:(_Integer|_List)}								:> GetRawDataAndColorMapElements["TIFF", f],
		{"RGBColorArray", f:(_Integer|_List)}						:> GetRGBColorArrayElement["TIFF", f],
		{"RowsPerStrip", f:(_Integer|_List)}						:> GetImageMetaData["TIFF", "RowsPerStrip", f],
		{"TileSize", f:(_Integer|_List)}							:> GetImageMetaData["TIFF", "TileSize", f],

		(* default "Image" or "ImageList", depending on file *)
		Automatic 													:> GetDefaultImageElement["TIFF"],
		"Elements"													:> GetListOfElements["TIFF"],
		GetListOfElements["TIFF"]
	},
  "AvailableElements" -> {
			"Animation", "Author", "BitDepth", "CameraTopOrientation", 
			"ColorMap", "ColorProfileData", "ColorSpace", "Comments", "CopyrightNotice", "Data", 
			"Device", "DeviceManufacturer", "Graphics", 
			"GraphicsList", "GrayLevels", "Image", "Image3D", "ImageCount", 
			"ImageCreationDate", "ImageEncoding", "ImageList", "ImageResolution", 
			"ImageSize", "RawData", "RGBColorArray", "RowsPerStrip", "TileSize",
			"Thumbnail", "ThumbnailList"},
  "Options" -> {"BitDepth", "ColorSpace", "ImageSize", "Comments"},
  "BinaryFormat" -> True,
  "AlphaChannel" -> True,
  "Sources" -> {"Convert`CommonGraphics`"}
  (* "DefaultElement" intentionally left out, converter decides whether to return Image or ImageList *)
]

End[]






