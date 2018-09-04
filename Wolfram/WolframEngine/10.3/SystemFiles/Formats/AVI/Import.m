(* ::Package:: *)

Which[
	
	(****************** OS X ******************)
	StringMatchQ[$SystemID, "MacOSX*"],
	ImportExport`RegisterImport["AVI",
		{	(* Raw Importers *)
			"ImageList" 								:> (System`Convert`MovieDump`GetImageList["AVI"][##]&),
			"GraphicsList" 								:> (System`Convert`MovieDump`GetGraphicsList["AVI"][##]&),
			"Animation" 								:> (System`Convert`MovieDump`GetAnimation["AVI"][##]&),
			"Data" 										:> (System`Convert`MovieDump`GetData["AVI"][##]&),
			{"Frames", f:(_Integer|{__Integer})}		:> (System`Convert`MovieDump`GetFrames["AVI", f][##]&),
			{"Data", f:(_Integer|{__Integer})}	 		:> (System`Convert`MovieDump`GetFramesData["AVI", f][##]&),
			{"ImageList", f:(_Integer|{__Integer})} 	:> (System`Convert`MovieDump`GetFramesImage["AVI", f][##]&),
			{"GraphicsList", f:(_Integer|{__Integer})}	:> (System`Convert`MovieDump`GetFramesGraphics["AVI", f][##]&),
			(System`Convert`MovieDump`GetInfo["AVI"][##]&)
		},
		{},
		"Sources" -> {"Convert`CommonGraphics`", "Convert`QuickTime`", "QuickTime.exe", "Convert`JMF`", "JLink`"},
		"FunctionChannels" -> {"FileNames"},
		"AvailableElements" -> {"Animation", "BitDepth", "ColorSpace", "Data", "Duration",
				"FrameCount", "FrameRate", "Frames", "GraphicsList", "ImageList",
				"ImageSize", "VideoEncoding"},
		"DefaultElement" -> "Frames",
		"BinaryFormat" -> True
	],
		
	(****************** Windows ******************)	
	StringMatchQ[$SystemID, "Windows*"],
	If[(System`ConvertersDump`installSource["QuickTime.exe", Automatic]; System`Convert`MovieDump`QuickTimeInstalledQ[]),
		(* QuickTime Player installed *)
		ImportExport`RegisterImport["AVI",
			{	(* Raw Importers *)
				"ImageList" 								:> (System`Convert`MovieDump`GetImageList["AVI"][##]&),
				"GraphicsList" 								:> (System`Convert`MovieDump`GetGraphicsList["AVI"][##]&),
				"Animation" 								:> (System`Convert`MovieDump`GetAnimation["AVI"][##]&),
				"Data" 										:> (System`Convert`MovieDump`GetData["AVI"][##]&),
				{"Frames", f:(_Integer|{__Integer})}		:> (System`Convert`MovieDump`GetFrames["AVI", f][##]&),
				{"Data", f:(_Integer|{__Integer})}	 		:> (System`Convert`MovieDump`GetFramesData["AVI", f][##]&),
				{"ImageList", f:(_Integer|{__Integer})} 	:> (System`Convert`MovieDump`GetFramesImage["AVI", f][##]&),
				{"GraphicsList", f:(_Integer|{__Integer})}	:> (System`Convert`MovieDump`GetFramesGraphics["AVI", f][##]&),
				(System`Convert`MovieDump`GetInfo["AVI"][##]&)
			},
			{},
			"Sources" -> {"Convert`CommonGraphics`", "Convert`QuickTime`", "QuickTime.exe", "Convert`JMF`", "JLink`"},
			"FunctionChannels" -> {"FileNames"},
			"AvailableElements" -> {"Animation", "BitDepth", "ColorSpace", "Data", "Duration",
					"FrameCount", "FrameRate", "Frames", "GraphicsList", "ImageList",
					"ImageSize", "VideoEncoding"},
			"DefaultElement" -> "Frames",
			"BinaryFormat" -> True
		],
	
		(* QuickTime Player not installed.  Use JMF. *)	
		ImportExport`RegisterImport["AVI",
			{	(*Raw Importers*)
				"Animation" 								:> System`Convert`MovieDump`toAnimation,
				"GraphicsList" 								:> System`Convert`MovieDump`toGraphicsAll,
				"ImageList" 								:> System`Convert`MovieDump`toImageAll,
				"Data" 										:> System`Convert`MovieDump`toDataAll,
				{"Frames", a_Integer} 						:> System`Convert`MovieDump`toFrames[a],
				{"Frames", "Elements"}						:> System`Convert`MovieDump`getFrameCountFrames,
				{"GraphicsList", "Elements"} 				:> System`Convert`MovieDump`noSubElements["GraphicsList"],
				{"ImageList", "Elements"} 					:> System`Convert`MovieDump`noSubElements["ImageList"],
				{"Data", "Elements"} 						:> System`Convert`MovieDump`noSubElements["Data"],
				System`Convert`MovieDump`getMovieInfo
			},
			{	(*Postimporters*)
				"Frames" 									:> System`Convert`MovieDump`getFrameList,
				{"Frames", a_Integer} 						:> System`Convert`MovieDump`StripGraphics[a]
			},
			"Sources" -> {"Convert`CommonGraphics`", "Convert`JMF`", "JLink`"},
			"AvailableElements" -> {"Animation", "BitDepth", "ColorSpace", "Data", "Duration",
					"FrameCount", "FrameRate", "Frames", "GraphicsList", "ImageList",
					"ImageSize", "VideoEncoding"},
			"DefaultElement" -> "Frames",
			"BinaryFormat" -> True
		]
	]	
		
	,
	(****************** AVI for all other platforms ******************)
	True,
	ImportExport`RegisterImport["AVI",
		{	(*Raw Importers*)
			"Animation" 								:> System`Convert`MovieDump`toAnimation,
			"GraphicsList" 								:> System`Convert`MovieDump`toGraphicsAll,
			"ImageList" 								:> System`Convert`MovieDump`toImageAll,
			"Data" 										:> System`Convert`MovieDump`toDataAll,
			{"Frames", a_Integer} 						:> System`Convert`MovieDump`toFrames[a],
			{"Frames", "Elements"}						:> System`Convert`MovieDump`getFrameCountFrames,
			{"GraphicsList", "Elements"} 				:> System`Convert`MovieDump`noSubElements["GraphicsList"],
			{"ImageList", "Elements"} 					:> System`Convert`MovieDump`noSubElements["ImageList"],
			{"Data", "Elements"} 						:> System`Convert`MovieDump`noSubElements["Data"],
			System`Convert`MovieDump`getMovieInfo
		},
		{	(*Postimporters*)
			"Frames" 									:> System`Convert`MovieDump`getFrameList,
			{"Frames", a_Integer} 						:> System`Convert`MovieDump`StripGraphics[a]
		},
		"Sources" -> {"Convert`CommonGraphics`", "Convert`JMF`", "JLink`"},
		"AvailableElements" -> {"Animation", "BitDepth", "ColorSpace", "Data", "Duration",
				"FrameCount", "FrameRate", "Frames", "GraphicsList", "ImageList",
				"ImageSize", "VideoEncoding"},
		"DefaultElement" -> "Frames",
		"BinaryFormat" -> True
	];

]

