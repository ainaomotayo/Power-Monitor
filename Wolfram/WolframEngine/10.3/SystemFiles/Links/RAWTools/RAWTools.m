BeginPackage["RAWTools`"]


Begin["`Private`"]

$InitRAWTools = False;

$ThisDirectory = FileNameDrop[$InputFileName, -1]
$BaseLibraryDirectory = FileNameJoin[{$ThisDirectory, "LibraryResources", $SystemID}];
$RawToolsLibrary = "RAWTools";
dlls["Windows"|"Windows-x86-64"|"Linux"|"Linux-x86-64"|"MacOSX-x86-64"] = {"libraw"};
dlls[___] := $Failed;

safeLibraryLoad[debug_, lib_] :=
	Quiet[
		Check[
			LibraryLoad[lib],
			If[TrueQ[debug],
				Print["Failed to load ", lib]
			];
			Throw[$InitRAWTools = $Failed]
		]
	]
safeLibraryFunctionLoad[debug_, args___] :=
	Quiet[
		Check[
			LibraryFunctionLoad[$RawToolsLibrary, args],
			If[TrueQ[debug],
				Print["Failed to load the function ", First[{args}], " from ", $RawToolsLibrary]
			];
			Throw[$InitRAWTools = $Failed]
		]
	]
  
InitRAWTools[debug_:False] := If[TrueQ[$InitRAWTools],
	$InitRAWTools,
	$InitRAWTools = Catch[
	  If[dlls[$SystemID] === $Failed,
	  	Message[RAWTools::sys, "Incompatible SystemID"];
	  	Throw[$Failed]
	  ];
	  Block[{$LibraryPath = Prepend[$LibraryPath, $BaseLibraryDirectory]},
		  safeLibraryLoad[debug, #]& /@ Flatten[{dlls[$SystemID], $RawToolsLibrary}];
		  $ErrorDescription = safeLibraryFunctionLoad[debug, "ErrorDescription", {}, "UTF8String"];
		  $ProfileLog = safeLibraryFunctionLoad[debug, "ProfileLog", {}, "UTF8String"];
		  $ReadImageRAW = safeLibraryFunctionLoad[debug, "ReadImageRAW",{{"UTF8String"}, _Integer, "Boolean", _Real, _Real}, "Image"];
		  $ReadThumbnailRAW = safeLibraryFunctionLoad[debug, "ReadThumbnailRAW",{{"UTF8String"}}, "Image"];
		  $ReadDataRAW = safeLibraryFunctionLoad[debug, "ReadDataRAW",{{"UTF8String"}}, {_Integer, _}];
		  $ReadMetadataStringRAW = safeLibraryFunctionLoad[debug, "ReadMetadataStringRAW", {{"UTF8String"}, {"UTF8String"}}, "UTF8String"];
		  $ReadMetadataIntegerRAW = safeLibraryFunctionLoad[debug, "ReadMetadataIntegerRAW", {{"UTF8String"}, {"UTF8String"}}, _Integer];
		  $ReadMetadataRealRAW = safeLibraryFunctionLoad[debug, "ReadMetadataRealRAW", {{"UTF8String"}, {"UTF8String"}}, _Real];
		  $ReadGPSDataRAW = safeLibraryFunctionLoad[debug, "ReadGPSDataRAW", {"UTF8String"}, {_Integer, 1}];
		  $ReadICCRAW = safeLibraryFunctionLoad[debug, "ReadICCRAW", {"UTF8String"}, {_Integer, 1}];
		  $GetColordataWhite = safeLibraryFunctionLoad[debug, "GetColordataWhite", {"UTF8String"}, {_Integer, 2}];
		  $GetColordataCamXYZ = safeLibraryFunctionLoad[debug, "GetColordataCamXYZ", {"UTF8String"}, {_Real, 2}];
		  $GetColordataCamMul = safeLibraryFunctionLoad[debug, "GetColordataCamMul", {"UTF8String"}, {_Real, 1}];
		  $GetColordataPreMul = safeLibraryFunctionLoad[debug, "GetColordataPreMul", {"UTF8String"}, {_Real, 1}];
		  $GetColordataCmatrix = safeLibraryFunctionLoad[debug, "GetColordataCmatrix", {"UTF8String"}, {_Real, 2}];
		  $GetColordataRGBCam = safeLibraryFunctionLoad[debug, "GetColordataRGBCam", {"UTF8String"}, {_Real, 2}];
		  $GetColordataCurve = safeLibraryFunctionLoad[debug, "GetColordataCurve", {"UTF8String"}, {_Integer, 1}];
		  $GetColordataBlack = safeLibraryFunctionLoad[debug, "GetColordataBlack", {"UTF8String"}, _Integer];
		  $GetColordataCblack = safeLibraryFunctionLoad[debug, "GetColordataCblack", {"UTF8String"}, {_Integer, 1}];
	  ];
	  True
	]
]


End[]
EndPackage[]
