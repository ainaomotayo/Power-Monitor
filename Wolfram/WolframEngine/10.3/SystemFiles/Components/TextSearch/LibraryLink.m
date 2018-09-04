Package["TextSearch`"]

PackageImport["PacletManager`"]

PackageScope["LoadLibraryLinkAPI"]

$typeSugarRules = {String -> "UTF8String", Boole -> (True|False), None -> "Void"};

SetAttributes[LoadLibraryLinkAPI, HoldRest];
LoadLibraryLinkAPI[{path_String, prefix_String}, rule___Rule] := 
	Block[{$path = path, $prefix = prefix},
		Apply[List] @ Map[declareLibFunc] @ ReplaceAll[Hold[rule], $typeSugarRules];
	];

SetAttributes[declareLibFunc, HoldFirst];
declareLibFunc[sym_Symbol[argTypes___] -> retType_] := (
	Clear[sym];
	Set[sym, checkLibError @ LibraryFunctionLoad[
		$path, 
		$prefix <> StringTrim[SymbolName[sym], "$lib"],
		{argTypes}, retType
	]];

);

SearchIndexObject::liberr = "Failed to load the internal search library. Please contact technical support.";
SetAttributes[checkLibError, HoldFirst];
checkLibError[expr_] := Block[
	{res = Check[expr, $Failed]},
	If[FailureQ[res], 
		Message[SearchIndexObject::liberr];
		Throw[$Failed];
	,
		res
	]
];


PackageScope["CheckedLibraryLoad"]

CheckedLibraryLoad[path_] := checkLibError[LibraryLoad[path]];


PackageScope["LoadCLuceneLink"]
PackageScope["$CLuceneLinkLibraryPath"]

LoadCLuceneLink[] := Block[{libDir},
	libDir = PacletResource["TextSearch", "libraryDir"];
	$CLuceneLinkLibraryPath = FileNameJoin[{libDir, "libTextSearch"}];
	If[!$windowsQ,
		(* hernanm built windows as a single library, but other platforms
			as multiple libraries, for some reason *)
		Scan[
			CheckedLibraryLoad[FileNameJoin[{libDir, #}]]&,
			{"libclucene-shared", "libclucene-core"}
		];
	];
	CheckedLibraryLoad[$CLuceneLinkLibraryPath];
];
