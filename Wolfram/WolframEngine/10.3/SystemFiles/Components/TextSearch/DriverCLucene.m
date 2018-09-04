Package["TextSearch`"]

PackageImport["PacletManager`"]

PackageExport["CLuceneHandle"]

DCreateHandle[path_String, "CLucene", version_ /; version > 0.1] := Catch[
	LoadCLuceneAPI[];
	CLuceneHandle[$libCreateHandle[path]]
];

DDeleteHandle[CLuceneHandle[id_Integer]] :=
	$libDeleteHandle[id]; 

LoadCLuceneAPI[] := (
	LoadCLuceneLink[];
	LoadLibraryLinkAPI[{$CLuceneLinkLibraryPath, "CLuceneLink_"},
		$libCreateHandle[String] -> Integer,
		$libDeleteHandle[Integer] -> None,
		$libCreateEmptyIndex[Integer] -> None,
		$libAddStringField[Integer, String, String] -> None,
		$libAddTextField[Integer, String, String] -> None,
		$libAddTextFileField[Integer, String, String] -> None,
		$libAddUnixTimeField[Integer, String, Integer] -> None,
		$libAddIntegerField[Integer, String, Integer] -> None,
		$libAddBooleanField[Integer, String, Boole] -> None,
		$libAddPathField[Integer, String, String] -> None,
		$libAddDocument[Integer] -> Boole,
		$libGetSchema[Integer] -> String,
		$libRemoveStaleFiles[Integer] -> String,
		$libRemoveSelectedDocuments[Integer] -> None,
		$libFinalizeWrites[Integer] -> Boole,
		$libSelect[Integer, String] -> Integer,
		$libSelectByColumn[Integer, String, String] -> Integer,
		$libGetDocumentCount[Integer] -> Integer,
		$libGetColumn[Integer, String, Boole] -> String,
		$libGetAllColumns[Integer, Boole] -> String,
		$libDumpAllColumns[Integer, String, Boole] -> String,
		$libGetLastError[Integer] -> String,
		$libEnableLogging[String] -> None
	];
	Clear[LoadCLuceneAPI];
);

DCreateEmptyIndex[CLuceneHandle[id_]] := 
	$libCreateEmptyIndex[id];

$writingIndexText = "writing to index";

DAddToIndex[CLuceneHandle[id_], files_] := Block[
	{$done = False, $AccumulatedData = 0, $id = id, error}, 
	Internal`WithLocalSettings[
		displayProgressBar[files];
	,
		Scan[addDocument, files];
		error = $libFinalizeWrites[id]; (*Note: this returns a boolean, use to spot problems*)
		done = True;  
	,
		If[!done, 
			libClearAll[]; (* definition missing; should this flush everything?*)
		,
			setProgressBarText[$writingIndexText];
		];
		deleteProgressBar[];
	];
];

addDocument[$ExplicitFile[path_]] := 
	Block[{$FileSizeLimitsEnabled = False}, addDocument[path]];

addDocument[path_String] := Block[
	{fields = ReadFileFields[path], $added},
	incrementProgressBar[path];
	If[FailureQ[fields], Return[]];
	addFields[fields];
	$added = !$libAddDocument[$id];
	(*Note: this returns lastError != NULL, so False is good
	Do something with the error?
	*)
	$IndexedFileCount++;    
];

addDocument[fields_Association] := Module[{},
	(* We should increment the progess bar with something
	path might be not present in the association*)
	addFields[fields];
	$libAddDocument[$id]; 
	(* note: this produces a boolean;
	should be used to detect errors*)
	$IndexedFileCount++;
]

addFields[fields_] := KeyValueMap[addField, fields];

CreateSearchIndex::badfield = "Field `` with value `` is not supported.";

addField[name_, TextFile[path_]] := $libAddTextFileField[$id, name, path];
addField[name_, File[path_]] := 	$libAddPathField[$id, name, path];
addField[name_, TextField[str_]] := $libAddTextField[$id, name, str];
addField[name_, str_String] :=      $libAddStringField[$id, name, str];
addField[name_, int_Integer] :=     $libAddIntegerField[$id, name, int];
addField[name_, d_DateObject] :=    $libAddUnixTimeField[$id, name, UnixTime @ d];
addField[name_, bool_?BooleanQ] :=  $libAddBooleanField[$id, name, bool];
addField[name_, value_] := (Message[CreateSearchIndex::badfield, name, value]; $Failed);

DRemoveStaleDocumentsFromIndex[CLuceneHandle[id_]] := 
	parse @ $libRemoveStaleFiles[id];

DGetDocumentCount[CLuceneHandle[id_]] := 
	$libGetDocumentCount[id];

DQueryColumnCount[CLuceneHandle[id_], queryColumn_, query_] := (
	$libSelectByColumn[id, queryColumn, query]
)

DQueryColumn[CLuceneHandle[id_], queryColumn_, query_, returnColumn_] := (
	$libSelectByColumn[id, queryColumn, query];
	parse @ $libGetColumn[id, returnColumn, True]
)

DQueryColumnBulkReturn[CLuceneHandle[id_], queryColumn_, query_] := (
	$libSelectByColumn[id, queryColumn, query];
	parse @ $libGetAllColumns[id, True]
);

DGetAllDocuments[CLuceneHandle[id_]] :=
	parse @ $libGetAllColumns[id, False]

parse[str_String] := 
	Block[{$Context = "TextSearch`SandboxContext`"},
		Quiet @ Check[ToExpression[str], $Failed]
	];