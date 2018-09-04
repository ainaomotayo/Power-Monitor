Package["TextSearch`"]

PackageExport["LegacyCLuceneHandle"]

DCreateHandle[path_String, "CLucene", 0.1] := Catch[
	LoadLegacyCLuceneAPI[];
	LegacyCLuceneHandle[path]
];

DDeleteHandle[_LegacyCLuceneHandle] := Null;

LoadLegacyCLuceneAPI[] := (
	LoadCLuceneLink[];
	LoadLibraryLinkAPI[{$CLuceneLinkLibraryPath, "LegacyCLuceneLink_"},
		$libCreateEmptyIndex[String] -> None,
		$libAddStringFieldToDocument[String, String] -> None,
		$libAddTextFieldToDocument[String, String] -> None,
		$libAddTextFileToDocument[String, String] -> None,
		$libAddDocumentToList[] -> None,
		$libAddListToIndex[String] -> None,
		$libClearWorkspace[] -> None,
		$libGetDocumentCount[String] -> Integer,
		$libQueryColumn[String, String, String, String] -> String,
		$libQuery[String, String, String] -> String,
		$libGetColumn[String, String] -> String,
		$libRemoveStaleDocumentsFromIndex[String] -> String,
		$libEnableLogging[String] -> None
	];
	Clear[LoadLegacyCLuceneAPI];
);

DCreateEmptyIndex[LegacyCLuceneHandle[path_String]] := 
	$libCreateEmptyIndex[path];

DGetDocumentCount[LegacyCLuceneHandle[path_]] := 
	$libGetDocumentCount[path];

addField[name_, File[path_]] := 	$libAddTextFileToDocument[name, path];
addField[name_, TextField[str_]] := $libAddTextFieldToDocument[name, str];
addField[name_, str_String] := 		$libAddStringFieldToDocument[name, str];
addField[name_, int_Integer] := 	$libAddStringFieldToDocument[name, IntegerString[int, 10, 10]];
addField[name_, d_DateObject] := 	$libAddStringFieldToDocument[name, IntegerString[UnixTime @ d, 10, 10]];
addField[name_, x_] := (Message[CreateSearchIndex::interr, x, name]; $Failed);

CreateSearchIndex::interr = "An internal error occurred in which the invalid value `` was used for field ``.";


PackageExport["$IndexFlushingThreshold"]

$IndexFlushingThreshold = 10000000; (* 10 megabytes *)

addDocument[$ExplicitFile[path_]] := 
	Block[{$FileSizeLimitsEnabled = False}, addDocument[path]];

$writingIndexText = "writing to index";
addDocument[path_String] := Block[
	{fields = ReadFileFields[path]},
	incrementProgressBar[path];
	If[FailureQ[fields], Return[]];
	KeyValueMap[addField, fields];
	$libAddDocumentToList[];
	$IndexedFileCount++;
	$AccumulatedData += fields["FileByteCount"];
	If[$AccumulatedData > $IndexFlushingThreshold,
		(* periodically flush accumulated data to the index *)
		setProgressBarText[$writingIndexText];
		$libAddListToIndex[$index];
		$AccumulatedData = 0;
	];
];

DAddToIndex[LegacyCLuceneHandle[path_], files_] := Block[
	{$done = False, $AccumulatedData = 0, $index = path},
	Internal`WithLocalSettings[
		displayProgressBar[files];
	,
		Scan[addDocument, files];
		done = True;
	,
		If[!done, 
			libClearAll[]; (* definition missing; should this flush everything?*)
		,
			setProgressBarText[$writingIndexText];
			$libAddListToIndex[path];
		];
		deleteProgressBar[];
	];
];

DQueryColumn[LegacyCLuceneHandle[path_], queryColumn_, query_, returnColumn_] := 
	StringSplit[$libQueryColumn[path, queryColumn, query, returnColumn], "\t"];

DRemoveStaleDocumentsFromIndex[LegacyCLuceneHandle[path_]] := 
	StringSplit[$libRemoveStaleDocumentsFromIndex[path], "\t"];

DGetIndexedFiles[LegacyCLuceneHandle[path_]] := 
	getColumn[path, "Location"];

getColumn[path_, colname_] :=
	StringSplit[$libGetColumn[path, colname], "\t"];	

EnableLibLogging[logPath_String] := 
	$libEnableLogging[logPath];

