Package["TextSearch`"]


PackageExport["WLNativeHandle"]

DCreateHandle[path_String, "WLNative", _] := (
	WLNativeHandle[path]
);

DDeleteHandle[_WLNativeHandle] := Null;


PathDictFile[path_] := PathDictFile[path] = FileNameJoin[{path, "PathDict.mx"}];
TermDictFile[path_] := TermDictFile[path] = FileNameJoin[{path, "TermDict.mx"}];
TimeDictFile[path_] := TimeDictFile[path] = FileNameJoin[{path, "TimeDict.mx"}];
InvIndexFile[path_] := InvIndexFile[path] = FileNameJoin[{path, "InvIndex.mx"}];

$unordered = Data`UnorderedAssociation[];

SetAttributes[BlockAll, HoldAll];
BlockAll[body_] := Block[
	{$PathDict = $unordered, 
	 $TermDict = $unordered,
	 $TimeDict = $unordered, 
	 $InvIndex = $unordered}, 
	body
];

SetAttributes[PersistAll, HoldRest];
PersistAll[path_, body_] := Block[
	{$PathDict, $TermDict, $TimeDict, $InvIndex, $Changed = False, result}, 
	GetPathDict[path]; GetTermDict[path]; GetTimeDict[path]; GetInvIndex[path];
	result = body;
	If[$Changed,
		PutPathDict[path]; PutTermDict[path]; PutTimeDict[path]; PutInvIndex[path];
	];
	result
];


PutPathDict[path_] := DumpSave[PathDictFile[path], $PathDict];
PutTermDict[path_] := DumpSave[TermDictFile[path], $TermDict];
PutTimeDict[path_] := DumpSave[TimeDictFile[path], $TimeDict];
PutInvIndex[path_] := DumpSave[InvIndexFile[path], $InvIndex];

GetPathDict[path_] := Get[PathDictFile[path]];
GetTermDict[path_] := Get[TermDictFile[path]];
GetTimeDict[path_] := Get[TimeDictFile[path]];
GetInvIndex[path_] := Get[InvIndexFile[path]];

DCreateEmptyIndex[WLNativeHandle[path_]] := BlockAll[
	PutPathDict[path];
	PutTermDict[path];
	PutTimeDict[path];
	PutInvIndex[path];
];

DGetDocumentCount[WLNativeHandle[path_]] := BlockAll[
	GetTimeDict[path];
	Length @ $TimeDict
];

addDocument[hash_ -> $ExplicitFile[path_]] := 
	Block[{$FileSizeLimitsEnabled = False}, addDocument[hash -> path]];

addDocument[hash_ -> path_] := Block[{text, modtime}, 
	incrementProgressBar[path];
	text = Replace[LimitedReadPlaintext[path], {TextFile[f_] :> ReadStringASCII[f], TextField[s_] :> s}];
	If[!StringQ[text], Return[]];
	$Changed = True;
	modtime = UnixTime @ FileDate[path];
	terms = Union @ StringCases[text, l:LetterCharacter.. :> Hash[ToLowerCase[l]]];
	AssociateTo[$TermDict, hash -> terms];
	AssociateTo[$TimeDict, hash -> modtime];
	Scan[Function[term,
		$InvIndex[term] = Append[Lookup[$InvIndex, term, {}], hash];
	], terms];
	$IndexedFileCount++;
];

DAddToIndex[WLNativeHandle[path_], files_] := PersistAll[path, 
	Block[{hashes, threaded, done},
	hashes = Hash /@ (files /. $ExplicitFile[f_] :> f);
	threaded = Thread[hashes -> files];
	AssociateTo[$PathDict, threaded /. $ExplicitFile[f_] :> f];
	Internal`WithLocalSettings[
	    displayProgressBar[files];
	,
		Scan[addDocument, threaded];
		done = True;
	,
		If[!done, 
			$Changed = False;,
			setProgressBarText["writing index"];
		];
		deleteProgressBar[];
	]
]];

DQueryColumn[WLNativeHandle[path_], "Plaintext", query_, "Location"] := BlockAll[
	GetPathDict[path];
	GetInvIndex[path];
	Lookup[$PathDict, $InvIndex[Hash[query]], {}]
];

DQueryColumn[WLNativeHandle[_], _, _, _] := $Failed;

DRemoveStaleDocumentsFromIndex[WLNativeHandle[path_]] := PersistAll[path, 
	KeyValueMap[removeDocumentIfStale, $PathDict]
];

removeDocumentIfStale[hash_, path_] := If[!documentStaleQ[hash, path], path,
	Scan[Function[term, 
		If[($InvIndex[term] = DeleteCases[$InvIndex[term], hash]) === {},
			Unset[$InvIndex[term]];
		];
	], $TermDict[hash]];
	Unset[$TimeDict[hash]];
	Unset[$PathDict[hash]];
	Unset[$TermDict[hash]];
	$Changed = True;
	Nothing
];

documentStaleQ[hash_, file_] := !FileExistsQ[file] || UnixTime[FileDate[file]] > $TimeDict[hash];

DGetIndexedFiles[WLNativeHandle[path_]] := BlockAll[
	GetPathDict[path];
	Keys[$PathDict]
];
    
DumpNativeIndex[WLNativeHandle[path_]] := PersistAll[
	path,
	<|"PathDict" -> $PathDict,
	"TermDict" -> $TermDict,
	"TimeDict" -> $TimeDict,
	"InvIndex" -> $InvIndex|>
];

