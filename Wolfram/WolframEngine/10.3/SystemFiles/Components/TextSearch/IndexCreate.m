Package["TextSearch`"]

PackageImport["Macros`"]

PackageExport["CreateSearchIndex"]
PackageExport["$DefaultDriver"]

$DefaultDriver = "CLucene";

SetArgumentCount[CreateSearchIndex, {1, 2}]

(* undocumented, not proper way to handle options, but convenient for testing *)
Options[CreateSearchIndex] = {
	"ReplaceExisting" -> False, 
	Method -> "CLucene"
};

CreateSearchIndex[sources_, opts:OptionsPattern[]] :=
	CreateSearchIndex[sources, Automatic, opts];

CreateSearchIndex[sources_, name:Except[_Rule|_RuleDelayed], OptionsPattern[]] := 
	If[!(name === Automatic || ValidSearchIndexObjectNameQ[name]), 
		Message[CreateSearchIndex::invindnm, name]; $Failed,
		iCreateSearchIndex[
			sources, 
			Replace[name, Automatic :> CreateUUID[]], 
			OptionValue["ReplaceExisting"], OptionValue[Method]
		]
	];

CreateSearchIndex::crfail = "Failed to create search index with name `` in directory ``.";
CreateSearchIndex::eind = "A SearchIndexObject with name `` already exists. Use DeleteSearchIndex to remove it.";
CreateSearchIndex::invdriver = "`` is not one of ``.";
CreateSearchIndex::indabort = "Abort occured during indexing. The partial index at `` will be deleted.";
General::indempty = "No files were indexed. Files may have been ignored, or the provided path(s) may be empty.";

iCreateSearchIndex[sources_, name_, replace_, driver_] := Module[
	{files, normalized, obj, handle, temp, finished, meta, exists},
	If[!MemberQ[$ValidDrivers, driver],
		Message[CreateSearchIndex::invdriver, driver, $ValidDrivers];
		Return[$Failed];
	];
	path = ToObjectPath[name];
	obj = SearchIndexObject[File[path]];
	If[DirectoryQ[path], 
		If[replace, 
			DeleteSearchIndex[obj];
		,
			Message[CreateSearchIndex::eind, name];
			Return[$Failed]
		];
	];
	If[FailureQ @ StepCreateDirectory[path], 
		Message[CreateSearchIndex::crfail, name, path];
		Return[$Failed];
	];
	normalized = NormalizeSources[sources, CreateSearchIndex];
	files = EnumerateFilesToIndex[normalized];
	If[files === {}, 
		Message[CreateSearchIndex::nosrcs];
		DeleteSearchIndex[obj];
		Return[$Failed];
	];
	meta = <|
		"IndexedPaths" -> normalized,
		"Driver" -> driver,
		"CreationDate" -> Now,
		"Version" -> 0.9
	|>;
	PutIndexMetadata[obj, meta];
	handle = CreateHandle[obj];
	If[FailureQ[handle],
		Return[$Failed];
	];
	DCreateEmptyIndex[handle];
	Internal`WithLocalSettings[
		$IndexedFileCount = 0;
		finished = False;
	,
		finished = DAddToIndex[handle, files] =!= $Failed;
	,
		If[!finished, 
			Message[CreateSearchIndex::indabort, First[obj]];
			DeleteSearchIndex[obj];
			Return[$Failed, Module],
		If[$IndexedFileCount === 0,
			Message[CreateSearchIndex::indempty];
			DeleteSearchIndex[obj];
			Return[$Failed, Module];
		]];
	];
	obj
]

(* we can't create nested folder in the cloud*)
StepCreateDirectory[path_] /; $CloudEvaluation := Module[{everyFolder, existingFolders, missingFolders},
	everyFolder =  Reverse@FoldList[FileNameJoin[{##}] &, FileNameSplit[path]];
	existingFolders =  NestWhile[Rest[#] &, everyFolder, ! DirectoryQ[#[[1]]] &];
	missingFolders = Complement[everyFolder, existingFolders];
	Scan[
		CreateDirectory,
		missingFolders
	];
]

StepCreateDirectory[path_] /; !$CloudEvaluation := CreateDirectory[path]

