Package["TextSearch`"]

PackageImport["GeneralUtilities`"]

PackageScope["ToObjectPath"]

ToObjectPath[name_String] := FileNameJoin[{$SearchIndicesDirectory, name}];
ToObjectPath[_] := $Failed;

ToObjectPathShort[name_String] := Block[{path},
	path = ToObjectPath[name];
	If[!DirectoryQ[path] && StringMatchQ[name, Repeated[HexadecimalCharacter, 8]],
		path = First[FileNames[name <> "-*", $SearchIndicesDirectory], $Failed];
	];
	path
];
	

PackageExport["SearchIndexObject"]

PackageScope["ValidSearchIndexObjectNameQ"]

ValidSearchIndexObjectNameQ[str_String] := 
	StringMatchQ[str, (WordCharacter | "-" | "_") ..];

ValidSearchIndexObjectNameQ[_] := False;

General::neind = "There is no SearchIndexObject with name ``.";
General::invindnm = "`` is not a valid name for a SearchIndexObject.";

SearchIndexObject[name_String] := Catch[
	name // FailUnless[ValidSearchIndexObjectNameQ, SearchIndexObject::invindnm, name] // 
	ToObjectPathShort // FailUnless[DirectoryQ, SearchIndexObject::neind, name] //
	File // SearchIndexObject
];


PackageExport["$SearchIndicesDirectory"]

EnsureDirectory[dir_] := If[DirectoryQ[dir], dir, CreateDirectory[dir]];

(* adapted from LocalObject code, should be factored out *) 
$SearchIndicesDirectory := $SearchIndicesDirectory = EnsureDirectory @ Switch[$OperatingSystem,
	"Unix", FileNameJoin[{$HomeDirectory, ".Wolfram", "SearchIndices"}],
	"MacOSX", FileNameJoin[{$HomeDirectory, "Library", "Wolfram", "SearchIndices"}],
	"Windows", FileNameJoin[{$WindowsAppData, "Wolfram", "SearchIndices"}]]

(* this is only for windows *)
$WindowsAppData := $WindowsAppData = "APPDATA" // 
	Replace[Developer`ToList[GetEnvironment["APPDATA"], _ -> None]] //
	Replace[None :> FileNameJoin[{$HomeDirectory, "AppData", "Local"}]];


PackageExport["SearchIndices"]

SetArgumentCount[SearchIndices, 0];

SearchIndices[] := SearchIndexObject[File[#]]& /@ Select[FileNames["*", $SearchIndicesDirectory], DirectoryQ];


PackageExport["ValidSearchIndexObjectQ"]

SearchIndexObject /: SystemOpen[SearchIndexObject[File[path_String]]] := SystemOpen[path];

ValidSearchIndexObjectQ[HoldPattern[SearchIndexObject[File[path_String]]]] := 
	DirectoryQ[path];

ValidSearchIndexObjectQ[_] := False;

$uuidRegex = 
	StringExpression @@ Riffle[Thread @ Repeated[HexadecimalCharacter, {8, 4, 4, 4, 12}], "-"];

totalByteCount[path_] := Switch[FileType[path],
	Directory,
		Total[totalByteCount /@ FileNames[All, path]],
	File,
		FileByteCount[path],
	_, 
		0
];

SearchIndexObject /: MakeBoxes[obj:SearchIndexObject[File[path_String]] ? ValidSearchIndexObjectQ, StandardForm] := Module[
	{name, shortname, indexedPaths, creationDate, indexLoc, indexSize = None, fileCounts, metadata, res},
	name = FileNameTake[path];
	If[StringMatchQ[name, $uuidRegex], shortname = StringTake[name, 8]]; 
	metadata = GetIndexMetadata[obj, {"IndexedPaths", "CreationDate", "IndexLocation"}];
	If[FailureQ[metadata], res = $Failed, 
	{indexedPaths, creationDate, indexLoc} = metadata;
	If[MissingQ[indexLoc], indexLoc = path];
	If[StringQ[indexLoc] && FileExistsQ[indexLoc], 
		indexSize = totalByteCount[indexLoc];
		fileCounts = Quiet @ DGetDocumentCount[CreateHandle[obj]];
		If[!IntegerQ[fileCounts], fileCounts = "unknown"];
	];
	res = BoxForm`ArrangeSummaryBox[
		SearchIndexObject, obj, 
		$searchIndexIcon,
		{
			If[StringQ[shortname], BoxForm`SummaryItem[{"Short name: ", shortname}], BoxForm`SummaryItem[{"Name: ", name}]],
			BoxForm`SummaryItem[{"Creation date: ", DateString[creationDate, "DateTimeShort"]}],
			If[IntegerQ[indexSize], BoxForm`SummaryItem[{"Index size: ", 
				Row[{fileCounts, " ", "files", " ", "(", bytesToSize[indexSize], ")"}]}], Nothing]
		},
		{
			If[StringQ[shortname], BoxForm`SummaryItem[{"Name: ", name}], Nothing],
			BoxForm`SummaryItem[{"Location: ", ParentDirectory[path]}],
			BoxForm`SummaryItem[{"Indexed paths: ", Column[indexedPaths, BaselinePosition -> 1]}]
		},
		StandardForm
	]];
	res /; res =!= $Failed
];

(* undocumented convenience... *)
$qpattern = All | _String | _Alternatives | _List | _Except | _Not;
(so_SearchIndexObject)[str:$qpattern] := TextSearch[so, str];
(so_SearchIndexObject)[str:$qpattern, prop_String] := TextSearch[so, str, prop];

PackageExport["GetDocumentCount"]

GetDocumentCount[obj_SearchIndexObject] := DGetDocumentCount[GetHandle @ obj];


PackageExport["GetIndexMetadata"]

GetIndexMetadata[SearchIndexObject[File[path_String]]] := 
	Quiet @ Check[Get @ FileNameJoin[{path, "IndexMetadata.wl"}], $Failed];

GetIndexMetadata[obj_, prop_] := 
	Block[{assoc = GetIndexMetadata[obj]},
	If[AssociationQ[assoc], Lookup[assoc, prop], $Failed]];

GetIndexMetadata[___] := $Failed;

SearchIndexObject /: Normal[so_SearchIndexObject] := 
	GetIndexMetadata[so];


PackageExport["PutIndexMetadata"]

PutIndexMetadata[SearchIndexObject[File[path_String]], meta_] := 
	Put[meta, FileNameJoin[{path, "IndexMetadata.wl"}]]; 


PackageExport["GetIndexedFiles"]

GetIndexedFiles[ts_TextSearchObject] := DGetIndexedFiles[GetHandle @ ts];

