Package["TextSearch`"]

PackageScope["NormalizeSources"]
PackageScope["IndexHandle"]

(*Note: NormalizeSources will also show hidden files, so
when it's used in the SummaryBox, such files will show up,
even if they are not indexed*)
NormalizeSources[spec_, head_] := Block[
	{$head = head, $expand = (head =!= TextSearch)},
	setupHardcodedPaths;
	DeleteCases[$Failed] @ Flatten @ List @ normalize @ spec
];

General::nesrc = "The search path `` does not exist.";
General::invsrc = "`` is not a valid file, directory, ContentObject or SearchIndexObject.";
General::invind = "`` is not a valid SearchIndexObject.";
General::badind = "Index `` is corrupt.";
General::nosrcs = "One or more valid directories, files, or SearchIndexObjects are required.";
General::cosupp = "CloudObject is not supported in this version of the Wolfram Language.";

General::csupp = "`` is not supported in this version of the Wolfram Cloud."

$expand = False;

PackageImport["GeneralUtilities`"]

checkPath[x_] := If[FileExistsQ[x], x, msg["nesrc", x]];

normalize[File[path_String]]  := normalizePath[path];
normalize[path_String]  := normalizePath[path];

normalizePath[path_String] := If[
	$CloudEvaluation, cloudns[],
	checkPath @ ExpandFileName @ path
];

msg[str_String, args___] := With[{head = $head}, Message[MessageName[head, str], args]; $Failed];

cloudns[] := msg["csupp", $head];

normalize[obj_SearchIndexObject] := Block[{tmp},
	If[!ValidSearchIndexObjectQ[obj], 
		msg["invind", obj],
	If[$expand,
		If[!Developer`StringVectorQ[tmp = GetIndexMetadata[obj, "IndexedPaths"]], 
			msg["badind", obj],
			checkPath /@ tmp
		]
	,
		If[FailureQ[tmp = CreateHandle @ obj],
			msg["badind", obj],
			IndexHandle @ tmp
		]
	]]
];

normalize[_CloudObject] := msg["cosupp"];

normalize[co_ContentObject ? ValidContentObjectQ] := Module[{loc},
	If[StringQ[loc = co["Location"]] && FileExistsQ[loc = ExpandFileName[loc]],
		loc, msg["invsrc", co]]];

normalize[list_List] := Map[normalize, list];

normalize[other_] := msg["invsrc", other];

setupHardcodedPaths := (
	normalizePath["ExampleData/Text"] := 
		PacletManager`PacletResource["TextSearch", "ExampleData"];
	Scan[
		(normalizePath["ExampleData/Text/" <> FileNameTake[#]] = #)&,
		FileNames["*", PacletManager`PacletResource["TextSearch", "ExampleData"], Infinity]
	];
	Clear[setupHardcodedPaths];
);

PackageScope["$ExplicitFile"]
PackageExport["EnumerateFilesToIndex"]

PackageImport["GeneralUtilities`"]

$IgnoredExt := $IgnoredExt = RegularExpression @ 
	StringInsert[
		ToRegularExpression["." ~~ Alternatives @@ $IgnoredFileExtensions ~~ EndOfString],
		"i", 3];

If[$OperatingSystem === "MacOSX", 
	$IgnoredPath = "/." | RegularExpression["^/Users/\\w*/Library/?$"],
	$IgnoredPath = $PathnameSeparator <> "."
];

EnumerateFilesToIndex[path_] :=  Block[{$pathbag = Internal`Bag[]}, 
	Scan[iEnumerateFilesToIndex0, ExpandFileName /@ Developer`ToList[path]]; 
	Internal`BagPart[$pathbag, All]
];

TextSearch::badext = "Cannot index file ``.";
TextSearch::badpath = "Cannot index path ``.";

iEnumerateFilesToIndex0[path_] := 
	Switch[FileType[path],
		File,
			If[MemberQ[$IgnoredFileExtensions, FileExtension[path]],
				Message[TextSearch::badext, path],
				Internal`StuffBag[$pathbag, $ExplicitFile[path]]
			], 
		Directory,
			If[!StringFreeQ[path, $IgnoredPath],
				Message[TextSearch::badpath, path],
				iEnumerateFilesToIndex1[path]
			]
	];

iEnumerateFilesToIndex1[path_] := If[StringFreeQ[path, $IgnoredPath],
    Switch[FileType[path],
        Directory, 
            Scan[iEnumerateFilesToIndex1, FileNames[All, path]],
        File, 
        	If[StringFreeQ[path, $IgnoredExt], Internal`StuffBag[$pathbag, path]]
    ]];  