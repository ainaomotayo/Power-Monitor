Package["TextSearch`"]


PackageScope["$CurrentVersion"]

$CurrentVersion = 0.9;


PackageScope["$HandleCache"]

$HandleCache = Data`UnorderedAssociation[];


PackageScope["DeleteHandle"]

DeleteHandle[obj_SearchIndexObject] := Block[{handle},
	handle = Lookup[$HandleCache, obj, Return[$Failed]];
	$HandleCache[obj] =.;
	DDeleteHandle[handle]
];


PackageScope["DeleteAllHandles"]

DeleteAllHandles[] := (
	Scan[DDeleteHandle, Values[$HandleCache]];
	$HandleCache = Data`UnorderedAssociation[];
);


PackageScope["CreateHandle"]

CreateHandle[obj_SearchIndexObject] :=
	Lookup[$HandleCache, obj, $HandleCache[obj] = iCreateHandle[obj]];

CreateHandle[_] := $Failed;

iCreateHandle[obj_] := Block[
	{meta, path, driver, version, handle},
	meta = GetIndexMetadata[obj];
	checkCorrupt[meta, obj];
	path = Lookup[meta, "IndexLocation", obj[[1,1]]];
	driver = Lookup[meta, "Driver", Lookup[meta, "IndexType", $Failed]];
	version = Lookup[meta, "Version", Infinity];
	If[version > $CurrentVersion, 
		Message[SearchIndexObject::version];
		Return[$Failed]
	];
	checkCorrupt[driver, obj];
	result = DCreateHandle[path, driver, version];
	checkCorrupt[result, obj];
	result
];

SearchIndexObject::version = "The given SearchIndexObject was created by a later version of Wolfram Language and cannot be used.";

checkCorrupt[thing_, obj_] := If[FailureQ[thing],
	Message[SearchIndexObject::badind, InputForm[obj]];
	Return[$Failed, Block]
];