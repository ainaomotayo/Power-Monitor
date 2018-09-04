Package["TextSearch`"]

PackageImport["Macros`"]

SetArgumentCount[UpdateSearchIndex, 1];

UpdateSearchIndex[e_] := iUpdateSearchIndex[e];

iUpdateSearchIndex[name_String] := 
	iUpdateSearchIndex[SearchIndexObject[name]];

iUpdateSearchIndex[e_] := (
	Message[UpdateSearchIndex::invind, e]; 
	$Failed
);

iUpdateSearchIndex[obj_SearchIndexObject] := Module[
	{meta, handle, notstale, docs, sources},
	If[!ValidSearchIndexObjectQ[obj],
		Message[UpdateSearchIndex::invind, obj];
		Return[$Failed];
	];
	handle = CreateHandle[obj];
	If[FailureQ[handle],
		Message[UpdateSearchIndex::badind, obj];
		Return[$Failed]];
	notstale = DRemoveStaleDocumentsFromIndex[handle];
	sources = NormalizeSources[obj, UpdateSearchIndex];
	If[sources === {}, 
		Message[UpdateSearchIndex::indempty, obj];
		Return[$Failed]];
	docs = EnumerateFilesToIndex[sources];
	docs = Complement[docs, notstale];
	docs = DeleteCases[docs, $ExplicitFile[s_] /; MemberQ[notstale, s]];
	$IndexedFileCount = 0;
	DAddToIndex[handle, docs];
];
