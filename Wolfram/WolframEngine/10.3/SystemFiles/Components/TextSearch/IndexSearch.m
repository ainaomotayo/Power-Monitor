Package["TextSearch`"]

PackageImport["Macros`"]

TextSearch::invq = "Invalid query element `` in ``.";
TextSearch::invqs = "Query string \"``\" is not a word or sequence of words.";
TextSearch::invneg = "Negation in query `` is invalid.";

compileQuery[query_] := Catch @ Block[{$fq = query, res}, 
	res = comp[query];
	If[!FreeQ[res, _QNot], 
		Message[TextSearch::invneg, $fq]; Throw[$Failed]];
	res
];

comp[list_List] := QIntersection[Map[comp, list]];

comp[alts_Alternatives] := QUnion[Map[comp, List @@ alts]];

comp[Verbatim[Except][e_]] := QNot[comp[e]];

comp[Verbatim[Except][p_, c_]] := comp[{c, Except[p]}];

comp[s_String] := compStr[StringTrim @ ToLowerCase @ s];

comp[All] := All;

compStr[s_] := Which[
	StringMatchQ[s, WordCharacter..], 
		QString[s],
	StringMatchQ[s, Repeated[WordCharacter.. ~~ Whitespace] ~~ WordCharacter..],
		QIntersection[Map[QString, StringSplit[s]]],
	True,
		Message[TextSearch::invqs, s]; Throw[$Failed];
];

comp[e_] := (Message[TextSearch::invq, e, $fq]; Throw[$Failed];)

QIntersection[e_List /; MemberQ[e, _QNot]] := 
	If[
		Count[e, Except[_QNot]] === 0, 
		Message[TextSearch::invneg, $fq]; Throw[$Failed],
		QComplement[
			QIntersection[DeleteCases[e, _QNot]], 
			QIntersection[Cases[e, QNot[s_] :> s]]
		]
	];

QIntersection[{e_}] := e;

ExecuteQuery[{src_}, q_, r_] := exec[src, q, r];

ExecuteQuery[srcs_List, q_, "Count"] := Total @ execList[srcs, q, "Count"];
ExecuteQuery[srcs_List, q_, r_] := Union @@ execList[srcs, q, r];

execList[srcs_, q_, r_] := Map[exec[#, q, r]&, srcs];

exec[s_, q_QIntersection | q_QComplement | q_QUnion, "Count"] :=
	Length @ exec[s, q, "Location"];

exec[s_, q_QIntersection | q_QComplement | q_QUnion, "Columns"] :=
	$Failed;

exec[s_, QIntersection[q_], r_] := Intersection @@ exec[s, q, r];
exec[s_, QComplement[q1_, q2_], r_] := Complement[exec[s, q1, r], exec[s, q2, r]];
exec[s_, QUnion[q_], r_] := Union @@ exec[s, q, r];

exec[s_, q_List, r_] := Map[exec[s, #, r]&, q];

exec[s_, All, r_] := execAllQuery[s, r];
exec[s_, QString[q_], r_] := execStringQuery[s, q, r];

exec[___] := $Failed;

execAllQuery[path_String, res_] := 
	filesToResult[
		EnumerateFilesToIndex[path],
		res
	];

execAllQuery[IndexHandle[h_], res_] := 
	NIFallback[
		Switch[res,
			"ContentObject",
				DGetAllColumns[h] // columnsToContentObjects,
			"Count",
				DGetDocumentCount[h],
			"Location",
				DGetIndexedFiles[h],
			"Columns",
				DGetAllColumns[h],
			"Association",
				DGetAllColumns[h] // columnsToAssocs,
			_, 
				Missing["UnknownProperty", res]
		],
		filesToResult[
			DGetIndexedFiles[h],
			res
		]
	];


(* search on disk*)
execStringQuery[path_String, str_, res_] := 
	filesToResult[
		IFileSearch[path, str, {True, True, Automatic, None}, "Files"],
		res
	];

(* search on index*)
zexecStringQuery[IndexHandle[h_], str_] := DQueryColumn[h, "Plaintext", str, "Location"];


execStringQuery[IndexHandle[h_], str_, res_] := 
	NIFallback[
		Switch[res,
			"ContentObject",
				DQueryColumnBulkReturn[h, "Plaintext", str] // columnsToContentObjects,
			"Count",
				DQueryColumnCount[h, "Plaintext", str],
			"Location",
				DQueryColumn[h, "Plaintext", str, "Location"],
			"Columns",
				DQueryColumnBulkReturn[h, str],
			"Association",
				DQueryColumnBulkReturn[h, str] // columnsToAssocs,
			_, 
				Missing["UnknownProperty", res]
		],
		filesToResult[
			DQueryColumn[h, "Plaintext", str, "Location"],
			res
		]
	];

filesToResult[files_, res_] := 
	Switch[res,
		"ContentObject",
			files // filesToContentObjects,
		"Count",
			files // Length,
		"Location",
			files,
		"Association",
			files // filesToAssocs,
		"Columns",
			files // filesToAssocs // assocsToColumns,
		_,
			Missing["UnknownProperty", res]
	];

filesToAssocs[files_] :=
	If[!Developer`StringVectorQ[files], $Failed,
		DeleteCases[$Failed] @ Map[ReadFileFields[#, False]&, files]];

filesToContentObjects[files_] :=
	If[!Developer`StringVectorQ[files], $Failed,
		ContentObject[File[#]]& /@ files];

assocsToColumns[assocs_] := Block[{keys, vals},
	If[!Developer`AssociationVectorQ[assocs], Return[$Failed]];
	Association[# -> Lookup[assocs, #]& /@ $FileFields]
];

columnsToAssocs[assoc_] := columnsTo[assoc, Identity];

columnsToContentObjects[assoc_] := columnsTo[assoc, ContentObject];

columnsTo[$NotImplemented, _] := $NotImplemented;
columnsTo[assoc_, f_] := Block[{keys, vals},
	If[!AssociationQ[assoc], Return[$Failed]];
	keys = Keys[assoc];
	vals = Values[assoc];
	Quiet @ Check[
		f[AssociationThread[keys, #]]& /@ Transpose[vals],
		$Failed
	]
];


PackageExport["TextSearch"]

SetArgumentCount[TextSearch, {2, 2}];

TextSearch[sources_, query_] := 
	iTextSearch[sources, query, "ContentObject"]

TextSearch[sources_, query_, type_String] := 
	iTextSearch[sources, query, type]
	
TextSearch::interr = "An internal error occurred while performing the search. Please contact Technical Support.";
TextSearch::cld = "Operation not yet supported in the Cloud";

(* fast path for 'ag' *)
iTextSearch[source_String ? FileExistsQ, query_String, result_] :=
	execStringQuery[source, query, result];

iTextSearch[sources_, query_, result_] := 
	Module[{cquery, paths, res},
	cquery = compileQuery[query];
	If[FailureQ[cquery], Return[$Failed]];
	paths = NormalizeSources[sources, TextSearch];
	If[Developer`EmptyQ[paths], 
		Message[TextSearch::nosrcs];
		Return[$Failed]];
	res = ExecuteQuery[paths, cquery, result];
	If[res === $Failed,
		Message[TextSearch::interr];
		Return[$Failed]
	];
	res
]

toContentObject[str_String] := ContentObject[File[str]];
toContentObject[_] := $Failed;

PackageExport["$LastError"]


PackageExport["TextSearchReport"]

SetArgumentCount[TextSearchReport, {2, 2}]

TextSearchReport[sources_, query_] := ConditionalRHS[True, {}, iTextSearchReport[sources, query]];

iTextSearchReport[sources_, query_] := Module[{res},
	res = iTextSearch[sources, query, "Association"];
	If[!Developer`AssociationVectorQ[res], Return[$Failed]];
	If[res === {}, Return[Dataset[{}]]];
    Dataset @ Map[
    	KeySortBy[#, Position[$keyOrder, #]&]&, 
    	MapAt[First, res, {All, "Location"}]
	]
];

$keyOrder = {"FileName", "ModificationDate", "CreationDate", "Snippet", "FileByteCount", "FileExtension", "Location"};