Package["TextSearch`"]

PackageImport["PacletManager`"]
PackageImport["GeneralUtilities`"]	
PackageImport["JLink`"]


PackageExport["LuceneHandle"]

DCreateHandle[path_String, "Lucene", _] := (
	SetupJavaLucene[];
	LuceneHandle[FileNameJoin[{path, "index"}]]
);

DDeleteHandle[_LuceneHandle] := Null;

$JavaLuceneSentinelObject = None;
$JavaLuceneVersion = 30;
$VersionObject;


PackageScope["SetJavaLuceneVersion"]

SetJavaLuceneVersion[version_:48|35|30] := (
	$JavaLuceneVersion = version;
);

SetupJavaLucene[] := If[!JavaObjectQ[$JavaLuceneSentinelObject],
	InstallJava[];
	AddToClassPath[If[$JavaLuceneVersion === 30,
		FileNameJoin[{DirectoryName[FindFile["DocumentationSearch`"], 2], "Java", "Lucene30"}],
		PacletResource["TextSearch", versioned["lucene35", "lucene48"]]
	]];
	Scan[LoadJavaClass, {
		"org.apache.lucene.util.Version",
		"org.apache.lucene.store.FSDirectory",
		versioned[Nothing, "org.apache.lucene.index.IndexWriterConfig$OpenMode", "org.apache.lucene.index.IndexWriterConfig$OpenMode"],
		versioned["org.apache.lucene.document.Field$Index", Nothing],
		"org.apache.lucene.document.Field$Store",
		"org.apache.lucene.index.DirectoryReader",
		"org.apache.lucene.search.BooleanClause$Occur"
	}];
	$VersionObject = versioned[
		org`apache`lucene`util`Version`LUCENEU30, 
		org`apache`lucene`util`Version`LUCENEU35, 
		org`apache`lucene`util`Version`LUCENEU48
	];
	$JavaLuceneSentinelObject = JavaNew["java.lang.Object"];
	$storeNo = Symbol["org`apache`lucene`document`Field$Store`NO"];
	$storeYes = Symbol["org`apache`lucene`document`Field$Store`YES"];
	$tokenYes = Symbol["org`apache`lucene`document`Field$Index`ANALYZED"];
	$tokenNo = Symbol["org`apache`lucene`document`Field$Index`NOTUANALYZED"];	
	KeepJavaObject[$JavaLuceneSentinelObject, Manual];
];

SetAttributes[versioned, HoldAll];
versioned[expr1_, expr2_] := If[$JavaLuceneVersion < 48, expr1, expr2];
versioned[expr1_, expr2_, expr3_] := Switch[$JavaLuceneVersion,
	30, expr1,
	35, expr2,
	48, expr3
];

(* TODO: handle locks *)
openIndex[path_String, createNew_] := Module[{analyzerStr, analyzer, index, fsdir, conf, writer},
	If[createNew, Quiet@DeleteDirectory[path, DeleteContents->True]];
	analyzer = versioned[
		JavaNew["org.apache.lucene.analysis.SimpleAnalyzer"],
		JavaNew["org.apache.lucene.analysis.SimpleAnalyzer", $VersionObject],
		JavaNew["org.apache.lucene.analysis.core.SimpleAnalyzer", $VersionObject]
		];
	index = JavaNew["java.io.File", path];
	fsdir = org`apache`lucene`store`FSDirectory`open[index];
	If[$JavaLuceneVersion === 30,
		maxField = JavaNew["org.apache.lucene.index.IndexWriter$MaxFieldLength", 25000];
		writer = JavaNew["org.apache.lucene.index.IndexWriter", fsdir, analyzer, createNew, maxField];
		,
		conf = JavaNew["org.apache.lucene.index.IndexWriterConfig", $VersionObject, analyzer];
		conf@setOpenMode[If[createNew,
			org`apache`lucene`index`IndexWriterConfig$OpenMode`CREATE,
			org`apache`lucene`index`IndexWriterConfig$OpenMode`CREATEUORUAPPEND
		]];
		writer = JavaNew["org.apache.lucene.index.IndexWriter", fsdir, conf];
	];
	{writer, fsdir}
];

DCreateEmptyIndex[LuceneHandle[path_]] := JavaBlock @ Block[
	{writer, fsdir},
	{writer, fsdir} = openIndex[path, True];
	writer@close[];
	fsdir@close[];
];

DAddToIndex[LuceneHandle[path_], files_] := JavaBlock @ Block[
	{$writer, fsdir, text, n, makeField},
	makeField = versioned[makeField35, makeField48];
	{$writer, fsdir} = openIndex[path, False];
	displayProgressBar[files];
	Scan[addDocument, files];
	deleteProgressBar[];
	$writer@close[];
	fsdir@close[];
];

makeField35[name_, TextFile[path_]] := JavaNew["org.apache.lucene.document.Field", name, JavaNew["java.io.FileReader", path]];
makeField48[name_, TextFile[path_]] := makeField48[name, TextField[ReadStringASCII[path]]];
(* TODO: ^^ look up the actual way to do this, because obviously it can be done *)

makeField35[name_, TextField[path_]] := JavaNew["org.apache.lucene.document.Field", name, value, $storeNo, $tokenYes];
makeField48[name_, TextField[path_]] := JavaNew["org.apache.lucene.document.TextField", name, value, $storeNo];

makeField35[name_, str_String | File[str_String]] := JavaNew["org.apache.lucene.document.Field", name, str, $storeYes, $tokenNo];
makeField48[name_, str_String | File[str_String]] := JavaNew["org.apache.lucene.document.StringField", name, str, $storeYes];
 
 (* right padding ensures lexical ordering, 10 digits is enough to store UnixTimes and all practical file sizes *)
makeField35[name_, int_Integer] := makeField35[name, IntegerString[int, 10, 10]]; 
makeField48[name_, int_Integer] := JavaNew["org.apache.lucene.document.LongField", name, int, $storeYes];

makeField35[name_, d_DateObject] := makeField35[name, UnixTime[d]];
makeField48[name_, d_DateObject] := makeField48[name, UnixTime[d]];

addDocument[$ExplicitFile[path_]] := 
	Block[{$FileSizeLimitsEnabled = False}, addDocument[path]];

addDocument[path_] := Block[
	{fields, doc},
	fields = ReadFileFields[path];
	incrementProgressBar[path]; 
	If[FailureQ[fields], Return[]];
	doc = JavaNew["org.apache.lucene.document.Document"];
	KeyValueMap[doc@add[makeField[#1,#2]]&, fields];
	$writer@addDocument[doc];
	$IndexedFileCount++;
];

$maxDocuments = 10000;

DQueryColumn[LuceneHandle[path_], queryColumn_, query_, returnColumn_] := JavaBlock @ Block[
	{queryObject, reader, searcher, hits, res},
	queryObject = toQuery[queryColumn, query];
	reader = initReader[path];
	searcher = JavaNew["org.apache.lucene.search.IndexSearcher", reader];
	hits = versioned[
		searcher@search[queryObject, $maxDocuments], 
		searcher@search[queryObject, Null, $maxDocuments]
	]@scoreDocs;
	res = If[returnColumn === "__id",
		#@doc &/@ hits,
		hitToString[searcher, #, returnColumn] & /@ hits
	];
	reader @ close[];
	res
];

DGetDocumentCount[LuceneHandle[path_]] := JavaBlock @ Block[
	{$reader, maxCount, liveDocs, count},
	$reader = initReader[path];
	maxCount = $reader@maxDoc[] - 1;
	count = 0;
	versioned[
		Do[
			If[!$reader@isDeleted[i], count++],
			{i, 0, maxCount}
		]
	,
		If[$reader@hasDeletions[],
			(* Some documents were deleted from the index *)
			LoadJavaClass["org.apache.lucene.index.MultiFields"];
			liveDocs = org`apache`lucene`index`MultiFields`getLiveDocs[$reader];
			Do[
				If[liveDocs@get[i], count++],
				{i, 0, maxCount}
			],
			(* No document have been deleted in this index *)
			count = maxCount;
		]
	];
	reader @ close[];
	count
];

DRemoveStaleDocumentsFromIndex[LuceneHandle[path_]] := JavaBlock @ Block[
	{$reader, maxCount, liveDocs, res},
	$reader = initReader[path, False];
	maxCount = $reader@maxDoc[] - 1;
	res = ReapBag @ versioned[
		Do[
			If[!$reader@isDeleted[i], 
				deleteDocumentIfStale[$reader@document[i]]],
			{i, 0, maxCount}
		];
	,
		If[$reader@hasDeletions[],
			(* Some documents were deleted from the index *)
			LoadJavaClass["org.apache.lucene.index.MultiFields"];
			liveDocs = org`apache`lucene`index`MultiFields`getLiveDocs[$reader];
			Do[
				If[liveDocs@get[i], deleteDocumentIfStale[$reader@document[i]]],
				{i, 0, maxCount}
			],
			(*No document have been deleted in this index*)
			Do[
				deleteDocumentIfStale[$reader@document[i]],
				{i, 0, maxCount}
			]
		];
	];
	$reader @ close[];
	res
];

deleteDocumentIfStale[doc_] := Module[
	{time = doc@get["ModificationDate"], loc = doc@get["Location"]},
	If[!FileExistsQ[loc] || TrueQ[UnixTime[FileDate[loc]] > FromDigits[time]], 
		$reader@deleteDocument[i];
	,
		SowBag[loc];
	];
];

	
DGetIndexedFiles[LuceneHandle[path_]] := JavaBlock @ Block[
	{reader, maxCount, res},
	reader = initReader[path];
	maxCount = reader@maxDoc[] - 1;
	res = ReapBag @ versioned[
		(*Lucene 3.5*)
		Do[
			If[!reader@isDeleted[i], 
				SowBag[reader@document[i]@get["Location"]]], 
			{i, 0, maxCount}
		]
	,
		(*Lucene 4.8*)
		If[ reader@hasDeletions[],
			(*Some documents were deleted from the index*)
			LoadJavaClass["org.apache.lucene.index.MultiFields"];
			liveDocs = org`apache`lucene`index`MultiFields`getLiveDocs[reader];
			Do[
				If[liveDocs@get[i], SowBag[reader@document[i]@get["Location"]]],
				{i, 0, maxCount}
			],
			(*No document have been deleted in this index*)
			Do[
				SowBag[reader@document[i]@get["Location"]],
				{i, 0, maxCount}
			]
		]
	];
	reader@close[];
	res
];

toQuery[field_, query_] := 
	JavaNew["org.apache.lucene.search.TermQuery", JavaNew["org.apache.lucene.index.Term", field, query]];

hitToString[searcher_, hit_, field_] := searcher@doc[hit@doc]@get[field];

initReader[path_, ro_:True] := Module[{index, fsdir, reader},
	index = JavaNew["java.io.File", path];
	fsdir = org`apache`lucene`store`FSDirectory`open[index];
	reader = org`apache`lucene`index`DirectoryReader`open[fsdir, ro];
	reader
]


