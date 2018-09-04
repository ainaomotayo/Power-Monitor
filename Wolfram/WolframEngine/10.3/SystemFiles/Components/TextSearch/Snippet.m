Package["TextSearch`"]

PackageImport["Macros`"]

PackageExport["Snippet"]

SetArgumentCount[Snippet, {1, 2}];

Snippet::invpgw = "PageWidth should be a positive integer.";
Snippet::invcnt = "Content should be string, File, or valid ContentObject."

posIntPatt = _Integer?Positive;

Options[Snippet] = {PageWidth -> 80};

Snippet[e_, n_:Except[_Rule], OptionsPattern[]] := 
	iSnippet[e, n, OptionValue[PageWidth]];

Snippet[e_, OptionsPattern[]] := 
	iSnippet[e, 2, OptionValue[PageWidth]];

iSnippet[content_, spec_, width_] := Catch @ Block[
	{$width = width, $spec = spec},
	If[!IntegerQ[$width], 
		Message[Snippet::invpgw];
		Throw[$Failed]];
	$spec = procSpec[$spec];
	snip[content]
];

nonzero = Except[0, _Integer];

procSpec[Span[a_, b_]] := Sequence[procStart @ a, procEnd @ b];
procSpec[Span[a_, b_, 1]] := procSpec[Span[a, b]];
procSpec[Span[a_, b_, c_Integer ? Positive]] := Sequence[procStart @ a, procEnd @ b, c];
procSpec[i_] := procInt[i];

procStart[All] := Front[1];
procStart[i_] := procInt[i];

procEnd[All] := Back[1];
procEnd[i_] := procInt[i];

procInt[Except[0, i_Integer]] := If[Positive[i], Front[i], Back[-i]];
procInt[_] := failspec[];

failspec[] := fail["invspec", $spec];
Snippet::invspec = "Specification `` should be an integer or Span.";

snip[str_String] := snipstr[str, $spec];
snip[File[path_]] := snipfile[path, $spec];
snip[c_ContentObject ? ValidContentObjectQ] := (
	Replace[c["Location"], File[path_String] :> Return @ snipfile[path, $spec]];
	Replace[c["Plaintext"], str_String :> Return @ snipstr[str, $spec]];
	fail["invco"];
);

snip[_] := fail["invcnt"];
Snippet::invco = "ContentObject does not contain a valid \"Location\" or \"Plaintext\" field.";

snipfile[file_, spec___] /; MemberQ[$ImportedFileExtensions, FileExtension[file]] := 
	snipstr[Quiet @ Check[Import[file, "Plaintext"], $Failed], spec];

snipfile[file_, spec_] := 
	snipstr[readfile[file, readupto[spec]], spec];

$width = 80;
readupto[Front[n_]][s_] := 
	toUTF @ BinaryReadList[s, "Byte", n * $width];

readupto[Back[n_]][s_] := Block[{len},
	SetStreamPosition[s, Infinity];
	len = StreamPosition[s];
	SetStreamPosition[s, Max[0, len - n * $width]];
	toUTF @ BinaryReadList[s, "Byte"]
];

snipfile[file_, spec1_, spec2_] := 
	catstr @ readfile[file, readbetween[spec1, spec2]];

snipfile[file_, spec1_, spec2_, n_] := 
	catstr @ readfile[file, readbetween[spec1, spec2, n]];

catstr[{}] := "";
catstr[list_List ? Developer`StringVectorQ] := 
	FromCharacterCode[Flatten @ Riffle[ToCharacterCode[list], 10]]
catstr[_] := $Failed;

readbetween[spec1_, spec2_, 1] := 
	readbetween[spec1, spec2];

readbetween[f_Front, Back[1], skip_] := 
	readbetween[f, Front[Infinity], skip];

readbetween[Front[1], Front[last_], skip_][s_] := Block[
	{bag = Internal`Bag[], str, i = 1},
	While[
		i <= last && StringQ[
			str = Read[s, Record, NullRecords -> True]
		]
	,
		Internal`StuffBag[bag, str];
		Skip[s, Record, skip-1, NullRecords -> True];
		i += skip;
	];
	Internal`BagPart[bag, All]
];

readbetween[Front[n1_], Front[n2_], skip_][s_] := (
	If[Skip[s, Record, n1-1, NullRecords -> True] =!= Null,
		Return[{}]];
	readbetween[Front[1], Front[n2-n1], skip][s]
);

readbetween[spec1_, spec2_, skip_][s_] := (
	readbetween[spec1, spec2][s][[ ;; ;; skip ]]
);

readbetween[Front[n1_], Front[n2_]][s_] := (
	If[n1 > 1 && Skip[s, Record, n1-1, NullRecords -> True] =!= Null,
		Return[{}]];
	readLines[s, 1+n2-n1]
);

readbetween[Front[n1_], Back[n2_]][s_] := Block[{res},
	If[n1 > 1 && Skip[s, Record, n1-1, NullRecords -> True] =!= Null,
		Return[{}]];
	res = readLines[s, 1000000]; (* this is for display purposes after all *)
	If[n2 > Length[res], {}, Drop[res, -(n2-1)]]
];

readbetween[Back[n1_], Back[n2_]][s_] := (
	findNthLastLine[s, n1];
	readLines[s, (n1 - n2)+1]
);

readLines[s_, n_ ? Positive] := ReadList[s, Record, n, NullRecords -> True];
readLines[_, _] := {};

findNthLastLine[s_, n_] := Block[
	{len, target, spill},
	SetStreamPosition[s, Infinity];
	len = StreamPosition[s];
	target = len - (n+1) * $width;
	While[target > 0,
		SetStreamPosition[s, target];
		If[Skip[s, Record, n+1, NullRecords -> True] === Null,
			SetStreamPosition[s, target];
			Skip[s, Record, NullRecords -> True];
			target = StreamPosition[s];
			Break[]
		];
		target -= 5 * $width;
	];
	SetStreamPosition[s, Max[0, target]];
	spill = ConstantArray[0, 5*n];
	Do[
		spill[[i]] = StreamPosition[s];
		If[Skip[s, Record, NullRecords -> True] =!= Null,
			target = spill[[Max[1,i-n]]]; Break[];
		],
		{i, 2*(n+1)}
	];
	SetStreamPosition[s, target];
];

Snippet::invfile = "Could not read from file ``.";
Snippet::nefile = "File `` does not exist.";
readfile[file_, func_] := Block[{stream},
	res = Check[
		Internal`WithLocalSettings[
			stream = OpenRead[file, BinaryFormat -> True];
			If[FailureQ[stream], Return[$Failed, Block]],
			func[stream],
			Close[stream]
		], $Failed];
	If[FailureQ[res], 
		If[!FileExistsQ[file], 
			fail["nefile", file],
			fail["invfile", file]
		]
	];
	res
];

snipstr[$Failed, ___] := $Failed;

snipstr[str_String, spec_] := Block[{res},
	res = StringTrim[str];
	res = takeChars[res, spec];
	res = InsertLinebreaks[res, $width];
	res = takeLines[res, spec];
	res
];

takeChars[s_, Front[n_]] := If[StringLength[s] <= n*$width, s, StringTake[s, n*$width]];
takeChars[s_, Back[n_]] :=  If[StringLength[s] <= n*$width, s, StringTake[s, -n*$width]];

takeLines[s_String, Front[n_]] := Block[{pos},
	pos = StringPosition[s, "\n", n];
	If[Length[pos] < n, s, 
		StringTake[s, pos[[n,1]]-1]
	]];

takeLines[s_String, Back[n_]] := Block[{pos},
	pos = StringPosition[s, "\n"];
	If[Length[pos] < n, s, 
		StringDrop[s, pos[[-n,1]]]
	]
];

snipstr["", _, _] := "";

snipstr[str_, Front[1], Back[1]] := str;

snipstr[str_, n1_, n2_] := Block[
	{pos, npos},
	pos = linePositions[str, max[n1, n2]];
	StringTake[str, take[pos, Length[pos], n1, n2] + {1,-1}]
];

snipstr[str_, n1_, n2_, d_] := 
	StringJoin[
		Riffle[
			Take[StringSplit[snipstr[str, n1, n2], "\n"], ;; ;; d],
			"\n"
		]
	];

max[Front[n1_], Front[n2_]] := n2;
max[_, _] := Infinity;

take[list_, n_, Front[a_], Front[b_]] :=
	If[a >= n, {1,2}, list[[{a, Min[b+1, n]}]]];

take[list_, n_, Front[a_], Back[b_]] :=
	If[a >= n || b >= n || a > (n - b), {1,2}, 
		list[[{a, -b}]]];

take[list_, n_, Back[a_], Back[b_]] := 
	If[b >= n || a < b, {1,2}, list[[-{Min[a,n]+1, Min[b,n]}]]];

linePositions[str_, n_] := Block[{pos},
	pos = StringPosition[str, "\n", n][[All, 1]];
	Developer`ToList[0, pos, StringLength[str]+1]
];

PackageImport["GeneralUtilities`"]

ReadPartialUTF8String[file_, n_Integer] :=
	Quiet @ Check[
		toUTF @ BinaryReadList[file, "Byte", n], 
		$Failed
	];

toUTF[list_] := 
	Replace[
		list, 
		e_List :> FromCharacterCode[e, "UTF8"]
	];

fail[msg_String, args___] := (
	Message[MessageName[Snippet, msg], args];
	Throw[$Failed];
);
