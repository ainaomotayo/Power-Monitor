Package["TextSearch`"]

PackageImport["GeneralUtilities`"]
PackageImport["Macros`"]

PackageScope["IFileSearch"]

$safeCharacters = (WordCharacter|"_"|":"|$PathnameSeparator);
escape[list_List] := StringJoin[Riffle[escape /@ list, " "]];
escape[str_String] := If[StringMatchQ[str, $safeCharacters..], str,
	"\"" <> StringReplace[str, {"\"" -> "\\\"", "\\" -> "\\\\"}] <> "\""];

IFileSearch[path_, query_, opts_List, rf_] := 
	Catch @ IFileSearch[path, 
		toRegularExpressionString[query] //
		FailUnless[StringQ, FileSearch::badre, query], 
		Replace[opts, {ic_, _, ww_, ea_} :> {ic, False, Replace[ww, Automatic -> False], ea}],
		rf
	];

FileSearch::badre = "Query `` is not compilable to a pure regular expression."

IFileSearch[path_, query_String, {ic_, lq_, ww_, ea_}, rf_] := Module[
	{stream, res, cmd, binpath, binname, escapedQuery, wholeq},
	binpath = $SilverSearcherPath;
	If[FailureQ[binpath], Return[$Failed]];
	binname = FileNameTake[binpath]; 
	If[!$windowsQ, binname = "./" <> binname];
	binpath = DirectoryName[binpath];
	If[StringLength[query] < 1, Return[$Failed]];
	If[query === "", Return[$Failed]];
	escapedQuery = escape[query];
	wholeq = If[escapedQuery =!= query, "", "-w "];
	cmd = StringJoin["!", binname, 
		" -t -0 ", (* search all text files, separate with null byte *)
		Switch[ic, Automatic, "--smart-case ", True, "-i ", _, ""],
		Switch[rf, 
			"Files",        "-l ", 
			"Positions",    "--group --only-matching ",
			"Lines",        "--group --nonumbers ", 
			"Strings",      "--group --nonumbers --only-matching ", 
			"Counts",       "--count ", 
			"Associations", "--group ", 
			"Snippets",     "--group --context --nonumbers ",
			_,              ""
		],
		Switch[ww, True, "-w ", Automatic, wholeq, _, ""],
		Switch[ea, None, "", _List, StringRiffle[ea] <> " ", _String, ea <> " ", _, ""],
		Switch[lq, True, "-Q ", _, ""],
		escapedQuery, " ", escape[path]];
	Internal`WithLocalSettings[
		SetDirectory[binpath];
		stream = OpenRead[cmd];
	,
		res = ReadList[stream, Record, RecordSeparators -> {FromCharacterCode[0]}]
	,
		Close[stream];
		ResetDirectory[];
	];
	res
];


PackageExport["FileSearch"]

Options[FileSearch] = {
	"IgnoreCase" -> True, (* Automatic *)
	"LiteralQuery" -> True,
	"WholeWords" -> Automatic,
	"ExtraArguments" -> None
};

FileSearch::invpath = "`` is not a string or list of strings.";
FileSearch::invquery = "`` is not a string or simple string pattern.";
FileSearch::invform = "`` is not one of ``.";

$validForms = {"Files", "Positions", "Lines", "Strings", "Counts", "Associations", "Snippets"};

FileSearch[path_, query_, rform_String:"Files", OptionsPattern[]] := ConditionalRHS[
	Developer`StringOrStringVectorQ[path], {"invpath", path},
	StringQ[query] || StringPatternQ[query], {"invquery", query},
	MemberQ[$validForms, rform], {"invform", rform, $validForms},	
	processOutput[
		IFileSearch[
			ExpandFileName[path], 
			query, 
			OptionValue[{IgnoreCase, "LiteralQuery", "WholeWords", "ExtraArguments"}],
			rform
		],
		rform
	]
];

processOutput[$Failed, _] := $Failed;

processOutput[result_, "Files"] := result;

processOutput[result_, "Counts"] := 
	Association[Rule @@@ StringSplit[result, ":" ~~ d:DigitCharacter.. :> FromDigits[d]]];

$groupHeaderPattern = (StartOfLine ~~ filename:Except["\n"].. ~~ EndOfString  /; SowBag[filename]);
processOutput[result_, form_] := Module[
	{keys, values},
	If[result === {}, Return[<||>]];
	keys = ReapBag[
		values = Rest @ Switch[form,
		"Positions",
			StringCases[result, {
				$groupHeaderPattern,
				StartOfLine ~~ d:DigitCharacter.. ~~ ":" :> FromDigits[d]
			}],
		"Lines"|"Strings",
			StringCases[StringReplace[result, "\n\n" -> "\n"], {
				$groupHeaderPattern,
				StartOfLine ~~ line:Shortest[___] ~~ Except[EndOfString, EndOfLine] :> line
			}],
		"Snippets",
			StringSplit[
				StringReplace[result, 
					StartOfLine ~~ filename:Except["\n"].. ~~ EndOfString :> 
						(SowBag[filename]; "")
				],
				"--\n"
			],
		"Associations",
			Association /@ StringCases[result, {
				$groupHeaderPattern,
				StartOfLine ~~ d:DigitCharacter.. ~~ ":" ~~ line:Shortest[___] ~~ EndOfLine :> 
					(FromDigits[d] -> line)
			}]
		];
	];		

	If[Length[keys] =!= Length[values], Print[keys, values];$Failed,
		AssociationThread[keys, values]
	]
];


toRegularExpressionString[str_String] := str;

toRegularExpressionString[patt_] := Quiet @ Replace[
	StringPattern`PatternConvert[patt], 
	{{a_String, _, {}, _} :> StringDrop[a, 5], _ -> $Failed}
];


PackageScope["$SilverSearcherPath"]

TextSearch::agmiss = "The search program used to do non-index searches is missing from this installation. Please contact Technical Support.";

$SilverSearcherPath := $SilverSearcherPath = Catch[
	PacletManager`PacletResource["TextSearch", "ag"] // 
	FailUnless[StringQ[#] && FileExistsQ[#]&, TextSearch::agmiss]
];