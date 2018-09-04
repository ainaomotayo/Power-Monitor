(* ::Package:: *)

BeginPackage["DocumentationSearch`", {"ResourceLocator`"}];

Needs["JLink`"]
Needs["HTTPClient`"]

FetchReferencedFunctions::usage="Fetches referenced functions from guide pages.";


DocumentationIndexMerger::usage="DocumentationIndexMerge[...] contains the index merger object.";
NewDocumentationIndexMerger::usage="NewDocumentationIndexMerger[dir] creates an index merger that merges index to output dir.";
MergeDocumentationIndex::usage="MergeDocumentationIndex[index] merges a single index directory into the output.";
MergeDocumentationIndexes::usage="MergeDocumentationIndexes[indexes] merges a list of index directories into the output.";
CloseDocumentationIndexMerger::usage="CloseDocumentationIndexMerger[merger] closes the index merger object.";

DocumentationNotebookIndexer::usage="DocumentationNotebookIndexer[...] contains the notebook indexer object.";
NewDocumentationNotebookIndexer::usage="NewDocumentationNotebookIndexer[directory] creates a new NotebookIndexer.";
CloseDocumentationNotebookIndexer::usage="CloseDocumentationNotebookIndexer[indexer] closes the notebook indexer object."

CreateSpellIndex::usage="CreateSpellIndex[indexDir, spellIndexDir] takes the words from indexDir and creates a spelling index at spellIndexDir.";

AddDocumentationNotebook::usage="AddDocumentationNotebook[indexer, notebook] adds a notebook to the index.";
AddDocumentationDirectory::usage="AddDocumentationDirectory[indexer, directory] adds a directory of notebooks to the index.";

SearchDocumentation::usage="SearchDocumentation[criteria] searches the documentation and returns results based on the search criteria.";
SearchDocumentationMetaData::usage="SearchDocumentationMetaData[] returns valid meta data that can be returned from a search."

DirectHitSearch::usage="DirectHitSearch[criteria] searches the documentation and returns a URI if there is a one specific hit for the given query.";

ExportSearchResults::usage="ExportSearchResults[results, format] exports the search results to the specified format.";

$NumberOfExtraPages::usage="$NumberOfExtraPages sets the number of pages on either side of the current page in the searh results navigation";
$NumberOfExtraPages = 3;

DocumentationIndexes::usage="DocumentationIndexes[] returns the index directories of documentation.";
DocumentationSpellIndexes::usage="DocumentationSpellIndexes[] returns the spell index directories of documentation.";
CloseDocumentationIndex::usage="CloseDocumentationIndex[indexDir] closes the indexDir";

$SearchLanguage

`Information`$Version = "DocumentationSearch Version 1.0.1 (November 2007)";

`Information`$VersionNumber = 1.0;

`Information`$ReleaseNumber = 1;

`Information`$CVSVersion = "$Id: DocumentationSearch.m,v 1.106.2.1 2015/11/17 16:49:43 tgayley Exp $"

Begin["`Private`"];

ExportSearchResults::format="`1` is not a recognized ExportSearchResults format.";
If[!ValueQ[$SearchLanguage], $SearchLanguage = Switch[$Language, "Japanese", "Japanese", "ChineseSimplified", "ChineseSimplified", _, "English"]];
$PackageDir = DirectoryName[$InputFileName];

DocumentationIndexes[] := 
  First /@ ResourcesLocate[ToFileName[{"Documentation", ToString[$SearchLanguage]}, "Index"]]

DocumentationSpellIndexes[] := 
  First /@ ResourcesLocate[ToFileName[{"Documentation", ToString[$SearchLanguage]}, "SpellIndex"]]


If[!MemberQ[DocumentationIndexes[], 
     ToFileName[{$InstallationDirectory, "Documentation", ToString[$SearchLanguage]}, "Index"]], 
  ResourceAdd[ToFileName[{$InstallationDirectory, "Documentation", ToString[$SearchLanguage]}, "Index"], ToFileName[{"Documentation", ToString[$SearchLanguage]}, "Index"]]
];

If[!MemberQ[DocumentationSpellIndexes[], 
     ToFileName[{$InstallationDirectory, "Documentation", "English"}, "SpellIndex"]], 
  ResourceAdd[ToFileName[{$InstallationDirectory, "Documentation", "English"}, "SpellIndex"], ToFileName[{"Documentation", "English"}, "SpellIndex"]]
];



NewDocumentationIndexMerger[directory_String /; FileType[directory] === Directory] := 
JavaBlock[
	Module[{merger},
		InstallJava[];
		AddToClassPath[ToFileName[{DirectoryName[FindFile["DocumentationSearch`"],2], "Java", "Lucene21"}]];
		merger = JavaNew["com.wolfram.documentationsearch.IndexMerger", directory];
		KeepJavaObject[merger];
		DocumentationIndexMerger[merger]
	]
]

MergeDocumentationIndex[DocumentationIndexMerger[merger_?JavaObjectQ], directory_String /; FileType[directory] === Directory] := 
JavaBlock[
	Module[{},
		merger@addIndex[directory];
	]
]

MergeDocumentationIndexes[DocumentationIndexMerger[merger_?JavaObjectQ], directories_List] := 
JavaBlock[
	Module[{},
		merger@addIndexes[directories];
	]
]

CloseDocumentationIndexMerger[DocumentationIndexMerger[merger_?JavaObjectQ]] :=
JavaBlock[
	Module[{},
		merger@close[];
	]
]

Options[NewDocumentationNotebookIndexer] := {
  "Language"->$SearchLanguage,
  (* New option from tgayley; controls whether to use Lucene 2.1 libs for indexing (needed for in-product
     indexes, compatible with CLucene used for in-product searching), or the newer Lucene 3 libs for 
     web search and anywhere else searching is still being done with Java.
  *)
  "InProductIndexFormat"->True
};

NewDocumentationNotebookIndexer[directory_String, opts___Rule] :=
  JavaBlock[
    Module[{indexer, useOpts, lang, oldLucene, fieldBoostFile},
      fieldBoostFile = FileNameJoin[{$PackageDir, "fieldBoost.json"}];
	  spellingsFile = FileNameJoin[{$PackageDir, "misspellings.json"}];
      useOpts  = canonicalOptions[Flatten[{opts}]];
      {lang, oldLucene} = {"Language", "InProductIndexFormat"} /. useOpts /. Options[ NewDocumentationNotebookIndexer ];
      InstallJava[];
      (* This app supports both old- and new-style doc indexes. We add the appropriate subdir to the class path. 
         Old-style indices are used for in-product search, to be compatible with CLucene. This is the default.
      *)
      If[TrueQ[oldLucene],
          (* In the app layout, the Lucene 2.1 libs are in the Java/Lucene21 dir. Here we put them ahead of the 3.x
             libs, which are in the standard location of just the Java dir.
          *)
          AddToClassPath[ToFileName[{DirectoryName[FindFile["DocumentationSearch`"],2], "Java", "Lucene21"}]],
      (* else *)
          AddToClassPath[ToFileName[{DirectoryName[FindFile["DocumentationSearch`"],2], "Java", "Lucene30"}]]
      ];
      Switch[lang, 
			"Japanese", 
				indexer = JavaNew["com.wolfram.documentationsearch.JapaneseDocumentationIndexer", directory],
      		_, 
				indexer = JavaNew["com.wolfram.documentationsearch.DocumentationIndexer", directory, fieldBoostFile, spellingsFile]];
      KeepJavaObject[indexer];
      DocumentationNotebookIndexer[indexer]
    ]
  ]
  
CloseDocumentationNotebookIndexer[DocumentationNotebookIndexer[indexer_?JavaObjectQ]] := 
  indexer@close[];

AddDocumentationNotebook[indexers_List,  notebook_String /; FileType[notebook] === File] :=
JavaBlock[
	Module[{nb = quietGet[notebook], text, doc, taggingRules, metaData, index},
		doc = JavaNew["com.wolfram.documentationsearch.DocumentationNotebook"];
		(*plain text of notebook*)
		Developer`UseFrontEnd[text = Import[notebook, "Plaintext"]];

		(*gather meta data*)
		taggingRules = TaggingRules /. Options[nb] /. {TaggingRules -> {}};
		metaData = "Metadata" /. taggingRules /. {"Metadata" -> {}};
		index = "index" /. metaData;
		
		(*add notebook to index*)
		If[TrueQ[index],
			(
				If[MatchQ[#[[1]], _DocumentationNotebookIndexer] && MatchQ[#[[2]], _Function],
					AddDocumentationNotebook[#[[1]], text, metaData, #[[2]], notebook],
					Print[ "Skipping argument to indexers... not {DocumentationNotebookIndexer, Function} pair"]
				]
			) & /@ indexers,
						
			Print["Skipping " <> notebook];
		]
	]
]

FetchReferencedFunctions[nbgot_] :=
	DeleteDuplicates[
		Flatten[Cases[nbgot,
			Cell[CellGroupData[{Cell[___, "GuideReferenceSection", ___], rest___}, ___], ___] :>
				Cases[{rest},
					TemplateBox[{_, uri_String} /; StringMatchQ[uri, "paclet:ref/*"], "RefLink", ___]
						:> StringReplace[uri, "paclet:ref/" -> ""], Infinity], Infinity]]];
	 
	 
AddDocumentationNotebook[jo:DocumentationNotebookIndexer[indexer_?JavaObjectQ], notebook_String /; FileType[notebook] === File] := 
	AddDocumentationNotebook[DocumentationNotebookIndexer[indexer], notebook, Function[1]];
	
AddDocumentationNotebook[jo:DocumentationNotebookIndexer[indexer_?JavaObjectQ], notebook_String /; FileType[notebook] === File, boostFunc_Function] := 
  JavaBlock[
    Module[{nb = quietGet[notebook], text, doc, taggingRules, metaData, index},
      doc = JavaNew["com.wolfram.documentationsearch.DocumentationNotebook"];
      (* plain text of notebook *)
      Developer`UseFrontEnd[text = Import[notebook, "Plaintext"]];
	  
      (* gather meta data *)
      taggingRules = TaggingRules /. Options[nb] /. {TaggingRules -> {}};
      metaData = "Metadata" /. taggingRules /. {"Metadata" -> {}};   
	  index = "index" /. metaData;   
      (* add notebook to index *)
      If[TrueQ[index],
      	AddDocumentationNotebook[jo, text, metaData, boostFunc, notebook],
      	Print["Skipping " <> notebook];
      ]
]];

AddDocumentationNotebook[DocumentationNotebookIndexer[indexer_?JavaObjectQ], text_String, metaData_List] := 
	AddDocumentationNotebook[DocumentationNotebookIndexer[indexer], text, metaData, Function[1], ""];
	
AddDocumentationNotebook[DocumentationNotebookIndexer[indexer_?JavaObjectQ], text_String, metaData_List, notebook_String] := 
	AddDocumentationNotebook[DocumentationNotebookIndexer[indexer], text, metaData, Function[1], notebook];
	
AddDocumentationNotebook[DocumentationNotebookIndexer[indexer_?JavaObjectQ], text_String, metaData_List, boostFunc_Function, notebook_String] := 
  JavaBlock[
    Module[{ doc, type, context, keywords, name, summary, title, uri, synonyms, status, label, lang},
      
      doc = JavaNew["com.wolfram.documentationsearch.DocumentationNotebook"];

      {type, context, keywords, name, label, summary, title, uri, synonyms, status, lang} = 
        {"type", "context", "keywords", "paclet", "label", "summary", "title", "uri", "synonyms", "status", "language"} 
          /. metaData /. 
            {"type" -> "", "context" -> "", "keywords" -> {}, "paclet" -> "", "label"->"",
             "summary" -> "", "title" -> "", "uri"->"", "synonyms" -> {}, "status" -> "None", "language"->"en"};

	  keywords = Union[Flatten[StringSplit[#] & /@ keywords]];

	  boost = boostFunc[uri];

	  If[
	  StringMatchQ[uri, "guide/*"] && StringLength[notebook] > 0 && FileExistsQ[notebook],(
	    keywords = Union[keywords, FetchReferencedFunctions[quietGet[notebook]]];
      )]
	  
	  doc@setBoost[boost];
      doc@setType[type];
      doc@setContext[context];
      doc@setKeywords[keywords];
      doc@setSynonyms[synonyms];
      doc@setPacletName[name];
      doc@setTitle[title];
      doc@setLabel[label];
      doc@setSummary[summary];
      doc@setContent[ text ];
      doc@setURI[uri];
      doc@setStatus[status];
      doc@setLanguage[lang];
      
      indexer@addNotebook[doc];      
    ]
  ]

AddDocumentationDirectory[indexers_List, directory_String] := 
  Module[{files},
    If[FileType[directory] =!= Directory, 
      Return[];
    ];
    Block[{$rootDirectory = $rootDirectory},     
      If[$rootDirectory === Null, 
        $rootDirectory = directory;
      ];
      
      files = FileNames[ToFileName[directory, "*"]];
      (
        Switch[FileType[#], 
          Directory, 
            Which[
               StringMatchQ[#, "*ExampleData" | "*Examples" | "*RawGuides"],
                  Print["Skipping ", #];
                  Null, 
                True, 
                  AddDocumentationDirectory[indexers, #]
            ],          
          File, 
            AddDocumentationNotebook[indexers, #] 
        ]
      ) & /@ files;
    ];
  ]; 

AddDocumentationDirectory[DocumentationNotebookIndexer[indexer_?JavaObjectQ], directory_String] := 
	AddDocumentationDirectory[DocumentationNotebookIndexer[indexer], directory, Function[1]];
	
AddDocumentationDirectory[DocumentationNotebookIndexer[indexer_?JavaObjectQ], directory_String, boostFunc_Function] := 
  Module[{files},
    If[FileType[directory] =!= Directory, 
      Return[];
    ];
    Block[{$rootDirectory = $rootDirectory},     
      If[$rootDirectory === Null, 
        $rootDirectory = directory;
      ];
      
      files = FileNames[ToFileName[directory, "*"]];
      (
        Switch[FileType[#], 
          Directory, 
            Which[
               StringMatchQ[#, "*ExampleData" | "*Examples" | "*RawGuides"],
                  Print["Skipping ", #];
                  Null, 
                True, 
                  AddDocumentationDirectory[DocumentationNotebookIndexer[indexer], #, boostFunc]
            ],          
          File, 
            AddDocumentationNotebook[DocumentationNotebookIndexer[indexer], #, boostFunc] 
        ]
      ) & /@ files;
    ];
  ]; 
  
  
CreateSpellIndex[indexDir_String, spellIndexDir_String, fields_List] := 
  JavaBlock[
      InstallJava[];
      LoadJavaClass["com.wolfram.documentationsearch.spelling.DidYouMeanIndexer"];
      DidYouMeanIndexer`createSpellIndex[fields, indexDir, spellIndexDir];
  ]
  
CreateSpellIndex[indexDir_String, spellIndexDir_String] := 
  JavaBlock[
      InstallJava[];
      LoadJavaClass["com.wolfram.documentationsearch.spelling.DidYouMeanIndexer"];
      DidYouMeanIndexer`createSpellIndex["text", indexDir, spellIndexDir];
  ]

quietGet[name_String] :=
  Module[{expr},
    Off[Syntax::"newl"];
    expr = Get[name];
    On[Syntax::"newl"];
    expr
  ]; 
    
DirectHitSearch[criteria_String] := 
  DirectHitSearch[DocumentationIndexes[], criteria]

DirectHitSearch[indexDir_String, criteria_String] := 
  DirectHitSearch[{indexDir}, criteria]

DirectHitSearch[indexDir:{__String}, criteria_String] := 
    Module[{startCriteria, limitCriteria, results, matches, systemSymbolMatch, systemFormatMatch, match},
  
        startCriteria = StringCases[criteria, RegularExpression[" start:(\\d+)"] -> "$1"];
        If[Length[startCriteria] > 0, Return[Null]];
    
        limitCriteria = StringCases[criteria, RegularExpression[" limit:(\\d+)"] -> "$1"];
        If[Length[limitCriteria] > 0, Return[Null]];    
    
        results = SearchDocumentation[indexDir, {}, "+(exacttitle:\"" <> criteria <> "\") +type:(Symbol OR Format)", 
                                        "Limit"->3, "MetaData"->{"Title", "Type", "URI"}];
        If[!MatchQ[results, {___Rule}], (* Was an error in the LibraryFunction call; should not occur. *) Return[Null]];
        {matches} = {"Matches"} /. results  /. {"Matches" -> {}};
        If[Length[matches] > 0,
            (* We can get more than one result in two cases: (1) a system symbol and a format have the same name (like "C"), in which
               case we give preference to the symbol; and (2) a user paclet symbol has the same name as a system symbol or format, in
               which case we give prefernece to the system one.
               Note that we always require an exact case match.
            *)
            systemSymbolMatch = Cases[matches, {criteria, "Symbol", _?(StringMatchQ[#, "ref/*"]&)}];
            If[Length[systemSymbolMatch] > 0,
                Return[First[systemSymbolMatch][[{1,3}]]]
            ];
            systemFormatMatch = Cases[matches, {criteria, "Format", _?(StringMatchQ[#, "ref/format/*"]&)}];
            If[Length[systemFormatMatch] > 0,
                Return[First[systemFormatMatch][[{1,3}]]]
            ];
            (* Must be a user paclet symbol. If there is just one result, allow a direct hit, otherwise fail here and fall through to search results page. *)
            If[Length[matches] == 1,
                match = First[matches];
                If[MatchQ[match, {criteria, _, _}],
                    Return[match[[{1,3}]]]
                ]
            ]
        ];
        (* Get here means no appropriate matches found. *)
        Null
    ]


Options[SearchDocumentation] := {
  "MetaData" -> {"Title", "Type", "ShortenedSummary", "URI", "Description", "Context"},
  "Start" -> 1, 
  "Limit" -> 10
};
  
  
CloseDocumentationIndex[indexDir_String]:=
    If[libFunctionsAvailable,
        closeSearcher[indexDir],
    (* else *)
        InstallJava[];
	    LoadJavaClass["com.wolfram.documentationsearch.DocumentationSearcher"];
	    DocumentationSearcher`closeSearcher[indexDir]
	]



SearchDocumentation[criteria_String, opts___Rule] := 
  SearchDocumentation[DocumentationIndexes[], 
    DocumentationSpellIndexes[], criteria, opts]

SearchDocumentation[indexDir_String, criteria_String, opts___Rule] := 
  SearchDocumentation[{indexDir}, DocumentationSpellIndexes[], criteria, opts]

SearchDocumentation[indexDir:{__String}, criteria_String, opts___Rule] := 
  SearchDocumentation[indexDir, DocumentationSpellIndexes[], criteria, opts]

SearchDocumentation[indexDir_String, spellIndexDir_String, criteria_String, opts___Rule] := 
  SearchDocumentation[{indexDir}, {spellIndexDir}, criteria, opts]

SearchDocumentation[indexDirIn:{__String}, spellIndexDirIn:{___String}, criteria_String, opts___Rule] := 
  Module[{useOpts, metaData, start, limit, result, newCriteria = criteria, searchLang = $SearchLanguage, 
  		indexDir = indexDirIn, spellIndexDir = spellIndexDirIn},
    useOpts  = canonicalOptions[Flatten[{opts}]];
    metaData = "MetaData" /. useOpts /. Options[ SearchDocumentation ];
    start    = "Start" /. useOpts /. Options[ SearchDocumentation ];
    limit    = "Limit" /. useOpts /. Options[ SearchDocumentation ];
    
    (* Process start in query *)
    startCriteria = StringCases[criteria, RegularExpression[" start:(\\d+)"] -> "$1"];
    If[Length[startCriteria] > 0, start = ToExpression[First[startCriteria]]];
    newCriteria = StringReplace[criteria, RegularExpression[" start:\\d+"] -> ""];
    
    (* Process limit in query *)
    limitCriteria = StringCases[newCriteria, RegularExpression[" limit:(\\d+)"] -> "$1"];
    If[Length[limitCriteria] > 0, limit = ToExpression[First[limitCriteria]]];
    newCriteria = StringReplace[newCriteria, RegularExpression[" limit:\\d+"] -> ""];
    	
    If[libFunctionsAvailable,
        search[indexDir, spellIndexDir, newCriteria, start, limit, metaData, searchLang] /. HoldPattern["Matches" -> m_] :> ("Matches" -> DeleteDuplicates[m]),
    (* else *)
        JavaBlock[        
            InstallJava[];
            (* If Java is used for searching (this is legacy only), we always use the new-style Java code. *)
            AddToClassPath[ToFileName[{DirectoryName[FindFile["DocumentationSearch`"],2], "Java", "Lucene30"}]];
            Switch[searchLang, 
                    "Japanese", 
                        LoadJavaClass["com.wolfram.documentationsearch.JapaneseDocumentationSearcher"];
                        result = JapaneseDocumentationSearcher`search[indexDir, newCriteria, start, limit],
                    _, 
                        LoadJavaClass["com.wolfram.documentationsearch.DocumentationSearcher"];
                        result = DocumentationSearcher`search[indexDir, spellIndexDir, newCriteria, start, limit]
            ];
            {"Query"->result@getQuery[],
             "ParsedQuery"->result@getParsedQuery[],
             "Start"->result@getStart[], 
             "Limit"->result@getLimit[],
             "SearchTime"->result@getSearchTime[], 
             "TotalMatches"->result@getTotalMatches[],  
             "Suggestions"->result@getSuggestion[],       
             "Matches"->result@getMatchValues[metaData]}
        ]
    ]
     
  ];
  

libFile = FindLibrary["DocSearch"];
libFunctionsAvailable =
    If[StringQ[libFile],
        Quiet[
            search = LibraryFunctionLoad[libFile, "search", LinkObject, LinkObject];
            closeSearcher = LibraryFunctionLoad[libFile, "closeSearcher", LinkObject, LinkObject]
        ];
        search =!= $Failed && closeSearcher =!= $Failed,
    (* else *)
        (* Library could not be found; will quietly fall back to using Java. *)
        False
    ]



SearchDocumentationMetaData[] := {
  "Title", "Summary", "URI", "URL", "Type", "Score", "Explanation", "Synonyms", "Boost",  
  "Keywords", "Context", "PacletName", "ShortenedSummary", "Language", "Description"
};

ExportSearchResults[results_List, "Notebook"] := 
  notebookSearchResult[results]

ExportSearchResults[results_List, format_] :=
  Message[ExportSearchResults::format, format] 


(* string to boxes *)
ItalizeWordsInString[str_String]:=
Module[{res},
  res = StringReplace[str, RegularExpression["(J/Link|\\w+)"] :> italizeWord@"$1"];
  Which[
    Head@res === StringExpression, 
      Flatten@{ List@@res },
    Head@res === String,
      res,
    True,
      $Failed
  ]
];

italizeWord[w_String, o___?OptionQ] :=
Module[{},
  wordList = wordList /. {o} /. Options[italizeWord];
  If[! FreeQ[wordList, w],
    StyleBox[w, FontSlant -> "Italic"], w]
];
Options[italizeWord] = {
  wordList -> {"Mathematica", "J/Link", "MathLink", "DatabaseLink", "NETLink", "MathLM", "MonitorLM", "Combinatorica"}
  };


(* Return notebook expr containing search results *)
notebookSearchResult[s_List]:= 
Module[{header, cells, nbExpr, query, start, limit, suggestions, 
        searchTime, totalMatches, matches, resultInfo={}, resultSearchCountCell=" ",
        suggestionCell=" ", allResultsCount, allWolframSitesLine },
  query        = "Query"        /. s /. {"Query"->""};
  start        = "Start"        /. s /. {"Start"->1};
  limit        = "Limit"        /. s /. {"Limit"->"10"};
  searchTime   = "SearchTime"   /. s /. {"SearchTime"->0};
  totalMatches = "TotalMatches" /. s /. {"TotalMatches"->"Unknown"};
  suggestions  = "Suggestions"  /. s /. {"Suggestions"->Null};
  matches      = "Matches"      /. s /. {"Matches"->{}};
  
  allWolframSitesLine = Cell[TextData[{
      magnifyingGlass[], " ",
      ButtonBox[ StringJoin[localization["Try your search"], " ", localization["on all Wolfram sites"]], 
              BaseStyle->"Hyperlink", 
              ButtonData->{URL["http://search.wolfram.com/?query="<>ExternalService`EncodeString[query]<>"&collection=tryonall"], None}
      ]
  }], "SearchAllSites"];
  Which[
    (* results found *)
    Length[matches] > 0, 
      resultSearchCountCell = 
        Cell[TextData[{
          ToString[start],
          " - ", 
          ToString[start + Length[matches] - 1],  
          " ", localization["of"], " ", 
          ToString[totalMatches], 
          " ", localization["for"], " ", 
          ButtonBox[ToString[query], 
             ButtonStyle -> "Link",
             ButtonData->query]
        }], "SearchCountCell"];
        suggestionCell = 
        If[suggestions =!= Null, 
          Cell[TextData[{
            localization["Did you mean"], ": ", 
            ButtonBox[suggestions, 
             ButtonStyle -> "Link",
             ButtonData->suggestions]
          }], "DidYouMean"], 
          " "
        ];
     ,
    (* no results found *)
    True, 
        resultInfo = {
          (* allWolframSitesLine, *)
          If[suggestions =!= Null, 
            Cell[TextData[{
              localization["Did you mean"], ": ", 
              ButtonBox[suggestions, 
                ButtonStyle -> "Link",
                ButtonData->suggestions]
            }], "DidYouMean"], 
            {}
          ],
          Cell[TextData[{
            warningSign[],
            localization[" Your search"], " - ", query, " - in ", 
            ButtonBox["Documentation Center", 
              BaseStyle->"Hyperlink",
              ButtonData->{URL["http://reference.wolfram.com/language"], None}
              ],
            localization[" did not match any documents"]
          }], "SearchCountCell", FontWeight->"Bold"],
          Cell[TextData[{
            localization["Suggestions"], "\n", 
            localization["\[FilledSmallSquare] Make sure all words are spelled correctly"],"\n", 
            localization["\[FilledSmallSquare] Try different keywords"], "\n", 
            localization["\[FilledSmallSquare] Try more general keywords"]        
          }], "SearchSuggestionsCell"]
        }
  ];
  cells = cellSearchResult/@matches;
  header = 
    Cell[BoxData[GridBox[{
      {
        Cell[TextData[{localization["Search Results"]}], "SearchPageHeading"],
        resultSearchCountCell
      },
      {
        suggestionCell,
        allWolframSitesLine
      }
      }]], "SearchPageHeadingGrid"];
  nbExpr = 
    Notebook[Flatten @ {
      header,
      resultInfo,
      cells,
      searchPageLinkCell[query, totalMatches, start, limit, $NumberOfExtraPages]
    }, 
    StyleDefinitions->FrontEnd`FileName[{"Wolfram"}, "Reference.nb"],
    Saveable->False, 
    WindowTitle->"Search Results: " <> StringTake[query, Min[StringLength[query], 60]]];
  nbExpr
];

notebookSearchResult[f___]:= ($Failed; Message[notebookSearchResult::args, f];)
notebookSearchResult::args = "Incorrect arguments: `1`";   

(* Return a cell expr from a list : *)
(* {"Title", "Type", "Summary", "URI"} *)
cellSearchResult[{title_String, type_String, summary_String, uri_String, description_String, context_String}]:= 
Module[{styledTitle, url=uri},
styledTitle = ItalizeWordsInString @ title;
url = If[StringMatchQ[url, "note/*"], 
         dothtml @ StringJoin["http://reference.wolfram.com/mathematica/", url, ".html" ],
         "paclet:" <> url];

Cell[TextData[Flatten@{
 Cell[
 	If[StringMatchQ[url, ___ ~~ "tutorial/HandsOnFirstLook01" ~~ ___], 
 	TextData[
 		ButtonBox[styledTitle, 
 			BaseStyle->"Link", 
 			ButtonFunction:>(Documentation`HelpLookup["paclet:tutorial/HandsOnFirstLook01", FrontEnd`NotebookCreate[]]& ), 
 			Evaluator->Automatic] 
 			],
 	BoxData[
 		TemplateBox[{Cell[TextData[
 			If[type === "Character Name", 
 				StringReplace[styledTitle, {"\\"->"\\[Backslash]"}], 
 				styledTitle
 				]]],
 				url}, "SearchResultLink", BaseStyle->{"SearchResultTitle"}
 				]
 			]
 		], "SearchResultTitle"],
 " ", StyleBox["(", "SearchResultType"],
 StyleBox[ ItalizeWordsInString @ description , "SearchResultType"],
 StyleBox[")", "SearchResultType"],
 If[StringLength[summary] > 0, 
   "\n",
   ""
 ],
 StyleBox[ ItalizeWordsInString @ summary, "SearchResultSummary"]
}], "SearchResultCell", 
  CellDingbat-> StyleBox["\[FilledSquare]",
    Which[
      type === "Symbol", "SymbolColor",
      type === "Guide", "GuideColor", 
      True, "OtherColor"] ] ] ];

cellSearchResult[f___]:= ($Failed; Message[cellSearchResult::args, f];)
cellSearchResult::args = "Incorrect arguments: `1`";   

searchPageLinkCell[query_String, totalResults_Integer, start_Integer, 
   limit_Integer, numPages_Integer] := 
  Module[{startPage, out = {}, currentPage = Ceiling[(start)/limit], 
    totalPages = Ceiling[totalResults/limit], prevnext={} },
   
   (*Check boundaries*)
   If[start > totalResults || totalResults < 0 || limit < 1 || numPages < 1 || totalResults <= limit, 
     Return[{}]
   ];
   
   (*determine starting page number*)
   If[Quotient[currentPage-1, numPages] === 0,    
     startPage = 1, 
     startPage = currentPage - numPages
   ];
   
   (*create range of pages*)
   For[i = startPage, i < currentPage + numPages + 1 && i <= totalPages, i++,
    If[i == currentPage,
      AppendTo[out, StyleBox[" "<>ToString[i]<>" ", FontColor -> RGBColor[0, 0, 1], FontWeight->"Bold"]];, 
      AppendTo[out, 
        ButtonBox[" "<>ToString[i]<>" ", BaseStyle -> "Link", 
         ButtonData -> query <> " start:" <> ToString[(i - 1)*limit + 1] <> " limit:" <> ToString[limit]]];
      ];
    ];
    
   (*add separator*)
   out = Riffle[out, "|"];
   
   (* Add first and last page *)
   If[startPage =!= 1, 
    out = Flatten@{ButtonBox["1", BaseStyle -> "Link", 
        ButtonData -> query <> " start:1 limit:" <> ToString[limit]], " ... ", out}];
   If[currentPage + numPages + 1 <= totalPages, 
    out = Flatten@{out, " ... ", 
       ButtonBox[ToString[totalPages], BaseStyle -> "Link", 
        ButtonData -> query <> " start:" <> ToString[(totalPages-1)*limit + 1] <> " limit:" <> ToString[limit]]}];
   
   (*add prev and next *)
   If[currentPage =!= 1, 
    prevnext = Flatten@{ButtonBox[localization["\[LeftGuillemet] PREVIOUS"], BaseStyle -> "Link", 
        ButtonData -> query <> " start:" <> ToString[(currentPage - 2) * limit + 1] <> " limit:" <> ToString[limit]], prevnext}];
   If[currentPage < totalPages, 
    prevnext = Flatten@{prevnext,  
       ButtonBox[localization["NEXT \[RightGuillemet]"], BaseStyle -> "Link", 
        ButtonData -> query <> " start:" <> ToString[currentPage * limit + 1] <> " limit:" <> ToString[limit]]}];
   (*add separator*)
   prevnext = If[Length@prevnext > 1, Riffle[prevnext, " | "], prevnext];
   
   
   (*return cell expr*)
    Cell[BoxData[GridBox[{
      {
        Cell[TextData[Flatten@{prevnext}], "SearchResultPageLinks"],
        Cell[TextData[Flatten@{out}], "SearchResultPageLinks"]
      }
      }]], "SearchResultPageLinksGrid"]
 ];
 
(* html anchors need to be file.html# xxx, this handles old bug where they could be file # xxx.html *)
dothtml[s_String] :=
  If[StringMatchQ[s, "*#*"],
    (StringTake[s, #] <> ".html" <> StringDrop[s, #])&[StringPosition[s, "#"][[-1, -1]] - 1],
    s];


SetAttributes[ canonicalOptions, {Listable}];
canonicalOptions[name_Symbol -> val_] := SymbolName[name] -> val;
canonicalOptions[expr___] := expr;


(* Localization of strings *)
localization[c___]:= c;

localization["Search Results"]:= localization["Search Results", ToString[$SearchLanguage] ]
localization["Search Results", language_String] :=
  Switch[language, 
    "Japanese", "\:691c\:7d22\:7d50\:679c",
    "ChineseSimplified", "\:641c\:7d22\:7ed3\:679c",
     _, "Search Results" ];
localization["Try your search"]:= localization["Try your search", ToString[$SearchLanguage] ]
localization["Try your search", language_String] :=
  Switch[language, 
    "Japanese", "\:691c\:7d22\:5bfe\:8c61\:ff1a",
    "ChineseSimplified", "\:5c1d\:8bd5\:60a8\:7684\:641c\:7d22\:ff1a",
     _, "Try your search" ];
localization["on all Wolfram sites"]:= localization["on all Wolfram sites", ToString[$SearchLanguage] ]
localization["on all Wolfram sites", language_String] :=
  Switch[language, 
    "Japanese", "\:3059\:3079\:3066\:306eWolfram\:30b5\:30a4\:30c8",
    "ChineseSimplified", "\:5728\:6240\:6709\:7684Wolfram \:7ad9\:70b9",
     _, "on all Wolfram sites" ];
localization["matches"]:= localization["matches", ToString[$SearchLanguage] ]
localization["matches", language_String] :=
  Switch[language, 
    "Japanese", "\:4ef6",
    "ChineseSimplified", "\:5339\:914d",
     _, "matches" ];
localization["Results"]:= localization["Results", ToString[$SearchLanguage] ]
localization["Results", language_String] :=
  Switch[language, 
    "Japanese", "\:7d50\:679c",
    "ChineseSimplified", "\:7ed3\:679c",
     _, "Results" ];
localization["of"]:= localization["of", ToString[$SearchLanguage] ]
localization["of", language_String] :=
  Switch[language, 
    "Japanese", "/",
    "ChineseSimplified", "/",
     _, "of" ];
localization["for"]:= localization["for", ToString[$SearchLanguage] ]
localization["for", language_String] :=
  Switch[language, 
    "Japanese", "\:691c\:7d22\:ff1a",
    "ChineseSimplified", "\:4f5c\:4e3a",
     _, "for" ];
localization["Did you mean"]:= localization["Did you mean", ToString[$SearchLanguage] ]
localization["Did you mean", language_String] :=
  Switch[language, 
    "Japanese", "\:3053\:306e\:5358\:8a9e\:3067\:3059\:304b\:ff1a",
    "ChineseSimplified", "\:60a8\:8ba4\:4e3a",
     _, "Did you mean" ];
localization["Your search"]:= localization["Your search", ToString[$SearchLanguage] ]
localization["Your search", language_String] :=
  Switch[language, 
    "Japanese", "\:691c\:7d22\:3055\:308c\:305f\:5358\:8a9e",
    "ChineseSimplified", "\:60a8\:7684\:641c\:7d22",
     _, "Your search" ];
localization["did not match any documents"]:= localization["did not match any documents", ToString[$SearchLanguage] ]
localization["did not match any documents", language_String] :=
  Switch[language, 
    "Japanese", "\:30de\:30c3\:30c1\:3057\:307e\:305b\:3093\:3067\:3057\:305f\:ff0e",
    "ChineseSimplified", "\:4e0d\:5339\:914d\:4efb\:4f55\:6587\:6863",
     _, "did not match any documents" ];
localization["Suggestions"]:= localization["Suggestions", ToString[$SearchLanguage] ]
localization["Suggestions", language_String] :=
  Switch[language, 
    "Japanese", "\:63d0\:6848",
    "ChineseSimplified", "\:5efa\:8bae",
     _, "SUGGESTIONS" ];
localization["Make sure all words are spelled correctly"]:= localization["Make sure all words are spelled correctly", ToString[$SearchLanguage] ]
localization["Make sure all words are spelled correctly", language_String] :=
  Switch[language, 
    "Japanese", "\:30b9\:30da\:30eb\:306e\:9593\:9055\:3044\:306f\:3042\:308a\:307e\:305b\:3093\:304b\:ff1f",
    "ChineseSimplified", "\:786e\:5b9a\:6240\:6709\:5b57\:7684\:62fc\:6cd5\:90fd\:6b63\:786e",
     _, "Make sure all words are spelled correctly" ];
localization["Try different keywords"]:= localization["Try different keywords", ToString[$SearchLanguage] ]
localization["Try different keywords", language_String] :=
  Switch[language, 
    "Japanese", "\:4ed6\:306e\:30ad\:30fc\:30ef\:30fc\:30c9\:3092\:304a\:8a66\:3057\:304f\:3060\:3055\:3044\:ff0e",
    "ChineseSimplified", "\:5c1d\:8bd5\:4e0d\:540c\:7684\:5173\:952e\:5b57",
     _, "Try different keywords" ];
localization["Try more general keywords"]:= localization["Try more general keywords", ToString[$SearchLanguage] ]
localization["Try more general keywords", language_String] :=
  Switch[language, 
    "Japanese", "\:4e00\:822c\:7684\:306a\:30ad\:30fc\:30ef\:30fc\:30c9\:3092\:304a\:8a66\:3057\:304f\:3060\:3055\:3044\:ff0e",
    "ChineseSimplified", "\:5c1d\:8bd5\:66f4\:591a\:901a\:7528\:7684\:5173\:952e\:5b57",
     _, "Try more general keywords" ];
localization["NEXT \[RightGuillemet]"]:= localization["NEXT \[RightGuillemet]", ToString[$SearchLanguage] ]
localization["NEXT \[RightGuillemet]", language_String] :=
  Switch[language, 
    "Japanese", "\:6b21\:3078",
    "ChineseSimplified", "\:4e0b\:4e00\:9875",
     _, "NEXT \[RightGuillemet]" ];
localization["\[LeftGuillemet] PREVIOUS"]:= localization["\[LeftGuillemet] PREVIOUS", ToString[$SearchLanguage] ]
localization["\[LeftGuillemet] PREVIOUS", language_String] :=
  Switch[language, 
    "Japanese", "\:623b\:308b",
    "ChineseSimplified", "\:4e0a\:4e00\:9875",
     _, "\[LeftGuillemet] PREVIOUS" ];


magnifyingGlass[]:=
Cell[GraphicsData["CompressedBitmap", "\<\
eJxVUk1vEkEY3rWlB436D8gSk8af0JignjQk8AdIOJDGpE1qlLaQoMA/4KZI
Qhq0bWI0XEwP6k2Kl2Js+IZtwZYFdoMW6IciSWUf3xm2tJ1kJrMz+7zPxzsO
9/Lco8fu5flZt/Rw0f10bn52SXrwZJGOJkRBEG7SvCEJbA/aGsskaAgTtAxh
DP4FYRK93ilarQ5kuYVarS2i3f4jCFeMW13/h/HQMQKZ0O2cQmkdoCQryOWq
CARCgohwOExsU8bfIypdP0OfjmpOodsdcEYGzmZrCISCMJvNIiySmfCmMY6x
kBpoWo/katoJgRXlAJWKgkymimCQA2Gx3BJ5bfLOiAyGI+zsqCiX6yiVftBU
UKCZz+8hl6mNwZJkZmLPCzBWBmbA7e0KXq++gd1ux/T0bdy9f4+5ZH4JLGFm
5g53SjGdFbhGkolZ1jg4GHwOl8uFVy8jeP/uLYKBZ8QocWaHwyFMjlGadsT1
JRIJ+LxLKBaL+PY9TdV1bG1tIRaLkVEL4vH4RZT6G/nSHnw+H76mkmg0GtTJ
XeSzORKlQ1VV7tRqtY5R1zkXs7ewsIDDbo/yOkSzqaC6s4tCoUD7JlZWVjjf
qBm68SJYQKwlstxANBqloFqcp9/v41f7J53LSKfTiEQisNls/M6ocB4QS1hV
j/Hx8ydsbn7BcDg0igxQr9dJWhlOpxNra2sjWvEi/ipPnHepeczjSiaT2N9X
MBj0kUql4PF4EAqFTGPC0eMFOp2/pP4EGxsfSOEL+P1+eL1erK+vX/Z5iVD8
Dwm2cTA=\
\>"], "Graphics",
 ImageSize->{15, 14},
 ImageMargins->3];

warningSign[]:=
Cell[GraphicsData["CompressedBitmap", "\<\
eJxVUstqIkEU7ba1e2ZgZn7Br5lZzReoCwkDCTiomex8g67cuApuRHcSCIK4
9IGIiogvFHwgMS4ERQJBRI2a8UzdshVT0HVvVZ9zz7lV9ct8d/37j/nu5sqs
/3lrtl3fXP3V/7Desi1JFAThG/u+6wXKwdLTJGG322GxWODl5YWtZrOZoGA6
nVJ2/ubzOV5fX7FerwUR7+/vjPsVy+WSIweDAVqtFprNJhqNxjnW63W+3+v1
MB6PqQJj7/d7xtah2+0iHA7D6/XC4/HA7XYLGrhcLkGG0+mkjPb4/0AggFwu
J2K73TKyhGg0ilQqhXK5zFbFYpHNpVKJMnXFZhE+n08kacbRIpPJIBKJ4Onp
Cc/PzxqMRiMG1Ol0MJlMfMX3ROTzeRF2u13EarViXBlvb2+wWq2oVCqcz2AU
tWqN4XCIYDCIWq3GOjwcDlyQRRQKBYRCIQIfBRXIsgyj0XiqgXQ6jfv7e04T
wQYn09hsNrDZbORHQ1Bm5JJMqn6/H+12W3PkHcMXVKvVkypHXrLo2GKxGMl9
YH2me4XFYkE2mz2zDAYD+v0+nSR3wLFqi5KqRndMN0R+FEXhSo+Pj7wWnQEf
B6i9HVkK/0M+yA/57HQ6cDgcmEwmJxXgn0rSqiQCUstk6eHhAYlE4lid+ZEu
ofK5m3g8zt8SvVQG06on/OnE+xDpkSeTSXr00rlT8eSGeOJ/ERVL1Q==\
\>"], "Graphics",
 ImageSize->{14, 15},
 ImageMargins->0];

(* English symbols *)
englishSymbols = 
{"abort", "above", "abs", "accumulate", "accuracy", "active", \
"after", "alias", "alignment", "all", "alpha", "alternatives", \
"analytic", "and", "animate", "animator", "annotation", "apart", \
"appearance", "append", "application", "apply", "array", "arrow", \
"arrowheads", "assuming", "assumptions", "attributes", "automatic", \
"axes", "axis", "back", "background", "backslash", "backward", \
"band", "baseline", "because", "beep", "before", "begin", "below", \
"beta", "binary", "binomial", "bit", "black", "blank", "blend", \
"block", "blue", "bold", "bookmarks", "bottom", "bounds", "box", \
"boxed", "boxes", "break", "brown", "button", "byte", "cancel", \
"cap", "cases", "catch", "ceiling", "cell", "center", "character", \
"characters", "check", "chop", "circle", "clear", "clip", "clock", \
"close", "closed", "coefficient", "collect", "colon", "column", \
"commonest", "compile", "compiled", "complement", "complex", \
"complexes", "compose", "composition", "compress", "condition", \
"congruent", "conjugate", "connect", "constant", "constants", \
"context", "contexts", "continuation", "continue", "contours", \
"copyable", "correlation", "cosh", "cot", "count", "covariance", \
"cross", "cuboid", "cup", "cyan", "cylinder", "darker", "dashed", \
"dashing", "date", "debug", "decimal", "decompose", "decrement", \
"default", "defer", "definition", "degree", "deletable", "delete", \
"delimiter", "delimiters", "denominator", "deploy", "deployed", \
"depth", "derivative", "diagonal", "dialog", "diamond", \
"differences", "dimensions", "direction", "directive", "directory", \
"discriminant", "disk", "dispatch", "display", "distribute", \
"divide", "dividers", "divisible", "divisors", "do", "document", \
"dot", "dotted", "down", "drop", "dump", "dynamic", "edit", \
"editable", "eigenvalues", "element", "eliminate", "empty", \
"enabled", "encode", "end", "enter", "environment", "equal", \
"equilibrium", "evaluate", "evaluated", "evaluator", "except", \
"exclusions", "exists", "exit", "expand", "exponent", "export", \
"expression", "extension", "extract", "factor", "factorial", "fail", \
"false", "file", "filling", "find", "first", "fit", "fits", "flat", \
"flatten", "floor", "fold", "font", "for", "format", "forward", \
"frame", "framed", "front", "full", "function", "gamma", "general", \
"generic", "get", "give", "glow", "gradient", "graphics", "gray", \
"greater", "green", "grid", "hash", "head", "heads", "hessian", \
"hold", "holdall", "homepage", "horizontal", "hue", "hyperlink", \
"hyphenation", "identity", "if", "implies", "import", "in", \
"increment", "indent", "indeterminate", "inequality", "infinity", \
"infix", "information", "inherited", "initialization", "inner", \
"input", "insert", "inset", "install", "integer", "integers", \
"integral", "integrate", "interactive", "interlaced", \
"interpolation", "interpretation", "interrupt", "intersection", \
"interval", "inverse", "invisible", "italic", "item", "join", \
"joined", "label", "labeled", "language", "large", "larger", "last", \
"launch", "left", "length", "less", "level", "lexicographic", \
"lighter", "lighting", "limit", "line", "links", "list", "listen", \
"literal", "locked", "log", "longest", "magenta", "magnification", \
"magnify", "manipulate", "manipulator", "manual", "map", "mat", \
"maximize", "mean", "median", "medium", "mesh", "message", \
"messages", "metacharacters", "method", "minimize", "minors", \
"minus", "missing", "mod", "modal", "mode", "modular", "module", \
"modulus", "momentary", "monitor", "most", "multiplicity", "names", \
"nearest", "needs", "negative", "nest", "next", "none", "nor", \
"norm", "normal", "normalize", "not", "notebook", "notebooks", \
"null", "number", "numerator", "off", "offset", "on", "opacity", \
"open", "opener", "operate", "option", "optional", "options", "or", \
"orange", "order", "ordering", "orderless", "out", "outer", "over", \
"overflow", "overlaps", "pane", "panel", "paneled", "parameter", \
"parenthesize", "part", "partition", "path", "pattern", "pause", \
"permutations", "pi", "pick", "piecewise", "pink", "pivoting", \
"placeholder", "plain", "play", "plot", "plus", "ply", "point", \
"polygamma", "polygon", "polynomials", "position", "positive", \
"postscript", "power", "ppm", "precedence", "precedes", "precision", \
"prefix", "previous", "prime", "primes", "print", "product", \
"projection", "proportion", "proportional", "protect", "protected", \
"pseudoinverse", "purple", "put", "quartics", "quartiles", "quiet", \
"quit", "quotient", "random", "range", "raster", "rational", \
"rationalize", "rationals", "raw", "re", "read", "real", "reap", \
"record", "rectangle", "red", "reduce", "refine", "refresh", \
"reinstall", "release", "remove", "removed", "repeated", "replace", \
"rescale", "residue", "resolve", "rest", "resultant", "return", \
"reverse", "rib", "riffle", "right", "root", "roots", "rotate", \
"round", "row", "rule", "run", "save", "scale", "scaled", "scan", \
"select", "selectable", "selection", "sequence", "series", "set", \
"setbacks", "setter", "setting", "shading", "shallow", "share", \
"short", "shortest", "show", "sign", "signature", "simplify", "sin", \
"skeleton", "skewness", "skip", "slider", "slot", "small", "smaller", \
"socket", "solve", "sort", "sorted", "sound", "sow", "space", \
"spacer", "spacings", "span", "sphere", "splice", "split", "square", \
"stack", "standardize", "star", "streams", "string", "stub", "style", \
"subscript", "subset", "subsets", "subtract", "succeeds", "sum", \
"superscript", "superstar", "switch", "symbol", "syntax", "tab", \
"table", "take", "tally", "tan", "tar", "temporary", "text", \
"therefore", "thick", "thickness", "thin", "thread", "through", \
"throw", "ticks", "tiff", "tilde", "times", "timezone", "timing", \
"tiny", "together", "toggle", "toggler", "tolerance", "top", "total", \
"trace", "translate", "transpose", "trig", "trigger", "true", "tube", \
"underflow", "underlined", "unequal", "unevaluated", "union", \
"unique", "unitize", "unset", "up", "update", "upset", "using", \
"value", "variables", "variance", "verbatim", "verbose", "version", \
"vertical", "viewpoint", "visible", "wedge", "which", "while", \
"white", "window", "with", "word", "write", "xor", "yellow", "zeta", \
"zip"};

End[];
 
EndPackage[];
`Information`$CVSVersion = "$Id: DocumentationSearch.m,v 1.106.2.1 2015/11/17 16:49:43 tgayley Exp $"
