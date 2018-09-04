 (* Mathematica Package *)

(* :Title: NotebookTemplating.m *)

(* :Authors:
    Andrew Hunt, andy@wolfram.com
    Adam Berry, adamb@wolfram.com
    Fang Liu, fangl@wolfram.com
    Nick Lariviere, nickl@wolfram.com
*)

(* :Package Version: 0.50 *)

(* :Mathematica Version: 10.0 *)
                     
(* :Copyright: (c) 2010, Wolfram Research, Inc. All rights reserved. *)

(* :Requirements: *)

(* :Discussion:
  
*)


BeginPackage["NotebookTemplating`"]

(* Main public functions *)

CreateTemplateNotebook::usage = "CreateTemplateNotebook[] creates a new notebook template, CreateNotebookTemplate[nbobj] converts nbobj into a notebook template."
TemplateNotebookQ::usage = "TemplateNotebookQ[nb] returns True if nb is a notebook template."
ClearTemplateNotebook::usage = "ClearTemplateNotebook[nb] clears the data from a notebook template."

NotebookTemplating`NotebookTemplateSlot;
NotebookTemplating`NotebookTemplateExpression;

(*HeadlessNotebookEvaluate::usage = "Evaluates the passed notebook expression, returning a new notebook expression";*)

(*ReapedHeadlessNotebookEvaluate::usage = "Evaluates the passed notebook expression, returning a) a new notebook expression, and b)\
the list of evaluation results." *)

(*ReportGenerateExpression::usage = "ReportGenerateExpression  "*)

Begin["`Private`"]

Needs["NotebookTemplating`Utilities`" ]
Needs["NotebookTemplating`Authoring`" ]

SetAttributes[NotebookTemplating`NotebookTemplateExpression, HoldFirst];

prot=Unprotect[ System`NotebookTemplate]

NotebookTemplate[ string_String] :=
    Module[ {file},
        file = FindFile[ string];
        NotebookTemplate[ NotebookOpen[file, Visible -> False]] /; StringQ[file] && FileType[file] === File
    ]
    

NotebookTemplate[ nb_Notebook] :=
    NotebookTemplate[ CreateDocument[nb]]
    
NotebookTemplate[ nbObj_NotebookObject] :=
    TemplateObject[ nbObj]
    
Map[ Protect, prot]

    
System`GenerateDocument[ template : (_?(StringQ[#] && # != "" &) | _Notebook | _NotebookObject), args_Association, opts:OptionsPattern[]] :=
	System`GenerateDocument[template, {args}, "", opts]
	
System`GenerateDocument[ template : (_?(StringQ[#] && # != "" &) | _Notebook | _NotebookObject), args_List, opts:OptionsPattern[]] :=
	System`GenerateDocument[template, args, "", opts]
 
System`GenerateDocument[ template : (_?(StringQ[#] && # != "" &) | _Notebook | _NotebookObject), opts:OptionsPattern[]] :=
	System`GenerateDocument[template, {}, "", opts]
	
System`GenerateDocument[ template : (_?(StringQ[#] && # != "" &) | _Notebook | _NotebookObject), outFile_String, opts:OptionsPattern[]] :=
	System`GenerateDocument[template, {}, outFile, opts]	

System`GenerateDocument[ template_NotebookObject, rules_Association, outFile_, opts:OptionsPattern[]]:=
	System`GenerateDocument[ template, {rules}, outFile, opts]
		
System`GenerateDocument[ template_NotebookObject, rules_List, outFile_, opts:OptionsPattern[]]:=
    Module[ {nbExpr},
    	nbExpr = NotebookGet[ template];
    	If[ Head[nbExpr] === Notebook,
    		System`GenerateDocument[nbExpr, rules, outFile, opts],
    		Message[ GenerateDocument::tlvalid, template];
    		$Failed]
    ]

	
System`GenerateDocument[ template_String, rules:(_List|_Association), outFile_, opts:OptionsPattern[]] :=
    If[ Or[FileExistsQ[template], !MatchQ[FindFile[template], $Failed]],
        System`GenerateDocument[ Import[ template], rules, outFile, opts],
        Message[ GenerateDocument::tlnotavail, template];
        Return[$Failed]
    ]

(* 
    Run report from file 
    in: ReportGenerate[outfile, template, {list of rules}]
    out: file
*)
System`GenerateDocument[templateFile_Notebook, data_Association, outFile_String, opts:OptionsPattern[]]:=
	System`GenerateDocument[templateFile, {data}, outFile, opts]

System`GenerateDocument[template_Notebook, data_List, outFile_String, opts:OptionsPattern[]] :=
    GenerateDocumentDriver[template, data, outFile, opts]

System`GenerateDocument[x___ /; (Message[System`GenerateDocument::argb,System`GenerateDocument,Length[{x}],1,3]/;False)] :=
   {}

System`GenerateDocument::ftmp= "A problem was encountered in saving a temporary file."
System`GenerateDocument::unavail= "The template slot `1` cannot be filled from the arguments. "
System`GenerateDocument::tlnotavail = "The template `1` is not available."
System`GenerateDocument::tlvalid = "The template `1` is not a valid NotebookObject."


Options[GenerateDocumentDriver] :={ 
    "PostProcess" -> True,
    "HeadlessMode" -> False, 
    "ProgressIndicator"->True,
    "RetainAsTemplate" -> False
}

GenerateDocumentDriver[templateFile_Notebook, data_List, outFile_String, opts:OptionsPattern[]] :=
    Module[ {nb,postProcess, replacedNotebook, evaluatedNotebook, feOpen, nbRes,dir,headless, progressIndicator, rat},        	
    Catch[
    	Quiet[
            postProcess =  OptionValue["PostProcess"];
            headless = OptionValue["HeadlessMode"];
            rat = OptionValue["RetainAsTemplate"];
            progressIndicator = OptionValue["ProgressIndicator"], OptionValue::nodef];
                
        feOpen = SameQ[ Head[ $FrontEnd], FrontEndObject];
        If[ (!feOpen) && (outFile === ""), Message[ FrontEndObject::notavail];Return[$Failed] ];
        nb = If[ MatchQ[templateFile,_Notebook],
                 templateFile,
                 Import[templateFile]
             ];
        replacedNotebook = ComputeEvaluationMarker[nb]; (*evaluate TemplateExpressions*)
        replacedNotebook = ReportGenerateExpression[replacedNotebook, data, "RetainAsTemplate" -> rat];
        evaluatedNotebook = If [postProcess,
        	postProcessReplacedNotebook[replacedNotebook,headless, rat, progressIndicator, nb](*replacedNotebook*),
        	replacedNotebook] ;  
                  
        If[ outFile === ""
        	
        	, 
        	If[!headless,
        	nbRes = ProcessWithFrontEnd[ NotebookPut[ evaluatedNotebook] ];
        	nbRes,
        	nbRes = evaluatedNotebook
        	]
        	,   	
        	ProcessWithFrontEnd[
        		dir=DirectoryName[ExpandFileName[outFile]];
        		If[!DirectoryQ[dir],CreateDirectory[dir]]
        		];
        	
        	Export[ outFile, evaluatedNotebook, If[ToLowerCase@FileExtension[outFile]==="cdf", "CDF", "NB"]];
        	If[!headless,
        	nbRes = FindFile[ outFile];
        	nbRes = ProcessWithFrontEnd[ NotebookOpen[ nbRes] ]
        	,
        	nbRes = FindFile[ outFile];
        	]
        ];
        nbRes
    	]
    ]




(* 
    Run report from notebook expression
    in: ReportGenerateExpression[template, {list of rules}]
    out: result expression
*)
ReportGenerateExpression[template_String, data_List:{}, opts___?OptionQ] :=
    ReportGenerateExpression[ ImportReportTemplate[template], data, opts]

ReportGenerateExpression[template_Notebook, data_List:{}, opts___?OptionQ] :=
    Module[ {temp, rat},
        rat = TrueQ["RetainAsTemplate" /. {opts}];
        temp = updateTemplateOptions[template, "NotebookTemplate" -> rat];
        blockVariableReplacement[temp, data, {}]
    ]

postProcessReplacedNotebook[replacedNotebook_Notebook, headless_, rat_, progressIndicator_, nb_Notebook] := 
    Module[ {res, tmp, tempFile, nbMenuOpts,oldOutputFormat},
        If[!headless && CloudSystem`$CloudNotebooks =!= True, 
            UsingFrontEnd[
                If[progressIndicator,tmp = PrintTemporary[ProgressIndicator[Dynamic[Clock[Infinity]], Indeterminate]]];
                nbMenuOpts = NotebooksMenu /. Options[$FrontEnd, NotebooksMenu];
 	              res = taggingInputCells[replacedNotebook, rat]; 	              
 	              tempFile = FileNameJoin[{$TemporaryDirectory, ToString[AbsoluteTime[DateString[]]] <> "_MathematicaReport.nb"}];
 	              Export[tempFile, res];
 	              If[Not[FileExistsQ[tempFile]], Message[GenerateDocument::ftmp];Throw[$Failed, "reportTag"]];
 	              oldOutputFormat = Options[$Output, FormatType];
 	              (*Added so that GenerateDocument works with standalone kernel. Bug 298488*)
 	              SetOptions[$Output, FormatType -> StandardForm];
 	              NotebookEvaluate[tempFile, InsertResults -> True];
 	              SetOptions[$Output, oldOutputFormat];
 	              res = Import[tempFile];
 	              DeleteFile[tempFile];
 	            SetOptions[$FrontEnd, NotebooksMenu->nbMenuOpts];  
 	              If[progressIndicator,Quiet[NotebookDelete[tmp]]]];
 	              ,  (* else headless *)
            res = taggingInputCells[replacedNotebook, rat];
            res = ReleaseHold[HeadlessNotebookEvaluate[res]]
        ];
        displayInputCells[res]
    ]



(*Input/Code cells: multiple evaluation within one cell (not compound expression) *)
HeadlessNotebookEvaluate[
  Cell[BoxData[a_List], style_ /; MemberQ[evaluationStyle, style], 
   c___]] := 
 Module[{resultExp, resultBox, displayForm, content}, 
  resultExp = DeleteCases[ToExpression /@ a, Null];
  Sow[resultExp, "Unused?"];
  resultBox = 
   With[{res = #}, 
      If[MemberQ[Head[#], displayForm], content = res[[1]];
       FormBox[MakeBoxes[content, StandardForm], Head[#]], 
       MakeBoxes[res, StandardForm]]] & /@ resultExp;
  Cell[CellGroupData[{Cell[BoxData[a], style, c], 
     Sequence @@ (Cell[BoxData[#], "Output"] & /@ resultBox)}, Open]]]

(*Input/Code cells, with Evaluatable can be False/True*)
HeadlessNotebookEvaluate[
  Cell[BoxData[a_], style_ /; MemberQ[evaluationStyle, style], c___, 
   Evaluatable -> False, d___]] :=
    Cell[BoxData[a], style, c, Evaluatable -> False, d]

HeadlessNotebookEvaluate[
  Cell[BoxData[a_], style_ /; MemberQ[evaluationStyle, style], 
   c___]] :=
    Module[ {resultExp, resultBox, displayForm, content},
        resultExp = ToExpression[a];
		Sow[resultExp, "Interesting"];
        displayForm = Head[resultExp];
        resultBox = 
         With[ {res = resultExp},
             If[ MemberQ[displayForms, displayForm],
                 content = res[[1]];
                 FormBox[MakeBoxes[content, StandardForm], displayForm],
                 MakeBoxes[res, StandardForm]
             ]
         ];
        If[MatchQ[resultBox, "Null"], 
        	Cell[BoxData[a], style, c],
 			Cell[CellGroupData[{Cell[BoxData[a], style, c], 
    			Cell[BoxData[resultBox], "Output"]}, Open]]]
    ]

HeadlessNotebookEvaluate[
  Cell[CellGroupData[{Cell[BoxData[a_], 
      style_ /; MemberQ[evaluationStyle, style], b___, 
      Evaluatable -> False, c___], Cell[BoxData[d_], "Output", f___]},
     g___]]] :=
    Cell[CellGroupData[{Cell[BoxData[a], style, b, Evaluatable -> False, 
        c], Cell[BoxData[d], "Output", f]}, g]]

HeadlessNotebookEvaluate[
  Cell[CellGroupData[{Cell[BoxData[a_], 
      style_ /; MemberQ[evaluationStyle, style], c___], 
     Cell[BoxData[d_], "Output", f___]}, g___]]] :=
    Module[ {resultExp, resultBox, displayForm},
        resultExp = ToExpression[a];
		Sow[resultExp, "Interesting"];
        displayForm = Head[resultExp];
        resultBox = 
         With[ {res = resultExp},
             If[ MemberQ[displayForms, displayForm],
                 FormBox[MakeBoxes[res, StandardForm], displayForm],
                 MakeBoxes[res, StandardForm]
             ]
         ];
        Cell[CellGroupData[{Cell[BoxData[a], style, c], 
           Cell[BoxData[resultBox], "Output", f]}, g]]
    ]

HeadlessNotebookEvaluate[
  Cell[CellGroupData[{Cell[BoxData[a_], 
      style_ /; MemberQ[evaluationStyle, style], b___, 
      Evaluatable -> False, c___], Cell[d_, "Output", f___]}, 
    g___]]] :=
    Cell[CellGroupData[{Cell[BoxData[a], style, b, Evaluatable -> False, 
        c], Cell[d, "Output", f]}, g]]

HeadlessNotebookEvaluate[
  Cell[CellGroupData[{Cell[BoxData[a_], 
      style_ /; MemberQ[evaluationStyle, style], c___], 
     Cell[d_, "Output", f___]}, g___]]] :=
    Module[ {resultExp, resultBox, displayForm},
        resultExp = ToExpression[a];
		Sow[resultExp, "Unused?"];
        displayForm = Head[resultExp];
        resultBox = 
         With[ {res = resultExp},
             If[ MemberQ[displayForms, displayForm],
                 FormBox[MakeBoxes[res, StandardForm], displayForm],
                 MakeBoxes[res, StandardForm]
             ]
         ];
        Cell[CellGroupData[{Cell[BoxData[a], style, c], 
           Cell[BoxData[resultBox], "Output", f]}, g]]
    ]


evaluatableCellQ[ style_, opts_List] :=
    Quiet[
        Module[ {eval},
            eval = Evaluatable /. opts;
            Which[
            	eval === True,
            	   True,
            	eval === False,
            	   False,
            	True,
            	   MemberQ[evaluationStyle, style]
            ]
        ]
    ]


createInput[ a_String] := ToExpression[a]

cnt = 0;

getFragment[inp_String] :=
    (
    Sow[ "base.notebooktemplating.temp" :> ""];
    inp
    )
    
getFragment[inp_] := 
    Module[ {res, name, rule},
    	cnt++;
    	name = "base.notebooktemplating.temp" <> ToString[cnt];
    	res = MakeExpression[inp, StandardForm];
    	res = Replace[ res, {HoldComplete[ ExpressionCell[TextForm[x_]]] -> HoldComplete[x], 
    		                   HoldComplete[ ExpressionCell[x_]] -> HoldComplete[x]}];
    	rule = 
    	   With[ {name1 = name},
        	 Apply[Function[val, (name1 :> val), {HoldAll}], res]];
    	Sow[rule];
    	ToString[ name, InputForm]
    ]

createInput[ TextData[a_List]] := 
    Module[ {tmp, rules},
    	{tmp, rules} = Reap[ Map[ getFragment, a]];
        If[ !MatchQ[ rules, {_}], Return[ $Failed]];        
        If[ !MatchQ[ tmp, {__String}], Return[ $Failed]];
    	tmp = StringJoin @@ tmp;
    	tmp = ToExpression[ tmp, InputForm, Hold];
    	tmp = tmp /. First[ rules];
    	ReleaseHold[ tmp]
    ]

makeOutputGroup[ inp_, style_, opts_] :=
    Module[ {resultExp, resultBox, displayForm, content},
    	resultExp = createInput[inp];
    	If[ resultExp === Null,
    	   Cell[inp, style, Apply[ Sequence, opts]],
    	   displayForm = Head[resultExp];
    	   resultBox = 
                With[ {res = resultExp},
                    If[ MemberQ[displayForms, displayForm] && Length[resultExp] === 1,
                        content = res[[1]];
                        FormBox[MakeBoxes[content, StandardForm], displayForm],
                        MakeBoxes[res, StandardForm]
                    ]
                ];
            Cell[CellGroupData[{Cell[inp, style, Apply[ Sequence, opts]], 
            Cell[BoxData[resultBox], "Output"]}]]
    	]
    ]

(*Text like cells, with Evaluatable can be False/True*)
HeadlessNotebookEvaluate[
    Cell[a_String, style_, opts___] /; evaluatableCellQ[style, {opts}]] :=
        makeOutputGroup[a, style, {opts}]

HeadlessNotebookEvaluate[
    Cell[TextData[a_], style_, opts___] /; evaluatableCellQ[style, {opts}]] :=
        makeOutputGroup[TextData[a], style, {opts}]


HeadlessNotebookEvaluate[Cell[CellGroupData[a_List, c___]]] :=
    Cell[CellGroupData[HeadlessNotebookEvaluate /@ a, c]]

HeadlessNotebookEvaluate[Notebook[a_List, c___]] := Notebook[HeadlessNotebookEvaluate /@ a, c]

HeadlessNotebookEvaluate[x___] := x

(* Include evaluation trail. *)
ReapedHeadlessNotebookEvaluate[Notebook[a_List, c___]] :=
    Reap[Notebook[HeadlessNotebookEvaluate /@ a, c], "Interesting"] /. {nb_Notebook, {res_List}} :> {nb, res}

ReapedHeadlessNotebookEvaluate[x___] := x

System`GenerateDocument::multivar="Template variable `1` appears more than once in the same group. Only the first will be used."     

ComputeEvaluationMarker[Notebook[a_List, c___]] := 
 Notebook[ComputeEvaluationMarker /@ a, c]

ComputeEvaluationMarker[Cell[CellGroupData[a_List, c___]]] := 
 Cell[CellGroupData[ComputeEvaluationMarker /@ a, c]]

ComputeEvaluationMarker[CellGroupData[a_List, c___]] := 
 Cell[CellGroupData[ComputeEvaluationMarker /@ a, c]]

ComputeEvaluationMarker[expr_Cell] := 
 expr /. {Cell[BoxData[FormBox[TemplateBox[{var_,"General",contentData_}, "NotebookTemplateExpression", ___], TextForm]], ___] :> 
    	formatExpr[ToExpression[var],contentData]}

ComputeEvaluationMarker[x___] := x 

formatExpr[ value_String, TextData] := value
formatExpr[ value_String, BoxData] := ToBoxes[value]

(*
formatExpr[ value_String, opt_, TextData] := StyleBox[ value, opt]
formatExpr[ value_String, opt_, BoxData] := StyleBox[ ToBoxes[value], opt]
*)

formatExpr[ value_, TextData] := Cell[ BoxData[ FormBox[ToBoxes[value],TextForm]]]
formatExpr[ value_, BoxData] := ToBoxes[value]     
        
ConvertValue[ value_, exp_TemplateBox] :=
    If[ Head[value] === String,
        EscapeString[ value],
        ToBoxes[value]
    ]
    
ConvertValue[value_, exp_Cell] :=
    If[ Head[value] === String,
        value,
        ToString[value]
    ]

EscapeString[string_String] :=
    StringJoin["\"" <> string <> "\""]
    

taggingInputCells[Notebook[a_List, c___], rat_] := 
 Notebook[Map[taggingInputCells[#, rat]&, a], c]
 
taggingInputCells[gp:Cell[CellGroupData[{first_Cell, other___}, c___]], rat_] := 
 Module[{cells,label, tag, res},
 	cells= gp;
 	label = GetEvaCellLabel[ first];
	tag = label /. MapThread[Rule, {evaluationTooltipLabels, evaluationTags}];
	If[!MatchQ[tag,{}],
    res = 
	If[MatchQ [tag, evaluationTags[[4]]],
	cells ->Sequence[],
	cells->Cell[CellGroupData[Map[taggingSubCells[#, tag, rat]&, {first, other}], c]]
	];
	cells/.res
	,
	Cell[CellGroupData[Map[taggingInputCells[#, rat]&, {first, other}], c]]]
 ] 

taggingInputCells[CellGroupData[a_List, c___], rat_] := 
 Cell[CellGroupData[Map[taggingInputCells[#, rat]&, a], c]]

taggingInputCells[expr_Cell, rat_] := 
 taggingFromLabel[expr, rat]

taggingInputCells[x___, rat_] := x 

taggingSubCells[a_List, tag_, rat_] := 
 Map[taggingSubCells[#, tag, rat]&, a]
 
taggingSubCells[Cell[CellGroupData[lis_List, c___]], tag_, rat_] := 
 Cell[CellGroupData[Map[taggingSubCells[#, tag, rat]&, lis], c]]
 
taggingSubCells[expr_Cell, tag_, rat_] := 
 taggingFromLabel[expr, tag, rat] 
 
taggingSubCells[x_,tag_, rat_]:=x 
    	
taggingFromLabel[cell_Cell, rat_]:=
	Module[{label, tag },
		label = GetEvaCellLabel[cell];
	    tag = label /. MapThread[Rule, {evaluationTooltipLabels, evaluationTags}];
        taggingFromLabel[cell, tag, rat]
    ]
 
taggingFromLabel[cell_Cell, tag_, rat_]:=
	Module[{cellNew },
		cellNew= cell;
        If[MatchQ[tag,evaluationTags[[3]]], (*Unevaluated*)
        	cellNew = Append[cell, Evaluatable -> False]];
        cellNew = If[TrueQ@rat, cellNew, removeLabelsEvaCell[cellNew]];	
        If[!MatchQ[tag,{}],
        	cellNew = addTaggingRls[cellNew, {tag -> True}]];
        If[
        	MatchQ[tag,evaluationTags[[4]]],(*Exclude*)
        	cellNew = Sequence[]];
        cellNew
    ] 
         
displayInputCells[ notebook_Notebook] :=
    getEvaluationGroups[notebook]
    
addTaggingRls[cell_Cell, rls_List]:=
	Module[{taggingRules, taggingRulesNew},
		taggingRules = TaggingRules /.Cases[Options[cell], x_ /; MatchQ[x, Rule[TaggingRules, ___]]];
		If[MatchQ[taggingRules, TaggingRules],
			Append[cell, TaggingRules -> rls],
			taggingRulesNew = Join[taggingRules,rls];
			Replace[cell,Rule[TaggingRules, _]:>taggingRulesNew,{1}] ]
	]
	
removeTaggingRls[cell_Cell, rl_Rule]:= 
	Module[{taggingRules, taggingRulesNew},
		taggingRules = TaggingRules /.Cases[Options[cell], x_ /; MatchQ[x, Rule[TaggingRules, ___]]];
		taggingRulesNew = DeleteCases[taggingRules,rl];
		If[MatchQ[taggingRulesNew, {}],
			Replace[cell,Rule[TaggingRules, _]:>Sequence[],{1}],
			Replace[cell,Rule[TaggingRules, _]:>taggingRulesNew,{1}] ]
	] 	    	

getEvaluationGroups[ Cell[CellGroupData[{first_Cell /; GetEvaCellTaggingRls[first] =!= -1, other___}, args___]]] := 
	fixGroup [Cell[CellGroupData[{first, other}, args]]]
 
getEvaluationGroups[Cell[CellGroupData[a_List, c___]]] := 
	Cell[CellGroupData[getEvaluationGroups /@ a,c]]

getEvaluationGroups[x_Cell] := fixInput[x] 

getEvaluationGroups[Notebook[a_List, c___]] := Notebook[getEvaluationGroups /@ a,c]

getEvaluationGroups[x___] := x
 
getOutputIndex[ cells_List] :=
    Module[ {pos},
        pos = Position[cells, Cell[ _,"Output",___]];
        If[ MatchQ[pos, {{_Integer},___}],Part[pos,1,1], -1]
    ]
 
 
fixGroup[ group:Cell[CellGroupData[{first:Cell[a_, style_ , b___], other__}, ext___]] ]:=
	Module[{inputQ},
		inputQ = MemberQ[evaluationStyle, style];
		If[inputQ,
			fixInput[group,"InputGroup"],
			fixInput[group,"Group"]
		]

	]
	
fixGroup[ group_List ]:=
	fixGroup/@group
	
fixGroup[x_]:=x		
 
fixInput[ group:Cell[CellGroupData[{inputCell_, other__}, ext___]], "InputGroup" ] :=
    Module[ {tag, res, index, len},
        len = Length[{other}];
        tag = GetEvaCellTaggingRls[inputCell];
        Which[ 
        	MatchQ[ tag, evaluationTags[[1]]] (*EvaluateDeleteInput*)
            ,
            res = First[ {other}];
            If[ 
            	len === 1,
            	   res,
            	   res = If[FreeQ[res, Rule[CellGroupingRules, _]], 
            	   	           Append[res, CellGroupingRules -> "InputGrouping"],res];
            	   Cell[ CellGroupData[ {res, Rest[{other}]}]]]
            ,       	
            MatchQ[ tag, evaluationTags[[2]]] (*EvaluateHideInput*)
            ,
            index = getOutputIndex[{other}];
            index = If[ index === -1, 2, index+1];
            res = removeTaggingRls[inputCell,tag->True];
            Cell[ CellGroupData[ {res, other}, {index}]]
            ,            
            MatchQ[ tag, evaluationTags[[3]]](*Unevaluated*)
            ,
            res = inputCell;
            res = removeTaggingRls[res,tag->True]; 
            res = DeleteCases [res, Rule[Evaluatable, False]];
            Cell[ CellGroupData[ {res, other}, ext]]
            ,
            True
            ,
            group
            ]
    ]
    
fixInput[ group:Cell[CellGroupData[{first_, other__}, ext___]], "Group" ] :=
    Module[ {tag, res, firstNew, len},
        len = Length[{other}];
        tag = GetEvaCellTaggingRls[first];
        Which[ 
        	MatchQ[ tag, evaluationTags[[1]]] (*EvaluateDeleteInput, deletes the whole group*)
            ,
            res = Sequence[]
            ,       	
            MatchQ[ tag, evaluationTags[[2]]] (*EvaluateHideInput, hide input, close the whole group*)
            ,
            firstNew = removeTaggingRls[first,tag->True];
            res = Cell[ CellGroupData[ fixGroup/@{firstNew, other}, Closed]]
            ,            
            MatchQ[ tag, evaluationTags[[3]]](*Unevaluated*)
            ,
            firstNew = removeTaggingRls[first,tag->True];
            firstNew = DeleteCases [firstNew, Rule[Evaluatable, False]];
            res = Cell[ CellGroupData[ {firstNew, other}, ext]]
            ,
            True
            ,
            res = group
            ];
           res
    ]    
    
fixInput[ expr_Cell ] :=
    Module[ {tag,res},
        tag = GetEvaCellTaggingRls[expr];
        res = expr;
        If[ 
        	MatchQ[ tag, evaluationTags[[3]]](*Unevaluated*)
            ,
            res = removeTaggingRls[expr,tag->True]; 
            res = DeleteCases [res, Rule[Evaluatable, False]]];
        If[ 
        	MatchQ[ tag, evaluationTags[[1]]] (*EvaluateDeleteInput*)
            ,
            res = Sequence[]; 
            ];    
         res
        ]                     

getNotebookFileName[notebook_NotebookObject]:=With[{title=Quiet[NotebookFileName[notebook]]},
	If[StringQ[title],
		StringReplace[Last[FileNameSplit[title]],".nb"->""],
		"Untitled",
		""]]
getNotebookFileName[___]:="Untitled"


varValue[data_, global_, var_] := Module[
	{arg, res},
	arg = ToExpression[ RowBox[ {"{", var, "}"}]];
	If[ MatchQ[ arg, {_String, ___}],
        arg = Prepend[ arg, 1]
    ];
    res = extractVariableValue[ data, arg];
    If[ res === $Failed,
    	res = extractVariableValue[ global, arg]
    ];
    res
]

extractVariableValue[data_, arg_] := Module[
	{res},
	res = With[{ex = Quiet[Extract[ data, arg]]},
		(* In cases where the value is a slot, this will strip out the ExpressionCell mucky-muck. *)
		ex /. ExpressionCell[_[n : _NotebookTemplating`NotebookTemplateSlot | _NotebookTemplating`NotebookTemplateExpression]] :> n
	];
	Which[
		Head[res] === Missing,
		    $Failed,
		Head[res] === Extract,
            $Failed,
        True,
            res
    ]
]


NotebookTemplating`NotebookTemplateSlot /: MakeBoxes[NotebookTemplating`NotebookTemplateSlot[name_], fmt_] := 
    MakeBoxes[NotebookTemplating`NotebookTemplateSlot[name, Null, "Named", BoxData], fmt]

NotebookTemplating`NotebookTemplateSlot /: MakeBoxes[NotebookTemplating`NotebookTemplateSlot[name_, def_], fmt_] := 
    MakeBoxes[NotebookTemplating`NotebookTemplateSlot[name, def, "Named", BoxData], fmt]

NotebookTemplating`NotebookTemplateSlot /: MakeBoxes[NotebookTemplating`NotebookTemplateSlot[name_, def_, type_], fmt_] := 
    MakeBoxes[NotebookTemplating`NotebookTemplateSlot[name, def, type, BoxData], fmt]

NotebookTemplating`NotebookTemplateSlot /: MakeBoxes[NotebookTemplating`NotebookTemplateSlot[name_, def_, type_, format_], fmt_] := 
    Cell[BoxData[FormBox[TemplateBox[{MakeBoxes[name, fmt], MakeBoxes[def, fmt], MakeBoxes[type, fmt], format}, "NotebookTemplateSlot"], TextForm]]];

NotebookTemplating`NotebookTemplateExpression /: MakeBoxes[NotebookTemplating`NotebookTemplateExpression[v_], fmt_] := 
    MakeBoxes[NotebookTemplating`NotebookTemplateExpression[v, General, BoxData], fmt]

NotebookTemplating`NotebookTemplateExpression /: MakeBoxes[NotebookTemplating`NotebookTemplateExpression[v_, format_], fmt_] := 
    MakeBoxes[NotebookTemplating`NotebookTemplateExpression[v, General, format], fmt]

NotebookTemplating`NotebookTemplateExpression /: MakeBoxes[NotebookTemplating`NotebookTemplateExpression[v_, type_, format_], fmt_] := 
    Cell[BoxData[FormBox[TemplateBox[{MakeBoxes[v, fmt], MakeBoxes[type, fmt], format}, "NotebookTemplateExpression"], TextForm]]];

formatTemplateVariable[ value_String, None, TextData] := value
formatTemplateVariable[ value_String, None, BoxData] := ToBoxes[value]

formatTemplateVariable[ value_String, TextData, opt___] := StyleBox[ value, opt]
formatTemplateVariable[ value_String, BoxData, opt___] := StyleBox[ ToBoxes[value], opt]

otherValueQ[value_] := MatchQ[value, 
	Except[Alternatives[
		_String
	]]
];
formatTemplateVariable[ value_?otherValueQ, None, TextData] := Cell[ BoxData[ FormBox[ToBoxes[value], TextForm]]]
formatTemplateVariable[ value_?otherValueQ, None, BoxData] := ToBoxes[value]

formatTemplateVariable[ value_, opt_, contentData_] := Cell[ BoxData[ StyleBox[ ToBoxes[value], opt]]]


makeValue[data_, global_, var_, default_] := Module[
	{value},
	value = varValue[data, global, var];
	If[ MatchQ[value, $Failed],
		NotebookTemplating`Authoring`Private`fixPreview[default],
		value
    ]
]


cellVariableReplacement[expr:Cell[content_, ext___], data_, global_] := Module[
	{contentNew},
	contentNew = (content /. {
        Cell[BoxData[FormBox[TemplateBox[{var_, default_, mode_, contentData_}, "NotebookTemplateSlot", ___], TextForm]], ___] :>
            formatTemplateVariable[makeValue[data, global, var, default], None, contentData]
        ,
        Cell[BoxData[TemplateBox[{var_, default_, mode_, contentData_}, "NotebookTemplateSlot", ___]], ___] :> 
            formatTemplateVariable[makeValue[data, global, var, default], None, contentData]
        ,
        Cell[BoxData[TemplateBox[{StyleBox[var_, opt___], default_, mode_, contentData_}, "NotebookTemplateSlot", ___]], ___] :> 
            formatTemplateVariable[makeValue[data, global, var, default], contentData, opt]
        , 
        Cell[BoxData[ StyleBox[TemplateBox[{var_, default_, mode_, contentData_}, "NotebookTemplateSlot", ___], opt___]], ___] :> 
            If[ MatchQ[varValue[data, global, var], $Failed],
                NotebookTemplating`Authoring`Private`fixPreview[default],
                (* else *)
                formatTemplateVariable[makeValue[data, global, var, default], contentData, opt]
            ]
        ,
        TemplateBox[{var2_, default_, mode_, contentData_}, "NotebookTemplateSlot", ___] :>
            Which[
                MatchQ[var2, RowBox[{__String}]], 
                If[ MatchQ[varValue[data, global, var2], $Failed],
                    NotebookTemplating`Authoring`Private`fixPreview[default],
                    (* else *)
                    ToBoxes[varValue[data, global, var2]]
                ],
                (*TODO: check this later*)
                MatchQ[var2, StyleBox[_, ___]],
                If[ MatchQ[varValue[data, global, var2], $Failed],
                    NotebookTemplating`Authoring`Private`fixPreview[default],
                    (* else *)
                    varValue[data, global, var2]
                ],
                True,
                If[ MatchQ[varValue[data, global, var2], $Failed],
                    NotebookTemplating`Authoring`Private`fixPreview[default],
                    (* else *)
                    ToBoxes[varValue[data, global, var2]]
                ]
            ]
        } /. TextData[ x : Except[_String | _StyleBox | _List | _ButtonBox | _Cell], ___] :>(*illegal TextData construct*)
            Cell[MakeBoxes[x]] (*handle this as Inline-Cell*)
    );
    Cell[contentNew, ext]
]

expValue[exp_Cell]:=
	Module[{label},
		label = expLabel[exp];
		label
	] 
	 
expValue[{}] := {}

makeNewData[ label_, oldData_, oldGlobal_, True] :=
	Module[ {blockName, globalData, blockData, nData, nGlobalData},
		Which[ 
			varLabelQ[label],
				blockName = varLabel[label];
				globalData = extractOuterData[oldData, blockName];
                blockData = varValue[oldData, oldGlobal, blockName];
                If[MatchQ[blockData,$Failed], blockData= {Association[]}]
				,
        	expLabelQ[label],
        		globalData = If[Length[oldData]>0, oldData[[1]], <||>];
        		blockData = expValue[label];
        	,
        	True,  (* Shouldn't get here *)
        		blockData = {oldData}  
		];
		(*
		 Needs a list around the data because really we 
		 are doing something like Template Apply
		*)
        nData = Map[ If[ListQ[#], #, {#}]&, blockData];
        nGlobalData = If[ListQ[globalData], globalData, {globalData}];
        {nData, nGlobalData} 
	]
	
makeNewData[ label_, oldData_, oldGlobal_, False] :=
	Module[ {nData},
		Which[ 
			varLabelQ[label],
				nData = varLabel[label];
                nData = varValue[oldData, oldGlobal, nData]
        	,
        	expLabelQ[label],
        		nData = expValue[label]
        	,
        	True,  (* Shouldn't get here *)
        		nData = {oldData}  
		];
		(*
		 Needs a list around the data because really we 
		 are doing something like Template Apply
		*)
		{Map[ If[ListQ[#], #, {#}]&, nData],{}}
	]
	
extractOuterData[oldData_, blockName_]:=
	Module[{arg},
		If[MatchQ[oldData, {}], <||>,
		arg = ToExpression[ RowBox[ {"{", blockName, "}"}]][[1]];
		Association[Select[Normal[oldData[[1]]], #[[1]] =!= arg &]]
		]
	]		

blockVariableReplacement[Notebook[a_List, c___], data_, global_] := 
 	Notebook[blockVariableReplacement[#, data, global] & /@ a, c]
  
blockVariableReplacement[ gps:Cell[CellGroupData[{headCell_, otherCells___}, opt1___],opt2___], data_, global_] :=
    Module[ { label, inherit, exp, newData, newGlobal, newHeadCell},
        label = getRepeatingBlockLabel[headCell];
        exp = If[
        		repeatingLabelQ[label],
        		inherit = inheritQ[label];
        		{newData, newGlobal} = makeNewData[label, data, global, inherit];
        		newHeadCell = removeLabelsRB[headCell];
                Map[ Quiet@blockVariableReplacement[ Cell[ CellGroupData[ {newHeadCell, otherCells},opt1],opt2], #, newGlobal]&, newData]
        		,
        		{Cell[
                    CellGroupData[blockVariableReplacement[#, data, global] & /@ {headCell, otherCells},  opt1],opt2]}
                     ];
        Sequence @@ exp
    ]
 

blockVariableReplacement[cell_Cell, data_, global_] :=
    Module[{label, inherit, newData, newGlobal, newCell , exp, expNew}, 
        label = getRepeatingBlockLabel[cell];
        exp = If[repeatingLabelQ[label],
        	inherit = inheritQ[label];
        	{newData, newGlobal} = makeNewData[label, data, global, inherit];
        	newCell = removeLabelsRB[cell];
        	expNew = newCell;
        	expNew = Map[ Quiet@cellVariableReplacement[ newCell, #, newGlobal]&, newData]
        	,
        	{cellVariableReplacement[cell, data, global]}
        ];
        Sequence@@exp             
    ]     

End[]

EndPackage[]


