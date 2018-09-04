System`Private`NewContextPath[{"OAuthClient`","System`"}];

OAuthClient`tokenOAuthDialog;
OAuthClient`notokenOAuthDialog;
OAuthClient`defaultServiceConnectIcon;

Begin["OAuthDialogDump`"] (* Begin Private Context *) 

Begin["`Private`"]

(Unprotect[#]; Clear[#])& /@ {OAuthClient`tokenOAuthDialog,OAuthClient`notokenOAuthDialog}
Unprotect[OAuthClient`defaultServiceConnectIcon];

$OAuthDialogSaveQ=False;
$Clicked=True;

stretchimage[image_, n_] := Block[{tarray, pos, swath, imagedata},
	imagedata = ImageData[image, "Byte"];
	tarray = Transpose[imagedata];
	pos = First@Ordering@Table[Norm[N@tarray[[i + 1]] - tarray[[i]]], {i, 1, Length[tarray]-1}];
	swath = tarray[[pos]];
	Image[Transpose[Join[tarray[[1 ;; pos]], Table[swath, {n}], tarray[[pos + 1 ;; -1]]]], 
		"Byte", ColorSpace -> "RGB", Interleaving -> True]
  ]
  
(*********************** Authentication Dialogs ***********************************)
graytext=Sequence[FontFamily -> "Helvetica",RGBColor[71/255, 71/255, 71/255]];
vertspace=Spacer[If[TrueQ[CloudSystem`$CloudNotebooks], {1,10},{1,20}]];

OAuthDialogDump`Private`key="";

tokenOAuthDialog[first_, name_String, icon_: defaultServiceConnectIcon]:=tokenOAuthDialog[first, {name,None}, icon]/; $CloudEvaluation

tokenOAuthDialog[{url_, redirectFun_, temptoken_}, {name_, uuid_}, icon_: defaultServiceConnectIcon] := 
Block[{dialog, nb, header, footer, ifield}, 
	$OAuthDialogSaveQ =  OAuthClient`$SaveConnectionDefault;
	$Clicked=False;
	  header = 
	  Panel[Grid[{{Spacer[20], 
	If[icon === "", "", ImageResize[icon, {Automatic, 32}]], 
	wolframconnector}}], Appearance -> headerbg, 
			Alignment -> {Left, Center}, ImageSize -> {500, 50}];
	  footer = 
	  With[{tt = temptoken}, 
		   Panel[Grid[{{Spacer[20], 
		  If[TrueQ[CloudSystem`$CloudNotebooks], 
			 Checkbox[Dynamic[$OAuthDialogSaveQ]], 
			 ClickPane[
					   Dynamic[
							   If[$OAuthDialogSaveQ, checkboxactive, 
								  checkbox]], ($OAuthDialogSaveQ = ! $OAuthDialogSaveQ) 
					   &]],Spacer[5], Style["\tSave Connection", graytext], 					  
		  Spacer[If[TrueQ[CloudSystem`$CloudNotebooks], 135, 180]], 
		  cancelbutton[(enddialog[redirectFun, $Canceled, tt,{name, uuid}]) &], 
		  Spacer[10], 
		  donebutton[(enddialog[redirectFun, key, tt,{name, uuid}]) &]}}, 
					  Alignment -> {Left, Center} , Spacings -> 0], 
				 Appearance -> footerbg, Alignment -> {Left, Center}, 
				 ImageSize -> {500, 50}]];
	  ifield = 
	  InputField[
				 Dynamic[key, (key = 
							   StringReplace[#, RegularExpression["(?ms) "] :> ""]) &], 
				 String, Enabled -> True, ContinuousAction -> True, 
				 FieldHint -> "Paste your access key here", 
				 FieldHintStyle -> {Black, FontFamily -> "Helvetica"}, 
				 FrameMargins -> 5, ImageSize -> {460, 58}];
(*  
	  dialog = 
	  DynamicModule[{}, 
					Column[{vertspace,vertspace,vertspace, header, vertspace, 
		  Row[{Spacer[20], Style["Step 1.", graytext], Spacer[20], 
			  signinbutton[name, url]}], (*Spacer[{1, 10}], *)
		  Row[{Spacer[(*20+42+20*)82], 
			  Style["(you may be asked to authorize the app)", graytext]}],
		  vertspace, 
		  Row[{Spacer[20], Style["Step 2.", graytext], Spacer[20], 
			  Style["Paste your access key into the field below.", Bold, 
					graytext]}], (* Spacer[{1, 21}],*) 
			  Row[{Spacer[(*20+42+20*)82],ifield}], 
		  vertspace, footer}, Spacings -> 0, Alignment -> {Left, Center}
		  (*,
						   Background -> 
						   RGBColor[0.9411764705882353`, 0.9411764705882353`, 
									0.9411764705882353`]
									*)]];
*)

	dialog = 
	  DynamicModule[{},
					Grid[{{}, {header}, {Row[{Spacer[20], Style["Step 1.", graytext], 
     Spacer[20], 
     signinbutton[name, url]}]}, {Row[{Spacer[(*20+42+20*)82], 
     Style["(you may be asked to authorize the app)", 
      graytext]}]}, {Row[{Spacer[20], Style["Step 2.", graytext], 
     Spacer[20], 
     Style["Paste your access key into the field below.", Bold, 
      graytext]}]}, {Row[{Spacer[(*20+42+20*)82], 
     ifield}]}, {footer}, {}}, Alignment -> Left, Spacings -> {Automatic, {1, 4, 2, 0.3, 2, 0.3, 2}}]];																
	  
	  If[TrueQ[CloudSystem`$CloudNotebooks], 
		 CloudSystem`CreateCloudDialog[ToBoxes[dialog]], 
		 CreateDocument[dialog, Modal -> False, Background -> White, 
						ShowCellBracket -> False, CellMargins -> {{0, 0}, {0, 0}}, 
						CellFrameMargins -> 0, CellFrameLabelMargins -> 0, 
						CellLabelMargins -> 0, WindowElements -> {}, 
						WindowFrameElements -> {"CloseBox"}, 
						WindowFrame -> "ModalDialog", Deployed -> True, 
						WindowSize -> {500, 303}, ShowStringCharacters -> False, 
						Evaluator -> CurrentValue["RunningEvaluator"]]]
   ] /; $CloudEvaluation

enddialog[redirectFun_, value_, temptoken_, {name_, uuid_}] := 
Block[{res},
	$Clicked=True;
  	NotebookClose[EvaluationNotebook[]];
	  res = Replace[redirectFun[value], 
	     HTTPClient`OAuth`Private`Token20[l_List] :> 
	      HTTPClient`OAuth`Private`Token20[First[l]], {0, Infinity}];
	  Set[HTTPClient`OAuth`Private`temporaryOAuthToken[temptoken], res];
	  OAuthDialogDump`Private`key = "";
	  If[TrueQ[$OAuthDialogSaveQ]&&StringQ[uuid],
	  	OAuthClient`saveServiceConnection[ServiceObject[name,"ID"->uuid],Automatic]
	  ];
	]/;!$Clicked

enddialog[___]:=NotebookClose[EvaluationNotebook[]]

tokenOAuthDialog[url_, name_,icon_:defaultServiceConnectIcon] := 
	Block[{nb, value = Null, done = False, key = "", header, footer,ifield, saveQ=OAuthClient`$SaveConnectionDefault},
		header = Panel[Grid[
			{{Spacer[20], If[icon==="","",ImageResize[icon, {Automatic, 32}]], wolframconnector}}], 
			Appearance -> headerbg, Alignment -> {Left, Center}, ImageSize -> {500, 50}];
  		footer = Panel[
  			Grid[{{Spacer[20], 
	  			ClickPane[Dynamic[If[saveQ, checkboxactive, checkbox]], (saveQ = ! saveQ) &], 
	  			Style["Save Connection", graytext], 
	       		Spacer[(*460 - 18 - 102 - 2*80 - 10*) 180], 
	       		cancelbutton[(value = $Canceled; done = True) &], Spacer[10], 
	       		donebutton[(value = key; done = True) &]}},Alignment -> {Left, Center},Spacings -> 0], 
    		Appearance -> footerbg, Alignment -> {Left, Center}, 
    		ImageSize -> {500, 50}];
    	ifield = InputField[Dynamic[key, (key = StringReplace[#, RegularExpression["(?ms) "] :> ""]) &], String,
   			Enabled -> True, ContinuousAction -> True, 
  			FieldHint -> "Paste your access key here", 
  			FieldHintStyle -> {Black, FontFamily -> "Helvetica"}, FrameMargins->5,
  			ImageSize -> {460, 58}];
  		nb = CreateDocument[DynamicModule[{},
	     	Column[
	      		{header,vertspace,
		       	Row[{Spacer[20], Style["Step 1.", graytext], Spacer[20], 
		        signinbutton[name, url]}], 
		        Spacer[{1,10}],
	       		Row[{Spacer[(* 20 + 42 + 20 *) 82], Style["(you may be asked to authorize the app)", graytext]}],vertspace,
	       		Row[{Spacer[20], Style["Step 2.", graytext], Spacer[20], 
	         		Style["Paste your access key into the field below.", Bold, graytext]}],
		        Spacer[{1,21}],
	       		Row[{Spacer[20], ifield}],
	       		vertspace,
	       		footer
	       		}, 
	       		Spacings -> 0, Alignment -> {Left, Center}, 
	      		Background -> RGBColor[0.9411764705882353`, 0.9411764705882353`, 0.9411764705882353`]]],
		    Modal -> True, Background -> White, ShowCellBracket -> False, 
			CellMargins -> {{0, 0}, {0, 0}}, 
		    CellFrameMargins -> 0, CellFrameLabelMargins -> 0, 
		    CellLabelMargins -> 0, WindowElements -> {}, Magnification->1,
		    WindowFrameElements -> {"CloseBox"}, WindowFrame -> "ModalDialog",
		     System`NotebookEventActions -> {"ReturnKeyDown" :> (value = key; 
		        done = True), {"MenuCommand", 
		        "HandleShiftReturn"} :> (value = key; done = True), 
		      "EscapeKeyDown" :> (value = $Canceled; done = True), 
		      "WindowClose" :> (value = $Canceled; done = True)}, 
		      Deployed->True,
		    WindowSize -> {500, 303}, ShowStringCharacters -> False, 
		    Evaluator -> CurrentValue["RunningEvaluator"]];
  		WaitUntil[done];
  		OAuthClient`$SaveConnection=saveQ;
  		FrontEndExecute[FrontEnd`NotebookClose[nb, Interactive -> True, "ClosingEvent" -> Null]];
  		value]
  		
tokenOAuthDialog[___] := $Failed

notokenOAuthDialog[url_, text_] :=
    Block[{nb, authorizedQ, done = False},
        nb = CreateDocument[
            DynamicModule[{
                     button = If[text==="Permissions",permissionButton[url],authenticationButton[url, text]],
                     smessage = Style["(you may be asked to authorize the app)", 10, FontFamily -> "Arial"],
                     dmessage = "Authorized"},
                Column[{
                    Panel[Row[{Spacer[10], Style["<WC ICON>",Red], Spacer[10],
                              Style["WolframConnector", FontFamily -> "Arial", FontColor -> GrayLevel[0.30], FontSize -> 17]}],
                               ImageSize -> {400, 40}, ImageMargins -> {{0, 0}, {0, 0}}, Alignment -> Left, Appearance -> topgradient, FrameMargins->{{0, 0}, {0,0}}]
                   ,
                       Row[{Spacer[22], Column[{button, smessage}, Center], Spacer[22]}] 
                  ,
                     Column[{Row[{Spacer[22],Button[Style["Cancel", FontFamily -> "Arial", FontColor -> Black, FontSize -> 12], (authorizedQ = $Canceled; done = True),
                                  Appearance -> graybutton, ImageSize -> {90, Automatic}], Spacer[5],
                                    Button[Style[dmessage, FontFamily -> "Arial", FontColor -> White, FontSize -> 12], (authorizedQ = "True"; done = True),
                                  Appearance -> redbutton, ImageSize -> {90, Automatic}], Spacer[22]}],Spacer[10]}]
                  }
                  ,
                      Alignment -> {Center, Center},Background -> {None, None, GrayLevel[.92]}, Dividers -> {None, {None, None, GrayLevel[.8], None}},
                      Spacings -> 2
                ]
            ],
            Modal -> True,
            Background -> White,
            ShowCellBracket->False,
            StyleDefinitions -> "Dialog.nb",
            CellMargins-> {{0, 0}, {0, 0}},
            CellFrameMargins->0,
            CellFrameLabelMargins -> 0,
            CellLabelMargins -> 0,
            WindowElements -> {},
            WindowFrameElements -> {"CloseBox"},
            WindowFrame -> "ModalDialog",
            System`NotebookEventActions -> {
                "ReturnKeyDown" :> (authorizedQ = "True"; done = True),
                {"MenuCommand", "HandleShiftReturn"} :> (authorizedQ = "True"; done = True),
                "EscapeKeyDown" :> (authorizedQ = $Canceled; done = True),
                "WindowClose" :>   (authorizedQ = $Canceled; done = True)},
            WindowSize -> {400, 175},
            ShowStringCharacters -> False,
            Evaluator -> CurrentValue["RunningEvaluator"]
        ];

        WaitUntil[done];
		
        FrontEndExecute[FrontEnd`NotebookClose[nb, Interactive -> True, "ClosingEvent" -> Null]];
        authorizedQ
    ]

notokenOAuthDialog[___] := $Failed


KeyDialog[name_,icon_:defaultServiceConnectIcon] := 
	Block[{nb, value = Null, done = False, key = "", header, footer,ifield, saveQ=OAuthClient`$SaveConnectionDefault},
		header = Panel[Grid[
			{{Spacer[20], If[icon==="","",ImageResize[icon, {Automatic, 32}]], wolframconnector}}], 
			Appearance -> headerbg, Alignment -> {Left, Center}, ImageSize -> {500, 50}];
  		footer = Panel[
  			Grid[{{Spacer[20], 
	  			ClickPane[Dynamic[If[saveQ, checkboxactive, checkbox]], (saveQ = ! saveQ) &], 
	  			Style["Save Connection", graytext], 
	       		Spacer[(*460 - 18 - 102 - 2*80 - 10*) 180], 
	       		cancelbutton[(value = $Canceled; done = True) &], Spacer[10], 
	       		donebutton[(value = key; done = True) &]}},Alignment -> {Left, Center},Spacings -> 0], 
    		Appearance -> footerbg, Alignment -> {Left, Center}, 
    		ImageSize -> {500, 50}];
    	ifield = InputField[Dynamic[key, (key = StringReplace[#, RegularExpression["(?ms) "] :> ""]) &], String,
   			Enabled -> True, ContinuousAction -> True, 
  			FieldHint -> "Paste your access key here", 
  			FieldHintStyle -> {Black, FontFamily -> "Helvetica"}, FrameMargins->5,
  			ImageSize -> {460, 58}];
  		nb = CreateDocument[DynamicModule[{},
	     	Column[
	      		{header,vertspace,
		       	Row[{Spacer[20], Style["Step 1.", graytext], Spacer[20]}], 
		        Spacer[{1,10}],
	       		Row[{Spacer[(* 20 + 42 + 20 *) 82], Style["(you may be asked to authorize the app)", graytext]}],vertspace,
	       		Row[{Spacer[20], Style["Step 2.", graytext], Spacer[20], 
	         		Style["Paste your access key into the field below.", Bold, graytext]}],
		        Spacer[{1,21}],
	       		Row[{Spacer[20], ifield}],
	       		vertspace,
	       		footer
	       		}, 
	       		Spacings -> 0, Alignment -> {Left, Center}, 
	      		Background -> RGBColor[0.9411764705882353`, 0.9411764705882353`, 0.9411764705882353`]]],
		    Modal -> True, Background -> White, ShowCellBracket -> False, 
			CellMargins -> {{0, 0}, {0, 0}}, 
		    CellFrameMargins -> 0, CellFrameLabelMargins -> 0, 
		    CellLabelMargins -> 0, WindowElements -> {}, 
		    WindowFrameElements -> {"CloseBox"}, WindowFrame -> "ModalDialog",
		     System`NotebookEventActions -> {"ReturnKeyDown" :> (value = key; 
		        done = True), {"MenuCommand", 
		        "HandleShiftReturn"} :> (value = key; done = True), 
		      "EscapeKeyDown" :> (value = $Canceled; done = True), 
		      "WindowClose" :> (value = $Canceled; done = True)}, 
		      Deployed->True,
		    WindowSize -> {500, 303}, ShowStringCharacters -> False, 
		    Evaluator -> CurrentValue["RunningEvaluator"]];
  		WaitUntil[done];
  		OAuthClient`$SaveConnection=saveQ;
  		FrontEndExecute[FrontEnd`NotebookClose[nb, Interactive -> True, "ClosingEvent" -> Null]];
  		value]
  		
(* new multiple key dialog *)

(* Resources *)
$TermsAndConditionsAccepted = False
$DoNotUseFEResources = True

(* Text *)
tr[id_String] := id /; TrueQ[$DoNotUseFEResources]

tr["CancelButton"] := "Cancel" /; TrueQ[$DoNotUseFEResources]
tr["EnterFieldHint", id_String] := "Enter " <> id <> " here" /; TrueQ[$DoNotUseFEResources]
tr["OKButton"] := "Done" /; TrueQ[$DoNotUseFEResources]
tr["SaveConnectionTxt"] := "Save Connection" /; TrueQ[$DoNotUseFEResources]
tr["Step"] := "Step " /; TrueQ[$DoNotUseFEResources]
tr["Step", num_String] := Style[tr["Step"] <> num <> ".", Bold] /; TrueQ[$DoNotUseFEResources]
 		
(* Controls *)
servicebutton[servicName_, url_] := Button[Style[Row[{"Go to ", tr[servicName]}], FontFamily -> "Arial", 
   FontColor -> White, FontSize -> 12, FontWeight -> Bold], SystemOpen[url], FrameMargins -> {{15, 15}, {Automatic, Automatic}},
  Appearance -> {
  	"Default" -> stretchimage[redbtn, 60 + 5*StringLength[servicName]],
    "Hover" -> stretchimage[redbtnpressed, 60 + 5*StringLength[servicName]],
    "Pressed" -> stretchimage[redbtnpressed, 60 + 5*StringLength[servicName]]
    },
  ImageSize -> {{80, 400}, 30}]

button[label_, function_Function, opts : OptionsPattern[]] := Button[label, function, FrameMargins -> {{10, 10}, {Automatic, Automatic}}, opts]
  
choicebtns[dyn : Dynamic[fieldVals_], labels : {okLbl_, cancelLbl_}, function_Function] := Grid[{{
    button[Style[cancelLbl, GrayLevel[.1]], 
     DialogReturn[$Canceled] &,
     Appearance -> {
       "Default" -> stretchimage[graybtn, 50],
       "Hover" -> stretchimage[graybtnpressed, 50],
       "Pressed" -> stretchimage[graybtnpressed, 50],
       "ButtonType" -> "Cancel"
       }
     ],
    button[Style[okLbl, GrayLevel[.1]], function, Appearance ->
      {
       "Default" -> stretchimage[redbtn, 50],
       "Hover" -> stretchimage[redbtnpressed, 50],
       "Pressed" -> stretchimage[redbtnpressed, 50],
       "Disabled" -> stretchimage[redbtnpressed, 50],
       "ButtonType" -> "Default"
       },
     Enabled -> Dynamic[$TermsAndConditionsAccepted]]
    }}]

choicebtns[dyn : Dynamic[fieldVals_], okLblID_, function_Function] := choicebtns[dyn, {tr[okLblID], tr["CancelButton"]}, function]

sccheckbox[dyn : Dynamic[symbol_]] := Checkbox[dyn, {True -> checkboxactive, False -> checkbox},
  Appearance -> None,
  (*FrameMargins -> None,*)
  ImageMargins -> 0,
  ContentPadding -> False
  ]
  
labeledcheckbox[dyn : Dynamic[symbol_], label_] := Grid[{{sccheckbox[dyn], label}},
  ItemSize -> {Automatic, 2.5},
  Alignment -> {Left, Center}
  ]
  
(* Header and Footer *)
backgroundStripe[content_, bkgrndImgID_: headerbg] := Panel[content, 
	ImageSize -> {Full, Automatic}, 
	Appearance -> {"Default" -> bkgrndImgID},
	Alignment -> {Left, Center},
	FrameMargins -> 10,
	ImageMargins -> 0
  ]

header["key"] := header[defaultServiceConnectIcon, wolframconnector]
header[icon_, title_] := Grid[{{icon, title}}]

footerControlBar[buttons_, checkbox_] := Grid[{{checkbox, footerControlBar[buttons]}}]
footerControlBar[buttons_] := Pane[buttons, Full, Alignment -> Right]

footer[dyn : Dynamic[fieldVals_], btnLabel_, btnfunction_Function, checkbox : False] := Pane[choicebtns[dyn, btnLabel, btnfunction], Full, Alignment -> Right]

(*footer[dyn : Dynamic[fieldVals_], btnLabel_, btnfunction_Function, checkboxState : True] := (
  $OAuthDialogSaveQ = TrueQ[$OAuthDialogSaveQ];
  footer[dyn, btnLabel, btnfunction, Toggler[Dynamic[$OAuthDialogSaveQ], {True -> checkboxactive, False -> checkbox}]]
  )*)
footer[dyn : Dynamic[fieldVals_], btnLabel_, btnfunction_Function, checkbox : True] := (
  $OAuthDialogSaveQ = TrueQ[$OAuthDialogSaveQ];
  footer[dyn, btnLabel, btnfunction, 
  labeledcheckbox[Dynamic[$OAuthDialogSaveQ], tr["SaveConnectionTxt"]]]
 )

(*footer[dyn : Dynamic[fieldVals_], btnLabel_, btnfunction_Function, checkbox_] := (
  Grid[{{
     Grid[{{checkbox, tr["SaveConnectionTxt"]}}, Alignment -> Baseline],
     footer[Dynamic[fieldVals], btnLabel, btnfunction, False]
     }}]
  )*)
footer[dyn : Dynamic[fieldVals_], btnLabel_, btnfunction_Function,lbldcheckbox_] := (
  Grid[{{lbldcheckbox,footer[Dynamic[fieldVals], btnLabel, btnfunction, False]}}]
 )

(* Body *)
instructionRow[content_] := instructionRow["", content]
instructionRow[stepNum_Integer, content_] := instructionRow[tr["Step", ToString[stepNum]], content]
instructionRow[label_, content_] := {label, content}

instructions[listOfInstr_List] := instructions[listOfInstr, Spacings -> {1, 1}]
instructions[listOfInstr_List, gridOpts : OptionsPattern[]] := Module[{stepCount = 1},
  	Grid[listOfInstr, Alignment -> {Left, Baseline}, ItemStyle -> {LineIndent -> 0, ParagraphIndent -> 0}, gridOpts]
  ]

body[dyn : Dynamic[fieldVals_], credentialsURL_, serviceName_, termsURL_] := Column[{
   instructions[{instructionRow[1,"Get credentials for the service:"]}],
   servicebutton[serviceName, credentialsURL],
   instructions[{instructionRow[2,"Paste credentials into the fields below:"]}],
   body[dyn],
   labeledcheckbox[Dynamic[$TermsAndConditionsAccepted], 
    Style[Row[{"By proceeding I agree to " <> serviceName, " ", 
       Button["Terms of Use", SystemOpen[termsURL], 
        BaseStyle -> {"Hyperlink"}, Appearance -> None]}], 
     FontSize -> 10]]
   },
  Spacings -> 1]

otherbody[serviceName_, termsURL_] := Column[{
   labeledcheckbox[Dynamic[$TermsAndConditionsAccepted], 
    Style[Row[{"By proceeding I agree to " <> serviceName, " ", 
       Button["Terms of Use", SystemOpen[termsURL], 
        BaseStyle -> {"Hyperlink"}, Appearance -> None]}], 
     FontSize -> 10]]
   },
  Spacings -> 1]

body[dyn : Dynamic[fieldVals_]] := Module[{var, fields, heads}, (
	heads = fieldVals[[All, 1]];
   	fields = (Join[{#[[1]]}, If[MatchQ[#[[2]], _String], {#[[2]]}, Prepend[{Rest[#[[2]]]}, First[#[[2]]]]]]) & /@ fieldVals;
   	fields = (If[Length[#] == 2, Append[#, False], ReplacePart[#, 3 -> If[KeyExistsQ[#[[3]], FieldMasked], FieldMasked /. #[[3]], False]]]) & /@ fields;
   	fields = InputField[Dynamic[var[#[[2]]], Function[{x}, var[#[[2]]] = StringReplace[x, RegularExpression["(?ms) "] :> ""]]], 
       	String, FieldMasked -> #[[3]], Enabled -> True, ContinuousAction -> True, FieldHint -> tr["EnterFieldHint", #[[1]]], 
       	FieldHintStyle -> {Black, FontFamily -> "Helvetica"}, FrameMargins -> 5, ImageSize -> {400, 30}] & /@ fields;
   
   	fieldVals = Replace[fieldVals, (_ -> x_String) :> (x -> var[x]), Infinity];
   	fieldVals = Replace[fieldVals, (_ -> x_List) :> (x[[1]] -> var[x[[1]]]), Infinity];
   	Grid[Transpose[Prepend[{fields}, # ~~ ":" & /@ heads]], Alignment -> Left]
  )]

(* Dialogs *)
serviceConnectLayout[headerLbl_, headerIcon_, initialFieldVals_List, includeCheckbox : (True | False), serviceURL_, serviceName_, termsURL_] := 
 DynamicModule[{fieldVals = initialFieldVals, bodyselection},
  bodyselection = body[Dynamic[fieldVals], serviceURL, serviceName, termsURL];
  Column[{backgroundStripe@header[headerIcon, headerLbl],
    	Pane[bodyselection, Full, FrameMargins -> {{10, 10}, {15, 15}}],
   		backgroundStripe@footer[Dynamic[fieldVals], tr["OKButton"], DialogReturn[fieldVals] &, includeCheckbox]
    }]
  ]

otherLayout[headerLbl_, headerIcon_, initialFieldVals_List, includeCheckbox : (True | False), serviceURL_, serviceName_, termsURL_] := 
 DynamicModule[{fieldVals = initialFieldVals, bodyselection},
  bodyselection = otherbody[serviceName, termsURL];
  Column[{backgroundStripe@header[headerIcon, headerLbl],
    	Pane[bodyselection, Full, FrameMargins -> {{10, 10}, {15, 15}}],
    	backgroundStripe@footer[Dynamic[fieldVals], tr["OKButton"], DialogReturn[fieldVals] &, includeCheckbox]
    }]
  ]
  
serviceConnectDialog[serviceconectLayout_] := DialogInput[ExpressionCell[
   serviceconectLayout,
   CellMargins -> 0,
   CellFrameMargins -> 0,
   CellFrameLabelMargins -> 0,
   StripOnInput -> True
   ],Background -> RGBColor[0.9411764705882353`, 0.9411764705882353`, 0.9411764705882353`],
  WindowSize -> {550, All}]

otherDialog[otherLayout_] := DialogInput[ExpressionCell[
   otherLayout,
   CellMargins -> 0,
   CellFrameMargins -> 0,
   CellFrameLabelMargins -> 0
   ],
  WindowSize -> {500, All}]
  
MultipleKeyDialog[name_, fieldsP_, accountURL_, tosURL_, icon_: defaultServiceConnectIcon] := Module[{values, serviceName = wolframconnector, includeCheckbox = True},
 (
  	$TermsAndConditionsAccepted = False;
  	values = serviceConnectDialog[serviceConnectLayout[serviceName, icon, fieldsP, includeCheckbox, accountURL, name, tosURL]];
  	If[MatchQ[values, _List], Select[values, MatchQ[#[[2]], _String] &], values]
 )]

OtherDialog[name_, tosURL_, icon_: defaultServiceConnectIcon] := Module[{values, serviceName = wolframconnector, includeCheckbox = True},
 (
 	$TermsAndConditionsAccepted = False;
  	values = otherDialog[otherLayout[serviceName, icon, {}, includeCheckbox, "", name, tosURL]];
  	If[MatchQ[values, _List], Select[values, MatchQ[#[[2]], _String] &], values]
 )]
 
(* Buttons *)
custombutton[content_, fun_, len_, {unclicked_, clicked_}] := 
    DynamicModule[{apper = unclicked}, 
        If[TrueQ[CloudSystem`$CloudNotebooks], 
        	With[{temp = DownValues /@ {HTTPClient`OAuth`Private`jsonAccessTokenExtractor, 
                HTTPClient`OAuth`Private`tokenSaneQ20, OAuthClient`Private`tokenobject, OAuthClient`Private`rawtoken,
                HTTPClient`OAuth`Private`HMACSha1SignatureService}, headersQ=OAuthClient`Private`$useAuthHeader},
                Button[content, 
                    Block[{HTTPClient`OAuth`Private`jsonAccessTokenExtractor, HTTPClient`OAuth`Private`tokenSaneQ20, 
                    	OAuthClient`Private`tokenobject, OAuthClient`Private`rawtoken,HTTPClient`OAuth`Private`HMACSha1SignatureService,
                    	OAuthClient`Private`$useAuthHeader}, 
                        MapThread[(DownValues[#1] = #2) &, {{HTTPClient`OAuth`Private`jsonAccessTokenExtractor, 
                            HTTPClient`OAuth`Private`tokenSaneQ20, OAuthClient`Private`tokenobject, OAuthClient`Private`rawtoken,
                            HTTPClient`OAuth`Private`HMACSha1SignatureService},
                            temp}];
                        OAuthClient`Private`$useAuthHeader=headersQ;
                        fun[#]] &, 
                    Appearance -> {"Default" -> unclicked, "Pressed" -> clicked}, ImageSize -> {Clip[len, {80, 400}], 30}]], 
            EventHandler[Dynamic[Panel[content, Alignment -> Center, 
            	Appearance -> stretchimage[apper, len], ImageSize -> {Clip[len, {80, 400}], 
					30}]], {"MouseDown" :> (apper = unclicked; fun[]), 
					"MouseUp" :> (apper = clicked)}]]]

signinbutton[text_, url_] := 
 	With[{len = Max[150, 13 StringLength[text]]}, 
  		Panel[Hyperlink[Style["Sign in to "<>text, FontFamily -> "Arial", FontColor -> White, 
       			FontSize -> 12, FontWeight -> Bold], url], 
   			Alignment -> {Center, Center}, 
   			FrameMargins->If[TrueQ[CloudSystem`$CloudNotebooks], {{15, Automatic},{Automatic, 7}}, Automatic],
   			Appearance -> stretchimage[redbtn, 100], 
   			ImageSize -> {Clip[len, {80, 400}], 30}]
   	]

cancelbutton[fun_] := 
 	custombutton[Row[{Style["Cancel", FontFamily -> "Arial", FontColor -> Black, 
    	FontSize -> 12]}], fun, 80, {graybtn, graybtnpressed}]

donebutton[fun_] := 
 	custombutton[Row[{Style["Done", FontFamily -> "Arial", FontColor -> White, 
     	FontSize -> 12]}], fun, 80, {redbtn, redbtnpressed}]

(* login images *)

oauthimagedirectory=FileNameJoin[{DirectoryName[System`Private`$InputFileName],"Images"}];

wolframconnector:=wolframconnector=Import[FileNameJoin[{oauthimagedirectory,"wolframconnector.png"}]];
headerbg:=headerbg=Import[FileNameJoin[{oauthimagedirectory,"headerbg.png"}]];
checkbox:=checkbox=Import[FileNameJoin[{oauthimagedirectory,"checkbox.png"}]];
checkboxactive:=checkboxactive=Import[FileNameJoin[{oauthimagedirectory,"checkboxactive.png"}]];
redbtn:=redbtn=Import[FileNameJoin[{oauthimagedirectory,"redbtn.png"}]];
redbtnpressed:=redbtnpressed=Import[FileNameJoin[{oauthimagedirectory,"redbtnpressed.png"}]];
graybtn:=graybtn=Import[FileNameJoin[{oauthimagedirectory,"graybtn.png"}]];
graybtnpressed:=graybtnpressed=Import[FileNameJoin[{oauthimagedirectory,"graybtnpressed.png"}]];
footerbg:=footerbg=Import[FileNameJoin[{oauthimagedirectory,"footerbg.png"}]];
defaultServiceConnectIcon:=defaultServiceConnectIcon=Import[FileNameJoin[{oauthimagedirectory,"defaultServiceConnectIcon.png"}]];


End[]

End[] (* End Private Context *)

SetAttributes[{OAuthClient`tokenOAuthDialog,OAuthClient`notokenOAuthDialog},{ReadProtected, Protected}];

System`Private`RestoreContextPath[];
