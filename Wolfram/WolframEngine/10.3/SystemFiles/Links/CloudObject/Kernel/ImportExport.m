BeginPackage["CloudObject`"]

System`ExportForm;
System`UpdateInterval;
System`CloudExport;
System`CloudImport;

Begin["`Private`"]

Unprotect[CloudObject];

(* Give non-"application/..." types precedence (e.g. image/png should be used instead of application/png).
 But prefer application/pdf over text/pdf. *)
uniqueType[types_List] := First @ SortBy[ToLowerCase /@ types, If[StringMatchQ[#, "application" ~~ __] && # =!= "application/pdf", 2, 1] &]
uniqueType[type_] := uniqueType[{type}]

(* Formats *)

mimeToFormat := mimeToFormat = Quiet[
    DeleteCases[
        Flatten @ Map[
            Function[{format}, Function[{mime}, mime -> format] /@ ImportExport`GetMIMEType[format]], 
            $ExportFormats
        ],
        $Failed
    ], 
    FileFormat::fmterr
];

mimetypeToFormat[type_, filename_: None] := ToLowerCase @ Replace[
    ToUpperCase[type],
    Join[mimeToFormat, {_ -> If[filename =!= None, FileFormat[filename], "Text"]}]
]

fileFormatLookup := fileFormatLookup = Dispatch[MapAt[ToLowerCase, mimeToFormat, {All,1}]]

formatToMimeType := formatToMimeType = Replace @ Dispatch[
    Join[{
        None|_Missing|Automatic -> "text/string",
        mime_String?(StringMatchQ[#, __ ~~ "/" ~~ __]&) :> mime,
        "HTMLFragment" -> "text/html",
        "JPG" -> "image/jpeg"  (* ImportExport`GetMIMEType lacks definition for "JPG", so add it here *)
        }, 
        Quiet[
            Map[# -> (If[Length[#] > 0, uniqueType @ #, "application/octet-stream"] &) @
            ImportExport`GetMIMEType[#] &, $ExportFormats
            ], 
            FileFormat::fmterr
        ],
        {_ -> "application/octet-stream"}
    ]
];

(* ExportForm *)

Unprotect[ExportForm];

notebookExprPattern = HoldPattern[_Notebook|_NotebookObject|_DocumentNotebook|_PaletteNotebook|_DialogNotebook];

toNotebook[expr : notebookExprPattern] := InputForm[expr]
toNotebook[expr_, cellopts___] := Notebook[{
	If[Head[expr] === Cell,
		Cell[First[expr], expr[[2]], cellopts],
	(* not a cell *)
	    Cell[BoxData[ToBoxes[expr]], "Output", cellopts]
	]
}]
toNotebookElement[expr_] := toNotebook[expr, TextAlignment->Center, ShowCellBracket->False]

(* Defining a global variable that can be override by block, FormFunction is doing that *)
$exportFormReplacements = Dispatch[{
    HoldPattern[_NotebookObject|_Notebook|_DocumentNotebook|_PaletteNotebook|_DialogNotebook] :> "HTMLCloudCDF",
    HoldPattern[_Image] :> "PNG",
    HoldPattern[_Sound] :> "MP3",
    HoldPattern[_XMLElement|_XMLObject] :> {"XML", "text/html"},
    HoldPattern[_FormFunction|_APIFunction] :> "HTML",
    HoldPattern[_Manipulate|_Graphics|_Graphics3D] :> "NBElement",
    (* By default, export as a notebook. Export will take care of wrapping in a Notebook expression. *)
    _ :> "HTMLCloudCDF"
}]

(* one-argument short forms, also used by CloudDeploy so it automatically chooses a proper format *)
ExportForm[expr_, Automatic, opts:OptionsPattern[]] := ExportForm[expr, Replace[expr, $exportFormReplacements], opts]
ExportForm[expr_, opts:OptionsPattern[]] := ExportForm[expr, Replace[expr, $exportFormReplacements], opts]

ExportForm /: ExportString[ExportForm[body_, fmt_, rest___], fmt1_:Inherited, rest1___] :=
    Replace[
        applyExportForm[ExportForm[body, Replace[fmt1, Inherited :> fmt], rest1, rest]],
        res:Except[$Failed] :> First[res]
    ]

ExportForm /: Export[path_, e_ExportForm, fmt_:Inherited, rest___] :=
    Export[path, ExportString[e, fmt, rest], "String"]

SetAttributes[ExportForm, ReadProtected];

Protect[ExportForm];

(*Import*)

Unprotect[CloudImport];

cloudImportWrapper[obj_CloudObject, importFn_Function, head_Symbol:CloudObject] := Module[
    {tempfilename, mimetype},
    {tempfilename, mimetype} = readObject[obj, head];
    If[tempfilename === $Failed, Return[$Failed]];
    cleanup[
        tempfilename,
        importFn[tempfilename, mimetype]
    ]
]

cloudImportWrapper[obj_CloudObject, Automatic, head_Symbol:CloudObject] := cloudImportWrapper[obj,
    Function[{fn, mimetypeIn}, Import[fn, mimetypeToFormat[mimetypeIn, fn]]],
    head
]

cloudImportWrapper[obj_CloudObject, format_, head_Symbol:CloudObject] := cloudImportWrapper[obj,
	Function[{fn, mimetypeIn}, Import[fn, format]],
    head
]

CloudSemanticImport[obj_CloudObject, args___] := cloudImportWrapper[obj,
    Function[{fn, mimetype},
        SemanticImport[fn, args]
    ], CloudSemanticImport
]

CloudImport[obj_CloudObject, format_ : Automatic] := cloudImportWrapper[obj, format, CloudImport]

CloudImport[uri_String, format_ : Automatic] := CloudImport[CloudObject[uri], format]

CloudObject /: Import[obj_CloudObject, format_ : Automatic] := cloudImportWrapper[obj, format, Import]

CloudObject /: HoldPattern[SemanticImport][obj_CloudObject, args___] := cloudImportWrapper[obj,
    Function[{fn, mimetype},
        SemanticImport[fn, args]
    ], SemanticImport
]

SetAttributes[CloudImport, ReadProtected];
Protect[CloudImport];

(*Export*)

Unprotect[CloudExport];

Options[CloudExport] = {Permissions->Automatic, IconRules->Automatic, MetaInformation->{}};

SetAttributes[interactiveBoxesQ, HoldAllComplete];
interactiveBoxesQ[_DynamicBox|_Graphics3DBox|_Dynamic|_DynamicModuleBox] = True;
interactiveBoxesQ[head_[args___]] := interactiveBoxesQ[head] || ReleaseHold[Map[interactiveBoxesQ, Hold[Or[args]], {2}]]
interactiveBoxesQ[other_] = False;

SetAttributes[applyExportForm, HoldFirst];

(* one argument version is doing the automatic behaviour *)
applyExportForm[ExportForm[args___], rest___] := applyExportForm[args, rest];
applyExportForm[expr_] := applyExportForm[Evaluate[ExportForm[expr, Automatic]]]
applyExportForm[expr_, Automatic, rest___] := applyExportForm[Evaluate[ExportForm[expr, Automatic]], rest]

(*should Inherited make its way down here we want to avoid splatting*)
applyExportForm[expr_, Inherited, rest___] := applyExportForm[expr, "WL", rest]

(* normalizing the export form to {format, mimetype} *)
applyExportForm[body_, format:Except[_List], rest___] := applyExportForm[body, {format, Automatic}, rest]

(* options in the list are becoming options in the export *)
applyExportForm[body_, {format_, mime:Except[_Rule|_RuleDelayed]:Automatic, opt__}, rest___] := applyExportForm[body, {format, mime}, opt, rest]
(* a list with one argument is becoming automatic *)
applyExportForm[body_, {format_:None}, rest___] := applyExportForm[body, {format, Automatic}, rest]  

(* Always export NotebookObjects and CellObjects as (interactive) CloudCDFs.
   The HTMLFragment result is just a static image of the whole notebook which doesn't look right. *)
applyExportForm[expr:_NotebookObject|_CellObject|_Manipulate, {"HTMLCloudCDF", mime___}, rest___] :=
    applyExportForm[expr, {"CloudCDF", mime}, rest]

applyExportForm[expr_, {"HTMLCloudCDF", mime_}, rest___] :=
    With[
        {boxes = ToBoxes[expr, StandardForm]},
        If[
            interactiveBoxesQ[boxes],
            applyExportForm[
                expr, {
                    "CloudCDF", 
                    mime
                }, 
                rest
            ],
            applyExportForm[
                expr, {
                    "HTMLFragment", 
                    Replace[mime, Automatic -> expressionMimeType["HTMLCloudCDF"]]
                }, 
                rest
            ]
        ]
    ]

applyExportForm[expr_String, {"HTML", mime_}, rest___] :=
    applyExportForm[expr, {"HTMLFragment", mime}, rest]

applyExportForm[expr_, {"HTML", mime_}, rest___] :=
    applyExportForm[expr, {"HTMLFragment", mime}, rest, "FullDocument" -> True]

applyExportForm[expr_, {"NBElement"|"CloudCDFElement", Automatic}, rest___] := 
    applyExportForm[expr, {"NB", expressionMimeType["NBElement"]}, rest]

applyExportForm[expr_, {"NBElement"|"CloudCDFElement", mime_}, rest___] := 
    applyExportForm[expr, {"NB", mime}, rest]

applyExportForm[expr_, {"CloudCDF", Automatic}, rest___] := 
    applyExportForm[expr, {"NB", expressionMimeType["CloudCDF"]}, rest]

applyExportForm[expr_, {"CloudCDF", mime_}, rest___] := 
    applyExportForm[expr, {"NB", mime}, rest]

(* This is an internal thing it should be removed -riccardod *)

applyExportForm[expr_, {f:"API"|"Computation"|"Form"|"Task"|"Grammar"|"Expression", Automatic}, rest___] := 
    applyExportForm[expr, {"String", expressionMimeType[f]}, rest]

applyExportForm[expr_, {"API"|"Computation"|"Form"|"Task"|"Grammar"|"Expression", mime_}, rest___] := 
    applyExportForm[expr, {"String", mime}, rest]

applyExportForm[expr_, {None|"Text", Automatic|None}, rest___] :=
    applyExportForm[expr, {None, "text/html; charset=utf-8"}, rest]

applyExportForm[expr_, {"WL"|"String", Automatic|None}, rest___] :=
    applyExportForm[expr, {"WL", "text/plain; charset=utf-8"}, rest]

applyExportForm[expr_, {"WL"|"String", mime_}, rest___] := {
    ToString[expr, InputForm, CharacterEncoding -> "UTF8"],
    formatToMimeType[mime]
}

applyExportForm[expr_, {None|"Text", mime_}, rest___] := {
    ToString[expr, CharacterEncoding -> "UTF8"],
    formatToMimeType[mime]
}

applyExportForm[expr_, {format_?StringQ, Automatic}, rest___] := 
    applyExportForm[expr, {format, format}, rest]

applyExportForm[expr_, {format_?StringQ, None}, rest___] := 
    applyExportForm[expr, {format, "text/plain; charset=utf-8"}, rest]

applyExportForm[expr_, {format_?StringQ, mime_}, rest___] := {
    ExportString[
        wrapExportExpr[expr, format], 
        format, 
        rest
    ], 
    formatToMimeType[mime]
}

applyExportForm[expr_, {func_, None|Automatic}, rest___] := 
    applyExportForm[expr, {func, "text/plain; charset=utf-8"}]

applyExportForm[expr_, {func_, mime_}, rest___] := {
    func[expr], 
    formatToMimeType[mime]
}

SetAttributes[wrapExportExpr, HoldFirst];
wrapExportExpr[expr_Manipulate, "NBElement"] :=
    toNotebookElement[Append[expr, SaveDefinitions->True]]
wrapExportExpr[expr_, "NBElement"] :=
    toNotebookElement[expr]
wrapExportExpr[expr_, "CloudCDF"] :=
    toNotebook[expr]
wrapExportExpr[expr:Alternatives[Sound[_SoundNote], Sound[{__SoundNote}]], "MP3"|"FLAC"|"WAV"|"OGG"]:=
    Sound`ToSampledSound[expr]
wrapExportExpr[expr_, form_] := 
    Unevaluated[expr]


CloudExport[expr_, format_, obj:CloudObject[uri_, objopts:OptionsPattern[CloudObject]], rest___Rule] :=
    Module[{content, mimetype, permissions, result},
        {content, mimetype} = applyExportForm[expr, format, rest];
        If[content === $Failed, Return[$Failed]];
        permissions = Quiet[OptionValue[CloudExport, {rest, objopts}, Permissions], OptionValue::nodef];
        writeObject[obj, content, mimetype, permissions,
            Quiet[OptionValue[CloudExport, {rest, objopts}, IconRules], OptionValue::nodef],
            Unevaluated[expr],
            Quiet[OptionValue[CloudExport, {rest, objopts}, MetaInformation], OptionValue::nodef],
            {},
            CloudExport]
    ]

CloudExport[expr_, format_, uri_String, rest___Rule] :=
    CloudExport[Unevaluated[expr], format, CloudObject[uri], rest]

CloudExport[expr_, format_, rest___Rule] :=
    CloudExport[Unevaluated[expr], format, CloudObject[], rest]

CloudExport[args___] := (ArgumentCountQ[CloudExport, Length[DeleteCases[{args},_Rule,Infinity]],2,3];Null/;False)

CloudObject /: Export[obj_CloudObject, expr_, format_, rest___] :=
    CloudExport[Unevaluated[expr], format, obj, rest]
    
CloudObject /: Export[obj_CloudObject, expr_] := (Message[Export::argtu];HoldForm[Export[obj, expr]])

SetAttributes[CloudExport, ReadProtected];
Protect[CloudExport];

Protect[CloudObject];

End[]

EndPackage[]
