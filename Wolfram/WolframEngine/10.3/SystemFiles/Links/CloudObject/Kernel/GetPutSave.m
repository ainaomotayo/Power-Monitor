BeginPackage["CloudObject`"]

System`CloudGet;
System`CloudPut;
System`CloudSave;

CloudObject`GetObjectMetadataFilename;

Begin["`Private`"]

Needs["Iconize`"]

Unprotect[CloudObject];

(* general read/write *)

cleanup[tempfilename_, expr_: Null] := (DeleteCloudObject[tempfilename, Asynchronous->True]; expr)

readObject[obj_CloudObject, head_Symbol : CloudObject] :=
    responseToFile[execute[obj], head]

(* content_ is a list of bytes, as returned by e.g. BinaryReadList *)
writeObject[obj_CloudObject, content_?StringQ, rest___] :=
    writeObject[obj, ToCharacterCode[content], rest]
writeObject[obj_CloudObject, content_, mimetype_,
        permissions_ : Automatic, iconRules_ : None, iconExpr_ : Null, metaInformation_ : {},
        params_ : {}, head_Symbol : CloudObject] :=
    Module[{result},
        result = responseToString @ execute[obj, Automatic,
            UseUUID -> False, Body -> content, Type -> mimetype,
            Parameters -> Join[params, {
                "permissions" -> escapeAndNormalizePermissions[permissions, mimetype, head],
                If[Length[metaInformation] > 0, "properties" -> encodeMetaInformation[metaInformation], Unevaluated[Sequence[]]]
            }]];
        If[result === $Failed, Return[$Failed]];
        If[iconRules =!= None,
            SetCloudIcons[obj, iconRules, Asynchronous->True, "Content" -> iconExpr,
                "Deployment" -> iconDeploymentType[mimetype, permissions]]
        ];
        obj
    ]

(*Put*)

Unprotect[CloudPut];

definitionsToString[defs_] := StringJoin @ Riffle[Flatten[List @@ Replace[Unevaluated @ defs,
    (HoldForm[symbol_] -> def_) :> (Replace[Unevaluated@def, {
        (Attributes -> attributes_) :>
            If[Length[attributes] > 0,
                ToString[Unevaluated[Attributes[symbol] = attributes], InputForm],
                {}
            ],
        (DefaultValues -> options_) :>
            ReplaceAll[options,
                (Verbatim[HoldPattern][lhs_] -> rhs_) :>
                    ToString[Unevaluated[lhs = rhs], InputForm]
            ],
        (Messages -> messages_) :>
            ReplaceAll[messages,
                (Verbatim[HoldPattern][messagename_] -> message_) :>
                    ToString[Unevaluated[messagename = message], InputForm]
            ],
        (name_ -> values_) :>
            ReplaceAll[Unevaluated@values, {
                (lhs_ -> rhs_) :> ToString[Unevaluated[lhs = rhs], InputForm],
                (Verbatim[HoldPattern][lhs_] :> rhs_) :> ToString[Unevaluated[lhs := rhs], InputForm],
                (lhs_ :> rhs_) :> ToString[Unevaluated[lhs := rhs], InputForm]
            }]
    }, {1}]), {1}
]], "\n\n"]

Options[CloudPut] = {SaveDefinitions->False, Permissions->Automatic, IconRules->Automatic, MetaInformation->{}};
Options[iCloudPut] = Join[Options[CloudPut], {"Append" -> False}];

iCloudPut[expr_, obj:CloudObject[uri_, objopts:OptionsPattern[CloudObject]], mimetype_String, opts:OptionsPattern[]] :=
    Module[{content, tempfilename,
        iconRules = OptionValue[iCloudPut, {opts, objopts}, IconRules],
        metaInformation = OptionValue[iCloudPut, {opts, objopts}, MetaInformation],
        permissions = OptionValue[iCloudPut, {opts, objopts}, Permissions],
        params = {"append" -> ExportString[TrueQ[OptionValue["Append"]], "JSON"]}
    },
        If[TrueQ[OptionValue[SaveDefinitions]],
        (* save definitions *)
            content = exprToStringBytesWithSaveDefinitions[Unevaluated[expr]],
        (* do not save definitions *)
            tempfilename = CreateTemporary[];
            Block[{$ContextPath={"System`"}, $Context = "System`"}, Put[Unevaluated[expr], tempfilename]];
            content = BinaryReadList[tempfilename];
        ];
        writeObject[obj, content, mimetype, permissions, iconRules, Unevaluated[expr], metaInformation, params]
    ]

$IncludedContexts = {}; (* Block override this with {"CloudObject"} to use CloudEvaluate from within CloudObject`Private` code *)

exprToStringBytesWithSaveDefinitions[expr_] :=
    Module[{defs, content, exprLine},
    	(* This fn is used by the package itself, so make sure the package context
    	 * is not excluded. *)
        defs = With[{excl = Join[OptionValue[Language`ExtendedFullDefinition, ExcludedContexts],{"MailReceiver"}]},
        	Language`ExtendedFullDefinition[expr, 
        	    ExcludedContexts -> Complement[excl, $IncludedContexts]]
        ];
        content = definitionsToString[defs];
        exprLine = Block[{$ContextPath={"System`"}, $Context="System`"}, ToString[Unevaluated[expr], InputForm]];
        content = content <> "\n\n" <> exprLine <> "\n";
        ToCharacterCode[content, "UTF-8"]
    ]

CloudPut[expr_, options : OptionsPattern[]] :=
    CloudPut[Unevaluated[expr], CloudObject[], options]

CloudPut[expr_, obj_CloudObject, opts:OptionsPattern[]] :=
    iCloudPut[Unevaluated[expr], obj, expressionMimeType["Expression"], opts]

CloudPut[expr_, uri_String, opts:OptionsPattern[]] :=
    CloudPut[Unevaluated[expr], CloudObject[uri], opts]

CloudPut[expr_, obj_, opts:OptionsPattern[]]:=
	(Message[CloudPut::invcloudobj, obj];$Failed)

CloudPut[args___] := (ArgumentCountQ[CloudPut,Length[DeleteCases[{args},_Rule,Infinity]],1,2];Null/;False)

CloudObject /: Put[expr_, obj_CloudObject] := CloudPut[Unevaluated[expr], obj]

CloudObject /: PutAppend[expr_, obj_CloudObject] := iCloudPut[Unevaluated[expr], obj, expressionMimeType["Expression"], "Append" -> True]

SetAttributes[CloudPut, {ReadProtected}];
Protect[CloudPut];

(*Save*)

Unprotect[CloudSave];

Options[CloudSave] = {IconRules->Automatic, MetaInformation->{}, Permissions->Automatic};
Attributes[CloudSave] = {HoldFirst};

CloudSave[expr_, obj:CloudObject[uri_, objopts:OptionsPattern[CloudObject]], opts:OptionsPattern[]] :=
    Module[{type, content, tempfilename},
        If[FileExistsQ[obj],
            {tempfilename, type} = readObject[obj, CloudSave];
            If[tempfilename === $Failed, Return[$Failed]],
        (* else *)
            tempfilename = CreateTemporary[]
        ];
        Save[tempfilename, Unevaluated[expr]];
        content = BinaryReadList[tempfilename];
        writeObject[obj, content, expressionMimeType["Expression"],
            OptionValue[CloudSave, {opts, objopts}, Permissions],
            OptionValue[CloudSave, {opts, objopts}, IconRules], Unevaluated[expr],
            OptionValue[CloudSave, {opts, objopts}, MetaInformation],
            {},
            CloudSave
        ]
    ]

CloudSave[expr_, uri_String, opts:OptionsPattern[]] := CloudSave[expr, CloudObject[uri], opts]

CloudSave[expr_, opts:OptionsPattern[]] := CloudSave[expr, CloudObject[], opts]

CloudSave[args___] := (ArgumentCountQ[CloudSave,Length[DeleteCases[{args},_Rule,Infinity]],1,2];Null/;False)

CloudObject /: Save[obj_CloudObject, expr_] := CloudSave[expr, obj]

SetAttributes[CloudSave, {ReadProtected}];
Protect[CloudSave];

(*Get*)

Unprotect[CloudGet];

bundleMimeTypeQ[mimetype_] :=
    StringQ[mimetype] &&
        StringMatchQ[mimetype, "application/vnd.wolfram.bundle" ~~ ___]

CloudGet[co_CloudObject] :=
    Module[{tempfilename, mimetype},
        {tempfilename, mimetype} = readObject[co, CloudGet];
        Which[
            tempfilename === $Failed, $Failed,

            mimetype === "inode/directory", Message[Get::noopen, co]; $Failed,

            bundleMimeTypeQ[mimetype], CloudGet[FileNameJoin[{co, ".bundle"}]],

            True, cleanup[tempfilename, Block[{$CharacterEncoding = "UTF-8"},
                Get[tempfilename]
            ]]
        ]
    ];

CloudGet[uri_String] := CloudGet[CloudObject[uri]]

CloudObject /: Get[co_CloudObject] := CloudGet[co]

CloudGet[args___] := (ArgumentCountQ[CloudSave,Length[DeleteCases[{args},_Rule,Infinity]],1,1];Null/;False)

SetAttributes[CloudGet,{ReadProtected}];
Protect[CloudGet];

Protect[CloudObject];

(* From Jan, plus a tiny amount of error checking, and also allow CloudObject or UUID. *)
GetObjectMetadataFilename[obj_CloudObject, subpath___String] :=
    Replace[getCloudAndUUID[obj], {
                {_, uuid_} :> GetObjectMetadataFilename[uuid, subpath],
                _ :> $Failed
    }];

GetObjectMetadataFilename[uuid_String?UUIDQ, subpath___String] :=
    (* TODO (Jan?): does this need to use the $HomeDirectory of the CloudObject owner, not the caller?
       Or is that tautological? *)
    FileNameJoin[{$HomeDirectory, ".Objects", "metadata", StringTake[uuid, 3], uuid, subpath}];

GetObjectMetadataFilename[___] := $Failed;

End[]

EndPackage[]
