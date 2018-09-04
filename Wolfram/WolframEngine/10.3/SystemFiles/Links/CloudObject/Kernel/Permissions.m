BeginPackage["CloudObject`"]

System`$Permissions;
System`Permissions;
System`SetPermissions;
System`ClearPermissions;
System`CloudPublish;


Begin["`Private`"]

$Permissions = "Private";
$normalizeUserTag = "normalizeUserTag"

permissionSpecs = "Read" | "Write" | "Execute" | "Edit" | "Save" | "EditRestricted" | "CellEdit" | "CellCreate" | "CellDelete" | "Evaluate" | "IncrementalEvaluate" | "Interact";

normalizePermissionsSpec["r", type_, _] = {"Read"};
normalizePermissionsSpec["w", type_, _] = {"Write"};
normalizePermissionsSpec["x", type_, _] = {"Execute"};

normalizePermissionsSpec["Edit", type_, _] = {"CellEdit", "CellCreate", "CellDelete"};
normalizePermissionsSpec["Use", _, _] = {"Execute"};
normalizePermissionsSpec["Modify", _, _] = {"Write"};

normalizePermissionsSpec[list_List, type_, head_] :=
    Map[normalizePermissionsSpec[#, type, head]&, list]
normalizePermissionsSpec[spec_String?(StringMatchQ[#, Characters["rwx"]..]&), type_, head_] :=
    Map[normalizePermissionsSpec[#, type, head]&, Characters[spec]]

normalizePermissionsSpec[Automatic, "application/vnd.wolfram.expression", _] = {"Read"};
normalizePermissionsSpec[Automatic, "application/vnd.wolfram.expression.api", _] = {"Execute"};
normalizePermissionsSpec[Automatic, "application/vnd.wolfram.expression.computation", _] = {"Execute"};
normalizePermissionsSpec[Automatic, "application/vnd.wolfram.expression.fci", _] = {"Execute"};
normalizePermissionsSpec[Automatic, "application/vnd.wolfram.expression.form", _] = {"Execute"};
normalizePermissionsSpec[Automatic, "application/vnd.wolfram.expression.grammar", _] = {"Execute"};
normalizePermissionsSpec[Automatic, "application/mathematica", _] = {"Read", "Interact"};
normalizePermissionsSpec[Automatic, "application/vnd.wolfram.notebook", _] = {"Read", "Interact"};
normalizePermissionsSpec[Automatic, "application/vnd.wolfram.notebook.element", _] = {"Read", "Interact"};
normalizePermissionsSpec[Automatic, _, _] = {"Read"};

normalizePermissionsSpec[All, type_, _] = Apply[List, permissionSpecs];

normalizePermissionsSpec[spec:permissionSpecs, type_, _] = {spec};

normalizePermissionsSpec[spec_, type_, head_] := (Message[head::invperm, spec]; {})

normalizeUserSpec[user_String] :=
    If[StringFreeQ[user, ":"] && FreeQ[{"All", "Authenticated", "Owner"}, user],
        "user:" <> user,
        user
    ]

isOwner[user_] := user === $WolframID || user === ("user-" <> $WolframUUID)

normalizePermissions["Public", type_, head_] :=
    {"All" -> Flatten[normalizePermissionsSpec[Automatic, type, head]], "Owner" -> {"Read", "Write", "Execute"}}
normalizePermissions["Private", type_, head_] :=
    {"Owner" -> {"Read", "Write", "Execute"}}
normalizePermissions[list_List, type_, head_] :=
    Join @@ Map[normalizePermissions[#, type, head]&, list]
normalizePermissions[user_String -> spec_, type_, head_] :=
    {normalizeUserSpec[user] -> Flatten[normalizePermissionsSpec[spec, type, head]]}
normalizePermissions[user_String -> _, _, head_] := (Message[head::selfperm, user]; {}) /; isOwner[user]
normalizePermissions[All -> spec_, type_, head_] := normalizePermissions["All" -> spec, type, head]
normalizePermissions[spec_String, type_, head_] := normalizePermissions[{"All" -> spec}, type, head]
normalizePermissions[Automatic, type_, head_] := normalizePermissions[$Permissions, type, head]
normalizePermissions[spec_, type_, head_] := (Message[head::invperm, spec]; {})
normalizePermissions[users_List -> spec_, type_, head_] := Join @@ Map[normalizePermissions[# -> spec, type, head]&, users]

groupIdentifier[group_PermissionsGroup] :=
    Module[{cloud, uuid},
        {cloud, uuid} = getCloudAndUUID[CloudObject @@ group];
        (* TODO: What should happen when the group is in a different cloud? *)
        If[uuid === None, Return[$Failed]];
        "wolfram:" <> uuid
    ]
normalizePermissions[group_PermissionsGroup -> spec_, type_, head_] :=
    Module[{id},
        id = groupIdentifier[group];
        If[id === $Failed, (Message[head::invperm, group -> spec]; Return[{}])];
        normalizePermissions[id -> spec, type, head]
    ]

escapeAndNormalizePermissions = Composition[toJSON, normalizePermissions]

fromServerPermissions["r"] := "Read"
fromServerPermissions["w"] := "Write"
fromServerPermissions["x"] := "Execute"
fromServerPermissions[p:("Read" | "Write" | "Execute" | "Edit" | "Save" |
	"EditRestricted" | "CellEdit" | "CellCreate" | "CellDelete" | "Evaluate" |
	"IncrementalEvaluate" | "Interact")] := p

fromServerPermissions[permjson_] :=
    ImportString[permjson, "JSON"] /. {
        serverPermissions_List :>
            Map[convertFromServerPermissions, serverPermissions],
        other_ :> ($lastServerPermissionsJSON = permjson; $Failed)
    }

fromServerUserClass[class_] :=
    If[StringMatchQ[class, "wolfram:" ~~ __],
        PermissionsGroup[class], (* TODO: denormalize to the group's name, take into account the cloud base *)
        StringReplace[class, StartOfString ~~ "user:" -> ""]
    ]

convertFromServerPermissions[class_ -> perms_String] :=
    fromServerUserClass[class] -> Cases[Map[fromServerPermissions, Characters[perms]], _String]
convertFromServerPermissions[class_ -> perms_List] :=
    fromServerUserClass[class] -> Cases[Map[fromServerPermissions, perms], _String]
    

normalizeUserSpecification[All, head_] := "All"
normalizeUserSpecification[usr_?(MemberQ[{"All", "Authenticated", "Owner"}, #] &), head_] := usr

normalizeUserSpecification[group_PermissionsGroup, head_] := 
	validateUserSpecification[group, head][[2]]
    
normalizeUserSpecification[usr_String, head_] :=
    validateUserSpecification[usr, head][[2]] 
		
normalizeUserSpecification[usr_, head_] :=	
	(Message[head::invusr, usr];Throw[$Failed, $normalizeUserTag])	
	
validateUserSpecification[All, head_] := "All"
validateUserSpecification[usr_?(MemberQ[{"All", "Authenticated", "Owner"}, #] &), head_] := usr	
	
validateUserSpecification[group_PermissionsGroup, head_] :=
	Module[{cloud, uuid},	
		{cloud, uuid} = getCloudAndUUID[CloudObject @@ group];
		If[uuid === None, Message[head::invusr, group]; Throw[$Failed, $normalizeUserTag], {group, uuid}] 
	]
	
validateUserSpecification[usr_String, head_] :=
	Module[{json, data},
    	If[Not[TrueQ[authenticatedQ[]]],
           With[{res=CloudConnect[]}, 
               If[UnsameQ[res, $WolframID], Message[head::notauth]; Throw[$Failed, $normalizeUserTag]]
           ]
        ];
        json = execute[$CloudBase, "GET", {"users"}, Parameters -> {"user" ->  usr}] /. { 
        	{_, bytes_List} :> FromCharacterCode[bytes],
            HTTPError[403, ___] :> (Message[head::noaccess, usr];
                                    Throw[$Failed, $normalizeUserTag]),(*not allowed*)
            HTTPError[404, ___] :> (Message[head::invusr, usr];
                                    Throw[$Failed, $normalizeUserTag]),(*not found*)
            other_ :> (checkError[other, head];
                       Throw[$Failed, $normalizeUserTag])};                      
        data = ImportString[json, "JSON"];
        data = data /. {value_List} :> value;
		If[!validUserDataQ[data],
			Message[head::srverr]; (*TODO: different message here?*)
			Throw[$Failed, $normalizeUserTag]
		];
		{usr, Lookup[data, "uuid"]}                         
    ]
    
validateUserSpecification[usr_, head_] :=	
	(Message[head::invusr, usr];Throw[$Failed, $normalizeUserTag])    		 
	
validUserDataQ[data_] := 
	MatchQ[data, _Association | _List] &&
	KeyExistsQ[data, "uuid"] && KeyExistsQ[data, "displayName"]	 && KeyExistsQ[data, "email"]
	
	
(*****************************************************************************)
(* general function for modifying permissions *)

modifyPermissions["Private", head_] :=
	modifyPermissions[$EvaluationCloudObject, "Private", head]


(*This is for special case SetPermissions["Private"].*)
modifyPermissions[obj_CloudObject, "Private", head_] :=
    Module[{opts, persInit, persNew},
        opts = Options[obj, Permissions];
        If[opts == {},
            {},
            persInit = Permissions /. opts;
            persNew = Select[persInit, #[[1]]==="Owner" &];
            Lookup[SetOptions[obj, Permissions->persNew], Permissions]
        ]
    ]
		
modifyPermissions[obj_CloudObject, "Public", head_] :=
	modifyPermissions[obj, {All-> Automatic}, head]

modifyPermissions[pers_Rule, head_] :=
	modifyPermissions[{pers}, head]

modifyPermissions[pers:{Rule[_, _] ..}, head_] :=
	modifyPermissions[$EvaluationCloudObject, pers, head]
	
modifyPermissions[obj_CloudObject, pers_Rule, head_] :=
	modifyPermissions[obj, {pers}, head]
	
modifyPermissions[obj_CloudObject, pers:{Rule[_, _] ..}, head_] :=
    Module[{persExisting, persCombined, persNew, modifiedPers},
        Catch[
            persExisting = Options[obj, Permissions];
            If[persExisting === $Failed, Return[$Failed]];
            (* Combine existing permissions with new ones.
                The new permissions come first so that they are prioritized by DeleteDuplicates. *)
            persCombined = Join[pers, Lookup[persExisting, Permissions, {}]];
            persNew = If[Length[persCombined] === 1,
                {validateUserSpecification[persCombined[[1,1]], head][[1]]->persCombined[[1,2]]}
                ,
                DeleteDuplicates[persCombined, normalizeUserSpecification[#1[[1]], head] == normalizeUserSpecification[#2[[1]], head] &]
            ];
            modifiedPers = SetOptions[obj, Permissions->persNew];
            If[modifiedPers === $Failed, Return[$Failed]];
            If[modifiedPers == {},
                {},
                Permissions /. modifiedPers
            ]
        , $normalizeUserTag]
    ]

    
(*****************************************************************************)
(* SetPermissions *)
SetPermissions[pers: Alternatives["Public","Private"]] :=
	SetPermissions[$EvaluationCloudObject, pers]
	
SetPermissions[pers_] :=
	SetPermissions[$EvaluationCloudObject, pers] 
	
SetPermissions[uri_String, pers_] := SetPermissions[CloudObject[uri], pers]	
	
SetPermissions[obj_CloudObject, pers_] := 
	modifyPermissions[obj, pers, SetPermissions]
	
SetPermissions[obj_, pers_]:=
	(Message[SetPermissions::invcloudobj, obj]; Return[$Failed])	
	
SetPermissions[args___] :=
    (ArgumentCountQ[SetPermissions, Length[DeleteCases[{args}, _Rule, Infinity]], 1, 2]; Null /; False)		

(*****************************************************************************)
(* ClearPermissions *) 

ClearPermissions[class_] := ClearPermissions[$EvaluationCloudObject, class]

ClearPermissions[uri_String, class_]:= ClearPermissions[CloudObject[uri], class]
	
ClearPermissions[obj_CloudObject, class:Alternatives[All, _String, _PermissionsGroup]] :=
	modifyPermissions[obj, class->{}, ClearPermissions]
		
ClearPermissions[obj_CloudObject, class:{Alternatives[All, _String, _PermissionsGroup]..}] :=
	modifyPermissions[obj, Replace[class, x_ :> (x -> {}), {1}], ClearPermissions]		

ClearPermissions[obj_CloudObject, class_] := 
	(Message[ClearPermissions::invusr, class]; $Failed)
	
ClearPermissions[obj_, class_]:=
	(Message[ClearPermissions::invcloudobj, obj]; Return[$Failed])	
	
ClearPermissions[args___] :=
    (ArgumentCountQ[ClearPermissions, Length[DeleteCases[{args}, _Rule, Infinity]], 1, 2]; Null /; False)				
	
(*****************************************************************************)
(* CloudPublish *)

Options[CloudPublish] := {Permissions->{All->Automatic}}

CloudPublish[] :=
    If[$CloudEvaluation,
        CloudPublish[$EvaluationCloudObject],
        CloudPublish[EvaluationNotebook[]]
    ]
    
CloudPublish[uri_String, pers:OptionsPattern[]]:= CloudPublish[CloudObject[uri], pers]    

CloudPublish[obj_CloudObject, pers:OptionsPattern[]] :=
    Module[{persVal, res},
    	persVal = OptionValue[Permissions];
        res = modifyPermissions[obj, persVal, CloudPublish];
    	If[res === $Failed, Return[$Failed]];
    	obj
    ]
	
CloudPublish[nb : notebookExprPattern, pers:OptionsPattern[]] :=
    Module[{obj},
        (* Set IconRules->None until the bug is fixed where this closes the corresponding notebook. *)
    	obj = CloudDeploy[nb, IconRules->None];
        If[obj === $Failed, Return[$Failed]];
        CloudPublish[obj, pers]
    ]
    
CloudPublish[obj_, pers:OptionsPattern[]]:=
	(Message[CloudPublish::invcloudobj, obj]; Return[$Failed])
	
CloudPublish[args___]:=
	(ArgumentCountQ[CloudPublish, Length[DeleteCases[{args}, _Rule, Infinity]], 0, 1]; Null /; False)	    

End[]

EndPackage[]
