BeginPackage["CloudObject`"]

System`CloudObjectInformation;
System`CloudObjectInformationData;
System`CloudObjects;

CloudObject`CloudObjectUUIDForm;


Begin["`Private`"]

(* CloudObjects *)

Options[CloudObjects] = {"Directory" -> Automatic, "Type" -> All}

queryTypeValue["CloudEvaluation"] := expressionMimeType["CloudEvaluation"];
queryTypeValue["Expression"|Expression] := expressionMimeType[Expression];
queryTypeValue["Notebook"|Notebook] := expressionMimeType[Notebook];
queryTypeValue["ExternalBundle"|ExternalBundle] := expressionMimeType[ExternalBundle];
queryTypeValue[type_String] := formatToMimeType[type];
queryTypeValue[symbol_Symbol] := expressionMimeType[symbol];
queryTypeValue[All] = All;
queryTypeValue[Verbatim[Alternatives][types___]] := Map[queryTypeValue, {types}];
queryTypeValue[list_List] := Map[queryTypeValue, list];
queryTypeValue[_] = $Failed;

iCloudObjects[cloud_String, path_, opts:OptionsPattern[CloudObjects]] :=
    Module[{query = {}, typevalue, type, obj, uuid},
        typevalue = OptionValue[CloudObjects, {opts}, "Type"];
        type = queryTypeValue[typevalue];
        If[type === $Failed,
            Message[CloudObjects::invtype, typevalue];
            type = All;
        ];
        If[MatchQ[type, _List],
            log["CloudObjects for multiple types `1`", type, DebugLevel->3];
            Return[Sort[Flatten[iCloudObjects[cloud, path, "Type"->#, opts]& /@ type]]];
        ];
        (* It would be as simple as the following, but the server does not actually support path=dir/* yet. *)
        (*If[type =!= All, AppendTo[query, "type" -> type]];
        responseToString[execute[cloud, "GET", {"files"}, Parameters->query], CloudObjects] /. (
            result_String :>
                uuidListingToCloudObjects[result]
        )*)
        If[type =!= All || path === All || path === "",
            log["CloudObjects: type=`1`, path=`2`", type, path, DebugLevel->3];
            If[type =!= All, AppendTo[query, "type" -> type]];
            If[path =!= All, AppendTo[query, "path" -> path]];
            responseToString[execute[cloud, "GET", {"files"}, Parameters->query], CloudObjects] /. (
                result_String :>
                    uuidListingToCloudObjects[result]
            ),
        (* No type is given. Use a different API which is actually working on the server. *)
            log["CloudObjects: path=`1`", path, DebugLevel->3];
            obj = CloudObject[cloud <> "/objects/" <> If[StringMatchQ[path, ___ ~~ "/*"], StringDrop[path, -2], path]];
            log["Directory object: `1`", obj, DebugLevel->3];
            uuid = getCloudAndUUID[obj][[2]];
            log["Directory UUID: `1`", uuid, DebugLevel->3];
            If[!UUIDQ[uuid], Message[CloudObject::cloudnf, dir]; Return[$Failed]];
            responseToString[execute[cloud, "GET", {"files", uuid}], CloudObjects] /. {
                result_String :>
                    uuidListingToCloudObjects[result]
            }
        ]
    ]

CloudObjects[All, opts:OptionsPattern[]] := iCloudObjects[$CloudBase, All, opts]
CloudObjects[None, opts:OptionsPattern[]] := iCloudObjects[$CloudBase, "", opts]
CloudObjects[obj_CloudObject, opts:OptionsPattern[]] :=
    Module[{cloud, path, name},
        {cloud, path} = getCloudAndPathList[obj];
        name = StringJoin[Riffle[Join[path, {"*"}], "/"]];
        iCloudObjects[cloud, name, opts]
    ]

CloudObjects[Automatic, opts:OptionsPattern[]] := CloudObjects[CloudDirectory[], opts]
CloudObjects[dir_String, opts:OptionsPattern[]] := CloudObjects[CloudObject[dir], opts]

(* If no directory is given as positional argument, take the option value (with default Automatic). *)
CloudObjects[opts:OptionsPattern[]] := CloudObjects[OptionValue["Directory"], opts]

(* Expand an Association out into a list of rules, which get treated as options. *)
CloudObjects[before___, assoc_Association, after___] := CloudObjects[before, Normal[assoc], after]

CloudObjects[dir_, type:(_String|_Symbol), opts:OptionsPattern[]] := CloudObjects[dir, "Type" -> type, opts]

(* List objects *)
CloudObjectsByType[contentType_String] :=
	Module[{response, uuids},
		response = responseToString @ execute[$CloudBase, "GET", {"files"},
			Parameters->{"type" -> contentType}]; (* TODO support multiple mime types, e.g. to handle notebooks *)
		If[!StringQ[response], Return[$Failed]];
		uuids = Map[FileNameTake[#, -1]&, StringSplit[response]];
		Map[cloudObjectFromUUID, uuids]
	]

unnamedCloudObjects[] :=
	execute[$CloudBase, "GET", {"files"}, Parameters -> {"path" -> ""}] /. {
		err_HTTPError :> ($Failed), (* TODO what message should this issue? *)
		{_, bytes_List} :>
			uuidListingToCloudObjects[bytes]
	}

uuidListingToCloudObjects[bytes_List] :=
	uuidListingToCloudObjects[FromCharacterCode[bytes]]

uuidListingToCloudObjects[listing_String] :=
	Sort[Cases[Map[uuidListEntryToCloudObject, StringSplit[listing]], _CloudObject]]

uuidListEntryToCloudObject[text_String] :=
	StringDrop[text, 7] /. {
		uuid_?UUIDQ :> cloudObjectFromUUID[uuid],
		_ :> $Failed
	}

(* CloudObjectInformation *)

CloudObjectInformation[obj_CloudObject] := cloudObjectInformation[obj]

CloudObjectInformation[obj_CloudObject, "UUID"] :=
	With[{result = Quiet[getCloudAndUUID[obj]]},
		If[MatchQ[result, {_String, _?CloudObject`UUIDQ}],
			Last[result],
		(* Else *)
			Message[CloudObjectInformation::cloudnf, obj];
			$Failed
		]
	]

CloudObjectInformation[obj_CloudObject, property_String] :=
	cloudObjectInformation[obj, CloudObjectInformation, "Elements" -> property]

CloudObjectInformation[obj_CloudObject, properties:{_String ..}] :=
	cloudObjectInformation[obj, CloudObjectInformation, "Elements" -> properties]

CloudObjectInformation[{}] := {}

CloudObjectInformation[objects:{_CloudObject ..}] := 
    cloudObjectInformation[objects, CloudObjectInformation]

CloudObjectInformation[objects:{_CloudObject ..}, property_String] := 
    cloudObjectInformation[objects, CloudObjectInformation, "Elements" -> property]

CloudObjectInformation[objects:{_CloudObject ..}, properties:{_String ..}] :=
	cloudObjectInformation[objects, CloudObjectInformation, "Elements" -> properties]

CloudObjectInformation[type_String, property_String] := 
	cloudObjectInformation[type, CloudObjectInformation, "Elements" -> property]

CloudObjectInformation[type_String, properties:{_String ..}] := 
	cloudObjectInformation[type, CloudObjectInformation, "Elements" -> properties]

Options[cloudObjectInformation] = {"Elements" -> Automatic}

cloudObjectInformation[obj_CloudObject, msghd_:CloudObjectInformation, opts:OptionsPattern[]] :=
	Module[{cloud, uuid, json, allinfo, files},
		{cloud, uuid} = getCloudAndUUID[obj];
		If[!(StringQ[cloud] && UUIDQ[uuid]), 
			Return[$Failed]
		];

		json = execute[cloud, "GET", {"files", uuid, "info"}] /. {
			HTTPError[404, ___] :> (Message[msghd::cloudnf, obj]; Return[$Failed]),
			{_String, content_List} :>
				($lastInfoJSON = FromCharacterCode[content]),
			other_ :> (Message[msghd::srverr]; Return[$Failed])
		};

		allinfo = JSONTools`FromJSON[json];
		If[!ListQ[allinfo],
			Message[msghd::srverr];
			Return[$Failed]
		];

		files = Lookup[allinfo,
			If[KeyExistsQ[allinfo, "files"], "files", "directoryListing"]];
		If[files === {},
			Message[msghd::srverr]; (* internal error -- info about directories is broken *)
			Return[$Failed]
		];
		
		objectInfo[First[files], "Elements" -> OptionValue["Elements"]]
	]

cloudObjectInformation[objects:{_CloudObject ..}, msghd_:CloudObjectInformation, opts:OptionsPattern[]] :=
    Module[{args, bad, uuids, cloud, elements, fields, json, files},

        args = Map[Prepend[getCloudAndUUID[#], #]&, objects];

		(* test for objects that cannot be resolved *)
        bad = SelectFirst[args, ! MatchQ[#, {_, _String, _?CloudObject`UUIDQ}] &];

        If[Head[bad] =!= Missing,
            Message[msghd::cloudnf];
            Return[$Failed]
        ];
        
        uuids = Map[Last, args];
        cloud = args[[1, 2]];
        elements = OptionValue["Elements"];
        fields = resolveInfoFields[elements];
        
        json = execute[cloud, "GET", {"files"}, 
            Parameters -> {"fields" -> commaSeparated[fields], (* indicate v2 of the API, to return JSON *)
                "uuid" -> commaSeparated[uuids]}] /. {
            HTTPError[404, ___] :> (
                Message[msghd::cloudnf, objects]; 
                Return[$Failed]),
            {_String, content_List} :>
                ($lastInfoJSON = FromCharacterCode[content]),
            other_ :> ($lastInfoResult = other; Message[msghd::srverr]; 
                Return[$Failed])
        };

        files = JSONTools`FromJSON[json];
        If[!ListQ[files],
            Message[msghd::srverr];
            Return[$Failed]
        ];

        Map[objectInfo[#, "Elements" -> elements]&, files]
    ]

cloudObjectInformation[type_String, msghd_:CloudObjectInformation, opts:OptionsPattern[]] := 
	Module[{elements, fields, json, info},

        elements = OptionValue["Elements"];
        fields = resolveInfoFields[elements];
		
        json = execute[$CloudBase, "GET", {"files"}, 
            Parameters -> {"fields" -> commaSeparated[fields], (* indicate v2 of the API, to return JSON *)
                "type" -> formatToMimeType[type]}] /. {
            HTTPError[404, ___] :> (
                Message[msghd::cloudnf, objects]; 
                Return[$Failed]),
            {_String, content_List} :>
                ($lastInfoJSON = FromCharacterCode[content]),
            other_ :> (
            	$lastInfoResult = other;
	            Message[msghd::srverr]; 
                Return[$Failed])
        };

        info = JSONTools`FromJSON[json];
        If[!ListQ[info],
            Message[msghd::srverr];
            Return[$Failed]
        ];

        Map[objectInfo[#, "Elements" -> elements]&, info]
	]

commaSeparated[elts_List] := StringJoin[Riffle[elts, ","]]

resolveInfoFields[Automatic] := {"all"}

resolveInfoFields[field_String] := resolveInfoFields[{field}]

resolveInfoFields[fields_List] := 
	Map[Lookup[$jsonFields, #, handleUnknownProperty[#]]&, fields]

handleUnknownProperty[propery_String] := 
(
	Message[CloudObjectInformation::noprop, property];
	$Failed
)

Options[objectInfo] = {"Elements" -> Automatic}

objectInfo[info_List, opts:OptionsPattern[]] := objectInfo[Association[info], opts]

objectInfo[info_Association, OptionsPattern[]] := 
    Module[{elements = OptionValue["Elements"], mimetype = Lookup[info, "mimeType", None], 
    	displayName, infoData = <||>},
        displayName = info["displayName"] /. {_Missing :> info["name"]};
        
		Do[
			infoData[elt] = 
	        <|
	            "UUID" -> info["uuid"],
	            "Path" -> info["path"] /. {Null -> None},
	            "Name" -> displayName,
	            "DisplayName" -> displayName,
	            "OwnerWolframUUID" -> info["ownerUUID"],
	            "MimeType" -> mimetype,
	            "FileType" ->
	                If[mimetype === "inode/directory" || bundleMimeTypeQ[mimetype],
	                    Directory,
	                    File
	                ],
	            "FileByteCount" :> FromDigits[info["fileSize"]],
	            "Created" :> DateObject[info["created"]],
	            "LastAccessed" :> DateObject[info["lastAccessed"]],
	            "LastModified" :> DateObject[info["lastModified"]],
	            "Permissions" :> fromServerPermissions[info["filePermission"]],
	            "Active" -> info["active"]
	        |>[elt],
			{elt, Switch[elements,
				Automatic, Keys[$jsonFields],
				_String, {elements},
				_, elements]
			}
		];
 
		Switch[elements,
			Automatic, System`CloudObjectInformationData[infoData],
			_String, First[Values[infoData]],
			_List, infoData
		]
    ]

$jsonFields = <|
    "UUID" -> "uuid",
    "Path" -> "path",
    "Name" -> "displayName",
    "DisplayName" -> "displayName",
    "OwnerWolframUUID" -> "ownerUUID",
    "MimeType" -> "mimeType",
    "FileType" -> "mimeType",
    "FileByteCount" -> "fileSize",
    "Created" -> "created",
    "LastAccessed" -> "lastAccessed",
    "LastModified" -> "lastModified",
    "Permissions" -> "filePermission",
    "Active" -> "active"
|>

(* TODO: This should probably be exposed through CloudObjectInformation, and maybe as a sub value of CloudObject.
 Maybe also as Normal[obj]. *)
CloudObjectUUIDForm[obj : CloudObject[url_, opts___]] :=
    Module[{cloud, uuid},
        {cloud, uuid} = getCloudAndUUID[obj];
        If[!StringQ[cloud], Return[$Failed]];
        CloudObject[URLBuild[{cloud, $CloudObjectsRoot, uuid}], opts]
    ]

CloudObjectUUIDForm[group_PermissionsGroup] := PermissionsGroup @@ CloudObjectUUIDForm[CloudObject @@ group]

End[]

EndPackage[]
