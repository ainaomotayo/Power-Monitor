Package["CloudStore`"]

PackageImport["Macros`"]
PackageImport["GeneralUtilities`"]

PackageExport["$LoggingEnabled"]

CloudObject;

SetAttributes[log, HoldAll];
log[args___] /; $LoggingEnabled := (Print[args];);

(* Some messages are issued by CloudObject` so we need to redefine failure functions 
    to avoid interferences
*)

SetAttributes[catchFailure, HoldAll];
SetAttributes[throwFailure, HoldAll];

throwFailure[msg_MessageName, args___] := Throw[Hold[Message[msg, args]], catchFailure];
throwFailure[f_Failure] := Throw[f, catchFailure];
catchFailure[expr_] := Catch[expr, catchFailure, wrapFailure];
wrapFailure[f_Failure, tag_] := f;
wrapFailure[Hold[Message[MessageName[sym_, name_], args___]], catchFailure] := 
	Failure["CloudStore", <|"MessageTemplate" :> MessageName[sym, name], "MessageParameters" -> {args}|>];
checkFailure[expr_] := If[Macros`Exceptions`PackagePrivate`failedQ[expr], throwFailure[expr], expr]; 

CloudObject`Private`makeAuthURL["programming.devel4.wolframcloud.com"] := "https://programming.devel4.wolframcloud.com/oauth/access_token"
CloudObject`Private`makeSignatureURL["programming.devel4.wolframcloud.com"] := "http://programming.devel4.wolframcloud.com"


PackageExport["CloudStore"]

CloudStore::alreadyexists="CloudStore `1` already exists.";
CloudStore::emptyname="Empty name.";
CloudStore::badrequest="``";
CloudStore::notauthorized="``";
CloudStore::unauthorized= "``";
CloudStore::notauthenticated="Could not authenticate";
CloudStore::internalerr="``";
CloudStore::badresponse="Received an invalid response from the server: `1`: `2`";
CloudStore::badopeonpart="Cannot `1` on subpart elements.";
General::invrsp = "Invalid server response.";


SetUsage[CloudStore, 
    "CloudStore[name$] represents a CloudStore with name name$, owned by the user currently logged to the cloud.",
    "CloudStore[{userid$, name$}] represents a CloudStore with name name$, owned by a user identified by its WolframID userid$ (UUID, ID, email).",
    "CloudStore[{userid$, name$}, {part$1, part$2, $$}] represents a subpart of the CloudStore named name$, corresponding to the Part operations part$i applied to the CloudStore content.",
    "CloudStore[{userid$, name$}][part$1, part$2, $$] returns the value of a subpart of the CloudStore named name$, corresponding to the Part operations part$i applied  to the CloudStore content."
]

PackageExport["$CloudStoreBase"]

$CloudStoreBase = "CloudStore";
$AnonymousBase = "CloudStore/Anonymous";
$CloudStoreMimetype = "application/vnd.wolfram.cloudexpressionstore";

(*Attributes[CloudPart] = {HoldFirst};*)
Options[CloudStore] = {"PartProtection"->Automatic, Permissions->Automatic};
Options[CreateCloudStore] = {};


PackageExport["CreateCloudStore"]

PackageExport["CreateCloudStore"]

SetUsage[CreateCloudStore, 
    "CreateCloudStore[\"name$\", value$] creates a CloudStore named name$, and sets its value to value$.",
    "CreateCloudStore[\"name$\"] creates a CloudStore named name$, initialized to Null.",
    "CreateCloudStore[] creates an anonymous CloudStore initialized to Null."
]

CreateCloudStore[] := CreateCloudStore["anon-"<>CreateUUID[], Null];

CreateCloudStore[name_String, value_:None] := catchFailure @ Module[
    {uuid, cloudStore},
    checkLogging[];
    If[name === "", Return[$Failed], uuid = createCloudObject[name]];
    cloudStore = CloudStore[{$WolframID, name}];
    CloudStoreSet[cloudStore, value, True]
]


createCloudObject[path_String] := Module[{base, url, response},
    base = getBase[path];
    url = URLBuild[{$CloudBase, "files"}, {"path" -> StringJoin[{$WolframID, "/", base, "/", path}]}];
    log["API request to: ", url];
    (* response = URLFetch[url, 
        {"StatusCode", "Content", "Headers"},
        "Headers"->{
            "Content-Type"->$CloudStoreMimetype,
            authenticationHeader["POST"]},
        "Method"->"POST",
        "VerifyPeer"->False,
        "DisplayProxyDialog"->False]; *)
    response = CloudObject`Private`authenticatedURLFetch[url,
       {"StatusCode", "Content", "Headers"},
       "Headers"->{"Content-Type"->$CloudStoreMimetype},
       "Method"->"POST",
       "VerifyPeer"->False, "DisplayProxyDialog"->False];

    log["Response : ", response];
    parseResponse[response]
]


CloudStore[name_String, part_List] := catchFailure[
    checkLogging[];
    CloudStore[{$WolframID, name}, part]
];

CloudStore[name_String] := catchFailure @ (checkLogging[]; CloudStore[{$WolframID, name}])

CloudStore[{user_String, name_String}] := CloudStore[{user, name}, {}];

CloudStore[cs_CloudStore] := cs;

CloudStore[CloudStore[{user_String, name_String}, part_List:{}], newPart_List] := CloudStore[{user, name}, Join[part, newPart]];

CloudStore[{user_String, name_String}, part_List:{}][newPart___] := Get[CloudStore[{user, name}, part][[newPart]]];

CloudStore /: SetOptions[cs_CloudStore, Permissions->perm_] := catchFailure @ Module[
    {obj},
    obj = getCloudObject[cs];
    SetOptions[obj, Permissions->perm]
]

CloudStore /: SetOptions[cs_CloudStore, "PartProtection"->protection_] := SetPartProtection[cs, protection];

CloudStore /: HoldPattern[Options[cs_CloudStore]] := catchFailure[Join[checkFailure @ Options[cs, Permissions], checkFailure @ Options[cs, "PartProtection"]]];

CloudStore /: HoldPattern[Options[cs_CloudStore, Permissions]] := catchFailure[Options[getCloudObject[cs], Permissions]];

CloudStore /: HoldPattern[Options[cs_CloudStore, "PartProtection"]] := catchFailure[{"PartProtection" -> checkFailure @ GetPartProtection[cs]}];


PackagePrivate["SetPartProtection"]

SetPartProtection[CloudStore[{user_String, name_String}, part_List:{}, opts:OptionsPattern[]], protection_] := catchFailure @ Module[
    {base, url, response, body},
    log["Setting write protection to: ", protection];
    checkLogging[];
    base = getBase[name];
    url = URLBuild[{$CloudBase, "cloudstore", "partprotection"}, 
               {"path" -> StringJoin[{$WolframID, "/", base, "/", name}]}];
    log["API Request to: ", url];
    (* response = URLFetch[url, 
        {"StatusCode", "Content", "Headers"},
        "BodyData"->ToString[protection],
        "Headers"->{"Content-Type"->"text/plain;charset=UTF-8", authenticationHeader["PUT"]},
        "Method"->"PUT",
        "VerifyPeer"->False, "DisplayProxyDialog"->False]; *)
    response = CloudObject`Private`authenticatedURLFetch[url,
       {"StatusCode", "Content", "Headers"},
       "Headers"->{"Content-Type"->"text/plain;charset=UTF-8"},
       "BodyData"->ToString[protection],
       "Method"->"PUT",
       "VerifyPeer"->False, "DisplayProxyDialog"->False];

    body = checkFailure @ parseResponse[response];
    {"PartProtection" -> protection}
];


PackagePrivate["GetPartProtection"]

GetPartProtection[CloudStore[{user_String, name_String}, _List:{}, opts:OptionsPattern[]]] := catchFailure @ Module[
    {base, userID, url, response, body},
    log["Getting write protection for: ", user, " - ", name];
    checkLogging[];
    base = getBase[name];
    userID = getUserID[user];
    url = URLBuild[{$CloudBase, "cloudstore", "partprotection"}, 
            {"path" -> StringJoin[{userID, "/", base, "/", name}]}];
    log["API Request to: ", url];
    (* response = URLFetch[url, 
        {"StatusCode", "Content", "Headers"},
        "Headers"->{"Content-Type"->"text/plain;charset=UTF-8", authenticationHeader["GET"]},
        "Method"->"GET",
        "VerifyPeer"->False, "DisplayProxyDialog"->False]; *)
    response = CloudObject`Private`authenticatedURLFetch[url,
       {"StatusCode", "Content", "Headers"},
       "Headers"->{"Content-Type"->"text/plain;charset=UTF-8"},
       "Method"->"GET",
       "VerifyPeer"->False, "DisplayProxyDialog"->False];

    body = checkFailure @ parseResponse[response];
    ToExpression[body]
];

CloudStore /: Part[cs_CloudStore, p___] := CloudStore[cs, {p}];

CloudStore /: Get[cs_CloudStore] /; !partQ[cs] := CloudStoreGet[cs];
CloudStore /: Get[cs_CloudStore] := CloudStoreGetPart[cs];

CloudStore /: Increment[cs_CloudStore] := CloudStoreMutate[cs, Increment];
(* CloudPart /: Increment[cp_CloudPart] := CloudStoreMutate[cp, Increment]; *)
CloudStore /: Decrement[cs_CloudStore] := CloudStoreMutate[cs, Decrement];
(* CloudPart /: Decrement[cp_CloudPart] := CloudStoreMutate[cp, Decrement]; *)

CloudStore /: AddTo[cs_CloudStore, value_] := CloudStoreMutate[cs, AddTo, value];
(* CloudPart /: AddTo[cp_CloudPart, value_] := CloudStoreMutate[cp, AddTo, value]; *)
CloudStore /: SubtractFrom[cs_CloudStore, value_] := CloudStoreMutate[cs, AddTo, -value];
(* CloudPart /: SubtractFrom[cp_CloudPart, value_] := CloudStoreMutate[cp, AddTo, -value]; *)

CloudStore /: Put[expr_, cs_CloudStore] := CloudStoreSet[cs, expr];
CloudStore /: Put[expr_, cs_CloudStore/; partQ[cs]] := CloudStoreMutate[cs, Set, value];
CloudStore /: Set[cs_CloudStore /; partQ[cs], value_] := CloudStoreMutate[cs, Set, value];

(* Both Unset and Set are HoldFirst so we have to use nulpotent property of CloudStore which is not really convenient *)
CloudStore /: Unset[cs_CloudStore /; partQ[cs]] := CloudStoreMutate[cs, Delete];

(* Upvalue on Set is probably a bad idea *)
(* CloudStore /: Set[cs_CloudStore /; partQ[cs], Nothing] := CloudStoreMutate[cs, Delete]; *)

CloudStore /: AppendTo[cs_CloudStore, value_] := CloudStoreMutate[cs, AppendTo, value];
(* CloudPart /: AppendTo[cp_CloudPart, value_] := CloudStoreMutate[cp, AppendTo, value]; *)
CloudStore /: AssociateTo[cs_CloudStore, value_] := CloudStoreMutate[cs, AssociateTo, value];
(* CloudPart /: AssociateTo[cp_CloudPart, value_] := CloudStoreMutate[cp, AssociateTo, value]; *)

partQ[CloudStore[{_String, _String}, part_List, opts:OptionsPattern[]]] := part =!= {}
partQ[cs_CloudStore] := False


PackagePrivate["CloudStoreGetPart"]

CloudStoreGetPart[cs_CloudStore] := cs;

CloudStoreGetPart[CloudStore[{user_, name_}, part_List]] := catchFailure @ Module[
	{userID, base, url, encodedPart, response, body},
    checkLogging[];
    userID = checkFailure @ getUserID[user];
    base = getBase[name];
    url = URLBuild[{$CloudBase, "cloudstore", "part"}, 
            {"path" -> StringJoin[{userID, "/", base, "/", name}]}];
    log["API Request to: ", url];
    encodedPart = checkFailure @ ExportString[formatPartOperator /@ part, "RawJSON"];
    log["Encoded Part: ", encodedPart];
    (* response = URLFetch[url,
        {"StatusCode", "Content", "Headers"},
        "BodyData"->encodedPart,
        "Headers"->{"Content-Type"->"application/json", authenticationHeader["PUT"]},
        "Method"->"PUT",
        "VerifyPeer"->False, "DisplayProxyDialog"->False
    ]; *)
    response = CloudObject`Private`authenticatedURLFetch[url,
       {"StatusCode", "Content", "Headers"},
       "Headers"->{"Content-Type"->"application/json"},
       "BodyData"->encodedPart,
       "Method"->"PUT",
       "VerifyPeer"->False, "DisplayProxyDialog"->False];

    body = checkFailure @ parseResponse[response];
    checkFailure @ ToExpression[body]
];



(* PackageExport["CloudStoreDeletePart"]

CloudStoreDeletePart[cs_CloudStore] := CloudStoreMutate[cs, Delete, Null];
 *)

formatPartOperator[All]:= <|"operator"->"All"|>;
formatPartOperator[index_Integer] := <|"index"->index|>;
formatPartOperator[key_String] :=<|"key"->key|>;
formatPartOperator[Key[key_]] := <|"symbol"->ToString[key, InputForm]|>;
formatPartOperator[Span[param__]] := <|"span"->{param}|>
formatPartOperator[list_List] := <|"list"->formatPartOperator/@list|>;
formatPartOperator[a_] := throwFailure[Part::pkspec1, a];


PackagePrivate["CloudStoreMutate"]

CloudStoreMutate[cs:CloudStore[{user_String, name_String}, part_List:{}], opeHead_Symbol, value_]:= catchFailure @ Module[
	{userID, delayedOpe = isDelayedOperator[opeHead], base, url, requestBody, response},
    checkLogging[];
    log["Mutation on CloudStore[{",user,", ", name, "}] = ", " operator : '", opeHead, "' part : ", part];
    userID = checkFailure @ getUserID[user];
    (*TODO : Useless for the moment need to think about the design of delayed operation like ++*)
    (* If[TrueQ[delayedOpe], returnedValue = cs]; *)

    requestBody = <||>;
    AssociateTo[requestBody, checkFailure @ formatMutableOperator[opeHead]];
    AssociateTo[requestBody, "value"->PreprocessData[value]];
    If[Length[part] > 0, AssociateTo[requestBody, "part"->formatPartOperator /@ part]];
    base = getBase[name];
    url = URLBuild[{$CloudBase, "cloudstore", "part", "write"}, 
        {"path" -> StringJoin[{userID, "/", base, "/", name}]}];
    log["API Request to: ", url];
    requestBody = ExportString[requestBody, "RawJSON"];
    log["Encoded Body: ", requestBody];
    (* response = URLFetch[url,
        {"StatusCode", "Content", "Headers"},
        "BodyData"->requestBody,
        "Headers"->{"Content-Type"->"application/json", authenticationHeader["PUT"]},
        "Method"->"PUT", 
        "VerifyPeer"->False, "DisplayProxyDialog"->False
    ]; *)
    response = CloudObject`Private`authenticatedURLFetch[url,
       {"StatusCode", "Content", "Headers"},
       "Headers"->{"Content-Type"->"application/json"},
       "BodyData"->requestBody,
       "Method"->"PUT",
       "VerifyPeer"->False, "DisplayProxyDialog"->False];
    checkFailure @ parseResponse[response];
    cs
    (* If[TrueQ[delayedOpe], returnedValue, cs] *)
];

CloudStoreMutate[cs_CloudStore, Increment] := CloudStoreMutate[cs, Increment, 1];
CloudStoreMutate[cs_CloudStore, Decrement] := CloudStoreMutate[cs, Decrement, -1];

CloudStoreMutate[cs_CloudStore, Delete] := CloudStoreMutate[cs, Delete, Null];

formatMutableOperator[AppendTo] := "operator"->"AppendTo";
formatMutableOperator[Set] := "operator"->"Set";
formatMutableOperator[AddTo | Increment | Decrement] := "operator"->"AddTo";
formatMutableOperator[AssociateTo] := "operator"->"AssociateTo";
formatMutableOperator[Delete] := "operator"->"Delete";
formatMutableOperator[_] := $Failed;
(* ++ and -- returns the previous value but change the symbol. *)
isDelayedOperator[Increment | Decrement] := True;
isDelayedOperator[_] := False;


PackagePrivate["CloudStoreGet"]

CloudStoreGet[CloudStore[{user_String, name_String}, ___]] := catchFailure @ Module[
	{userID, base, url, response, body},
    checkLogging[];
    userID = checkFailure @ getUserID[user];
    base = getBase[name];
    url = URLBuild[{$CloudBase, "cloudstore"}, 
        {"path" -> StringJoin[{userID, "/", base, "/", name}]}];
    log["API Request to: ", url];
(*     response = URLFetch[url,
        {"StatusCode", "Content", "Headers"},
        "Headers"->{"Accept"->"application/json;charset=UTF-8, text/*, */*;q=0.9", authenticationHeader["GET"]},
        "VerifyPeer"->False, "DisplayProxyDialog"->False];
 *)    
    response = CloudObject`Private`authenticatedURLFetch[url,
       {"StatusCode", "Content", "Headers"},
       "Headers"->{"Accept"->"application/json;charset=UTF-8, text/*, */*;q=0.9"},
       "VerifyPeer"->False, "DisplayProxyDialog"->False];

    body = checkFailure @ parseResponse[response];
    checkFailure @ ToExpression[body]
];


PackageExport["DeleteCloudStore"]

SetUsage[DeleteCloudStore, 
    "DeleteCloudStore[cloudstore$] permanently deletes the CloudStore cloudstore$. The data is lost.",
    "DeleteCloudStore[\"name$\"] permanently deletes the CloudStore named name$, owned by the user currently logged to the cloud."
]



DeleteCloudStore[cs_String] := DeleteCloudStore[CloudStore[cs]];

DeleteCloudStore[cs:CloudStore[{_String, name_String}, ___]] := catchFailure @ Module[
	{uuid, url, response, body, auth},
    uuid = getCloudObjectUUID[cs];
    If[uuid === "", throwFailure[CloudStore::notauthorized, "Not authorized or missing resource."]];
    url = URLBuild[{$CloudBase, "files", uuid}];
    log["API Request to: ", url];
    (* response = URLFetch[url,
        {"StatusCode", "Content", "Headers"},
        "Method"->"DELETE",
        "Headers"->{"Accept"->"application/json;charset=UTF-8, text/*, */*;q=0.9", authenticationHeader["DELETE"]},
        "VerifyPeer"->False, "DisplayProxyDialog"->False];
     *)
    response = CloudObject`Private`authenticatedURLFetch[url,
       {"StatusCode", "Content", "Headers"},
       "Headers"->{"Accept"->"application/json;charset=UTF-8, text/*, */*;q=0.9"},
       "Method"->"DELETE",
       "VerifyPeer"->False, "DisplayProxyDialog"->False];
    parseResponse[response];
];


PackagePrivate["CloudStoreSet"]

CloudStoreSet[cs:CloudStore[{user_String, name_String}, ___], expr_, override_:False] := catchFailure @ Module[
	{userID, base, url, paramOverride = If[override, "1", "0"], response, body},
    checkLogging[];
    userID = getUserID[user];
    base = getBase[name];
    url = URLBuild[{$CloudBase, "cloudstore", "write"}, 
    {"path" -> StringJoin[{userID, "/", base, "/", name}],
     "override" -> paramOverride}];
    log["API Request to: ", url, " setting new value to ", expr];
    (* response = URLFetch[url, 
        {"StatusCode", "Content", "Headers"},
        "BodyData"->PreprocessData[expr],
        "Headers"->{"Content-Type"->"application/json;charset=UTF-8", authenticationHeader["PUT"]},
        "Method"->"PUT",
        "VerifyPeer"->False, "DisplayProxyDialog"->False]; *)
    response = CloudObject`Private`authenticatedURLFetch[url,
       {"StatusCode", "Content", "Headers"},
       "Headers"->{"Accept"->"application/json;charset=UTF-8, text/*, */*;q=0.9"},
       "BodyData"->PreprocessData[expr],
       "Method"->"PUT",
       "VerifyPeer"->False, "DisplayProxyDialog"->False];

    log["Response : ", response];
    (* Note : there is a path parameter "override = 1" that allow the user to bypass the partprotection.
        Not used for the moment though.*)

    body = parseResponse[response];
    cs
];


PackageExport["CloudStoreExistsQ"]

CloudStoreExistsQ[name_String] := CloudStoreExistsQ[CloudStore[name]];

CloudStoreExistsQ[cs_CloudStore] := catchFailure[getCloudObjectUUID[cs] =!= ""]


PackageExport["CloudStores"]

SetUsage[CloudStores, 
    "CloudStores[] returns the list of the named CloudStore owned by the user currently logged to the cloud.",
    "CloudStores[None] returns the list of the anonymous CloudStore.",
    "CloudStores[\"All\"] returns the list of all the CloudStore."
]

CloudStores::invscope= "Invalid value. Expecting \"Any\", \"Named\" or \"Anonymous\".";

CloudStores[] := CloudStores["Named"];

CloudStores[None] := CloudStores["Anonymous"];

(* CloudStores[scope_String] := catchFailure @ Module[
    {csNames, filter},
    checkLogging[];
    allCSUuid = CloudObjects["", $CloudStoreMimetype];
    log["Found ", allCSUuid//Length, " CloudObjects"];
    csNames = CloudObjectInformation[allCSUuid, "Path"];
    filter = Switch[scope,
        "Named", $CloudStorePathMatcher,
        "Any", $AnyCloudStorePathMatcher,
        "Anonymous", $AnonCloudStorePathMatcher,
        _, throwFailure[CloudStores::invscope]];
    Replace[csNames, 
     {path_?(StringMatchQ[#, filter]&) :> buildCloudStoreFromPath[path],
        _ :> Nothing}, {1}]
]; *)
CloudStores[scope_String] := catchFailure @ Module[
    {url, response, body, jsonPathList},
    checkLogging[];
    url = URLBuild[{$CloudBase, "cloudstore", "list"}, 
        {"scope" -> scope}];
    log["API Request to: ", url];
    (* response = URLFetch[url,
        {"StatusCode", "Content", "Headers"},
        "Headers"->{"Accept"->"application/json;charset=UTF-8, text/*, */*;q=0.9", authenticationHeader["GET"]},
        "VerifyPeer"->False, "DisplayProxyDialog"->False];
     *)
    response = CloudObject`Private`authenticatedURLFetch[url,
       {"StatusCode", "Content", "Headers"},
       "Headers"->{"Accept"->"application/json;charset=UTF-8, text/*, */*;q=0.9"},
       "VerifyPeer"->False, "DisplayProxyDialog"->False];
    
    body = checkFailure @ parseResponse[response];
    jsonPathList = ImportString[body, "RawJSON"];
    CloudStore[{#[[1]], #[[3]]}] & @ StringSplit[#, "/", 3] & /@ jsonPathList[[All, "path"]]
];


$CloudStorePathMatcher = RegularExpression["[^/]*/CloudStore/.*"];
$AnonCloudStorePathMatcher = RegularExpression["[^/]*/\\.CloudStore/.*"];
$AnyCloudStorePathMatcher = RegularExpression["[^/]*/\\.?CloudStore/.*"];
$AnyBaseCloudStoreMatcher = RegularExpression["[^/]*/\\.?CloudStore/"]

buildCloudStoreFromPath[path_String] := CloudStore[{$WolframID, StringCases[path, $AnyBaseCloudStoreMatcher ~~ name__ :> name, 1][[1]]}];


getCloudObject[cs_CloudStore] := Module[
    {uuid},
    uuid = getCloudObjectUUID[cs];
    If[uuid === "", throwFailure[CloudStore::notauthorized, "Not authorized or missing resource."]];
    CloudObject[URLBuild[{$CloudBase, "objects", uuid}]]
];


getCloudObjectUUID[CloudStore[{user_String, name_String}, ___]] := Module[{base, url, response},
    checkLogging[];
    base = getBase[name];
    userID = getUserID[user];
    url = URLBuild[{$CloudBase, "files"}, 
        {"path" -> StringJoin[{userID, "/", base, "/", name}]}];
    log["API Request to: ", url];
    (* response = URLFetch[url,
        {"StatusCode", "Content", "Headers"},
        "Headers"->{"Accept"->"application/json;charset=UTF-8, text/*, */*;q=0.9", 
        authenticationHeader["GET"]},
        "VerifyPeer"->False, "DisplayProxyDialog"->False]; *)
    
    response = CloudObject`Private`authenticatedURLFetch[url,
       {"StatusCode", "Content", "Headers"},
       "Headers"->{"Accept"->"application/json;charset=UTF-8, text/*, */*;q=0.9"},
       "VerifyPeer"->False, "DisplayProxyDialog"->False];

    parseResponse[response]
]

makeOAuthHeader[args___] := (CloudObject; CloudObject`Private`makeOAuthHeader[args]);
(* Notabene : makeOAuthHeader ends up calling makeSignatureBase[{non_,time_},url_String,method_String] but url is not used. *)
authenticationHeader[method_String] := ("Authorization" -> makeOAuthHeader[System`$CloudBase <> "/cloudstore", method]);

isAnonymousUser[$AnonymousUser] := True;
isAnonymousUser[_] := False;


checkLogging[] := If[
	log["Checking authentication status"];
	Not[TrueQ[CloudObject`Private`authenticatedQ[]]],
    log["Not authenticated, logging in."];
	With[{res=CloudConnect[]}, (* TODO: what to do for stand-alone kernel? *)
		If[UnsameQ[res, $WolframID], throwFailure[General::notauth]]
    ]
];


getUserID[user_String] /; uuidQ[user] := If[!cachedUUIDQ[user], getUserIDFromServer[user], getIDFromCache[user]];
getUserID[user_String] /; !uuidQ[user] := user;

getUserIDFromServer[uuid_String] /; uuidQ[uuid] && !cachedUUIDQ[uuid] := Module[
    {url, response, parsedResponse, email},
    checkLogging[];
    url = URLBuild[{$CloudBase, "users", "user-"<>uuid, "info"}];
    log["API Request to: ", url];
    (* response = URLFetch[url,
        {"StatusCode", "Content", "Headers"},
        "Headers"->{"Accept"->"application/json;charset=UTF-8, text/*, */*;q=0.9", 
        authenticationHeader["GET"]},
        "VerifyPeer"->False, "DisplayProxyDialog"->False]; *)

    response = CloudObject`Private`authenticatedURLFetch[url,
       {"StatusCode", "Content", "Headers"},
       "Headers"->{"Accept"->"application/json;charset=UTF-8, text/*, */*;q=0.9"},
       "VerifyPeer"->False, "DisplayProxyDialog"->False];

    log["Response : ", response];
    If[response[[1]] != 200, 
        throwFailure[CloudStore::badrequest, "Failed to find user corresponding to uuid: " <> uuid ],
        parsedResponse = ImportString[response[[2]], "RawJSON"];
        email = parsedResponse[["email"]];
        cacheUUID[uuid, email];
        uuid
    ]
];

getBase[name_String] /; anonymousQ[name] := $AnonymousBase;
getBase[name_String] := $CloudStoreBase;

$REUUID = RegularExpression["[a-f0-9]{8}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{12}"];
uuidQ[user_String] := StringMatchQ[user, $REUUID];

$REAnonymousCloudStoreName = RegularExpression["anon-[a-f0-9]{8}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{12}"];
anonymousQ[name_String] := StringMatchQ[name, $REAnonymousCloudStoreName];
anonymousQ[CloudStore[{_String, name_String}, ___]] := anonymousQ[name];

$cachedUserUUID = <||>;
cachedUUIDQ[uuid_String] := KeyExistsQ[$cachedUserUUID, uuid];
getIDFromCache[uuid_String] := $cachedUserUUID[[uuid]];
cacheUUID[uuid_String, user_String] := AssociateTo[$cachedUserUUID, uuid->user];

parseResponse[{statusCode_Integer, body_String, headers_List}] := Module[
    {contentType},
    log["Status code: ", statusCode, "\nHeaders: ", headers, "\nContentData: ", body];
    If[statusCode === 200, 
        body,
        contentType = "Content-Type" /. (Rule @@@ headers); (* From CloudObject` *)
        If[contentType == "application/json",
            log["Json error response."];
            parseJSONErrorResponse[{statusCode, body}],
            log["Plain text error response."];
            parsePlainTextErrorResponse[{statusCode, body}]
        ]
    ]

]
parseResponse[resp__] := (log["Error response from server:\n",resp];throwFailure[General::invresp];)

parsePlainTextErrorResponse[{statusCode_Integer, body_String}] := Switch[statusCode,
    200, body,
    400, throwFailure[CloudStore::badrequest, body],
    401, throwFailure[CloudStore::unauthorized, body],
    403, throwFailure[CloudStore::notauthorized, body],
    429, throwFailure[msghd::rejreq],
    500, throwFailure[CloudStore::internalerr, "An internal error occured."],
    503, throwFailure[msghd::unavailable],
    _, throwFailure[CloudStore::badresponse, statusCode, body]
];
parseJSONErrorResponse[{statusCode_Integer, json_String}] := Module[
    {parsedJSON},
    parsedJSON = ImportString[json, "RawJSON"];
    If[KeyExistsQ[parsedJSON, "errorCode"],
        parsePlainTextErrorResponse[{statusCode, parsedJSON[["errorCode"]]}],
        throwFailure[CloudStore::badresponse, json];
    ]
]


PackageExport["PreprocessData"]

SetUsage[PreprocessData,
    "PreprocessData[expr$] compresses any sub-expression of expr$ bigger than a threshold (~1kbit) that is not natively handled by CloudStore."
]

$maxDepth = 1;
PreprocessData[expr_] := ToString[depthCompress[expr], InputForm] // removeInputFormHold

removeInputFormHold[toString_String] := StringTake[toString, {6, -2}];

depthCompress[expr_] := Block[{$maxDepth = Depth[expr]},
    depthCompress[Hold[expr], 1] /. CD :> CompressedData
];
depthCompress[expr_, depth_Integer] /; $maxDepth < depth := expr;
depthCompress[expr_, depth_Integer] := depthCompress[compress[expr, depth], depth+1];

$CompressableHeads = Except[_String | _List | _Hold | _Association];
$MaxUncompressed = 1024;
compress[expr_, depth_Integer] := Replace[expr,
    (f:$CompressableHeads /; ByteCount[f] >= $MaxUncompressed) :> Block[{}, CD @ Compress[f]/;True]
    ,{depth}];


CloudStore /: MakeBoxes[cs:CloudStore[{_String, _String}, _List], StandardForm] := 
	MakeCloudStoreBoxes[cs];

MakeCloudStoreBoxes[cs:CloudStore[{user_String, name_String}, part_List:{}]] := catchFailure @ Module[
	{ownCS, boxName, boxPart, boxUser, displayedBoxes, hiddenBoxes = {}},
	ownCS = user === $WolframID;

	(* If[ownCS, boxName = BoxForm`SummaryItem[{"Name : ", name}],
		boxName = BoxForm`SummaryItem[{"\[FivePointedStar]Name: ", name}]
	]; *)
    boxName = BoxForm`SummaryItem[{"Name : ", name}];
	If[partQ[cs], 
        boxPart = BoxForm`SummaryItem[{"Part : ", ToString[part]}];
		displayedBoxes = {boxName, boxPart};
		,
		displayedBoxes = {boxName};
    ];

    If[ownCS, boxUser = BoxForm`SummaryItem[{"User : ", "self"}],
        boxUser = BoxForm`SummaryItem[{"User : ", user}]];
    If[partQ[cs], AppendTo[hiddenBoxes, boxUser], AppendTo[displayedBoxes, boxUser]];
    
	BoxForm`ArrangeSummaryBox[CloudStore, cs, 
        Style["\[CapitalSigma]", Directive["Message", 35]], 
        displayedBoxes, hiddenBoxes, StandardForm]
]

PackagePrivate["RawDump"]

SetUsage[RawDump,
    "RawDump[cloudstore$] return all the Redis entries related to the CloudStore cloudstore$.",
    "RawDump[name$] return all the Redis entries related to the CloudStore of name name$."
];


RawDump[symbol_String]:= RawDump[CloudStore[symbol]];

RawDump[CloudStore[{user_String, path_String}, ___]] := catchFailure @ Module[
    {base, url, response, body},
    checkLogging[];
    base = getBase[path];
    userID = checkFailure @ getUserID[user];
    url = URLBuild[{$CloudBase, "cloudstore", "rawdump"},
        {"path" -> StringJoin[{userID, "/", base, "/", path}]}];
    log["API Request to: ", url];
 (*    response = URLFetch[url,
        {"StatusCode", "Content", "Headers"},
        "Headers"->{"Accept"->"application/json;charset=UTF-8", authenticationHeader["GET"]},
        "VerifyPeer"->False, "DisplayProxyDialog"->False];
         *)
    response = CloudObject`Private`authenticatedURLFetch[url,
       {"StatusCode", "Content", "Headers"},
       "Headers"->{"Accept"->"application/json;charset=UTF-8"},
       "VerifyPeer"->False, "DisplayProxyDialog"->False];

    body = checkFailure @ parseResponse[response];
    checkFailure @ Dataset[checkFailure @ ToExpression[body]]
];

SetAttributes[CloudStore, {ReadProtected}];
Protect[CloudStore];

