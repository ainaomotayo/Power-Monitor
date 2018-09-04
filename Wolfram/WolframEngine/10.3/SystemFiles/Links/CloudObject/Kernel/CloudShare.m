(* Mathematica Package *)

BeginPackage["CloudObject`"]
(* Exported symbols added here with SymbolName::usage *)  

System`CloudShare;
System`SharingList;

Begin["`Private`"] (* Begin Private Context *) 

Unprotect[System`CloudShare];


CloudShare[user:Alternatives[_String, _PermissionsGroup]]:= CloudShare[{user}];

CloudShare[users:{Alternatives[_String, _PermissionsGroup]..}]:= 
	If[$CloudEvaluation,
        CloudShare[$EvaluationCloudObject, users],
        CloudShare[EvaluationNotebook[], users]
    ]

CloudShare[obj_CloudObject, user:Alternatives[_String, _PermissionsGroup]]:= CloudShare[obj, {user}]
	
CloudShare[obj_CloudObject, users:{Alternatives[_String, _PermissionsGroup]..}]:=
	CloudShare[obj, Map[# -> Automatic &, users]]
	
CloudShare[obj_CloudObject, per:Rule[_, _]] := CloudShare[obj, {per}]	
	
CloudShare[obj_CloudObject, pers:{Rule[_, _] ..}] :=
    Module[ {cloud, uuid, sharees, addingSharees, persOld, persList, persNew},
     Catch[	
        {cloud, uuid} = Quiet[getCloudAndUUID[obj]];
        If[ !(StringQ[cloud] && UUIDQ[uuid]),
            Message[CloudObject::cloudnf, obj];
            Return[$Failed]
        ];
        sharees = Keys[pers]; 
        addingSharees = addSharees[cloud, uuid, sharees];
        If[addingSharees===$Failed, Return[$Failed]];
        persOld = Options[obj, Permissions];
        If[ !MatchQ[persOld, {_Rule...}], Return[$Failed]];
        persList = Lookup[persOld, Permissions];
        persNew = addPermissions[persList, pers];
        SetPermissions[obj, persNew ];        
        obj
     , 
     $normalizeUserTag]    
    ]	
	
CloudShare[nb : notebookExprPattern, users_]:=
	Module[{obj},
        (* Set IconRules->None until the bug is fixed where this closes the corresponding notebook. *)
    	obj = CloudDeploy[nb, IconRules->None];
        If[obj === $Failed, Return[$Failed]];
        CloudShare[obj, users]
    ]
    
CloudShare[obj_, users_]:=
	(Message[CloudShare::invcloudobj, obj]; $Failed)    	
	
CloudShare[args___]:= 
	(ArgumentCountQ[CloudShare, Length[DeleteCases[{args}, _Rule, Infinity]], 1, 2]; Null /; False) 

getSharees[obj_CloudObject, key_String] := getSharees[obj, {key}]

getSharees[obj_CloudObject, keys_List]:=
 	Module[{cloud, uuid},
        {cloud, uuid} = getCloudAndUUID[obj];
        If[!(StringQ[cloud] && UUIDQ[uuid]),
            Message[CloudObject::cloudnf, obj];
            Return[$Failed];
        ];
        getSharees[cloud, uuid, keys]
    ]
    
getSharees[cloud_, uuid_, keys_]:=
 	Module[{json, data},
        json = execute[cloud, "GET", {"files", uuid, "sharees"}] /. { 
        	{_, bytes_List} :> FromCharacterCode[bytes],
            HTTPError[404, ___] :> (Message[CloudObject::cloudnf, obj]; 
            						Return[$Failed]),
            other_ :> (checkError[other, CloudShare]; 
            		   Return[$Failed])};
        data = ImportString[json, "JSON"];
        If[data === {}, 
        	{},
        	Lookup[data, keys] ]
        
    ]    

addSharees[obj_CloudObject, users:{Alternatives[_String, _PermissionsGroup]..}]:=
	Module[{cloud, uuid},
		{cloud, uuid} = Quiet[getCloudAndUUID[obj]];
		addSharees[cloud, uuid, users]		
	]

addSharees[cloud_, uuid_, users:{Alternatives[_String, _PermissionsGroup]..}] :=
    Module[ {normalizeGroup, usersNew, body}, 
    	normalizeGroup = Map[validateUserSpecification[#, CloudShare]&, Select[users, MatchQ[#, _PermissionsGroup]& ]];
    	usersNew = users/.Apply[Rule, normalizeGroup, {1}];
    	body = ExportString[usersNew,"JSON"];
        execute[cloud, "POST", {"files", uuid, "sharees"}, Body -> body] /. { 
        	{_, bytes_List} :> FromCharacterCode[bytes],          
            HTTPError[404, ___] :> (Message[CloudObject::cloudnf, obj];
                                    Return[$Failed]),                       
            HTTPError[400, content_List,"application/json"] :> (invalidShareeError[content, Reverse/@normalizeGroup];
                                    Return[$Failed]),                       
            other_ :> (checkError[other, CloudShare];
                       Return[$Failed])
            }
    ]
   
addSharees[___] := $Failed    

(*This clears previousr sharees and replace it with new*) 
setSharees[obj_CloudObject, users:{Alternatives[_String, _PermissionsGroup]..}]:=
	Module[{cloud, uuid},
		{cloud, uuid} = Quiet[getCloudAndUUID[obj]];
		setSharees[cloud, uuid, users]		
	]
	   
setSharees[cloud_, uuid_, users:{Alternatives[_String, _PermissionsGroup]..}] :=
    Module[ {normalizeGroup, usersNew, body},
    	normalizeGroup = Map[validateUserSpecification[#, CloudShare]&, Select[users, MatchQ[#, _PermissionsGroup]& ]];
    	usersNew = users/.Apply[Rule, normalizeGroup, {1}];
    	body = ExportString[usersNew,"JSON"]; 
        execute[cloud, "PUT", {"files", uuid, "sharees"}, Body -> body] /. {
        	{_, bytes_List} :> FromCharacterCode[bytes],            
            HTTPError[404, ___] :> (Message[CloudObject::cloudnf, obj];
                                    Return[$Failed]),                      
            HTTPError[400, content_List,"application/json"] :> (invalidShareeError[content, Reverse/@normalizeGroup];
                                    Return[$Failed]),                        
            other_ :> (checkError[other, CloudShare];
                       Return[$Failed])
            }
    ]
    
setSharees[___] := $Failed 

deleteAllSharees[obj_CloudObject]:=
	Module[{cloud, uuid},
		{cloud, uuid} = Quiet[getCloudAndUUID[obj]];
		deleteAllSharees[cloud, uuid]		
	]       
    
deleteAllSharees[cloud_, uuid_] := (**DELETE removes sharees from the sharee list*)
    execute[cloud, "DELETE", {"files", uuid, "sharees"}] /. {
        	HTTPError[204, ___] :> Return[Null],
        	HTTPError[404, ___] :> (Message[CloudObject::cloudnf, obj];
                                    Return[$Failed]),
            other_ :> (checkError[other, CloudShare];
                       Return[$Failed])
            }
    
deleteAllSharees[___] := $Failed 

addPermissions[users:{Alternatives[_String, _PermissionsGroup, _Rule]...}, addPers_] :=
    Module[ {pers, persJoin, persGather},
    	pers = Replace[users, x : Alternatives[_String, _PermissionsGroup] :> (x -> Automatic), {1}];
    	persJoin = Join[pers, addPers];
        persGather = GatherBy[persJoin, normalizeUserSpecification[#[[1]], CloudShare] &];
        Map[#[[1, 1]] -> DeleteDuplicates[Flatten[#[[All, 2]]]] &, persGather]
    ]  
    
addPermissions[___] := $Failed      
	
invalidShareeError[users_, normalizedGroup_] :=
    Module[{failedUsers},
    	failedUsers = Complement[users, normalizedGroup];
    	Message[CloudShare::invusr, failedUsers]
    ]
	
convertFromServerPermissionsGroup[displayName_, uuid_] :=
    Module[ {},
        If[ StringMatchQ[uuid, "user-" ~~ __],
            displayName,
            PermissionsGroup[displayName]
        ]
    ]			               

Protect[System`CloudShare];

End[] (* End Private Context *)

EndPackage[]
