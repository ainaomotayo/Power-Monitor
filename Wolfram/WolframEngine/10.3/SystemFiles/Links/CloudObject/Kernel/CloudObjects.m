BeginPackage["CloudObject`"]

System`CloudObject;
System`IconRules;
System`MetaInformation;

CloudDebug::usage = "CloudDebug[expr] evaluates expr with cloud debugging enabled."

Begin["`Private`"]

userUUIDPrefix = "user-";

$CloudDebug = False;
$CloudDebugLevel = 2;

$CloudObjectsRoot = "/objects";
$CloudFilesRoot = "/files";

Options[log] = {DebugLevel -> 1};
Attributes[log] = {HoldRest};
log[msg_String, Shortest[args___], OptionsPattern[]] :=
    If[$CloudDebug && $CloudDebugLevel >= OptionValue[DebugLevel],
        Print[ToString @ StringForm[msg, args]]
    ]

DebugLevel::usage = "DebugLevel is an option to log.";

SetAttributes[CloudDebug, HoldFirst];
Options[CloudDebug] = {DebugLevel -> 1};
CloudDebug[expr_, opts:OptionsPattern[]] := 
	Block[{$CloudDebug = True, $CloudDebugLevel = OptionValue[DebugLevel]},
	    expr
	]

extractURL[CloudObject[uri_, ___]] := uri

parseURI[uri_, currentCloud_, currentUser_, rootDir_, currentDir_, objectsRoot_, filesRoot_] :=
    Module[{protocol, host, port, pathname, search,
            baseobj, basepath, basesearch,
            cloud = currentCloud, user = currentUser, cloudprefix, request,
    	    uuid = None, path = None, ext = None, extraPath = {}
        },
        {protocol, host, port, pathname, search} = URLParse[uri, {"Scheme", "Domain", "Port", "Path", "Query"}];
        log["Parsed URL: `1`", {protocol, host, port, pathname, search}, DebugLevel -> 3];
        If[protocol === None,
            baseobj = If[Length[pathname] >= 1 && First[pathname] === "",
                pathname = Rest[pathname]; rootDir,
                currentDir
            ];
            {protocol, host, port, basepath, basesearch} = URLParse[extractURL[ReleaseHold[baseobj]], {"Scheme", "Domain", "Port", "Path", "Query"}];
            pathname = Join[basepath, pathname];
        ];
        log["Parsing URI `1`", {protocol, host, port, pathname, search}, DebugLevel -> 3];
        Switch[protocol,
            "wolfram",
                If[host === None && Length[pathname] >= 1,
                	{uuid, ext} = ParseUUID[First[pathname]];
                    extraPath = Rest[pathname]
                ],
            "user",
                If[host === None && Length[pathname] >= 1,
                	user = First[pathname];
                    path = Rest[pathname]
                ],
            "http" | "https" | None,
                If[protocol =!= None && host =!= None,
                    cloud = URLBuild[{"Scheme" -> protocol, "Domain" -> host, "Port" -> port}];
                    If[pathname === {} || First[pathname] =!= "",
                        PrependTo[pathname, ""]
                    ]
                ];
                If[protocol === None && Length[pathname] >= 1,
                    If[First[pathname] =!= "",
                        (* relative (non-absolute) path *)
                        pathname = Join[rootPath, pathname]
                    ]
                ];
                log["Path `1` (object root: `2`, files root: `3`)", pathname, objectsRoot, filesRoot, DebugLevel -> 3];
                If[MatchQ[pathname, {___, objectsRoot | filesRoot, __}],
                    {cloudprefix, request, pathname} = Replace[pathname,
                    	{Shortest[prefix___], type : objectsRoot | filesRoot, rest___} :> {{prefix}, type, {rest}}
                    ];
                    If[StringMatchQ[cloud, ___ ~~ "/"] && cloudprefix =!= {} && First[cloudprefix] === "",
                        cloud = StringDrop[cloud, -1]
                    ];
                    cloud = cloud <> StringJoin[Riffle[cloudprefix, "/"]];
	                If[request === filesRoot,
				        request = objectsRoot;
				        AppendTo[search, "view" -> "file"];
				    ];
                    If[UUIDQ[First@pathname],
                    (* URI is of the form .../objects/<uuid>..." *)
                        log["UUID-based URI: `1`", pathname, DebugLevel -> 3];
                        {uuid, ext} = ParseUUID[First@pathname];
                        extraPath = Rest[pathname],
                    (* URI is of the form .../objects/<username>..." *)
                        log["Username-based URI `1`", pathname, DebugLevel -> 3];
                        user = First[pathname];
                        If[user === "~",
                            If [currentUser === None,
                                user = (CloudConnect[];$WolframUUID),
                                user = currentUser
                            ];
                            user = If[user === None || user === $Failed || !StringQ[user], "", userUUIDPrefix <> user]
                        ];
                        log["User: `1`", user, DebugLevel -> 3];
                        path = Rest[pathname];
                    ]
                ]
        ];
        log["Parsed URI: `1`", {cloud, uuid, user, path, ext, extraPath, search}, DebugLevel -> 3];
        {cloud, uuid, user, path, ext, extraPath, search}
    ]

parseURI[uri_, base_] := Module[{},
	parseURI[uri, base, $WolframUUID,
	            Hold[$CloudRootDirectory],
                Hold[$CloudDirectory],
	            StringSplit[$CloudObjectsRoot, "/", All][[2]],
	            StringSplit[$CloudFilesRoot, "/", All][[2]]
	        ]
]

parseURI[uri_] := parseURI[uri, $CloudBase]

getUUID[CloudObject[uri_String, ___]] :=
	parseURI[uri] /. {
		{_, uuid_String, __} :> uuid,
		_ :> $Failed
	}

getUUID[_] := $Failed

cloudObjectFromUUID[uuid_String] := CloudObject[$CloudBase <> $CloudObjectsRoot <> "/" <> uuid]

Unprotect[CloudObject]

Options[CloudObject] = {Permissions->Automatic, IconRules->Automatic, MetaInformation->{}, SharingList -> {}};

getCloudBase[base_String] :=
    Module[ {scheme, domain, port, path, query, fragment},
        {scheme, domain, port, path, query, fragment} = URLParse[base, {"Scheme", "Domain", "Port", "PathString", "QueryString", "Fragment"}];
        If[ And[
                StringQ[scheme],
                StringQ[domain],
                (*MatchQ[path, None|"/"|""],*)
                query === None,
                fragment === None
            ],
			(* memoize the result *)
           	getCloudBase[base] = 
            (* This will append a slash at the end. *)
            URLBuild[{
                "Scheme" -> scheme,
                "Domain" -> domain,
                "Port" -> port,
                "Path" -> path
            }],
        (* invalid cloud base *)
            Message[CloudObject::invbase, base];
            If[ base === $CloudBase,
                getCloudBase["https://www.wolframcloud.com/"],
                getCloudBase[$CloudBase]
            ]
        ]
    ]

getCloudBase[Automatic|None|Null] = Automatic;

getCloudBase[base_] := 
	(Message[CloudObject::invbase, base]; getCloudBase[$CloudBase])

CloudObject[opts:OptionsPattern[]] := Module[{base = getCloudBase[$CloudBase]},
    CloudObject[JoinURL[{base, $CloudObjectsRoot, System`CreateUUID[]}], opts]
]

cloudBaseToDirectory[cbase_String] := Module[{scheme, path},
    {scheme, path} = URLParse[cbase, {"Scheme", "Path"}];
    If[scheme === None,
        CloudObject[cbase],
        If[path === {},
            CloudObject[CloudObject`JoinURL[cbase, $CloudObjectsRoot, "~"]],
            CloudObject[cbase]
        ]
    ]
]

CloudObject[uri_String, opts:OptionsPattern[]] :=
    Module[{base = getCloudBase[$CloudBase], cloud, uuid, user, path, ext, extraPath, search, newURI},
        CloudObject[newURI, opts]
    /; (
    	(* optimize the very common case of base/objects/{uuid} *)
    	If[uuidUriQ[uri],
    		False,
    	(* Else *)
	        {cloud, uuid, user, path, ext, extraPath, search} = parseURI[uri, base];
	        If[uuid === None,
	            If[path === None,
	                (* don't check for shortened URLs unless the parse fails *)
	                If[URLUtilities`URLShortenedQ[uri],
	                    newURI = URLExpand[uri];
	                    log["New named URI: `1`", newURI, DebugLevel -> 3];
	                    newURI =!= uri,
	                (* else *)
		                Message[CloudObject::invuri, uri];
		                False
	                ],
	            (* else *)
	                If[user === None || user === "",
	                    Message[CloudObject::unauth, uri]; False,
	                (* else *)
	                    newURI = StringJoin[JoinURL[{cloud, $CloudObjectsRoot, user, path}],
	                        If[ext === None, "", "." <> ext]
	                    ];
	                    log["New named URI: `1`", newURI, DebugLevel -> 3];
	                    newURI =!= uri
	                ]
	            ],
	        (* else: explicit UUID set *)
	            newURI = StringJoin[JoinURL[{cloud, $CloudObjectsRoot, uuid, extraPath}],
	                If[ext === None, "", "." <> ext]
	            ];
	            log["New UUID-based URI: `1`", newURI, DebugLevel -> 3];
	            newURI =!= uri
	        ]
    	]
    )]

uuidUriQ[uri_String] := 
	StringMatchQ[uri, objectsBase[$CloudBase] ~~ "/" ~~ 
		Repeated[HexadecimalCharacter, {8}] ~~ "-" ~~ 
		Repeated[HexadecimalCharacter, {4}] ~~ "-" ~~ 
		Repeated[HexadecimalCharacter, {4}] ~~ "-" ~~ 
		Repeated[HexadecimalCharacter, {4}] ~~ "-" ~~ 
		Repeated[HexadecimalCharacter, {12}] ~~ ___]

objectsBase[cloudbase_String] := 
	objectsBase[cloudbase] = JoinURL[cloudbase, $CloudObjectsRoot]

CloudObject[uri_, Automatic, opts:OptionsPattern[]] := CloudObject[uri, opts]

CloudObject[uri_, cbase_CloudObject, opts:OptionsPattern[]] :=
    Block[{$CloudDirectory = cbase}, CloudObject[uri, opts]]

CloudObject[uri_, cbase_String, opts:OptionsPattern[]] :=
    CloudObject[uri, cloudBaseToDirectory[cbase], opts]

CloudObject[CloudObject[uri_, opts1:OptionsPattern[]], opts2:OptionsPattern[]] :=
    CloudObject[uri, opts1, opts2]

CloudObject[args___] := (ArgumentCountQ[CloudObject, Length[DeleteCases[{args}, _Rule, Infinity]], 0, 2]; Null /; False)

(* Only use hyperlinks inside CloudObject in desktop Mathematica. Otherwise, a "This feature is not supported" dialog is shown. *)
If[$CloudEvaluation === True,
    (* In the Cloud, use RawBoxFormat to produce interactive output. *)
    BoxForm`MakeConditionalTextFormattingRule[CloudObject];
    Format[CloudObject[uri_String], StandardForm] :=
        CloudSystem`RawBoxFormat[Interpretation[CloudObject[Hyperlink[uri]], CloudObject[uri]]],
(* In desktop Mathematica, use MakeBoxes rather than Format *)
	BoxForm`MakeConditionalTextFormattingRule[CloudObject];
	CloudObject /: MakeBoxes[co: CloudObject[_String], fmt_] :=
        MakeCloudObjectBoxes[co, fmt];
	BoxForm`MakeConditionalTextFormattingRule[CloudObjectInformationData];
	CloudObjectInformationData /: MakeBoxes[data: CloudObjectInformationData[_Association], fmt_] :=
		MakeCloudObjectInformationDataBoxes[data, fmt];
]


SetAttributes[{MakeCloudObjectBoxes, MakeCloudObjectInformationDataBoxes}, HoldAllComplete]

MakeCloudObjectBoxes[CloudObject[uri_], fmt_] :=
	With[{boxes = MakeBoxes[Defer[CloudObject][Hyperlink[uri]], fmt]},
		InterpretationBox[boxes, CloudObject[uri], SelectWithContents -> True]]

MakeCloudObjectInformationDataBoxes[CloudObjectInformationData[data_Association], fmt_] :=
	Module[{name, type, normal},
		name = Replace[data[["Name"]], {s_String :> s, _ -> "--untitled--"}];
		type = Replace[data[["FileType"]],
			{File->"File information", Directory->"Directory information", _->"CloudObject information"}];
		normal = Normal[data]; (* needed for serializing -- see bug 263825 *)
		With[{
			boxes = ToBoxes[Panel[
				Column[{
					Row[{Style["CloudObject: ", FontColor -> Gray], name}],
					Item[
						OpenerView[{
							Style[type, Bold],
							BoxForm`Undeploy[Grid[List @@@ normal, Alignment -> {{Right, Left}}]]},
							False,
							Deployed -> True
						],
						Alignment -> Left
					]
					},
					Dividers -> {False,{False,True}},
					Spacings -> {Automatic,{Automatic,1.2}},
					FrameStyle -> LightGray,
					BaselinePosition -> {2,1}
				],
				BaselinePosition -> Baseline
			], fmt],
			normal = normal
			},
			InterpretationBox[boxes, CloudObjectInformationData[Association[normal]], SelectWithContents -> True]
		]
	]

End[]

EndPackage[]
