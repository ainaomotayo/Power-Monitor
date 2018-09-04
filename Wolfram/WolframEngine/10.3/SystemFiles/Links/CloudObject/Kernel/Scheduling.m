(* Mathematica package *)
BeginPackage["CloudObject`"];

System`ScheduledTask;
System`EvaluateScheduledTask;
System`NextScheduledTaskTime;
System`ScheduledTaskActiveQ;
System`ScheduledTaskInformation::usage = "ScheduledTaskInformation[CloudObject] returns information about a task.
ScheduledTaskInformation[CloudObject, property] returns the value of the specified property.";
System`ScheduledTaskInformationData;
System`CloudSubmit;(*::usage = "CloudSubmit[expr] deploys expr for immediate asynchronous cloud execution.";*)
System`ContinuousTask;
System`AbortScheduledTask;
System`AutoRefreshed;

(* Option symbols *)
System`NotificationFunction;
System`IncludeGeneratorTasks;
System`AutoRemove;
System`RestartInterval;

Begin["`Private`"];

Needs["JLink`"];

Unprotect[ScheduledTask, ContinuousTask, CloudObject];
SetAttributes[ScheduledTask, {HoldAll, ReadProtected}];

Options[ScheduledTask] = Options[ContinuousTask] = Sort@{
	NotificationFunction -> Automatic,
	TimeZone -> Automatic,
	AutoRemove -> False
};

ScheduledTask /: CloudDeploy[task_ScheduledTask, co_CloudObject, OptionsPattern[]] := With[
    {res = Catch[iCloudDeployScheduledTask[task, co], $tag]}, 
    res
]

SetAttributes[ContinuousTask, {HoldAll, ReadProtected}];
Options[ContinuousTask] = {
    NotificationFunction -> Automatic,
    RestartInterval -> Automatic,
    TimeZone -> Automatic
};

(* Continuous tasks must be pausable, so don't use None for timespec. Currently only
 * interval tasks are pausable with start and end dates. *)
ContinuousTask /: CloudDeploy[ContinuousTask[expr_, o:OptionsPattern[]], co_CloudObject, oCD:OptionsPattern[]] := 
    continuousTaskStaging[ContinuousTask[expr, 20*24*3600, o], co, oCD]

ContinuousTask /: CloudDeploy[ct:ContinuousTask[expr_, end:Except[HoldPattern[_Quantity]], o:OptionsPattern[]], co_CloudObject, oCD:OptionsPattern[]] := 
    continuousTaskStaging[ContinuousTask[expr, {Now, 20*24*3600, DateObject[end]}, o], co, oCD]

ContinuousTask /: CloudDeploy[ct:ContinuousTask[expr_, tspan:HoldPattern[_Quantity], o:OptionsPattern[]], co_CloudObject, oCD:OptionsPattern[]] := 
    continuousTaskStaging[ContinuousTask[expr, {Now, 20*24*3600, Now + tspan}, o], co, oCD]

continuousTaskStaging[ContinuousTask[expr_, tspec_, o:OptionsPattern[]], co_CloudObject, oCD:OptionsPattern[]] := With[
    {options = Join[Flatten[{o}], {"Continuous" -> True}, Options[ContinuousTask]]}, 
    Catch[iCloudDeployScheduledTask[ScheduledTask[expr, tspec, options], co, oCD], $tag]
];


iCloudDeployScheduledTask[st:ScheduledTask[expr_, sched_, o:OptionsPattern[]], obj:CloudObject[uri_String, ___], iO:OptionsPattern[]] := Module[
    {
    	cloud, uuid, name,
        continuous = TrueQ[Lookup[Flatten[{o}], "Continuous", False]],
        runImmediately = False,
        cronned,
        params, taskJson, rJson
    },
    {cloud, uuid, name} = getCloudAndUUIDOrPath[obj] /. None -> Null;
    name = Replace[name, {
        n : {"user-" ~~ $WolframUUID, __} :> FileNameJoin[Rest[n]],
        n_List :> FileNameJoin[n]
    }];

    {runImmediately, cronned} = Which[
        continuous,
        {True, timespec2Cron[ReleaseHold[sched]]},
        
        nowQ[sched],
        {True, timespec2Cron[None]},
        
        True,
        {False, timespec2Cron[ReleaseHold[sched]]}
    ];
    
    If[MatchQ[cronned, $Failed],
    	Message[ScheduledTask::sched, sched];
    	Throw[$Failed, $tag]
    ];

    params = {"task" -> (taskJson = generateTaskJson[st, {name, uuid}, cronned, Flatten[{o}]])};
    (* Print@params; *)

    With[{mh = ScheduledTask},
        rJson = Replace[execute[cloud, "POST", {"tasks"}, Parameters -> params], {
            {_String, content_List} :> ($lastInfoJSON = FromCharacterCode[content])
            , HTTPError[400, ___] :> (Message[mh::argu]; Throw[$Failed, $tag])
            , HTTPError[403, content_, ___] :> (
	            	ToExpression[Lookup[ImportString[content, "JSON"], "error", "ScheduledTask::restr"], InputForm, Message];
	            	Throw[$Failed, $tag]
	            )
            , other_ :> (Message[mh::srverr]; Message[mh::crea]; Throw[$Failed, $tag])
        }];
    ];
    (* Print[rJson]; *)
    
    (* Remove this after task rewrite *)
    If[continuous,
        Check[
        	With[{end = Replace[Lookup[ImportString[taskJson, "JSON"], "endTimestamp"], {
            		t_?NumericQ :> FromUnixTime[Round[t/1000]]
            	}]},
	            SetOptions[obj, MetaInformation -> "__ContinuousEndDate" -> end]
        	],
	        Throw[$Failed, $tag]
        ]
    ];

    If[TrueQ[runImmediately], RunScheduledTask @@ {obj}];
    obj
]


generateTaskJson[st:ScheduledTask[expr_, __], {name:(_String|Null), uuid:(_String|Null)}, cronned_, o:{___?OptionQ}] := Module[
	{start, stdSched, end,
	   opts, taskJson},
	
    With[{res = standardizeSchedule@cronned},
        If[MatchQ[res, $Failed],
            Message[ScheduledTask::sched, cronned];
            Throw[$Failed, $tag],
            (* else *)
            {start, stdSched, end} = res;
        ]
    ];
    (* Print[{start, stdSched, end}]; *)

    opts = Replace[Join[
        {
            "Continuous" -> Lookup[o, "Continuous", False]
        },
        o,
        Options[ScheduledTask],
        {
            "Expression" -> ToString[Unevaluated[expr], InputForm]
            , "TaskType" -> "script"
            , "StartDate" -> start
            , "EndDate" -> end
            , "CronSchedule" -> With[{s = First[stdSched]}, If[StringQ[s], s, Null]]
            , "RepeatCount" -> Last[stdSched]
            , "RepeatInterval" -> With[{s = First[stdSched]}, If[IntegerQ[s], s, Null]]
            , RestartInterval -> Null
            , "Name" -> name
            , "UUID" -> uuid
        }
    ], {Rule[TimeZone, Automatic] :> Rule[TimeZone, $TimeZone]}, 1];
    (* Print[opts]; *)

    validateTaskArg["options", {}] := True;
    validateTaskArg["options", unknown_List] := (Message[ScheduledTask::optx, First[unknown], st]; False);
    validateTaskArg["notification", Automatic|All|None] := True;
    validateTaskArg["notification", _String -> Automatic|All] := True;
    validateTaskArg["notification", {___String}] := True;
    validateTaskArg["notification", {({__String} -> (Automatic|All|None|_Function|_List))..}] := True;
    validateTaskArg["notification", _Function] := True;
    validateTaskArg["notification", a__] := (Message[ScheduledTask::badarg, a, NotificationFunction]; False);
    validateTaskArg["restart", Automatic] := True;
    validateTaskArg["restart", Null|None] := True;
    validateTaskArg["restart", _Function] := True;
    validateTaskArg["restart", HoldPattern[_Quantity]] := True;
    validateTaskArg["restart", r_?NumericQ /; r >= 0] := True;
    validateTaskArg["restart", r__] := (Message[ScheduledTask::badarg, r, RestartInterval]; False);

    validateTaskArg[__] := False;

    (* Client-side validation *)
    If[And @@ # === False, Throw[$Failed, $tag]] & @ MapThread[
        validateTaskArg[#1, #2] &,
        {
            {"options", "notification", "restart"},
            {
                FilterRules[o, Except[Join[Options[ScheduledTask], Options[ContinuousTask], 
                    {"Visible" -> True, "Continuous" -> False}]]], 
                Lookup[opts, NotificationFunction],
                Lookup[opts, RestartInterval, Null]
            }
        }
    ];

    taskJson = ExportString[unpresentifyTaskMeta[opts], "JSON", "Compact" -> True];
    (* Print[params]; *)
    taskJson
]


iCloudDeployScheduledTask[ScheduledTask[args___], ___] := 
    (ArgumentCountQ[ScheduledTask, Length[DeleteCases[{args}, _Rule|_RuleDelayed, Infinity]], 2, 2]; $Failed)
iCloudDeployScheduledTask[___] := $Failed


toSchedule[n_String] := Module[{cron},
    cron = StringSplit[n];
    If[Length@cron < 3 || Length@cron > 7, Return[$Failed]];
    cron = cron /. {
        (*{s_, m_, h_, dom_, m_, dow_, y_}:>{s, m, h, dom, m, dow, y},
        {s_, m_, h_, dom_, m_, dow_}:>{s, m, h, dom, m, dow, "*"},
        {h_, dom_, m_, dow_, y_}:>{"*", "*", h, dom, m, dow, y},
        {h_, dom_, m_, dow_}:>{"*", "*", h, dom, m, dow, "*"}*)
        
        
        {s_, m_, h_, dom_, mo_, dow_, y_}:>{s, m, h, dom, mo, dow, y}, (* quartz expression *)
        {m_, h_, dom_, mo_, dow_, y_}:>{"*", m, h, dom, mo, dow, y}, (* classic cron with optional year *)
        {m_, h_, dom_, mo_, dow_}:>{"*", m, h, dom, mo, dow, "*"}, (* classic cron *)
        {h_, dom_, mo_, dow_}:>{"*", "*", h, dom, mo, dow, "*"}
        
        
    };
    StringJoin[Riffle[ToUpperCase@cron, " "]]
]

toSchedule[___] := $Failed

current[spec_] := DateString[DateList[], spec]
(* We can remove this and instead use dowToCron *)
currentDOW[] := With[{date = DateList[]}, Which[
    DayMatchQ[date, Sunday], "1",
    DayMatchQ[date, Monday], "2",
    DayMatchQ[date, Tuesday], "3",
    DayMatchQ[date, Wednesday], "4",
    DayMatchQ[date, Thursday], "5",
    DayMatchQ[date, Friday], "6",
    DayMatchQ[date, Saturday], "7",
    True, "*"
]]

(* this really needs to get fixed... the first section of each cron expression*)
$TimeSpecs = {
   "Hourly" :> StringJoin[current[{"MinuteShort"}]," * * * ? *"],
   "Daily" :> StringJoin[current[{"MinuteShort", " ", "HourShort"}], " * * ? *"],
   "Weekly" :> StringJoin[current[{"MinuteShort", " ", "HourShort"}], " ? * ", currentDOW[], " *"],
   "Monthly" :> StringJoin[current[{"MinuteShort"," ", "HourShort", " ", "DayShort"}], " * ? *"],
   "Yearly" :> StringJoin[current[{"MinuteShort", " ", "HourShort", " ", "DayShort", " ", "MonthShort"}], " ? *"]
};

(*validateTimeSpec[{n_Integer?Positive, di:(_Integer|_DirectedInfinity)}] := {n, di}
validateTimeSpec[__] := $Failed
validateCronSpec[{cron_, di_DirectedInfinity}] := toSchedule[cron]*)

$AvailableTimeSpecs = First /@ $TimeSpecs;

resolveSecs = {
    {a_?NumberQ, "Second"} :> Round[a],
    {a_?NumberQ, "Minute"} :> Round[a*60],
    {a_?NumberQ, "Hour"} :> Round[a*3600],
    {a_?NumberQ, "Day"} :> Round[a*3600*24],
    {a_?NumberQ, "Week"} :> Round[a*3600*24*7],
    {a_?NumberQ, "Month"} :> Round[a*3600*24*30], (* 30 days *)
    {a_?NumberQ, "Quarter"} :> Round[a*3600*24*90], (* 90 days *)
    {a_?NumberQ, "Year"} :> Round[a*3600*24*365] (* 365 days *)
};

(* CRON Output *)
timespec2Cron[string_String] /; MemberQ[$AvailableTimeSpecs, string] := {string /. $TimeSpecs, Infinity}
timespec2Cron[d_DateObject] := With[{spec=DateObjectToCronSpecification[d]},If[StringQ[spec],{spec, Infinity},$Failed]]
timespec2Cron[cron_String] := With[{spec=toSchedule[cron]},If[StringQ[spec],{spec, Infinity},$Failed]]
timespec2Cron[{spec_String, di_DirectedInfinity}] := Module[{tmp}, 
    tmp = timespec2Cron[spec];
    tmp[[2]] = di;
    tmp]
    
(* List[Integer, DirectedInfinity] Output *)
timespec2Cron[HoldPattern[q_Quantity]]:= If[CompatibleUnitQ[q, "Seconds"] ,{QuantityMagnitude[UnitConvert[q, "Seconds"]] /. resolveSecs, Infinity},$Failed] 
timespec2Cron[n_Integer?Positive] := {n, Infinity}
timespec2Cron[{spec_}] := timespec2Cron[{spec, 1}]
timespec2Cron[{n_Integer?Positive, di_DirectedInfinity}] := Module[{tmp}, 
    tmp = timespec2Cron[n];
    tmp[[2]] = di;
    tmp]

(* ambiguous *)
timespec2Cron[{spec : Except[_List], count_Integer}] := With[{s = timespec2Cron[spec]}, {First[s], count} /; s =!= $Failed]
timespec2Cron[{start_DateObject, timespec_, end_DateObject}] := {start, timespec2Cron[timespec], end}
timespec2Cron[{start_DateObject, timespec_}] := {start, timespec2Cron[timespec]}
timespec2Cron[{timespec_, end_DateObject}] := {timespec2Cron[timespec], end}

(* None/dummy *)
timespec2Cron[dummy:None|Null] := {Null, 1};

(* failures *)
timespec2Cron[_DateObject, _DateObject] := Message[ScheduledTask::sched, "2 argument timespec is ambiguous. Please use a different form."]
timespec2Cron[__] := $Failed


standardizeSchedule[sched_] := Module[{tmp}, (* We need to get the timespec... then the start timestamp and the end timestamp *)
    tmp = Position[sched, {_:(String|Integer), _DirectedInfinity}];
    If[Length[Flatten[tmp]] == 1, tmp = Flatten[tmp][[1]] (*ELSE issue a message?*)];
    
    If[MemberQ[sched, _DateObject],
        (*it contains DateObjects so we know that it isn't just the timespec *)
        If[tmp === 1, Return[{Null, sched[[1]], sched[[2]]}],
        (* ELSE *)
            If[tmp === 2 && Length@sched == 2, 
                (* {timespec, DateObject} *)
                Return[{sched[[1]], sched[[2]], Null}], 
                (* ELSE *)
                Return[sched] (*DateObject, timespec, DateObject*)]
            ]
        ,
        Return[{Null, sched, Null}]
    ]
]

(* Now? *)
SetAttributes[nowQ, HoldAll];
nowQ[Now] := True;
nowQ[{Now}] := True;
nowQ[{Now, count_}] := MatchQ[ReleaseHold[count], _Integer|Infinity|DirectedInfinity];
nowQ[{start_, Now, end_}] := MatchQ[ReleaseHold[start], _DateObject] && MatchQ[ReleaseHold[end], _DateObject];
nowQ[{start_DateObject, Now}] := MatchQ[ReleaseHold[start], _DateObject];
nowQ[{Now, end_DateObject}] := MatchQ[ReleaseHold[end], _DateObject];
nowQ[___] := False;


CloudObject /: StopScheduledTask[co_CloudObject, OptionsPattern[]] /; Quiet[documentGeneratorQ[co]] := Module[
    {i = System`DocumentGeneratorInformation[co, "Task"]},
    If[Head[i] === ScheduledTaskInformationData,
        Catch[iCloudStopScheduledTask[{$CloudBase, Lookup[i[[1]], "UUID"]}], $tag];
        co,
        (* else *)
        Message[DocumentGenerator::notask, co];
        i
    ] 
]

CloudObject /: StopScheduledTask[task_CloudObject, OptionsPattern[]] := With[
    {res = Catch[iCloudStopScheduledTask[task], $tag]},
    res
]

iCloudStopScheduledTask[obj_CloudObject, mh_:StopScheduledTask] := (iCloudStopScheduledTask[safeCloudAndUUIDFetch[obj, mh], mh]; obj)
iCloudStopScheduledTask[{cloud_String, uuid_String}, mh_:StopScheduledTask] := Module[
    {json},
    json = Replace[execute[cloud, "POST", {"tasks", uuid, "pause"}], {
        HTTPError[404, ___] :> (Message[ScheduledTask::tasknf, cloudObjectFromUUID[uuid]]; Throw[$Failed, $tag])
        , {_String, content_List} :> ($lastInfoJSON = FromCharacterCode[content])
        , other_ :> (Message[mh::srverr, obj]; Message[ScheduledTask::nostop, cloudObjectFromUUID[uuid]]; Throw[$Failed, $tag])
    }];
    uuid
];

(*iCloudStopScheduledTask[st_,OptionsPattern[]] := (Message[ScheduledTask::nostop,st];$Failed)*)


CloudObject /: StartScheduledTask[co_CloudObject, o:OptionsPattern[]]  /; Quiet[documentGeneratorQ[co]] := Module[
    {i = System`DocumentGeneratorInformation[co, "Task"]},
    If[Head[i] === ScheduledTaskInformationData,
        Catch[iCloudStartScheduledTask[{$CloudBase, Lookup[i[[1]], "UUID"]}], $tag];
        co,
        (* else *)
        Message[DocumentGenerator::notask, co];
        i
    ]
]

CloudObject /: StartScheduledTask[task_CloudObject, OptionsPattern[]] := With[
    {res = Catch[iCloudStartScheduledTask[task], $tag]}, res
]

iCloudStartScheduledTask[obj_CloudObject, mh_:StartScheduledTask] := (iCloudStartScheduledTask[safeCloudAndUUIDFetch[obj, mh], mh]; obj)
iCloudStartScheduledTask[{cloud_String, uuid_String}, mh_:StartScheduledTask] := Module[
    {json},
    json = Replace[execute[cloud, "POST", {"tasks", uuid, "resume"}], {
        HTTPError[404, ___] :> (Message[ScheduledTask::tasknf, cloudObjectFromUUID[uuid]]; Throw[$Failed, $tag])
        , {_String, content_List} :> ($lastInfoJSON = FromCharacterCode[content])
        , other_ :> (Message[mh::srverr, cloudObjectFromUUID[uuid]]; Message[ScheduledTask::nostart]; Throw[$Failed, $tag])
    }];
    uuid
];

(*iCloudResumeScheduledTask[st_, OptionsPattern[]] := handleSchedulingResponse[$Failed]*)


(*
 * Equivalent to "Run now" in web interface.
 *)
CloudObject /: RunScheduledTask[co_CloudObject, o:OptionsPattern[]] /; Quiet[documentGeneratorQ[co]] := Module[
    {res = Catch[iCloudRunDocumentGenerator[co, RunScheduledTask, o], $tag]},
    res
]

CloudObject /: RunScheduledTask[task_CloudObject, OptionsPattern[]] := With[
    {res = Catch[iCloudRunScheduledTask[task], $tag]},
    res
]

iCloudRunScheduledTask[obj_CloudObject, mh_:RunScheduledTask] := (iCloudRunScheduledTask[safeCloudAndUUIDFetch[obj, mh], mh]; obj)
iCloudRunScheduledTask[{cloud_String, uuid_String}, mh_:RunScheduledTask] := Module[
    {json},
    json = Replace[execute[cloud, "POST", {"tasks", uuid, "execute"}], {
        HTTPError[404, ___] :> (Message[ScheduledTask::tasknf, cloudObjectFromUUID[uuid]]; Throw[$Failed, $tag])
        , {_String, content_List} :> ($lastInfoJSON = FromCharacterCode[content])
        , other_ :> (Message[mh::srverr, obj]; Throw[$Failed, $tag])
    }];
    uuid
];

(*iCloudRunScheduledTask[st_,OptionsPattern[]] := handleSchedulingResponse[$Failed]*)



CloudObject /: RemoveScheduledTask[co_CloudObject, o:OptionsPattern[]] /; Quiet[documentGeneratorQ[co]] := Module[
	{res = Catch[iCloudRemoveDocumentGenerator[co, o], $tag]},
	res
]

$autoRefreshedMimeTypes = {"application/vnd.wolfram.bundle.autorefreshed"};
(* Slow! *)
autoRefreshedQ[co_CloudObject] := MemberQ[$autoRefreshedMimeTypes, CloudObjectInformation[co, "MimeType"]];

CloudObject /: RemoveScheduledTask[co_CloudObject, o:OptionsPattern[]] /; Quiet[autoRefreshedQ[co]] := Module[
    {taskId},
    taskId = Lookup[Lookup[Options[co, MetaInformation -> "__Task"], MetaInformation], "__Task"];
    Catch[
        iCloudRemoveScheduledTask[cloudObjectFromUUID[taskId]];
        Check[
            DeleteDirectory[co, DeleteContents -> True],
            Throw[$Failed, $tag]
        ];
        co,
        $tag
    ]
]

CloudObject /: RemoveScheduledTask[task_CloudObject] := With[
	{res = Catch[iCloudRemoveScheduledTask[task], $tag]},
	res
]

iCloudRemoveScheduledTask[obj_CloudObject, mh_:RemoveScheduledTask] := (iCloudRemoveScheduledTask[safeCloudAndUUIDFetch[obj, mh], mh]; obj)
iCloudRemoveScheduledTask[{cloud_String, uuid_String}, mh_:RemoveScheduledTask] := Module[
    {json},
    json = Replace[execute[cloud, "DELETE", {"tasks", uuid}], {
        HTTPError[404, ___] :> (Message[ScheduledTask::tasknf, cloudObjectFromUUID[uuid]]; Throw[$Failed, $tag])
        , {_String, content_List} :> ($lastInfoJSON = FromCharacterCode[content])
        , other_ :> (Message[mh::srverr]; Message[ScheduledTask::norm, cloudObjectFromUUID[uuid]]; Throw[$Failed, $tag])
    }];
    uuid
];

iCloudRemoveScheduledTask[st_, OptionsPattern[]] := (Message[ScheduledTask::norm, st]; $Failed)


Unprotect[EvaluateScheduledTask];

CloudObject /: EvaluateScheduledTask[co_CloudObject, o:OptionsPattern[]] /; Quiet[documentGeneratorQ[co]] := Module[
	{res = Catch[iCloudEvaluateDocumentGeneratorCE[co, CloudObject[], o], $tag]},
	res
]

CloudObject /: EvaluateScheduledTask[co_CloudObject, uri_String, o:OptionsPattern[]] /; Quiet[documentGeneratorQ[co]] := Module[
	{res = Catch[iCloudEvaluateDocumentGeneratorCE[co, CloudObject[uri], o], $tag]},
	res
]

CloudObject /: EvaluateScheduledTask[co_CloudObject, dest_CloudObject, o:OptionsPattern[]] /; Quiet[documentGeneratorQ[co]] := Module[
	{res = Catch[iCloudEvaluateDocumentGeneratorCE[co, dest, o], $tag]},
	res
]

CloudObject /: EvaluateScheduledTask[co_CloudObject] := Module[
	{expr},
	expr = Replace[
		Check[
			ScheduledTaskInformation[co, "Expression"],
			$Failed
		],
		x_String :> ToExpression[x, InputForm, Hold]
    ];
    
	(* Pre-1.22 tasks store their code in the cloud object and have execution expressions like
	 *      "EvaluateScheduledTask[CloudObject[\"http://www.wolframcloud.\com/objects/user-74e17eb9-8669-4795-b270-032b6ad916af/task\\"]]"
	 * In 1.22+ this results in recursion if evaluated naively, so check for expressions of this form. 
	 *)
    ReleaseHold @ Replace[expr, {
    	Hold[EvaluateScheduledTask[co2_CloudObject]] :> Replace[
    		CloudGet[co2],
    		ScheduledTask[code_, ___] :> (code)
    	] /; CloudObjectInformation[co, "UUID"] === CloudObjectInformation[co2, "UUID"]
    }]
]

(*
 * Hybrid task listing
 *)
Unprotect[ScheduledTasks];
SetAttributes[ScheduledTasks, {ReadProtected}];
Options[ScheduledTasks] = Options[iCloudScheduledTasks] = {
	IncludeGeneratorTasks -> False
};
$cloudScheduledTasksFlag = True;
ScheduledTasks[o:OptionsPattern[]] /; TrueQ[And[$CloudConnected, $cloudScheduledTasksFlag]] := Block[
	{$cloudScheduledTasksFlag = False},
	Join[ScheduledTasks[], Catch[iCloudScheduledTasks[o], $tag]]
]

iCloudScheduledTasks[o:OptionsPattern[]] := Module[
    {raw, med, json, msghd = ScheduledTasks},

    With[{mh = msghd},
        json = Replace[execute[$CloudBase, "GET", {"tasks"}], {
            {_String, content_List} :> ($lastInfoJSON = FromCharacterCode[content])
            , other_ :> (Message[mh::srverr]; Throw[$Failed, $tag])
        }];

        Check[raw = Lookup[ImportString[json, "JSON"], "tasks", {Missing[]}],
            Message[mh::srverr];
            Throw[$Failed, $tag]
        ];
    ];
    
    If[Length[raw] == 0, Return[{}]];
    
    med[1] = Lookup[raw, {"nextTimestamp", "taskType", "uuid"}, Missing[]];

    (* This sorts by next run date, soonest first. *)
    med[2] = With[{culled = Reverse @ SortBy[DeleteCases[med[1], {___, Missing[], ___}], First]},
    	If[TrueQ[OptionValue[IncludeGeneratorTasks]],
    		culled,
    		(* else *)
    		Select[culled, #[[2]] =!= "document-generator" &]
    	]
    ];
    cloudObjectFromUUID /@ med[2][[All, -1]]
]


(* ScheduledTaskInformation *)
Unprotect[ScheduledTaskInformation, ScheduledTaskInformationData];
SetAttributes[ScheduledTaskInformation, {ReadProtected}];

ScheduledTaskInformation::noprop = "`1` is not a property returned by ScheduledTaskInformation.";


gatherNotificationFunction[n:{{__}...}] := SortBy[
    Sort[("email" /. #)] -> ToExpression["condition" /. #[[1, -1]]] & /@ GatherBy[n, "condition" /. # &],
    Last
];


(* Output is a flat list digestible by the web ui. *)
denormalizeNotificationFunction[notifRaw_] := Module[
    {notif = Replace[notifRaw, {
        Null | None -> {},
        All -> {{$WolframID} -> All},
        Automatic -> {{$WolframID} -> Automatic},
        f_Function :> {{$WolframID} -> f},
        {u__String} :> {{u} -> Automatic},
        (* non-mail channels *)
        Rule[s_String, cond : Automatic | All] :> {Rule[{$WolframID}, {s, cond}]}
    }]},
    
    notif = notif /. {
        (addr_ -> cond_) :> Flatten[{addr}] -> Replace[cond, {None -> Null, ns:Except[_String] :> ToString[InputForm[ns]]}]
    };
    
	With[{pairs = DeleteDuplicates @ Flatten[(Thread[List @@ #1, List, 1] &) /@ notif, 1]},
		{"email" -> First@#, "condition" -> Last@#} & /@ pairs
	]
]


$taskInfoNormalizationRules = {
    Rule[tag:"StartDate"|"EndDate"|"LastRunDate"|"NextRunDate", t_?NumericQ] :> 
        Rule[tag, FromUnixTime[Round[t/1000]]],
    Rule[tag:"StartDate"|"EndDate"|"LastRunDate"|"NextRunDate", Null] :> 
        Rule[tag, None],
    Rule["Log", uuid_String] :> {Rule["Log", cloudObjectFromUUID[uuid]], Rule["LogUUID", uuid]},
    Rule[NotificationFunction, notif_] :> Rule[NotificationFunction, gatherNotificationFunction[notif]],
    Rule["Name", Null] -> Rule["Name", None],
    Rule[RestartInterval, r_] :> Rule[RestartInterval, ToExpression[r]],
    Rule["RepeatCount", 0|-1] :> Rule["RepeatCount", Infinity]
};


$taskInfoDenormalizationRules = {
    Rule[tag:"startTimestamp"|"endTimestamp"|"lastTimestamp"|"nextTimestamp", t:(_DateObject|_?NumericQ)] :> 
        Rule[tag, Round[1000*UnixTime[t]]],
    Rule[tag:"startTimestamp"|"endTimestamp"|"lastTimestamp"|"nextTimestamp", t_None] :> 
        Rule[tag, Null],
    Rule["name", None] -> Rule["name", Null],
    Rule["notificatees", notif_] :> Rule["notificatees", denormalizeNotificationFunction[notif]],
    Rule["restartInterval", r_] :> Rule["restartInterval", ToString[InputForm[r]]],
    Rule["repeatCount", Infinity|DirectedInfinity] -> Rule["repeatCount", 0]
};


$taskMetaToWLKeyMap = Association[
    "active" -> "Active", 
    "autoRemove" -> AutoRemove, 
    "completed" -> "Completed", 
    "continuous" -> "Continuous", 
    "cronSchedule" -> "CronSchedule", 
    "endTimestamp" -> "EndDate", 
    "lastTimestamp" -> "LastRunDate", 
    "log" -> "Log", 
    "name" -> "Name", 
    "nextTimestamp" -> "NextRunDate", 
    "notificatees" -> NotificationFunction,
    "owner" -> "Owner", 
    "paused" -> "Paused", 
    "repeatCount" -> "RepeatCount", 
    "repeatInterval" -> "RepeatInterval", 
    "restartInterval" -> RestartInterval, 
    "startTimestamp" -> "StartDate", 
    "status" -> "Status", 
    "taskData" -> "Expression", 
    "taskType" -> "TaskType", 
    "timeZoneOffsetHrs" -> TimeZone, 
    "timeZone" -> "TimeZoneFullName", 
    "timeZoneAbbr" -> "TimeZoneAbbreviation",
    "uuid" -> "UUID",
    "visible" -> "Visible"
];

$WLToTaskMetaKeyMap = Association[Reverse /@ Normal[$taskMetaToWLKeyMap]];

$presentableTaskInfoKeys = Key /@ List[
    "Active", 
    AutoRemove, 
    "Completed",
    "Continuous", 
    "CronSchedule", 
    "EndDate", 
    "LastRunDate", 
    "Log", 
    "Name", 
    "NextRunDate", 
    NotificationFunction,
    "Owner",
    "Paused", 
    "RepeatCount", 
    "RepeatInterval", 
    RestartInterval, 
    "StartDate", 
    "Status", 
    "Expression", 
    TimeZone, 
    "UUID",
    "Visible"
];


$outgoingTaskMetaKeys = Key /@ List[
    (* "active", *) 
    "autoRemove", 
    "completed", 
    "continuous", 
    "cronSchedule", 
    "endTimestamp", 
    "lastTimestamp", 
    (* "log", *) 
    "name", 
    "nextTimestamp", 
    "notificatees", 
    "paused", 
    "repeatCount", 
    "repeatInterval", 
    "restartInterval", 
    "startTimestamp", 
    "status", 
    "taskData", 
    "taskType", 
    (* "timeZone", *) 
    "timeZoneOffsetHrs",
    (* "timeZoneAbbr", *)
    "uuid"
];


taskMetaToWL[raw_List] := Module[
    {med, well},
    If[MatchQ[Lookup[raw, "visible"], _],
        (* Replace json keys with WL symbols/strings *)
        med = DeleteCases[
            Replace[raw, Rule[lhs_, rhs_] :> Rule[$taskMetaToWLKeyMap[lhs], rhs], 1],
            Rule[Missing[__], _]
        ];
        
        well = Association @@ Flatten[Replace[med, $taskInfoNormalizationRules, 1], 1];
        KeySort @ Prepend[well, "Active" -> (!well["Paused"] && !well["Completed"])],
        (* else *)
        Null
    ]
]


WLToTaskMeta[System`ScheduledTaskInformationData[a_Association]] := WLToTaskMeta[Normal[a]];
WLToTaskMeta[a_Association] := WLToTaskMeta[Normal[a]];
WLToTaskMeta[raw:OptionsPattern[]] := Module[
    {med, well},

    (* Replace WL symbols and strings with json keys *)
    med = DeleteDuplicates[DeleteCases[
        Replace[raw, Rule[lhs_, rhs_] :> Rule[$WLToTaskMetaKeyMap[lhs], rhs], 1],
        Rule[Missing[__], _]
    ], First[#1] === First[#2] &];
    
    well = Replace[med, $taskInfoDenormalizationRules, 1];
    well
]


unpresentifyTaskMeta[raw_] := With[{med = WLToTaskMeta[raw]},
	DeleteCases[
		Normal[Apply[Association, med][[$outgoingTaskMetaKeys]]],
		Rule[_, Missing[__]]
	]
];


presentifyTaskMeta[med_Association] := med[[$presentableTaskInfoKeys]];


ScheduledTaskInformation[obj_CloudObject] := Replace[
    Catch[iCloudScheduledTaskInformation[obj], $tag], {
    assoc_Association :> System`ScheduledTaskInformationData[presentifyTaskMeta @ assoc]
}]

ScheduledTaskInformation[obj_CloudObject, property_] := Replace[
	Catch[iCloudScheduledTaskInformation[obj], $tag], {
    assoc_Association :> If[KeyExistsQ[assoc, property],
        presentifyTaskMeta[assoc][property],
        (* else *)
        Message[ScheduledTaskInformation::noprop, property];
        $Failed
    ]
}]

iCloudScheduledTaskInformation[obj_CloudObject] := Module[
    {raw, cloud, uuid, json, msghd = ScheduledTaskInformation},
    With[{mh = msghd},
    	{cloud, uuid} = safeCloudAndUUIDFetch[obj, mh];

	    json = Replace[execute[cloud, "GET", {"tasks", uuid}], {
	        HTTPError[404, ___] :> (Message[ScheduledTask::tasknf, cloudObjectFromUUID[uuid]]; Throw[$Failed, $tag])
	        , {_String, content_List} :> ($lastInfoJSON = FromCharacterCode[content])
	        , other_ :> (Message[mh::srverr]; Throw[$Failed, $tag])
	    }];

	    Check[raw = Lookup[ImportString[json, "JSON"], "task"],
	        Message[mh::srverr];
	        Throw[$Failed, $tag]
	    ]
    ];

    taskMetaToWL[raw]
];

CloudObject /: ScheduledTaskActiveQ[co_CloudObject, o:OptionsPattern[]] /; Quiet[documentGeneratorQ[co]] := Module[
	{i = System`DocumentGeneratorInformation[co, "Task"]},
    If[MatchQ[i, _ScheduledTaskInformationData],
        i[[1]]["Active"] && Not[i[[1]]["Paused"]],
		(* else *)
		i
	] 
]

CloudObject /: ScheduledTaskActiveQ[task_CloudObject] := With[
    {i = Catch[iCloudScheduledTaskInformation[task], $tag]},
    If[MatchQ[i, _Association],
        Or[
        	i["Active"] && Not[i["Paused"]],
        	i["Continuous"] && Not[i["Paused"]] (* when working: && TrueQ[Now < i["EndDate"] *)
        ],
        (* else *)
        i
    ]
]


CloudObject /: NextScheduledTaskTime[co_CloudObject, o:OptionsPattern[]] /; Quiet[documentGeneratorQ[co]] := Module[
    {i = System`DocumentGeneratorInformation[co, "Task"]},
    If[MatchQ[i, _ScheduledTaskInformationData],
    	If[TrueQ[Lookup[i[[1]], "Visible"]],
	        Lookup[i[[1]], "NextRunDate", Message[ScheduledTask::nonext, co]; $Failed],
	        (* else *)
	        None
    	],
        (* else *)
        i
    ] 
]


CloudObject /: NextScheduledTaskTime[task_CloudObject] := With[
    {i = Catch[iCloudScheduledTaskInformation[task], $tag]},
    If[MatchQ[i, _Association],
        Lookup[i, "NextRunDate", Message[ScheduledTask::nonext, co]; $Failed],
        (* else *)
        i
    ] 
]

(*iCloudNextScheduledTaskTime[st_, OptionsPattern[]] := (Message[ScheduledTask::nonext, st]; $Failed)*)


(* CloudSubmit *)
Unprotect[CloudSubmit];
SetAttributes[CloudSubmit, {HoldFirst, ReadProtected}];
Options[CloudSubmit] = {
	NotificationFunction -> Automatic,
	CloudBase :> $CloudBase
};

CloudSubmit[expr_, o:OptionsPattern[]] := Block[
	{$CloudBase = Lookup[Join[Flatten[{o}], Options[CloudSubmit]], CloudBase, $CloudBase]},
	CloudSubmit[expr, CloudObject[], o]
];

CloudSubmit[expr_, obj:_String|_CloudObject, o:OptionsPattern[]] := Module[
    {opts = Join[Flatten[{o}], Options[CloudSubmit]]},
    Block[{$CloudBase = Lookup[opts, CloudBase, $CloudBase]},
        With[{taskOpts = Sequence @@ Join[FilterRules[opts, Options[ScheduledTask]], {AutoRemove -> True}]},
        	CloudDeploy[
        		ScheduledTask[expr, {Now}, taskOpts],
            	obj
            ]
        ]
    ]
]


(* AbortScheduledTask *)
Unprotect[AbortScheduledTask];
SetAttributes[AbortScheduledTask, {ReadProtected}];

CloudObject /: AbortScheduledTask[co_CloudObject] /; Quiet[documentGeneratorQ[co]] := Module[
    {i = System`DocumentGeneratorInformation[co, "Task"]},
    If[Head[i] === ScheduledTaskInformationData,
        Catch[iCloudAbortScheduledTask[{$CloudBase, Lookup[i[[1]], "UUID"]}], $tag];
        co,
        (* else *)
        i
    ] 
]

CloudObject /: AbortScheduledTask[task_CloudObject] := With[
    {res = Catch[iCloudAbortScheduledTask[task], $tag]},
    res
]

iCloudAbortScheduledTask[obj_CloudObject, mh_:ScheduledTask] := (iCloudAbortScheduledTask[safeCloudAndUUIDFetch[obj, mh], mh]; obj)
iCloudAbortScheduledTask[{cloud_String, uuid_String}, mh_:ScheduledTask] := Module[
    {raw, json},

    json = Replace[execute[cloud, "POST", {"tasks", uuid, "abort"}], {
        (* HTTPError[400, ___] :> (Message[mh::tasknf, cloudObjectFromUUID[uuid]]; Throw[$Failed, $tag]) *)
        HTTPError[404, ___] :> (Message[mh::tasknf, cloudObjectFromUUID[uuid]]; Throw[$Failed, $tag])
        , HTTPError[409, ___] :> (Message[mh::norun, cloudObjectFromUUID[uuid]]; Return[obj])
        , {_String, content_List} :> ($lastInfoJSON = FromCharacterCode[content])
        , other_ :> (Message[mh::srverr, cloudObjectFromUUID[uuid]]; Throw[$Failed, $tag])
    }];

    Check[raw = Lookup[ImportString[json, "JSON"], "status"],
        Message[mh::srverr];
        Throw[$Failed, $tag]
    ];
    
    uuid
];


(* AutoRefreshed *)

Unprotect[AutoRefreshed];
SetAttributes[AutoRefreshed, {HoldFirst, ReadProtected}];
Options[AutoRefreshed] = {
};

AutoRefreshed /: CloudDeploy[
    ct:AutoRefreshed[
        expr_, 
        tspec:Except[_Rule|_RuleDelayed]:3600, 
        fmt:Except[_Rule|_RuleDelayed]:"WL", 
        o:OptionsPattern[AutoRefreshed]
    ], 
    co_CloudObject, 
    oCD:OptionsPattern[CloudDeploy]
    ] := 
    Catch[autoRefreshedStaging[AutoRefreshed[expr, tspec, fmt, o], co, oCD], $tag]


(*
 * This function hijacks the InstantAPIServer internals to provide the documented support
 * for export and response forms.
 *) 
SetAttributes[exploy, HoldFirst];
Options[exploy] = Options[CloudDeploy];

exploy[expr_, fmt_, dest:CloudObject[uri_, destOpts:OptionsPattern[CloudObject]], o:OptionsPattern[]] := 
	Replace[
		Block[
	    	{$EvaluationEnvironmentParameters = <||>},
            (* This will basically use Delayed to render. *)
	    	System`GenerateHTTPResponse[AutoRefreshed[expr, None, fmt]][
	    		{"Body", "ContentType", "CharacterEncoding"}
	    	]
	    ],
        {body_, contentType_, charset_} :> 
            writeObject[
                dest, 
                ToCharacterCode[body], 
                contentType <> ";charset=" <> charset,
                OptionValue[Permissions],
                OptionValue[CloudObject, {destOpts}, IconRules],
                Unevaluated[expr],
                OptionValue[CloudObject, {destOpts}, MetaInformation],
                {},
                AutoRefreshed
            ]	
	]
 
autoRefreshedStaging[AutoRefreshed[expr_, tspec_, fmt_, o:OptionsPattern[]], co_CloudObject, oCD:OptionsPattern[]] := Module[
    {taskObj, taskUuid, contentObj, uuid, perm},
    (* Create bundle (directory) *)
    responseCheck[
        Replace[execute[co, Automatic, UseUUID -> False, Type -> "application/vnd.wolfram.bundle.autorefreshed"], {
            HTTPError[400, ___] :> (Message[CreateDirectory::filex, co]; Throw[$Failed, $tag])
        }],
        AutoRefreshed,
        co
    ];

	Check[
        uuid = GetUUID[co];

	    (* (This works for unnamed bundles too) *)
	    contentObj = FileNameJoin[{co, uuid <> "-content"}];
	    (* To get task obj into unnamed bundle, Put placeholder first *)
	    taskObj = FileNameJoin[{co, uuid <> "-task"}];
	    CloudPut[1, taskObj];
	    taskUuid = GetUUID[taskObj];
	    taskObj = cloudObjectFromUUID[taskUuid];

        perm = OptionValue[CloudDeploy, {oCD}, Permissions];

		With[{dest = contentObj, f = CloudObject`Private`exploy, p = perm},
            (* Uncomment expr if you want a synchronous (cloud) evaluation to take place here, not in service kernel *)
			CloudEvaluate[f[ (*expr*) "Object pending evaluation", fmt, dest, Permissions -> p]]
		];

        (* Deploy generating task *)
	    With[{taskOpts = Sequence @@ Join[FilterRules[Flatten[{o}], Options[ScheduledTask]], {"Visible" -> False}],
	    	dest = contentObj, permOpt = RuleDelayed[Permissions, Lookup[Options[co, Permissions], Permissions]]},
	    	iCloudDeployScheduledTask[
		        ScheduledTask[
		        	CloudObject`Private`exploy[expr, fmt, dest, permOpt],
		        	tspec,
		        	taskOpts
		        ],
		        taskObj,
		        oCD
	    	]
	    ];

        (* Track contents in bundle meta *)
        SetOptions[co, {
            MetaInformation -> Join[
                {"__Content" -> GetUUID[contentObj], "__Task" -> taskUuid},
                Flatten[{OptionValue[CloudDeploy, {oCD}, MetaInformation]}]
            ]
            ,
            Permissions -> perm
            (* bad interaction between icon rules and bundle, possibly weird mime type: IconRules -> OptionValue[CloudDeploy, {oCD}, IconRules] *)
        }];
        (* Fill in content! *)
        RunScheduledTask @@ {taskObj};
        ,
	    
        Throw[$Failed, $tag]
    ];

    co
];


(* etc. *)
GetNameFromURI[uri_String] := With[{split = StringSplit[uri, "/"]},
	If[Length[split] < 2, Message[ScheduledTask::nouri, uri]; Throw[$Failed, $tag], Last[split]]
]

GetUUID[obj_CloudObject] := Module[{res}, If[MatchQ[res = getCloudAndUUID[obj], {_, id_String}], Last[res], Throw[$Failed, $tag]]]
GetUUID[obj_String] := GetUUID[CloudObject[obj]]
GetUUID[___] := Throw[$Failed, $tag]

safeCloudAndUUIDFetch[CloudObject`Private`deleteable[obj_CloudObject], mh_:ScheduledTask] := safeCloudAndUUIDFetch[obj, mh];
safeCloudAndUUIDFetch[CloudObject`Private`preexisting[obj_CloudObject], mh_:ScheduledTask] := safeCloudAndUUIDFetch[obj, mh];
safeCloudAndUUIDFetch[obj_CloudObject, mh_:ScheduledTask] := Replace[getCloudAndUUID[obj], {
	{_, None} :> (Message[mh::cloudnf, obj]; Throw[$Failed, $tag])
}];
safeCloudAndUUIDFetch[None, mh_] := {$CloudBase, Null};
safeCloudAndUUIDFetch[___] := Throw[$Failed, $tag];


Protect[ScheduledTask, CloudObject, ScheduledTasks, EvaluateScheduledTask, ScheduledTaskInformation, ScheduledTaskInformationData,
    CloudSubmit, ContinuousTask, AbortScheduledTask, AutoRefreshed];

$Flag = False;


(* begin helper functions for timespec2cron[DateObject] *)
DatePatternQ[list_List] := MatchQ[list, {_?DatePatternElementQ ..}]

$DaysOfTheWeek = {Sunday, Monday, Tuesday, Wednesday, Thursday, 
   Friday, Saturday};
DatePatternElementQ[_?NumberQ] := True
DatePatternElementQ[Verbatim[Blank[]]] := True
DatePatternElementQ[day_Symbol] := MemberQ[$DaysOfTheWeek, day]
DatePatternElementQ[
  Verbatim[Alternatives][_?DatePatternElementQ ..]] := True
DatePatternElementQ[___] := False

QuartzValueQ[year_Integer, {1}] := TrueQ[1970 <= year <= 2099]
QuartzValueQ[month_Integer, {2}] := TrueQ[1 <= month <= 12]
QuartzValueQ[dayofmonth_Integer, {3}] := TrueQ[1 <= dayofmonth <= 31]
QuartzValueQ[hour_Integer, {4}] := TrueQ[0 <= hour <= 23]
QuartzValueQ[minute_Integer, {5}] := TrueQ[0 <= minute <= 59]
QuartzValueQ[seconds_Integer, {6}] := 
 TrueQ[0 <= seconds <= 59](*probably need numberQ*)

QuartzValueQ[___] := False


Clear[ElementToCron]
ElementToCron[element_?DatePatternElementQ, n : Except[{3}]] := 
  Which[QuartzValueQ[element, n], ToString[Ceiling[element]], 
   MatchQ[element, Verbatim[Blank[]]], "*", 
   MatchQ[element, 
    Verbatim[Alternatives][
     Repeated[PatternTest[Blank[], QuartzValueQ[#, n] &]]]], 
   StringJoin[
    Riffle[ToString[Ceiling[#]] & /@ (List @@ element), ","]], True, 
   Throw[$Failed, $tag]];

(*DOM[] is a wrapper for "day of the month" and DOW[] is for "day of \
the week"*)

ElementToCron[day_?DatePatternElementQ, n : {3}] := 
 Which[QuartzValueQ[day, n], DOM[ToString[Ceiling[day]]], 
  MatchQ[day, Verbatim[Blank[]]], DOM["*"], 
  MemberQ[$DaysOfTheWeek, Verbatim[day]], 
  DOW[ToString[First[Flatten[Position[$DaysOfTheWeek, day]]]]], 
  MatchQ[day, 
   Verbatim[Alternatives][
    Repeated[PatternTest[Blank[], MemberQ[$DaysOfTheWeek, #] &]]]], 
  DOW[StringJoin[
    Riffle[ToString[
        First[Flatten[Position[$DaysOfTheWeek, #]]]] & /@ (List @@ 
        day), ","]]], 
  MatchQ[day, 
   Verbatim[Alternatives][
    Repeated[PatternTest[Blank[], QuartzValueQ[#, n] &]]]], 
  DOM[StringJoin[
    Riffle[ToString[Ceiling[#]] & /@ (List @@ day), ","]]], True, 
  Throw[$Failed, $tag]]

ElementToCron[___] := Throw[$Failed, $tag]

currentTime[spec_] := DateString[DateList[], spec]

(* should probably get minutes and seconds in here as well 
Changed the name here
*)
$current := {currentTime["MonthShort"], currentTime["DayShort"], 
  currentTime["HourShort"], currentTime["MinuteShort"], 
  currentTime["SecondShort"]}

Clear[PadAppropriately];
PadAppropriately[list_List] := 
 Join[list, Take[$current, {Length[list], -1}]]


Clear[OrderForDOM, OrderForDOW];
OrderForDOM[list_List] := 
 Insert[Reverse[PadAppropriately[list]], "?", 6]

OrderForDOW[list_List] := 
 Insert[Part[PadAppropriately[list], {6, 5, 4, 2, 3, 1}], "?", 4]


DateObjectToCronSpecification[
  HoldPattern[dObj : DateObject[date_List, ___?OptionQ]?DateObjectQ], 
  target_: $TimeZone] := 
 With[{d = DateAndTime`DateObjectToDateList[dObj, target]}, 
  StringJoin[
   Riffle[Join[
     Reverse[ToString[Ceiling[#]] & /@ Rest[d]], {"?"}, {ToString[
       First[d]]}], " "]]]

DateObjectToCronSpecification[
  HoldPattern[
   dObj : DateObject[_List, 
      TimeObject[time_List, ___?OptionQ], ___?OptionQ]?DateObjectQ], 
  target_: $TimeZone] := 
 With[{d = DateAndTime`DateObjectToDateList[dObj, target]}, 
  StringJoin[
   Riffle[Join[
     ToString[Ceiling[#]] & /@ 
      Reverse[Rest[d]], {"?"}, {ToString[First[d]]}], " "]]]

DateObjectToCronSpecification[DateObject[l_?DatePatternQ]] := 
 If[0 < Length[l] < 7, 
  Catch[With[{cron = MapIndexed[ElementToCron, l]}, 
    StringJoin[
     Riffle[If[FreeQ[cron, DOW], OrderForDOM[cron], 
        OrderForDOW[cron]] /. {DOM[d_] :> d, DOW[d_] :> d}, 
      " "]]], $tag], $Failed]

DateObjectToCronSpecification[___] := $Failed


End[]

EndPackage[]
