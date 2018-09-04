BeginPackage["ProcessLink`"];

System`StartProcess;
System`RunProcess;
System`KillProcess;
System`ProcessInformation;
System`ProcessStatus;
System`ProcessObject;
System`Processes;
System`$SystemShell;

System`ProcessDirectory;
System`ProcessEnvironment;

(*TODO: move to Streams.m file*)
System`WriteLine;
System`ProcessConnection;
System`EndOfBuffer;
System`ReadString;
System`ReadLine;
(*TODO END*)


Begin["`Private`"];

(*TODO: move to Streams.m file*)
Clear[getCachedStream, ProcessConnection, cachedReadString, setStreamCache,
	getStreamCache, pushStreamCache, popStreamCache];

$Loaded;

If[!TrueQ[$Loaded],
	(* the cache always contains a string *)
	Clear[$StreamCache];
	$StreamCache[_] := "";

	(* this cache contains the actual streams *)
	Clear[$StreamObjectCache];
	$StreamObjectCache[___] := None;
	
	$Loaded = True;
];

(* Infinity works fine here, but it causes bug https://bugs.wolfram.com/show?number=277599
"ReadLine is incredibly slow on large file (5 megas)" *)
binaryReadBlock = 10000;

patternOfPatterns = (_Blank | _BlankSequence | _BlankNullSequence | _PatternTest | 
	_Pattern | _PatternSequence | _StringExpression | _Alternatives);

terminatorPattern = EndOfBuffer | EndOfFile | _String | patternOfPatterns;

safeStringTake[l_, n_] := If[Abs[n] > StringLength[l], l, StringTake[l, n]]
safeStringDrop[l_, n_] := If[Abs[n] > StringLength[l], "", StringDrop[l, n]]

HoldFirst[safeTimeConstrained];
safeTimeConstrained[expr_, time_, ret_] := If[time <= 0, ret, TimeConstrained[ReleaseHold[expr], time, ret]]

getCachedStream[p_, type_] := If[$StreamObjectCache[p, type] =!= None,
	$StreamObjectCache[p, type]
	,
	With[{fun = Switch[
		type,
		"StandardOutput", iGetInputStream,
		"StandardInput", iGetOutputStream,
		"StandardError", iGetErrorStream
	]},
		$StreamObjectCache[p, type] = fun[p];
		$StreamObjectCache[p, type]
	]
]

iGetOutputStream[ProcessObject[p_]] := If[NumberQ[p], 
	OpenWrite["in:" <> ToString[p], Method -> "RunPipe", BinaryFormat -> True], 
	Message[StartProcess::stopen, "standard output"]; $Failed]

iGetInputStream[ProcessObject[p_]] := If[NumberQ[p], 
	OpenRead["out:" <> ToString[p], Method -> "RunPipe", BinaryFormat -> True], 
	Message[StartProcess::stopen, "standard input"]; $Failed]

iGetInputStream[so_System`SocketObject] := With[
	{inputStreamOpened="InputStreamOpened"/.Socket`SocketInformation[so]},
	If[
		TrueQ[inputStreamOpened],
		"InputStream"/.Socket`SocketInformation[so]
		,
		OpenRead[so]
	]
]
iGetErrorStream[ProcessObject[p_]] := If[NumberQ[p], 
	OpenRead["err:" <> ToString[p], Method -> "RunPipe", BinaryFormat -> True], 
	Message[StartProcess::stopen, "stardard error"]; $Failed]

processStreamQ[st_InputStream] := 
  MemberQ[Method /. Options[st], "RunPipe"];
processStreamQ[pr_ProcessObject] := True;
processStreamQ[___] := False;

popStreamCache[st_InputStream] := With[{c = $StreamCache[st]},
	Quiet[Unset[$StreamCache[st]], Unset::norep];
	c
]
popStreamCache[pr_ProcessObject] := popStreamCache[getCachedStream[pr, "StandardOutput"]]

popStreamCache[___] := $Failed

pushStreamCache[st_InputStream, value_String] := With[{c = $StreamCache[st]},
	$StreamCache[st] = c <> value;
]

pushStreamCache[pr_ProcessObject] := pushStreamCache[getCachedStream[pr, "StandardOutput"]]

pushStreamCache[___] := $Failed

getStreamCache[st_InputStream] := $StreamCache[st]

getStreamCache[pr_ProcessObject] := getStreamCache[getCachedStream[pr, "StandardOutput"]]

getStreamCache[___] := $Failed

setStreamCache[st_InputStream, value_String] := $StreamCache[st] = value;


setStreamCache[pr_ProcessObject, value_String] := setStreamCache[getCachedStream[pr, "StandardOutput"], value]

setStreamCache[___] := $Failed

(* read all from a stream (non-blocking), including its cache, and empty the cache
returns _String | EndOfFile | $Failed *)
cachedReadString[st_InputStream] := With[{l = BinaryReadListEOF[st]},
	If[l === EndOfFile,
		With[{c = popStreamCache[st]}, If[c =!= "", c, EndOfFile]],
		popStreamCache[st] <> StringJoin@@l
	]
]
cachedReadString[so_System`SocketObject] :=cachedReadString[getCachedStream[so, "StandardOutput"] ]
cachedReadString[pr_ProcessObject] := cachedReadString[getCachedStream[pr, "StandardOutput"]]
cachedReadString[_] := $Failed

(* non-blocking binary read list, but can return EOF (output is _String | EOF)  *)
BinaryReadListEOF[st_] := Module[{l},
	l = BinaryReadList[st, "Character8", binaryReadBlock, "AllowIncomplete" -> True];
	If[l =!= {},
		l,
		l = BinaryRead[st, "Character8", "AllowIncomplete" -> True];
		Switch[l,
			Block, {},
			EndOfFile, l,
			_String, {l},
			_, {}
		]
	]
]

getMaxStringLength[_] := $Failed
getMaxStringLength[str_String] := StringLength[str]
getMaxStringLength[alt_Alternatives] := With[{lens = getMaxStringLength /@ List@@alt},
	If[Cases[lens, $Failed, {1}, 1] === {},
		Max[lens],
		$Failed
	]
]

ReadString::notfound = "Specified terminator not found.";

ReadString::iterm = "Invalid terminator value `1`.";

(* higher performance implementation of ReadString, only works on terminators that are strings or string alternatives *)
(* GenericGetString reads the stream every time, storing the result in a buffer, and then
uses Position in the full buffer to look for terminator. That is very wasteful, when the buffer
becomes too big, Position gets slower and slower. quickGetString only looks for the terminator in
the last n bytes of the buffer + the most recently read characters, where n is the string length
of the terminator.
 *)
processStreamQuickGetString[st_, terminator_, constraintTime_] := Module[
	{termlen, oldbuf = "", lastbuf = "", str, pos = {}, first = True, startTime, readTmp, aborted = False},
	
	startTime = SessionTime[];
	
	(* oldbuf = buffer of all text that was already checked
	lastbuf = some extra remaining chars that can still contain parts of the terminator
	str = most recently read buffer *)
	
	termlen = getMaxStringLength[terminator];
	
	While[pos === {} && !aborted,
		If[first,
			str = popStreamCache[st];
			first = False;
			,
			readTmp = safeTimeConstrained[BinaryReadListEOF[st], constraintTime - (SessionTime[] - startTime), $TimedOut];
			str = Switch[readTmp,
				$TimedOut, aborted = True; "",
				EndOfFile, readTmp,
				_, StringJoin @@ readTmp
			];
		];
		If[!StringQ[str],
			lastbuf = oldbuf <> lastbuf;
			Return[If[lastbuf === "", str, Message[ReadString::notfound]; lastbuf]]
		];
		lastbuf = lastbuf <> str;
		str = "";
		
		pos = StringPosition[lastbuf, terminator];
		If[pos === {},
			oldbuf = oldbuf <> safeStringDrop[lastbuf, -(termlen - 1)];
			lastbuf = safeStringTake[lastbuf, -(termlen - 1)];
		];
		
		If[constraintTime <= SessionTime[] - startTime, aborted = True];
	];
	
	If[pos =!= {},
		setStreamCache[st, StringDrop[lastbuf, pos[[1, 2]]]];
		oldbuf <> StringTake[lastbuf, pos[[1, 1]] - 1]
		,
		oldbuf <> lastbuf
	]
]

delimitersToStringList[term_]:= Switch[Head[term], 
	String, {term}, 
	Symbol,{},
	_,Complement[Apply[List,term],{EndOfFile}]];
 
generalStreamQuickGetString[st_, terminator_, constraintTime_] := 
	Read[st, Record, RecordSeparators->delimitersToStringList[terminator] ]

(* simple generic version of ReadString, can work for any delimiter, but it is most likely very
inefficient, except for simple terminators like EndOfFile and EndOfBuffer *)
genericGetString[st_, terminator_, constraintTime_] := Module[{str, buff = "", pos = {}, startTime, aborted = False},
	startTime = SessionTime[];
	While[pos === {} && !aborted,
		str = cachedReadString[st];
		If[!StringQ[str],
			Return[If[buff === "", str, Message[ReadString::notfound]; buff]]
		];
		buff = buff <> str;
		pos = StringPosition[buff, terminator];

		If[constraintTime <= SessionTime[] - startTime, aborted = True];
	];

	If[pos =!= {},
		setStreamCache[st, StringDrop[buff, pos[[1, 2]]]];
		StringTake[buff, pos[[1, 1]] - 1]
		,
		buff
	]
]

genericGetString[st_, EndOfBuffer, constraintTime_] := cachedReadString[st]

processStreamGetStringAll[st_, constraintTime_] := Module[{bag = Internal`Bag[], res = "", startTime, aborted = False},
	startTime = SessionTime[];
	res = cachedReadString[st];
	If[StringQ[res], Internal`StuffBag[bag,  res]];
	
	While[res =!= EndOfFile && !aborted,
		res = safeTimeConstrained[BinaryReadListEOF[st], constraintTime - (SessionTime[] - startTime), $TimedOut];
		
		Switch[res,
			$TimedOut, aborted = True,
			EndOfFile, None,
			_, Internal`StuffBag[bag, StringJoin@@res]
		];
	];
	StringJoin@@Internal`BagPart[bag, All]
]
generalStreamGetStringAll[st_, constraintTime_] :=  Read[st, Record, RecordSeparators->{} ];

genericGetString[st_, EndOfFile, constraintTime_] :=	
	If[processStreamQ[st],
		processStreamGetStringAll[st, constraintTime],
		generalStreamGetStringAll[st, constraintTime]
	]
	
(* reads full contents until terminator is found; by default term is EndOfFile so it reads the stream fully *)
Options[ReadString] = {
	TimeConstraint -> Infinity
};

ReadString[st:(_ProcessObject | _InputStream | _System`SocketObject), terminator : terminatorPattern : EndOfFile, opts:OptionsPattern[]] := With[
	{termlen = getMaxStringLength[terminator]},
	If[NumberQ[termlen],
		If[processStreamQ[st],
			processStreamQuickGetString[st, terminator, OptionValue[TimeConstraint]],
			generalStreamQuickGetString[st, terminator, OptionValue[TimeConstraint]]
			],
		genericGetString[st, terminator, OptionValue[TimeConstraint]]
	]
]

ReadString[filename_String, terminator : terminatorPattern : EndOfFile, opts:OptionsPattern[]] := Module[{file, result},
	file = OpenRead[filename, BinaryFormat -> True];
	If[!MatchQ[file, _InputStream], Return[$Failed]];
	result = ReadString[file, terminator, TimeConstraint -> OptionValue[TimeConstraint]];
	Close[file];
	result
]

ReadString[st:(_ProcessObject | _InputStream), terminator:_] /; (Message[ReadString::iterm, terminator]; False) := None

ReadString[args___] /; (If[Length[{args}] < 1 || Length[{args}] > 2, Message[ReadString::argt, ReadString, Length[{args}], 1, 2]]; False) := None

(*  reads line until completion, blocks; can return EndOfFile if no more to read *)
Options[ReadLine] = {
	TimeConstraint -> Infinity
};

ReadLine[st:(_ProcessObject | _InputStream|_System`SocketObject), opts:OptionsPattern[]] :=
	Quiet[ReadString[st, ("\n" | "\r\n" | "\r" ), TimeConstraint -> OptionValue[TimeConstraint]], ReadString::notfound]

ReadLine[args___] /; (If[Length[{args}] =!= 1, Message[ReadLine::argx, ReadLine, Length[{args}]]]; False) := None


ProcessObject /: BinaryReadList[p : ProcessObject[_], args___] := BinaryReadList[getCachedStream[p, "StandardOutput"], args]

ProcessObject /: BinaryRead[p : ProcessObject[_], args___] := BinaryRead[getCachedStream[p, "StandardOutput"], args]

ProcessObject /: BinaryWrite[p : ProcessObject[_], args___] := BinaryWrite[getCachedStream[p, "StandardInput"], args]

ProcessConnection[pr : ProcessObject[p_], stream : ("StandardOutput" | "StandardInput" | "StandardError")] := getCachedStream[pr, stream]

ProcessConnection[args___] /; (If[Length[{args}] =!= 2, Message[ProcessConnection::argrx, ProcessConnection, Length[{args}], 2]]; False) := None

ProcessObject /: Import[p : ProcessObject[_], args___] := Import[getCachedStream[p, "StandardOutput"], args]
 
ProcessObject /: Read[p : ProcessObject[_], args___] := Read[getCachedStream[p, "StandardOutput"], args]

ProcessObject /: Write[p : ProcessObject[_], args___] := Write[getCachedStream[p, "StandardInput"], args]
 
ProcessObject /: WriteString[pr : ProcessObject[_], args___] := BinaryWrite[pr, args]


WriteLine[st:(_ProcessObject | _OutputStream |_System`SocketObject), str_String] := (WriteString[st, str <> "\n"]) 

WriteLine[args___] /; (If[Length[{args}] =!= 2, Message[WriteLine::argrx, WriteLine, Length[{args}], 2]]; False) := None

ReadString::usage = "ReadString[\"file\"] reads the complete contents of a file and returns it as a string.\nReadString[stream] reads everything from a stream and returns it as a string.\nReadString[proc] reads everything generated by an external process and returns it as a string.\nReadString[src,term] reads until the terminator term is encountered.";
ProcessConnection::usage = "ProcessConnection[proc, \"stream\"] returns the stream object for a given stream.";
WriteLine::usage = "WriteLine[stream, \"string\"] writes \"string\", followed by a newline, to the specified output stream.\nWriteLine[proc, \"string\"] writes \"string\" to an external process proc.";
EndOfBuffer::usage = "EndOfBuffer is a symbol that represents the end of currently available data in the buffer for a process or stream.";
ReadLine::usage = "ReadLine[stream] reads a line of text from a stream and returns it as a string.\nReadLine[proc] reads a line of text generated by an external process and returns it as a string.";


SetAttributes[
	{ProcessConnection, ReadLine, ReadString, WriteLine},
	{Protected, ReadProtected}];
(*TODO END*)
Clear[runProcess, KillProcess, RunProcess, startProcess, commandToAbsolutePath];

WindowsQ = !StringFreeQ[$System, "Windows"];

$SystemShell = If[WindowsQ, "cmd", "/bin/sh"];

addRunningProcess[rp_] := ($RunningProcesses = Flatten[{$RunningProcesses, rp}];);
removeRunningProcesses[rps_] := ($RunningProcesses = Complement[$RunningProcesses, rps]);  

isValidProcess[proc_]:= MemberQ[$RunningProcesses, proc];
isNotValidProcess[proc_]:= Not[isValidProcess[proc]];
If[!StringQ[$libraryPath],
	$libraryPath = FindLibrary["libProcessLink"];
	If[$libraryPath === $Failed,
		Message[LibraryFunction::load, "libProcessLink"];
	,
		$RunningProcesses = {};
		If[LibraryLoad[$libraryPath] === $Failed, 
			Message[LibraryFunction::load, $libraryPath]];
	];
];
maxRunArgs = 200;
runs = Quiet[
	Map[
		LibraryFunctionLoad[$libraryPath, "run", Join[{Integer, "UTF8String"}, "UTF8String" & /@ Range[#]], Integer] &,
		Range[maxRunArgs + 1]
	],
	LibraryFunction::overload];
killProcess = LibraryFunctionLoad[$libraryPath, "killProcess", {Integer, Integer}, "Boolean"];
freeProcess = LibraryFunctionLoad[$libraryPath, "freeProcess", {Integer}, "Void"];
hasFinishedQ = LibraryFunctionLoad[$libraryPath, "hasFinishedQ", {Integer}, "Boolean"];
getExitValue = LibraryFunctionLoad[$libraryPath, "getExitValue", {Integer}, Integer];
hasExitValue = LibraryFunctionLoad[$libraryPath, "hasExitValue", {Integer}, "Boolean"];
waitFor = LibraryFunctionLoad[$libraryPath, "waitFor", {Integer}, "Void"];

environmentToList[environment_Association] :=
	Select[StringJoin[ToString[StringTrim[#]] & /@ {#[[1]], "=", #[[2]]}] & /@ Normal[environment],
		StringCount[#, "="] == 1 &]

StartProcess::argtm = "Too many arguments.";

absoluteFileNameQ[file_] := With[{abs = Quiet[ExpandFileName[file]]},
	StringQ[abs] && StringMatchQ[abs, file, IgnoreCase -> True]
];

caseInsensitiveValue[a_Association, key_String] := With[
	{values = Select[Normal[a], StringMatchQ[key, #[[1]], IgnoreCase -> True] &]},
	
	If[MatchQ[values, {__}],
		values[[1, 2]],
		Missing["KeyAbsent", key]
	]	
];

getEnvValue[environment_Association, key_String] := If[WindowsQ,
	caseInsensitiveValue[environment, key],
	environment[key]
];

commandToAbsolutePath[command_String, environment_Association, processDirectory_String] := Module[
	{paths, systemPath},

	If[absoluteFileNameQ[command], 
		Return[If[FileExistsQ[command] && Not[DirectoryQ[command]], command, $Failed]]];

	systemPath = getEnvValue[environment, "PATH"];
	systemPath = If[StringQ[systemPath],
		StringSplit[systemPath, If[WindowsQ, ";", ":"]],
		None
	];
	
	paths = Select[Flatten[{processDirectory, Directory[], systemPath}],
		StringQ[#] && absoluteFileNameQ[#] && DirectoryQ[#] &];
	
	paths = FileNameJoin[{#, command}] & /@ paths;
	
	If[WindowsQ && !StringMatchQ[command, ___ ~~ (".exe" | ".com"), IgnoreCase -> True],
		paths = DeleteDuplicates[Flatten[{paths, (# <> ".exe") & /@ paths, (# <> ".com") & /@ paths}]]
	];
	
	paths = Select[paths, FileExistsQ, 1];
	
	If[MatchQ[paths, {__}], paths[[1]], $Failed]
];

commandToAbsolutePath[command_, environment_, processDirectory_] := $Failed;

startProcess[method_String, {cmd_, args___}, environment : (_Association | Inherited), processDirectory : (_String | Inherited)] :=
Module[{cmd2, commands, runid, fullArgs, retid, currDir, environment2, envList, process},
	
	environment2 = If[environment === Inherited, Association[GetEnvironment[]], environment];
	envList = environmentToList[environment2];
	currDir = If[processDirectory === Inherited, Directory[], processDirectory];

	cmd2 = commandToAbsolutePath[cmd, environment2, currDir];
	If[cmd2 === $Failed,
		If[method == "StartProcess", Message[StartProcess::pnfd, cmd], Message[RunProcess::pnfd, cmd]];
		Return[$Failed]
	];
	
	commands = {cmd2, args};
	(* we remove the quotes on windows, the arguments have to be parsed by the OS, so quotes on commands aren't harmless.
	To be improved *)
	commands = If[WindowsQ, StringReplace[#, "\"" -> ""]& /@ commands, commands];
	(* we find the function in "runs" which has the right number of arguments, and we call it *)
	runid = Length[Join[commands, envList]];
	If[runid <= maxRunArgs + 1,
		fullArgs = Flatten[{Length[envList], currDir, commands, envList}];
		retid = runs[[runid]] @@ fullArgs;
		If[!NumberQ[retid],
			If[method == "StartProcess", Message[StartProcess::pnfd, cmd], Message[RunProcess::pnfd, cmd]];
			$Failed
			,
			process = ProcessObject[retid];
			addRunningProcess[process];
			process
		]
		,
		Message[StartProcess::argtm]; $Failed
	]
]

startProcess[_, _, environment : (_Association | Inherited), processDirectory : (_String | Inherited)] :=
	Message[StartProcess::nffil, method];

Options[StartProcess] = {
	ProcessEnvironment -> Inherited,
	ProcessDirectory -> Inherited
};

StartProcess[command_String, opts:OptionsPattern[]] :=
	startProcess["StartProcess", {command}, OptionValue[ProcessEnvironment], OptionValue[ProcessDirectory]]

StartProcess[{command_String, args: (_String | (_String -> _String))...}, opts:OptionsPattern[]] := With[
	{cmdlist = ToString /@ Prepend[Flatten[List @@@ List[args]], command]},
	startProcess["StartProcess", cmdlist, OptionValue[ProcessEnvironment], OptionValue[ProcessDirectory]]
]

StartProcess[args___] /; (If[Length[{args}] < 1 || Length[{args}] > 2, Message[StartProcess::argt, StartProcess, Length[{args}], 1, 2]]; False) := None 

StartProcess::stopen = "Could not open the `1` stream"; 

iGetExitValue[ProcessObject[id_]] := If[hasExitValue[id], getExitValue[id], None]

iWaitFor[pr : ProcessObject[id_]] := (waitFor[id]; iGetExitValue[pr])

NoInputProvided;(* used as default value for RunProcess' input expr *)

(* interleave stream reads until EOF is found in all of them *)
readStreams[streams_List] := Module[{strings, rets = {}, len},
	len = Length[streams];
	strings = "" & /@ streams;
	While[Count[rets, EndOfFile | $Failed] =!= len,
		rets = ReadString[#, EndOfBuffer] & /@ streams;
		Map[
			(strings[[#]] = strings[[#]] <> Replace[rets[[#]], (EndOfFile | EndOfBuffer | $Failed) -> ""]) &,
			Range[len]];
	];
	strings
]

runProcessOutputs = {"ExitCode", "StandardOutput", "StandardError"};

runProcess[commands_List, environment : (_Association | Inherited), processDirectory : (_String | Inherited), inputexpr_, return_: All] := Module[
	{process, out, err, all, done = False},

	process = startProcess["RunProcess", commands, environment, processDirectory];
	If[!MatchQ[process, _ProcessObject], Return[$Failed]];
	(*WithLocalSettings is not docummented: args are preprocessing, code, postprocessing, will run both with
	abort and timeconstrained (checkAbort doesn't work with time constrained)*)
	Internal`WithLocalSettings[None,
		If[!MatchQ[process, ProcessObject[___]], Return[$Failed]];
		If[inputexpr =!= NoInputProvided, BinaryWrite[process, ToString[inputexpr]];Close[ProcessConnection[process, "StandardInput"]]];
		{out, err} = readStreams[{process, ProcessConnection[process, "StandardError"]}];
		all = With[{t = Thread[runProcessOutputs -> {iGetExitValue[process], out, err}]}, Association[t] ];
		done = True;
		If[return === All, all, all[return]]
	, (KillProcess[process])]
]

Options[RunProcess] = {
	ProcessEnvironment -> Inherited,
	ProcessDirectory -> Inherited
};

commandExistQ[command_String, envir_, dir_] := Module[{ e1 = envir, d1 = dir},
	If[e1 === Inherited, e1 = Association[GetEnvironment[]]];
	If[d1 === Inherited, d1 = Directory[]];
	StringQ[commandToAbsolutePath[command, e1, d1]]
	];
	
containsSpaceQ[s_String] := StringContainsQ[s, " "];

RunProcess[command_String, opts:OptionsPattern[]] := 
	If[commandExistQ[command, OptionValue[ProcessEnvironment], OptionValue[ProcessDirectory]],
		RunProcess[{command}, opts],
		If[containsSpaceQ[command], Message[RunProcess::pnfds, command], Message[RunProcess::pnfd, command]];$Failed]
	
RunProcess[commands_List, opts:OptionsPattern[]] := 
	runProcess[commands, OptionValue[ProcessEnvironment], OptionValue[ProcessDirectory], NoInputProvided, All]

RunProcess[command_String, ret: (All | Alternatives @@ runProcessOutputs), inputexpr_ : NoInputProvided, opts:OptionsPattern[]] /; !MatchQ[inputexpr, _Rule | _RuleDelayed] :=
	If[commandExistQ[command, OptionValue[ProcessEnvironment], OptionValue[ProcessDirectory]],
		RunProcess @@ {{command}, ret, inputexpr, opts}, (* using @@ to get rid of a pointless Workbench warning *)
		If[containsSpaceQ[command], Message[RunProcess::pnfds, command], Message[RunProcess::pnfd, command]];$Failed]
		 
RunProcess[commands_List, ret: (All | Alternatives @@ runProcessOutputs), inputexpr_ : NoInputProvided, opts:OptionsPattern[]] /; !MatchQ[inputexpr, _Rule | _RuleDelayed] :=
	runProcess[commands, OptionValue[ProcessEnvironment], OptionValue[ProcessDirectory], inputexpr, ret]

RunProcess[commands : (_List | _String), ret_String, inputexpr_ : NoInputProvided, opts:OptionsPattern[]] /; !MatchQ[inputexpr, _Rule | _RuleDelayed] :=
	With[{values = StringJoin@@Riffle[Append[ToString[#, InputForm] & /@ runProcessOutputs, "and All"], ", "]},
		Message[General::optvp, 2, "RunProcess", values];
	]

RunProcess[args___] /; (If[Length[{args}] < 1 || Length[{args}] > 3, Message[RunProcess::argb, RunProcess, Length[{args}], 1, 3]]; False) :=
	None 

KillProcess[po:ProcessObject[p_], signal_ : -1] := (
	(* we quiet the error about streams that have already been closed *)
	Quiet[Close[ProcessConnection[po, #]]& /@ {"StandardInput", "StandardOutput", "StandardError"}];
	killProcess[p, signal];
	FreeProcess[po];
)

KillProcess[args___ /; ArgumentCountQ[KillProcess, Length[{args}], 1, 2]] := $Failed /; False;

PackageScope["FreeProcess"];
FreeProcess[pobj : ProcessObject[p_]] := (
	removeRunningProcesses[{pobj}];
	freeProcess[p];
)

ProcessInformation[pr : ProcessObject[p_]] := With[{t = {"ExitCode" -> ProcessInformation[pr, "ExitCode"]}}, Association[t] ]

ProcessInformation[pr : ProcessObject[p_], "ExitCode"] := If[isNotValidProcess[ProcessObject[id]], None, iGetExitValue[pr]]

ProcessInformation[args___] /; (If[Length[{args}] < 1 || Length[{args}] > 2, Message[ProcessInformation::argt, ProcessInformation, Length[{args}], 1, 2]]; False) := None

ProcessStatus[ProcessObject[id_]] := If[isNotValidProcess[ProcessObject[id]]|| hasFinishedQ[id], "Finished", "Running"]
 
ProcessStatus[pr : ProcessObject[_], r_] := (ProcessStatus[pr] === r)

ProcessStatus[args___] /; (If[Length[{args}] < 1 || Length[{args}] > 2, Message[ProcessStatus::argt, ProcessStatus, Length[{args}], 1, 2]]; False) := None

Processes[] := (
	removeRunningProcesses[Select[$RunningProcesses, ProcessStatus[#] === "Finished" &]];
	$RunningProcesses
)

Processes[args__] /; (Message[Processes::argrx, Processes, Length[{args}], 0]; False) := None

StartProcess::usage = "StartProcess[\"executable\"] executes an external program, yielding a ProcessObject to represent the resulting subprocess.\nStartProcess[{\"executable\", arg 1, arg 2, ...}] executes an external program, passing it the specified arguments arg i.";
RunProcess::usage = "RunProcess[\"command\"] runs the specified external command, returning information on the outcome.\nRunProcess[{\"command\", arg 1, arg 2, \[Ellipsis]}] runs the specified command, with command-line arguments arg i.\nRunProcess[command, \"prop\"] returns only the specified property.\nRunProcess[command, prop, input] feeds the specified initial input to the command."  
KillProcess::usage = "KillProcess[proc] kills the external process represented by the ProcessObject proc.";
ProcessInformation::usage = "ProcessInformation[proc] gives information about an external process proc.\nProcessInformation[proc, \"prop\"] gives information about the property \"prop\".";
ProcessStatus::usage = "ProcessStatus[proc] gives the current status of the external process represented by the ProcessObject proc.\nProcessStatus[proc, \"status\"] returns True if the process has the status given, and returns False otherwise.";
ProcessObject::usage = "ProcessObject[...] is an object that represents a runnable external process.";
Processes::usage = "Processes[] returns a list of currently running external processes, started in this Wolfram Language session.";
$SystemShell::usage = "$SystemShell is a symbol that specifies the system shell for the OS that is currently being used.";

SetAttributes[
	{KillProcess, ProcessInformation, ProcessObject, Processes, ProcessStatus, RunProcess, StartProcess},
	{Protected, ReadProtected}];


End[];

EndPackage[];
