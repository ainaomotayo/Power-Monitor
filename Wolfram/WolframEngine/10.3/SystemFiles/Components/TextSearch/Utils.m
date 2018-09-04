Package["TextSearch`"]

PackageScope["hasFE"]

PackageExport["CreateSearchIndexFromStrings"]

PackageScope["$linuxQ"]
PackageScope["$windowsQ"]

$linuxQ = !StringFreeQ[$SystemID, "Linux"];
$windowsQ = !StringFreeQ[$SystemID, "Windows"];

CreateSearchIndexFromStrings[strs_List] := With[
	{tmp = Quiet[CreateDirectory[FileNameJoin[{$TemporaryDirectory, "TextSearch"}]], CreateDirectory::filex]},
    CreateSearchIndex[
    	Export[FileNameJoin[{tmp, ToString[Hash @ #] <> ".txt"}], #, "Text"]& /@ strs
   	]
];


PackageScope["ReadStringASCII"]

(* this is what Import[..., "String"] does anyway. 
it's not UTF8 safe, though. unfortunately reading UTF8 is incredibly slow 
(see KERN/Kernel/StartUp/Convert/Text.m for how gross it is).
we should solve this at the kernel level.
 *)
ReadStringASCII[file_] := Module[{res, stream}, 
	Internal`WithLocalSettings[
		stream = OpenRead[file],
		res = Read[stream, Record, RecordSeparators -> {}],
		Close[stream]
	];
	Replace[res, EndOfFile -> ""]
];


PackageScope["FailUnless"]

SetAttributes[{FailUnless, makeMessage}, HoldAll];
FailUnless[test_, rest___][arg_] := If[test[arg], arg, makeMessage[rest]; Throw[$Failed]];

makeMessage[{head_, str_String}, args___] := With[{sym = head}, Message[MessageName[sym, str], args]];
makeMessage[] := Null;
makeMessage[args___] := Message[args];


