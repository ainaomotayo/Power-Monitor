(* ::Package:: *)

Begin["System`Convert`JSONDump`"]

writeJSON[filename_String, expr_, opts___] := 
	Developer`WriteJSONFile[filename, expr, "IssueMessagesAs" -> Export, opts];
writeJSON[stream_OutputStream, expr_, opts___] := 
	Developer`WriteJSONStream[stream, expr, "IssueMessagesAs" -> Export, opts];

ImportExport`RegisterExport["RawJSON",
	writeJSON,
	(* Developer`ToJSONFile[#1, #2, "IssueMessagesAs"->Export, ##3] & *)
	"FunctionChannels" -> {"FileNames", "Streams"}
    (* "Sources" -> {"Convert`JSON`"} *)
]

End[]