(* ::Package:: *)

Begin["System`Convert`JSONDump`"]

parseJSON[filename_String, opts___] := Developer`ReadJSONFile[filename, "IssueMessagesAs" -> Import];
parseJSON[stream_InputStream, opts___] := Developer`ReadJSONStream[stream, "IssueMessagesAs" -> Import];

ImportExport`RegisterImport["RawJSON",
	parseJSON,  
  (* "AvailableElements" -> {"Data"},
  "DefaultElement" -> "Data", *)
	"FunctionChannels" -> {"FileNames", "Streams"}
  (* "Sources" -> {"Developer`"} *)
]

End[]
