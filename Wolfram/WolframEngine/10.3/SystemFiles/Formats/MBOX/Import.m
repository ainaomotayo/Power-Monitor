(* ::Package:: *)

Begin["System`Convert`MBoxDump`"]


ImportExport`RegisterImport[
 "MBOX",
 {
   "Elements" :> getElements,
  {"From" | "Cc" | "To" | "Subject" | "Date" | "EmailClient" | "MIMEVersion" | "ReplyTo"} :> (ImportLog[##,
     "RestrictTo" -> {"From", "Cc", "To", "Subject", "Date", "EmailClient",
       "MIMEVersion", "ReplyTo"}] &),
  "Attachments" :>  ("Attachments" -> ("Attachments" /. ImportLog[##, "RestrictTo" -> "Attachments"]) &),
  "RawAttachments" :> ("RawAttachments" -> ("RawAttachments" /. ImportLog[##, "RestrictTo" -> "RawAttachments"]) &),
  "Data" :> ("Data" -> ("Data" /. ImportLog[##, "RestrictTo" -> "Data"]) &),
  "RawData" ->  ("RawData" -> ImportLog[##, "RestrictTo" -> "RawData"] &),
  "Messages" ->  ("Messages" -> ImportLog[##, "RestrictTo" -> "Messages"] &),
  ImportLog
 },
 "AvailableElements" -> {"Attachments", "Cc", "Data", "Date", "EmailClient", "From",
			"Messages", "MIMEVersion", "RawAttachments", "RawData", "ReplyTo",
			"Subject", "To"},
 "Sources" -> ImportExport`DefaultSources["MBox"]
]


End[]