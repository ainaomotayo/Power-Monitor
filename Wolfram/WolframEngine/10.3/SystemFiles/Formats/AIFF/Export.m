(* ::Package:: *)

Begin["System`Convert`AudioDump`"]


ImportExport`RegisterExport[
 "AIFF",
 ExportAudio["AIFF", ##]&,
 "Options" -> {"AudioChannels", "AudioEncoding", "SampleRate"},
 "Sources" -> ImportExport`DefaultSources["Audio"],
 "DefaultElement" -> "Sound",
 "FunctionChannels" -> {"FileNames"},
 "BinaryFormat" -> True
]


End[]
