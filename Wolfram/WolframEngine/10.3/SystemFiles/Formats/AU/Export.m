(* ::Package:: *)

Begin["System`Convert`AudioDump`"]


ImportExport`RegisterExport[
 "AU",
 ExportAudio["AU", ##]&,
 "Options" -> {"AudioChannels", "AudioEncoding", "SampleRate"},
 "Sources" -> ImportExport`DefaultSources["Audio"],
 "DefaultElement" -> "Sound",
 "FunctionChannels" -> {"FileNames"},
 "BinaryFormat" -> True
]


End[]
