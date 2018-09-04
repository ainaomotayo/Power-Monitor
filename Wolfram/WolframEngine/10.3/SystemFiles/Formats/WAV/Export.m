(* ::Package:: *)

ImportExport`RegisterExport[
 "WAV",
 System`Convert`AudioDump`ExportAudio["WAV", ##]&,
 "Options" -> {"AudioChannels", "AudioEncoding", "SampleRate"},
 "Sources" -> ImportExport`DefaultSources["Audio"],
 "DefaultElement" -> "Sound",
 "FunctionChannels" -> {"FileNames"},
 "BinaryFormat" -> True
]
