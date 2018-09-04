(* ::Package:: *)

ImportExport`RegisterExport[
 "OGG",
 System`Convert`AudioDump`ExportAudio["OGG", ##]&,
 "Options" -> {"AudioChannels", "CompressionLevel", "SampleRate"},
 "Sources" -> ImportExport`DefaultSources["Audio"],
 "DefaultElement" -> "Sound",
 "FunctionChannels" -> {"FileNames"},
 "BinaryFormat" -> True
]
