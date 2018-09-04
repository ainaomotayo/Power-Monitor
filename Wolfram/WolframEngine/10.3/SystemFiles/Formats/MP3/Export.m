(* ::Package:: *)

ImportExport`RegisterExport[
 "MP3",
 System`Convert`AudioDump`ExportAudio["MP3", ##]&,
 "Options" -> {"AudioChannels", "CompressionLevel", "SampleRate"},
 "Sources" -> ImportExport`DefaultSources["Audio"],
 "DefaultElement" -> "Sound",
 "FunctionChannels" -> {"FileNames"},
 "BinaryFormat" -> True
]
