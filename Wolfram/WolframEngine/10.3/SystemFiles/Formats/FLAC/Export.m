(* ::Package:: *)

ImportExport`RegisterExport[
 "FLAC",
 System`Convert`FLACDump`ExportFLAC,
 "Options" -> {"AudioChannels", "SampleDepth", "SampleRate"},
 "FunctionChannels" -> {"FileNames"},
 "BinaryFormat" -> True
]
