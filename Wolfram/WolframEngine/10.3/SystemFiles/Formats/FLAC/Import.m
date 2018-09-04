(* ::Package:: *)

Begin["System`Convert`FLACDump`"]


ImportExport`RegisterImport[
 "FLAC",
 { 
  	"Data"            :> ImportFLAC,
  	"Sound"           :> ImportFLACSound,
  	"SampledSoundList" :> ImportFLACSampledSoundList,
  	ImportFLACMetaInfo
 },
 "DefaultElement" -> "Sound",
 "Options" -> {"AudioChannels", "SampleDepth", "SampleRate"},
 "FunctionChannels" -> {"FileNames"},
 "AvailableElements" -> {"AudioChannels", "Data", "Duration", "Length", "MetaInformation",
                         "SampleDepth", "SampledSoundList", "SampleRate", "Sound"},  
 "Sources" -> ImportExport`DefaultSources[{"Audio", "FLAC"}],
 "BinaryFormat" -> True
]


End[]
