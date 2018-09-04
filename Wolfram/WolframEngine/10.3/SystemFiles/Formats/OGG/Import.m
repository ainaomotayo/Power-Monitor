(* ::Package:: *)

Begin["System`Convert`AudioDump`"]

ImportExport`RegisterImport[
 "OGG",
 ImportAudio["OGG", ##]&,
 {
   (* Post-processors *)
   "Sound" -> ElementsToSound,
   "SampledSoundList" -> ElementsToSampledSoundList
 },
 "DefaultElement" -> "Sound",
 "Options" -> {"AudioChannels", "SampleRate"},
 "Sources" -> ImportExport`DefaultSources["Audio"],
 "AvailableElements" -> {"AudioChannels", "Data", "SampledSoundList", "SampleRate", "Sound"},
 "FunctionChannels" -> {"FileNames"},
 "BinaryFormat" -> True
]


End[]
