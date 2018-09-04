(* ::Package:: *)

Begin["System`Convert`AudioDump`"]


ImportExport`RegisterImport[
 "Wave64",
 ImportAudio["Wave64", ##]&,
 {
   (* Post-processors *)
   "Sound" -> ElementsToSound,
   "SampledSoundList" -> ElementsToSampledSoundList,
   "AudioEncoding" -> ElementsToAudioEncoding
 },
 "DefaultElement" -> "Sound",
 "Options" -> {"AudioChannels", "AudioEncoding", "SampleRate"},
 "Sources" -> ImportExport`DefaultSources["Audio"],
 "AvailableElements" -> {"AudioChannels", "AudioEncoding", "Data", "SampledSoundList", "SampleRate", "Sound"},
 "FunctionChannels" -> {"FileNames"},
 "BinaryFormat" -> True
]


End[]
