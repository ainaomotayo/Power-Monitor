(* ::Package:: *)

Begin["System`Convert`AudioDump`"]


ImportExport`RegisterImport[
 "SND",
 ImportAudio["SND", ##]&,
 { (* Post-processors *)
   "Sound" -> ElementsToSound,
   "SampledSoundList" -> ElementsToSampledSoundList,
   "AudioEncoding" -> ElementsToAudioEncoding
 },
 "AvailableElements" -> {"AudioChannels", "AudioEncoding", "Data", "SampledSoundList", "SampleRate", "Sound"},
 "DefaultElement" -> "Sound",
 "Options" -> {"AudioChannels", "AudioEncoding", "SampleRate"},
 "Sources" -> ImportExport`DefaultSources["Audio"],
 "FunctionChannels" -> {"FileNames"},
 "BinaryFormat" -> True
]


End[]
