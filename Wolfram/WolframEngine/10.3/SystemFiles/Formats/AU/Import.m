(* ::Package:: *)

Begin["System`Convert`AudioDump`"]


ImportExport`RegisterImport[
 "AU",
 ImportAudio["AU", ##]&,
 { (* Post-processors *)
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
