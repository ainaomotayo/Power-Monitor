BeginPackage["OGGTools`"]
Begin["`Private`"]

InitOGGTools[] := 
  Module[{dir, dlls, OGGToolsDll, DLLTable}, 
   DLLTable = {"MacOSX-x86-64" -> {FileNameJoin[{$InstallationDirectory, "SystemFiles", "Links", 
         "OGGTools", "LibraryResources", "MacOSX-x86-64"}], {"libogg.dylib", "libogg.0.dylib", "libvorbis.dylib", "libvorbis.0.dylib", "libvorbisenc.dylib", "libvorbisenc.2.dylib", "libvorbisfile.dylib", "libvorbisfile.3.dylib"}}, 
     "Linux" -> {FileNameJoin[{$InstallationDirectory, "SystemFiles", "Links", 
     	"OGGTools", "LibraryResources", "Linux"}], {"libogg.so", "libogg.so.0", "libvorbis.so", "libvorbis.so.0", "libvorbisenc.so", "libvorbisenc.so.2", "libvorbisfile.so", "libvorbisfile.so.3"}}, 
     "Linux-x86-64" -> {FileNameJoin[{$InstallationDirectory, "SystemFiles", "Links", 
     	"OGGTools", "LibraryResources", "Linux-x86-64"}], {"libogg.so", "libogg.so.0", "libvorbis.so", "libvorbis.so.0", "libvorbisenc.so", "libvorbisenc.so.2", "libvorbisfile.so", "libvorbisfile.so.3"}}, 
     "Windows" -> {FileNameJoin[{$InstallationDirectory, "SystemFiles", "Links", 
     	"OGGTools", "LibraryResources", "Windows"}], {"msvcr100.dll", "libogg.dll", "libvorbis.dll", "libvorbisfile.dll"}}, 
     "Windows-x86-64" -> {FileNameJoin[{$InstallationDirectory, "SystemFiles", "Links", 
     	"OGGTools", "LibraryResources", 
         "Windows-x86-64"}], {"msvcr100.dll", "libogg.dll", "libvorbis.dll", "libvorbisfile.dll"}}};
   dlls = Cases[DLLTable, ($SystemID -> l_) :> l];
   If[dlls === {}, Message[OGGTools::sys, "Incompatible SystemID"];
    $InitOGGTools = $Failed;
    Return[$Failed]];
   {dir, dlls} = First@dlls;
   Needs["CCompilerDriver`"];
   LibraryLoad[FileNameJoin[{dir, #}]] & /@ dlls;
   OGGToolsDll = FileNameJoin[{dir, "OGGTools"}];
   $encodeVorbis = LibraryFunctionLoad[OGGToolsDll,"EncodeVorbis", {{_Real, _},"UTF8String",Integer, Real}, "UTF8String"];
   $decodeVorbis = LibraryFunctionLoad[OGGToolsDll,"DecodeVorbis", {"UTF8String"}, {_Real, _}];
   $readMetadataVorbis = LibraryFunctionLoad[OGGToolsDll, "ReadMetadataVorbis", {"UTF8String"}, {_Integer,_}];
   $InitOGGTools = If[$encodeVorbis === $Failed || $decodeVorbis === $Failed || $readMetadataVorbis === $Failed, $Failed, True];
   $InitOGGTools
];

End[]
EndPackage[]