(* Mathematica Package *)

(* Created by the Wolfram Workbench Jan 9, 2014 *)

BeginPackage["UUID`"]

UUID::usage = "UUID generate a random UUID "

Begin["`Private`"]

$LibraryResourcePath = 
	FileNameJoin[{
		DirectoryName[$InputFileName],
		"LibraryResources",
		$SystemID
	}];
	
If[FreeQ[$LibraryPath, $LibraryResourcePath], 
	PrependTo[$LibraryPath, $LibraryResourcePath]
];

$UUIDLibrary = FindLibrary["uuid-link"];


initializeQ[] := initializeQ[] = (
	generateUUID = Quiet@LibraryFunctionLoad[$UUIDLibrary, "generateUUID", {}, "UTF8String"];
	generateUUID =!= $Failed
)

UUID[] /; initializeQ[] := generateUUID[] 

End[]

EndPackage[]
