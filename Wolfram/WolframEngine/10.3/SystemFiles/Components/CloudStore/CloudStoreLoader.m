(* All loading of the paclet's Wolfram Language code should go through this file. *)

(* Developer maintains this list of symbols.
   autoloadSymbols must agree with the symbols listed in the Kernel extension in the PacletInfo.m file.
*)        

PacletManager`Package`loadWolframLanguageCode["CloudStore", "CloudStore`", DirectoryName[$InputFileName], 
	"Main.m",
	"AutoUpdate" -> False, "ForceMX" -> False, "Lock" -> False, 
	"AutoloadSymbols" -> {
		"System`CloudStore",
		"System`CreateCloudStore",
		"System`DeleteCloudStore",
		"System`CloudStores"
	}
]
