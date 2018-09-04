BeginPackage["StreamLink`"]

StreamLinkBegin

Begin["`Private`"]

(* This must be global, otherwise the library will be unloaded when the Module exits. *)
createStreamLinkName

StreamLinkBegin[] :=
	Module[{name, link},
		createStreamLinkName = LibraryFunctionLoad["StreamLink", "getLinkName",  {}, "UTF8String"];
		name = createStreamLinkName[];
		link = LinkConnect[name, LinkProtocol->"IntraProcess"];
		$ParentLink = link;
	]

End[]

EndPackage[]