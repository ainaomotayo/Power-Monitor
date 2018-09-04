(* Mathematica Package *)
  
BeginPackage["StripeLoad`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

If[!ListQ[System`$Services],Get["OAuth`"]]

Block[{dir=DirectoryName[System`Private`$InputFileName]},
	OAuthClient`addOAuthservice["Stripe",dir]
]


End[] (* End Private Context *)
EndPackage[]
