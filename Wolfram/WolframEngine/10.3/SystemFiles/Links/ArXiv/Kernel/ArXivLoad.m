(* Mathematica Package *)
  
BeginPackage["ArXivLoad`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

If[!ListQ[System`$Services],Get["OAuth`"]]

Block[{dir=DirectoryName[System`Private`$InputFileName]},
	KeyClient`addKeyservice["ArXiv",dir]
]


End[] (* End Private Context *)
EndPackage[]
