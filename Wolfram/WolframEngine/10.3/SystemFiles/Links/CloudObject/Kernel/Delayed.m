(* Mathematica package *)
BeginPackage["CloudObject`"]

System`Delayed;

Begin["`Private`"]

Unprotect[Delayed];

Options[Delayed] = {UpdateInterval -> Infinity};
SetAttributes[Delayed, {HoldFirst, ReadProtected}];

Protect[Delayed]

End[]

EndPackage[]
