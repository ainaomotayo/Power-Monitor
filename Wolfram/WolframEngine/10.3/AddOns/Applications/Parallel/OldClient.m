(* :Title: OldClient.m -- set up older subkernels  *)

(* :Context: Parallel`OldClient` *)

(* :Author: Roman E. Maeder *)

(* :Summary:
   runtime initialization of a new subkernel with a lower Mathematica version to
   bring it at the same level as new kernels that read those initializations
   from Parallel`Client`.
   This file is read only if an old subkernel needs to be initialized.
   The lowest parallel language version supported is 7.0, which corresponds to Mathematica 7
 *)

(* :Package Version: 1.0  *)

(* :Mathematica Version: 8 *)


BeginPackage["Parallel`OldClient`"]

initOldKernel::usage = "initOldKernel[kernel, version] sets up compatibility definitions in kernel."

(* needs symbols from other parallel contexts *)
BeginPackage["Parallel`Developer`"]
EndPackage[]

BeginPackage["Parallel`Protected`"]
EndPackage[]

BeginPackage["Parallel`Client`"] (* make sure symbols are found *)

`HoldCompound

`CallBackPacket
`ReplyPacket
`CallBack
`remoteIdentity

`setSharedVariable
`setSharedFunction
`unsetShared

`makeDefinitions

EndPackage[]

(* master side of HoldCompound *)
holdCompound = Parallel`Client`HoldCompound

Begin["`Private`"]

`$PackageVersion = 1.0;
`$thisFile = $InputFileName

(* definitions previously done at init time through mathlink; note the comma in place of ; *)

$initDefsPre8 = holdCompound[
	HoldCompound=CompoundExpression, Protect[HoldCompound],
	
    SetAttributes[{CallBack, CallBackPacket, ReplyPacket}, HoldFirst],
    remoteIdentity[args___] := args,
    CallBack[packet_] := Module[{r},
      LinkWrite[Parallel`Client`Private`$link, CallBackPacket[packet]];
      While[Head[r = LinkRead[Parallel`Client`Private`$link]] =!= ReplyPacket, 
            If[r===$Failed || Head[r]===LinkRead, Quit[]] ];
      If[ r === ReplyPacket[packet], Null, r[[1]] ]
    ],
    Protect[ CallBackPacket, ReplyPacket, remoteIdentity, CallBack ],

    SetAttributes[{setSharedVariable, unsetShared}, HoldFirst],
	setSharedVariable[s_, attrs_] := Module[{},
		Unprotect[s]; ClearAll[s];
		(* for all variables: read access *)
		s := CallBack[s];
	    s/: c:HoldPattern[Part[s,__]] := CallBack[c];
	    s/: c:HoldPattern[Extract[s,__]] := CallBack[c];
	    (* for mutable variables *)
	    If[ !MemberQ[attrs, Protected], With[{pp = Unprotect[Part]},
	        s/: c:HoldPattern[s =_]    := CallBack[c];
	        s/: c:HoldPattern[s:=rhs_] := CallBack[s:=Parallel`Developer`SendBack[rhs]];
	        Part/: c:HoldPattern[Part[s,args__]=rhs_] :=
	        	Replace[{args}, {brgs___} :> CallBack[Part[s,brgs]=rhs]];
	        s/: c:(s++) := CallBack[c];
	        s/: c:(s--) := CallBack[c];
	        s/: c:(++s) := CallBack[c];
	        s/: c:(--s) := CallBack[c];
	        s/: c:AppendTo[s,rhs_]  := CallBack[c];
	        s/: c:PrependTo[s,rhs_] := CallBack[c];
	        s/: c:(s+=v_) := CallBack[c];
	        s/: c:(s-=v_) := CallBack[c];
	        s/: c:(s*=v_) := CallBack[c];
	        s/: c:(s/=v_) := CallBack[c];
	        Protect[pp];
	    ]];
	    Attributes[s] = Union[attrs,{Protected}];
	],
	setSharedFunction[s_, attrs_] := Module[{},
		Unprotect[s]; ClearAll[s];
		(* for all functions: read access *)
		d_s := CallBack[d];
	    (* for mutable functions *)
	    If[ !MemberQ[attrs, Protected], With[{pp = Unprotect[Part]},
	        s/: HoldPattern[s[args___]  = rhs_] :=
	        	Replace[{args}, {brgs___} :> CallBack[s[brgs] = rhs]];
	        s/: HoldPattern[s[args___] := rhs_] := 
	        	Replace[{args}, {brgs___} :> CallBack[s[brgs]:= Parallel`Developer`SendBack[rhs]]];
	        s/: HoldPattern[s[args___]++] := 
	        	Replace[{args}, {brgs___} :> CallBack[s[brgs]++]];
	        s/: HoldPattern[s[args___]--] := 
	        	Replace[{args}, {brgs___} :> CallBack[s[brgs]--]];
	        s/: HoldPattern[++s[args___]] := 
	        	Replace[{args}, {brgs___} :> CallBack[++s[brgs]]];
	        s/: HoldPattern[--s[args___]] := 
	        	Replace[{args}, {brgs___} :> CallBack[--s[brgs]]];
	        Protect[pp];
	    ]];
	    Attributes[s] = Union[attrs,{Protected}];
	],
	unsetShared[s_] := With[{pp = Unprotect[Part]},
		Unprotect[s]; ClearAll[s];
		Quiet[ Part/: c:HoldPattern[Part[s,__]=_] =. ];
		Protect[pp];
	],

	SetAttributes[MangledOwnValues, HoldFirst],
	MangledOwnValues /: (MangledOwnValues[sym_] = {Verbatim[HoldPattern][sym_] :> rhs_}) := (sym = Unevaluated[rhs]),
	MangledOwnValues /: (MangledOwnValues[sym_] = junk_) := (OwnValues[sym] = junk),

	vlDefs[HoldForm[s_] -> vals_] := ( Unprotect[s];
		Replace[ vals, (vt_ -> l_) :> With[{vtm = vt /. OwnValues->MangledOwnValues},vtm[s] = Flatten[l]], 1 ]
    ),
	SetAttributes[makeDefinitions, HoldFirst],
	makeDefinitions[Language`DefinitionList[vls___]] := vlDefs /@ {vls},

	setSharedFunction[Parallel`Concurrency`acquire, {HoldFirst, Protected}],
	setSharedFunction[Parallel`Concurrency`release, {HoldFirst, Protected}],

	SetAttributes[CriticalSection, HoldAll],
	CriticalSection[locks_List, code_] :=
	 Module[{res},
	  While[ ! Parallel`Concurrency`acquire[locks, $KernelID], Pause[0.1] ];
	  res = (code);
	  Parallel`Concurrency`release[locks];
	  res
	 ],

	SetAttributes[{ParallelSubmit}, HoldAll],
	Protect[ParallelSubmit,WaitAll,WaitNext,Parallel`Developer`QueueRun,Parallel`Developer`DoneQ],
	SetAttributes[EvaluationObject, HoldAllComplete],

	setSharedVariable[$KernelCount, {Protected}],
	
    Null
]

(* case by case for all supported older language versions *)

initOldKernel[kernels_, subLanguageVersion_] := Module[{},
	With[{masterVersion = Parallel`Private`$ParallelLanguageVersion},
		kernelEvaluate[Parallel`Client`$masterVersion=masterVersion;, kernels]]; (* record our setting *)
	If[subLanguageVersion === Null, (* 7.0.1 *)
		With[{clientCode=$initDefsPre8}, kernelEvaluate[ clientCode, kernels ]];
	];
]


End[]

EndPackage[]
