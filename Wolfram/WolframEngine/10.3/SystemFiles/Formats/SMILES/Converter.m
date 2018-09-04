(* ::Package:: *)

(* ::Subsubtitle:: *)
(*Simplified Molecular Input Line Entry Specification (SMILES) Converter*)


(* ::Section::Closed:: *)
(*COPYRIGHT*)


(*************************************************************************

                        Mathematica source file

        Copyright 1986 through 2010 by Wolfram Research Inc.

This material contains trade secrets and may be registered with the
U.S. Copyright Office as an unpublished work, pursuant to Title 17,
U.S. Code, Section 408.  Unauthorized copying, adaptation, distribution
or display is prohibited.

*************************************************************************)


(* ::Section:: *)
(*BEGIN CONVERTER CONTEXT*)


Begin["System`Convert`SMILESDump`"];


(* ::Section:: *)
(*IMPORT*)


(* ::Subsection:: *)
(*Utilities*)


trim[s_] := StringReplace[s, {StartOfLine ~~ Whitespace -> "", 
      Whitespace ~~ EndOfLine -> ""}]

$ElementNames := {"He", "Li", "Be", "Ne", "Na", "Mg", "Al", "Si", "Cl",
    "Ar", "Ca", "Sc", "Ti", "Cr", "Mn", "Fe", "Co", "Ni", "Cu", "Zn", 
   "Ga", "Ge", "As", "Se", "Br", "Kr", "Rb", "Sr", "Zr", "Nb", "Mo", 
   "Tc", "Ru", "Rh", "Pd", "Ag", "Cd", "In", "Sn", "Sb", "Te", "Xe", 
   "Cs", "Ba", "La", "Ce", "Pr", "Nd", "Pm", "Sm", "Eu", "Gd", "Tb", 
   "Dy", "Ho", "Er", "Tm", "Yb", "Lu", "Hf", "Ta", "Re", "Os", "Ir", 
   "Pt", "Au", "Hg", "Tl", "Pb", "Bi", "Po", "At", "Rn", "Fr", "Ra", 
   "Ac", "Th", "Pa", "Np", "Pu", "Am", "Cm", "Bk", "Cf", "Es", "Fm", 
   "Md", "No", "Lr", "Rf", "Db", "Sg", "Bh", "Hs", "Mt", "Ds", "Rg", 
   "Uub", "Uut", "Uuq", "Uup", "Uuh", "Uus", "Uuo", "H", "B", "C", "N", 
   "O", "F", "P", "S", "K", "V", "Y", "I", "U", "W", "c", "n", "o", 
   "p", "s", "as", "se", "*"};

elementNameRules = {"c" -> "C", "n" -> "N", "o" -> "O", "p" -> "P", 
   "s" -> "S", "as" -> "As", "se" -> "Se"};

validSymbols := 
Join[ 
  $ElementNames,
  {"[", "]", "(", ")", "-", "=", "#", ":", "+", ":", ">>", "%", ".", "/", "\\", "@", ">>", DigitCharacter}
]

parseBrackets[in_, left_, right_] := 
Module[
  {temp, out, next},
  If[in === {}, Return[{}]];
  temp = Switch[#[[All, 2]],
           {left, right},
           #[[All, 1]],
           
           {left, left},
           #[[1]],
           
           {right, right},
           #[[2]],
            
           _,
           Null
         ] & /@ Partition[in, 2, 1];
  temp = DeleteCases[temp, Null]; 
  next = Select[temp, MemberQ[{left, right}, #[[2]]] &];
  out = Select[temp, NumberQ[#[[2]]] &];
  If[next === {}, out, Join[out, parseBrackets[next, left, right]] ]
]

FindCyclicIndex[t_] := 
Module[
  {temp, nums},
  temp = Take[t, {2, -1}][[All, 2]]; 
  nums = If[ MemberQ[temp, "%"],
             StringSplit[StringJoin[temp], "%"], 
             temp
         ];
   {t[[1]], ToExpression /@ nums}
]

getCyclicBonds[t_] := 
{#, 1} & /@ 
(Rule @@@ Flatten[ Partition[#, 2] & /@
(Split[ Sort@Flatten[ Thread /@ (Rule @@@ Transpose[{t[[All, 2]], t[[All, 1, 1]]}])], #1[[1]] === #2[[1]] &][[All, All, 2]]), 1])

atomsQ[s_String] := If[MemberQ[$ElementNames, s], True, s]

getAdjacentBonds[t_] := DeleteCases[
  Switch[atomsQ /@ #[[All, 2, 2]],
     {True, True}, {#[[1, 2, 1]] -> #[[2, 2, 1]], 1},
     {True, "-", True}, {#[[1, 2, 1]] -> #[[3, 2, 1]], 1},
     {True, "=", True}, {#[[1, 2, 1]] -> #[[3, 2, 1]], 2},
     {True, "#", True}, {#[[1, 2, 1]] -> #[[3, 2, 1]], 3},
     {True, "(", True}, {#[[1, 2, 1]] -> #[[3, 2, 1]], 1},
     {True, "(", "-", True}, {#[[1, 2, 1]] -> #[[4, 2, 1]], 1},
     {True, "(", "=", True}, {#[[1, 2, 1]] -> #[[4, 2, 1]], 2},
     {True, "(", "#", True}, {#[[1, 2, 1]] -> #[[4, 2, 1]], 3},
     {True, "/", True}, {#[[1, 2, 1]] -> #[[3, 2, 1]], 1},
     {True, "\\", True}, {#[[1, 2, 1]] -> #[[3, 2, 1]], 1},
     _, Null
  ] & /@ t,
  Null
]

AromaticAtomQ[t_] := MemberQ[{"c", "n", "o", "p", "s", "as", "se"}, t]

bondTypeNames = {"Single", "Double", "Triple", "Aromatic", 
   "SingleOrDouble", "SingleOrAromatic", "DoubleOrAromatic", "Any"};

findPreAtom[i_, atoms_] := 
If[ i > atoms[[1, 1]], 
    Select[atoms, #[[1]] < i &][[-1]],
    {0, Null}
] 

findNextAtom[i_, atoms_] := 
If[ i < atoms[[-1, 1]],
    Select[atoms, #[[1]] > i &][[1]],
    {0, Null}
]


findPrePos[i_, t_, ParenthesesPos_] := 
If[ i === 1,
    t,
    If[ ParenthesesPos[[i - 1, 2]] =!= t - 1,
        t,
        findPrePos[i - 1, ParenthesesPos[[i - 1, 1]] ]
    ]
] 


findCharge[t_List, SquareBrkAtoms_] :=
Module[
  {len, next},
  len = Length[t]; If[len > 1, Return[len] ]; 
  next = Extract[SquareBrkAtoms, {t[[1, 1]], t[[1, 2]] + 1}][[2]];
  If[ MemberQ[{"1", "2", "3", "4", "5", "6", "7", "8", "9"}, next],
      ToExpression[next],
      1
  ]
]


findCyclicAtoms[i_, symbols_, atoms_, ParenthesesPos_] :=
Module[
  {temp, pos},
  temp = symbols[[i - 1]];
  If[ (Length[temp]<2)||((Length[temp]>1)&&(temp[[2]] =!= ")")), findPreAtom[i, atoms],
      pos = Position[ParenthesesPos, {_, i - 1}][[1, 1]];
      findPreAtom[ findPrePos[pos, ParenthesesPos[[pos, 1]], ParenthesesPos], atoms]
  ]
]

getHydrogenNums[symbols_, HydrogenPos_]:=
Module[
  {HydrogenNums},
  HydrogenNums = (If[ListQ[#], #[[2]], 1] & /@ symbols[[HydrogenPos + 1]] /. 
    {"1" -> 1, "2" -> 2, "3" -> 3, "4" -> 4, "5" -> 5, "6" -> 6, "7" -> 7, "8" -> 8, "9" -> 9, _String -> 1}) - 1;
  HydrogenNums = Prepend[HydrogenNums, Length[symbols] ];
  HydrogenNums = Total[Take[HydrogenNums, #]] & /@ Range[Length[HydrogenNums]];
  If[ Length[#] < 2, {}, Take[#, {2, -1}] ] & /@ (Range @@@ (Partition[HydrogenNums, 2, 1]))
]


getSMILES[in_String, opts___?OptionQ] := 
Module[
  {symbols, SquareBrkPos, SquareBrkAtoms, SquareBrkKeyPos, 
   removedPos = {}, chargerules, charges = {}, 
   PurifiedSymbols, CyclicPos, CyclicAtoms, edgerules = {}, 
   atoms = {}, indexSymbols, onlyAtoms, adjacence, 
   ParenthesesPos = {}, 
   leftatoms, rightatoms, HydrogenPos, HydrogenNums, 
   index},
    
  (* check for "" input *)
  If[!StringQ[in], Message[Import::fmterr, "SMILES"]; Return[$Failed] ];
  symbols = StringCases[in, validSymbols];
  
  If[ in =!= StringJoin[symbols],
      Message[Import::fmterr, "SMILES"];
      Return[$Failed]
  ];
  If[ MemberQ[symbols, ">>"],
      Message[Import::unsup, "SMILES"]; 
      Return[$Failed]
  ];
  If[ FreeQ[MemberQ[$ElementNames, #] & /@ Union[symbols], True],
      Message[Import::fmterr, "SMILES"]; Return[$Failed]
  ];

  symbols = Transpose[{Range[Length[symbols]], symbols}];
  
  (*Square Bracket*)
  SquareBrkPos = parseBrackets[Select[symbols, #[[2]] === "[" || #[[2]] === "]" &], "[", "]" ];
  If[ SquareBrkPos =!= {},
      SquareBrkAtoms = Take[symbols, #] & /@ SquareBrkPos;
      SquareBrkKeyPos = Select[#, MemberQ[$ElementNames, #[[2]]] &][[1, 1]] & /@  SquareBrkAtoms;
      removedPos = Complement[Flatten[Range @@@ SquareBrkPos], SquareBrkKeyPos];
      
      (*Find charges*)
      chargerules = Join[
        (#[[1, 1]] -> findCharge[#, SquareBrkAtoms]) & /@ Split[Position[SquareBrkAtoms, "+"], #1[[1]] === #2[[1]] &],
        (#[[1, 1]] -> - findCharge[#, SquareBrkAtoms]) & /@ Split[Position[SquareBrkAtoms, "-"], #1[[1]] === #2[[1]] &],
        {_Integer -> 0}
      ];
      charges = MapThread[ Rule, {SquareBrkKeyPos, Range[Length[SquareBrkKeyPos]] /. chargerules}];
      PurifiedSymbols = Delete[symbols, List /@ removedPos];
      ,
      (* SquareBrkPos === {} *)
      PurifiedSymbols = symbols
  ];

  (* Cyclic Structures Step 1 "c1cnc[nH]c(=O)1" *)
  CyclicPos = Select[PurifiedSymbols,  MemberQ[{"0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "%"}, #[[2]]] &];
  If[ CyclicPos =!= {}, 
      PurifiedSymbols = Complement[PurifiedSymbols, CyclicPos];
      CyclicPos = Split[CyclicPos, #1[[1]] === #2[[1]] - 1 &]
  ];

  (*Adjacent bonds*)
  indexSymbols = Transpose[{Range[Length[#]], #}] &[PurifiedSymbols];
  onlyAtoms = Select[indexSymbols, MemberQ[$ElementNames, #[[2, 2]]] &];
  adjacence = Partition[onlyAtoms[[All, 1]], 2, 1];
  edgerules = Join[edgerules, getAdjacentBonds[Take[indexSymbols, #] & /@ adjacence]];

  (*Branch Structure*)
  atoms = Select[PurifiedSymbols, MemberQ[$ElementNames, #[[2]]] &];
  ParenthesesPos = parseBrackets[ Select[PurifiedSymbols, #[[2]] === "(" || #[[2]] === ")" &], "(", ")"];
  
  If[ ParenthesesPos =!= {},
      leftatoms = findPreAtom[#, atoms]& /@ (MapThread[ findPrePos[#1, #2, ParenthesesPos]&, {Range[Length[ParenthesesPos]], ParenthesesPos[[All, 1]]}]); 
      rightatoms = findNextAtom[#, atoms]& /@ ParenthesesPos[[All, 2]];
      edgerules = Join[
        edgerules,
        Transpose[{
            MapThread[ Rule, {leftatoms[[All, 1]], rightatoms[[All, 1]]}],
            symbols[[ rightatoms[[All, 1]] - 1]] /. {{_, "="} -> 2, {_, "#"} -> 3, {_, _String} -> 1}
        }]
      ]
   ];

  (*Cyclic Structures Step 2*)
  If[ CyclicPos =!= {},
      CyclicAtoms = MapThread[ Prepend, {CyclicPos, findCyclicAtoms[#, symbols, atoms, ParenthesesPos]&  /@ CyclicPos[[All, 1, 1]]}];
      edgerules =  Join[edgerules, getCyclicBonds[FindCyclicIndex /@ CyclicAtoms] ]
   ];
      
  (*Append hydrogen atoms in square bracket*)
  If[ SquareBrkPos =!= {},
      HydrogenPos = If[# === {}, -1, #[[1, 1]]] & /@ (Select[#, #[[2]] === "H" &] & /@ SquareBrkAtoms);
      HydrogenNums = getHydrogenNums[symbols, HydrogenPos];
      edgerules = Join[ edgerules,
            {#, 1} & /@
            Flatten[{
              DeleteCases[ MapThread[Rule, {SquareBrkKeyPos, HydrogenPos}], _ -> -1], 
              MapThread[Thread[#1 -> #2] &, {SquareBrkKeyPos, HydrogenNums}]
            }]
      ];
      atoms = Join[atoms, {#, "H"} & /@ Flatten[{DeleteCases[HydrogenPos, -1], HydrogenNums}]]
  ];
  
  atoms = Sort[atoms];
  index = MapThread[Rule, {atoms[[All, 1]], Range[Length[atoms]]}];
  edgerules = Sort[DeleteCases[ edgerules /. {{_ -> 0, _} -> Null, {0 -> _, _} -> Null}, Null]];

  (* Find Aromatic bonds*)
  If[ Select[atoms[[All, 2]], AromaticAtomQ] =!= {},
      atoms = Rule @@@ atoms;
      edgerules = Map[
                    If[ AromaticAtomQ /@ ({#[[1, 1]], #[[1, 2]]} /. atoms) === {True, True}, {#[[1]], 4}, #] &,
                    edgerules
                  ]
  ];

  (*For charges*)
  If[ charges =!= {} && Union[charges[[All, 2]]] =!= {0}, 
      chargerules = Join[Rule @@@ 
      Transpose[{charges[[All, 1]] /. index, 
        charges[[All, 2]]}], {_Integer -> 0}];
      charges = Range[Length[atoms]] /. chargerules
      ,
      charges = Table[0, {Length[atoms]}]
   ];
  
  {
    (* "VertexTypes" *) atoms[[All, 2]] /. elementNameRules, 
    (* "EdgeRules" *) edgerules[[All, 1]] /. index,
    (* "EdgeTypes" *) bondTypeNames[[edgerules[[All, 2]]]],
    (* "FormalCharges" *) charges
  }
]


(* ::Subsection:: *)
(*Importers*)


(* ::Subsubsection:: *)
(*Conditional Raw Importers*)


(* There are no registered conditional raw importers for this format *)


(* ::Subsubsection:: *)
(*Default Raw Importer*)


ImportSMILES[file_InputStream, opts___?OptionQ] := 
Module[
  {lines, out},
  lines = Import[file, "Lines"];
  If[! ListQ[lines], Message[Import::fmterr, "SMILES"]; Return[$Failed] ];
  lines = DeleteCases[trim /@ lines, ""];
  If[ Length[lines] < 1, Message[Import::fmterr, "SMILES"]; Return[$Failed] ];
  
  out = getSMILES[#, opts] & /@ lines;
  If[ Union[out] === {$Failed},
      Return[$Failed], 
      out = Transpose[ out /. $Failed -> {$Failed, $Failed, $Failed, $Failed}]
  ];
  Sort@MapThread[Rule, {{"VertexTypes", "EdgeRules", "EdgeTypes", "FormalCharges"}, out}]
]



(* ::Subsubsection:: *)
(*Post Importers*)


(* There are no registered post-importers for this format *)


(* ::Section::Closed:: *)
(*END CONVERTER CONTEXT*)


End[];  
