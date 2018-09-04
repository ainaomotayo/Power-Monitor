BeginPackage["QuantityArray`"]

System`QuantityArray;
QuantityArray`QuantityArrayQ;
QuantityArray`QuantityArrayMagnitude;
QuantityArray`QuantityArrayUnit;
QuantityArray`QuantityArrayUnitConvert;
QuantityArray`QuantityArrayCommonUnits;
QuantityArray`ChangeFlattening;
QuantityArray`$NormalizeOnUnitConvertError;

(Unprotect[#]; ClearAll[#])& /@ {
	QuantityArray, QuantityArrayQ,
	QuantityArrayMagnitude, QuantityArrayUnit,
	QuantityArrayUnitConvert, QuantityArrayCommonUnits
};


Begin["`Private`"]

(* Register QuantityArray as a type of StructuredArray *)
StructuredArray`RegisterArrayStructure[QuantityArray];


(********* Utilities *********)


(*** Levels utilities ***)

(* QuantityArray uses internally a levels notation as in Flatten[array, levels].
   This allows any Flatten or Transpose operation without actually having to change
   the array of numbers inside. *)

(* Validate a levels spec *)
levelsQ[{}] := True;
levelsQ[levels: {{__Integer}..}] := PermutationListQ[Flatten[levels]];
levelsQ[_] := False;

(* Right action on a dimensions list. Return the dimensions of Flatten[A, levels]
   for an array A of dimensions dims. *)
dimsFlatten[dims_, levels_] := Times@@@ Extract[dims, List /@ levels];

(* Right product of levels. Return levels such that Flatten[Flatten[A, list1], list2]
   is identitcal to Flatten[A, levels] for any array A of appropriate depth. This is
   actually an external product in general. *)
levelsFlatten[levels1_List, levels2_List] := Join@@@ Extract[levels1, List /@ levels2];

(* The identity of that product, on both sides *)
levelsIdentity[n_Integer] := List /@ Range[n];
levelsIdentityQ[levels_List] := SameQ[levels, levelsIdentity[Length[levels]]];

(* Levels without flattenings. Just a permutation. {} is not accepted here *)
permutationLevelsQ[levels_List] := SameQ[DeleteDuplicates[Length /@ levels], {1}];


(*** Array utilities ***)

(* Avoid ArrayDepth confusion, say ArrayDepth[x[1]] should be 0, and not 1 *)
arrayDepth[array: _List | _SparseArray | _StructuredArray] := ArrayDepth[array];
arrayDepth[x_] := 0;
arrayDimensions[array: _List | _SparseArray | _StructuredArray] := Dimensions[array];
arrayDimensions[x_] := {};

(* Extend ArrayQ to admit scalars *)
arrayQ[x_, 0] := !ArrayQ[x];
arrayQ[x_, depth_] := ArrayQ[x, depth];
arrayQ[x_, 0, test_] := test[x];
arrayQ[x_, depth_, test_] := ArrayQ[x, depth, test];

(* Array cyclic transposition. Result has dimensions RotateRight[Dimensions[A], n] *)
transposeRight[array: _List | _SparseArray | _StructuredArray_, n_] :=
	Transpose[array, RotateLeft[Range[arrayDepth[array]], n]];
transposeRight[x_, n_] := x;

(* A key concept with QuantityArray is that of 'deep threading'. Given an array A of
   dimensions {d1, ..., dn} and an array U of dimensions {dm, ..., dn} with 1 <= m <= n.
   then we want to construct an array Q of dimensions {d1, ..., dn} such that the element
   Q[[i1, ..., in]] equals f[A[[i1, ..., in]], U[[im+1, ..., in]]] for some threading
   function f, in our case always Quantity. The following code and all uses of MapThread
   in this file assume that f automatically threads in its first argument, like Quantity does. *)
mapThread[f_, arrays_List, n_Integer?Positive] := MapThread[f, arrays, n];
mapThread[f_, arrays_List, 0] := Apply[f, arrays];
mapThread[f_, arrays_List, n_Integer?Negative] :=
	transposeRight[MapThread[f, transposeRight[#, -n]& /@ arrays, -n], n];
(* TODO: Note that MapThread unsparsifies the threaded levels. Can we avoid it? *)

(* Flatten chokes with scalars. This function does not. It always returns a depth 1 list *)
flattenList[list_List] := Flatten[list];
flattenList[array_?ArrayQ] := Flatten[array];
flattenList[x_] := {x};

(* Flatten does all the work even for the identity levels. Avoid that.
   Transpose is faster than Flatten, mainly for large arrays *)
myFlatten[array_, levels_List] := Which[
	levelsIdentityQ[levels],
		array,
	permutationLevelsQ[levels],
		Transpose[array, InversePermutation[Flatten[levels]]],
	True,
		Flatten[array, levels]
];

(* Given arrays A and B, find an array newB such that deepThread[f, {Flatten[A, levels], B}]
   is equal to Flatten[deepThread[f, {A, newB}], levels] *)
unFlatten[B_, levels_List, dimsA_List] := Catch[Module[{
	levs, dims, newB, range
},
	levs = Take[levels, -arrayDepth[B]];
	If[levs === {}, Return[B]];
	dims = Extract[dimsA, List /@ levs];
	If[DeleteCases[Length /@ levs, 1] === {},
		(* There are no nontrivial flattenings, just a possible permutation *)
		newB = B,
		(* Partitition to remove the flattenings *)
		newB = Fold[reshaperotate, B, dims]
	];
	(* Enlarge B if there are missing levels and then correct the permutation *)
	range = Complement[Range[Min@ levs, Max@ levs], Flatten@ levs];
	newB = ConstantArray[newB, dimsA[[range]]];
	Transpose[newB, Ordering@ Ordering@ Join[range, Flatten@ levs]]
], "unFlattenError"];

reshaperotate[array_, dims_List] := If[Length[array] === Times@@ dims,
	transposeRight[ArrayReshape[array, Join[dims, Rest@ Dimensions@ array]], - Length[dims]],
	Throw[$Failed, "unFlattenError"]
];

(* Given arrays A and B, find an array newB such that Flatten[deepThread[f, {A, B}], levels]
   is equal to deepThread[f, {Flatten[A, levels], newB}] *)
deFlatten[B_, levels_List, dimsA_List] := Module[{
	depthA = Length[dimsA], rangeA, levelsA, min, replB
},
	(* Levels in A that are deepThreaded with B *)
	rangeA = Range[depthA - arrayDepth[B] + 1, depthA];
	(* Levels in A that move when threading with B and flattening *)
	levelsA = Drop[levels, - Min[1, First /@ Position[levels, Alternatives@@ rangeA]] + 1];
	(* The smallest level of A that moves when threading with B *)
	min = Min[levelsA];
	range = Range[min, depthA];
	(* Replicate B to consider all those levels *)
	replB = replicateArray[B, dimsA[[range]]];
	(* Apply the flattening to that array *)
	levelsA = Select[levels, Min[#] >= min &] - min + 1;
	Flatten[replB, levelsA]
];

(* Replacements do not directly work on SparseArray objects, so compensate for that *)
replaceAll[array_SparseArray, rules_] := 
	SparseArray[Replace[ArrayRules[array], rules, {2, Infinity}, Heads->True], Dimensions[array]];
replaceAll[array_StructuredArray, rules_] := replaceAll[Normal[array, StructuredArray], rules];
replaceAll[array_, rules_] := ReplaceAll[array, rules];


(*** Replication and reduction of arrays ***)

(* Replication of an array A means constructing {A, ..., A}. Reduction is the inverse process.
   Replication is always possible. Reduction is only possible for lists of identical elements.
   The key point is that deepThread[f, {A, B}] is equal to deepThread[f, {A, replicateArray[B]}]
   and deepThread[f, {A, reduceArray[B]}] for adequate choices of dimensions and depths *)

(* Replication of array B to complete the dimensions of an array A into which it can be deep-threaded *)
replicateArray[B_, Adims_] := ConstantArray[B, Drop[Adims, -arrayDepth[B]]];

(* Recursively extract a subarray of a list of identical elements *)
reduceArray[{}] := {};
reduceArray[A_List] := If[SameQ@@ A, reduceArray[First[A]], A];
reduceArray[A_SparseArray] := If[A["NonzeroValues"] === {},
	A["Background"],
	If[SameQ@@ A, reduceArray[First[A]], A]
];
reduceArray[x_] := x;

(* Combine both operations to adjust two arrays until they have exactly the same dimensions.
   During the process, both A and B do not lose or gain information. They are just replicated
   or reduced. Preferably reduced to obtain smaller resulting matrices. Return $Failed if such
   operation is not possible due to dimension incompatibility *)
makeSameDimensions[A_, B_] := Module[{newA, newB, depthA, depthB, n},
	depthA = arrayDepth[A];
	depthB = arrayDepth[B];
	(* Check consistency of deepest dimensions of input A and B *)
	n = Min[depthA, depthB];
	If[Take[Dimensions[A], -n] =!= Take[Dimensions[B], -n],
		Return[$Failed]
	];
	(* Add or remove shallow levels until they fully agree *)
	newA = A;
	newB = B;
	While[depthA =!= depthB,
		If[depthB > depthA,
			If[SameQ@@ newB,
				newB = First[newB],
				newA = ConstantArray[newA, Dimensions[newB][[depthB - depthA]]]
			],
			If[SameQ@@ newA,
				newA = First[newA],
				newB = ConstantArray[newB, Dimensions[newA][[depthA - depthB]]]
			]
		];
		depthA = arrayDepth[newA];
		depthB = arrayDepth[newB]
	];
	If[Dimensions[newA] === Dimensions[newB],
		{newA, newB},
		$Failed
	]
];


(********* Code for QuantityArray *********)


(*** QuantityArray as constructor ***)

(* Canonicalization of units. Call the WA unit-interpreter before rejecting a unit *)
CanonUnitArray[units_] := Catch[canonUnitArray[units], "InvalidUnit"];
canonUnitArray[array_?ArrayQ] := Map[canonUnit, array, {arrayDepth[array]}];
canonUnitArray[list_List] := canonUnitArray /@ list;
canonUnitArray[unit_] := canonUnit[unit];
CanonUnit[unit_] := Catch[canonUnit[unit], "InvalidUnit"];
canonUnit[q_Quantity] := Throw[$Failed, "InvalidUnit"];
canonUnit[Times[Power["DimensionlessUnit", _.], units__]] := canonUnit[Times[units]];
canonUnit[unit_?KnownUnitQ] := canonUnit[unit] = unit;
canonUnit[unit_] := With[{canon = Quiet[QuantityUnit[Quantity[unit]]]},
	If[KnownUnitQ[canon],
		canonUnit[unit] = canon,
		Throw[$Failed, "InvalidUnit"]
	]
]	

(* Checks with no messages *)
dimensionsQ[dims_] := VectorQ[dims, Internal`PositiveMachineIntegerQ];
unitQ[unit_] := UnsameQ[CanonUnit[unit], $Failed];
unitArrayQ[units_] := ArrayQ[units, _, unitQ];

(* Checks with messages *)
Acheck[numbers: _List | _SparseArray | _StructuredArray] :=
	ArrayQ[numbers] || (Message[QuantityArray::rect]; False);
(* Anything else is considered to be a scalar *)
Acheck[x_] := True;

Ucheck[units_?ArrayQ] := unitArrayQ[units] || (Message[QuantityArray::uarray, units]; False);
Ucheck[unit_] := unitQ[unit] || (Message[QuantityArray::uarray, unit]; False);

QAcheck[numbers_, units_] := With[{
	ndims = arrayDimensions[numbers],
	udims = arrayDimensions[units]
},
	(Length[udims] <= Length[ndims] && Take[ndims, -Length[udims]] === udims)||
	(Message[QuantityArray::units, ndims, udims]; False)
];

(* Check whether units are dimensionless *)
dimensionlessUnitBlockQ[units_] := SameQ[Complement[DeleteDuplicates[Flatten[{units}]], {1, "DimensionlessUnit"}], {}];

(* This definition is Block'ed during the operation of toIdentityLevels *)
unitMatrixLooksTransposedQ[units_] := Length[DeleteDuplicates[Flatten[units]]] > 1 && SameQ@@ Transpose[units];

(* Internal constructor. The units array is always normalized and reduced.
   The numbers are already supposed Quantity-free. *)
quantityArray[scalar_, unit_, {}] := Quantity[scalar, unit];
quantityArray[numbers_, units_?dimensionlessUnitBlockQ, levels_] := myFlatten[numbers, levels];
(* For a single unit, always operate on the numbers array. Suggested by Sasha *)
quantityArray[numbers_, unit_?unitQ, levels_] := With[{newnumbers = myFlatten[numbers, levels]},
	quantityArray[newnumbers, unit, levelsIdentity[arrayDepth[newnumbers]]]
] /; !levelsIdentityQ[levels];
(* For a matrix of units, use the opposite permutation if it is worth it. Suggested by Sasha *)
quantityArray[numbers_, units_?MatrixQ, levels_?permutationLevelsQ] := With[{depth = arrayDepth[numbers]},
	quantityArray[
		Transpose[numbers, Join[Range[depth-2], {depth, depth-1}]],
		First /@ units,
		Replace[levels, {depth -> depth-1, depth-1 -> depth}, {2}]
	]
] /; unitMatrixLooksTransposedQ[units];
quantityArray[numbers_, units_, levels_] := StructuredArray[
	QuantityArray,
	dimsFlatten[arrayDimensions[numbers], levels],
	StructuredArray`StructuredData[
		QuantityArray,
		Developer`ToPackedArray[numbers],
		reduceArray[units],
		levels
	]
];
quantityArray[numbers_, units_] :=
	quantityArray[numbers, units, levelsIdentity[arrayDepth[numbers]]];

(* Following Quantity, we interpret a nested QuantityArray structure as implying
   a product of units *)
quantityFreeQ[x: _List | _?ArrayQ] := Developer`PackedArrayQ[x] ||
	(Internal`LiterallyAbsentQ[x, Quantity] &&
	 Internal`LiterallyAbsentQ[x, QuantityArray]);
quantityFreeQ[x_] := !QuantityQ[x] && !QuantityArrayQ[x];

(* Internal constructor starting from an array with Quantity objects *)
quantityArrayQuantity[HoldPattern[Quantity[mag_, unit_, ___]], units_] :=
	If[quantityFreeQ[mag],
		quantityArray[mag, CanonUnitArray[unit units]],
		quantityArrayQuantity[mag, unit units]
	];
quantityArrayQuantity[array: _List | _SparseArray | _StructuredArray, units_] :=
	Module[{amags, aunits, qa},
		If[QuantityArrayQ[array],
			qa = toIdentityLevels[array];
			amags = getNumbers[qa];
			aunits = getUnits[qa],
			amags = QuantityMagnitude[array];
			aunits = QuantityUnit[array]
		];
		quantityArray[
			amags,
			CanonUnitArray[Times@@ makeSameDimensions[reduceArray@ aunits, reduceArray@ units]]
		]
	];

(* Separate the case of having Quantity objects in the first argument *)
makeQuantityArray[numbers_, units_] := If[quantityFreeQ[numbers],
	quantityArray[numbers, CanonUnitArray[units]],
	quantityArrayQuantity[numbers, units]
];

(* Public constructor. No argument to add a levels specification *)
QuantityArray[numbers_, units_] := With[
	{qa = makeQuantityArray[numbers, units]},
	qa /; QuantityArrayQ[qa] || QuantityQ[qa] || ArrayQ[qa, _, NumericQ]
] /; Acheck[numbers] && Ucheck[units] && QAcheck[numbers, units];


(*** QuantityArray as converter ***)

(* Construct a faster version of QuantityMagnitude and QuantityUnit for arrays *)

getmagnitudeSowUnit[mag_, unit_] := (Sow[unit]; mag);
getmagnitudeSowUnit[mag_, unit_, _] := (Sow[unit]; mag);
getunit[q_][q_[_, unit_]] := unit;
getunit[q_][q_[_, unit_, _]] := unit;
getunit[q_][_] := "DimensionlessUnit";

(* Return {array, numbers, flattenedunits, nelems}. We return the (possibly modified) array to avoid
   doing those modifications (toIdentityLevels or Normal) again. We return the nelems to perform some
   basic checks *)
getmagsunits[qa: HoldPattern[StructuredArray[QuantityArray, __]]] := With[
	{qaid = toIdentityLevels[qa]},
	{qaid, getNumbers[qaid], flattenList[reduceArray@ getUnits[qaid]], Times@@ Dimensions[qaid]}
];
getmagsunits[array_StructuredArray] := getmagsunits[Normal[array, StructuredArray]];
getmagsunits[array_SparseArray] := Module[{dims = arrayDimensions[array], magrules, units},
	{magrules, units} = Reap[Replace[ArrayRules[array], Quantity -> getmagnitudeSowUnit, {3}, Heads -> True]];
	{array, SparseArray[magrules, dims], units, Length[magrules]}
];
getmagsunits[array_List] := With[{dims = arrayDimensions[array]},
	Join[{array}, Reap[Replace[array, Quantity -> getmagnitudeSowUnit, {Length[dims] + 1}, Heads -> True]], {Times@@ dims}]
] /; FreeQ[array, _SparseArray | _StructuredArray];
getmagsunits[array_List] := Module[{mus = getmagsunits /@ array},
	If[arrayDepth[mus] < 2,
		Throw[$Failed, "QAConversionError"]
	];
	mus = Transpose[mus];
	If[Length[mus] < 4,
		Throw[$Failed, "QAConversionError"]
	];
	{mus[[1]], mus[[2]], flattenList[mus[[3]]], Total[mus[[4]]]}
];

(* Return the reduced units array. If the input is a QuantityArray then it will be already in IdentityLevels form.
   We have also normalized already other types of Structured array, but the definition is left here for safety *)
getunitsarray[qa: HoldPattern[StructuredArray[QuantityArray, __]]] := reduceArray@ getUnits[toIdentityLevels[qa]];
getunitsarray[array_StructuredArray] := getunitsarray[Normal[array, StructuredArray]];
getunitsarray[array_SparseArray] := Module[{q, dims = arrayDimensions[array]},
	SparseArray[MapAt[getunit[q], ArrayRules[array] /. Quantity -> q, {All, 2}], dims]
];
getunitsarray[array_List] := Module[{q, depth = arrayDepth[array]},
	Map[getunit[q], array /. Quantity -> q, {depth}]
] /; FreeQ[array, _SparseArray | _StructuredArray];
getunitsarray[array_List] := joinUnits[getunitsarray /@ array, Dimensions /@ array];
joinUnits[{}, {}] := {};
joinUnits[ulist_List, adims_List] := If[SameQ@@ ulist,
	First[ulist],
	reduceArray[MapThread[replicateArray, {ulist, adims}]]
];

(* Return a list {numbers, reducedunits} for a given input array *)
quantityMagnitudeUnit[array_] := Module[{newarray, mags, units, nelems},
	{newarray, mags, units, nelems} = getmagsunits[array];
	If[units === {},
		units = "DimensionlessUnit",
		units = flattenList[units];
		If[Length[units] === nelems && SameQ@@ units,
			(* Shortcut. All elements are Quantity objects and have the same unit *)
			units = First[units],
			units = reduceArray@ getunitsarray[newarray]
		]
	];
	{mags, units}
];

(* QuantityArray[A], where A is an array of Quantity objects, returns a QuantityArray
   representation of the same object. It does not try to change the units to get
   homogeneity. This should be done externally, with UnitConvert or CommonUnits.
   This conversion always produces a result with identity flattening. No clever choice. *)

(* This definition must be separated from the rest, for speed.
   TODO: QuantityArray on another QuantityArray could do some sort of canonicalization
   or simplification *)
QuantityArray[qa: HoldPattern[StructuredArray[QuantityArray, __]]] := qa;
(* On general types of rectangular arrays *)
QuantityArray[nqa_?ArrayQ] := Module[{nu, numbers, units},
	nu = Catch[quantityMagnitudeUnit[nqa], "QAConversionError"];
	If[ListQ[nu] && Length[nu] === 2, {numbers, units} = nu];
	QuantityArray[numbers, units] /; ArrayQ[numbers] && (ArrayQ[units] || unitQ[units])
];
(* Other lists. Thread *)
QuantityArray[list_List] := QuantityArray /@ list;
(* Do not require the ThreadDepth to be 0, due to bug 279113 *)
QuantityArray[HoldPattern[Quantity[numbers_, unit_, (ThreadDepth | "ThreadDepth") -> _]]] :=
	QuantityArray[numbers, unit];
QuantityArray[q_Quantity] := q;

(* On any other 1 or 2-args input, stay unevaluated without complaining *)

QuantityArray[args___] := $Failed /;
	(System`Private`Arguments[QuantityArray[args], {1, 2}]; False)


(*** Basic manipulation of QuantityArray objects ***)

(* Accessors *)
getNumbers[HoldPattern[StructuredArray[QuantityArray, dims_, data_]]] := getNumbers[data];
getUnits[  HoldPattern[StructuredArray[QuantityArray, dims_, data_]]] := getUnits[data];
getLevels[ HoldPattern[StructuredArray[QuantityArray, dims_, data_]]] := getLevels[data];

getNumbers[HoldPattern[StructuredArray`StructuredData[QuantityArray, numbers_, units_, levels_]]] := numbers;
getUnits[  HoldPattern[StructuredArray`StructuredData[QuantityArray, numbers_, units_, levels_]]] := units;
getLevels[ HoldPattern[StructuredArray`StructuredData[QuantityArray, numbers_, units_, levels_]]] := levels;

(* Combined accessor, as needed by Internal`QuantityToValue.
   The default values for the options are TargetUnits -> Automatic and "Compatibility" -> All.
   This last option handles the special case of matrices *)
compatibleUnit[units_] := First[commonUnitListOrMessage[Quantity][flattenList[units]]];
getData[qa: HoldPattern[StructuredArray[QuantityArray, __]], options___] := Catch[
	Module[{numbers = getNumbers[qa],
		units = getUnits[qa],
		levels = getLevels[qa],
		dim, depth, funits, targetunits, compatibility, defunits
	},
		{targetunits , compatibility} = {System`TargetUnits, "Compatibility"} /.
			Flatten[{options}] /. {System`TargetUnits -> Automatic, "Compatibility" -> All};
		depth = arrayDepth[numbers];
		Which[
			compatibility === All,
				(* We want all elements of the array to have the same unit *)
				funits = compatibleUnit[units];
				If[targetunits =!= Automatic && unitQ[targetunits],
					funits = targetunits
				];
				numbers = arrayUnitBlockConvert[numbers, units, funits],
			compatibility === "Rowwise" && depth === 2,
				(* We want all elements of each row of the matrix to have the same unit,
				   but different rows could have different units *)
				If[!levelsIdentityQ[levels],
					(* Undo flattening and retry *)
					Return[getData[toIdentityLevels[qa], options]],
					dim = Length[numbers];
					If[arrayDepth[units] === 2 && Length[units] === dim,
						defunits = compatibleUnit /@ units,
						defunits = compatibleUnit[units];
						defunits = ConstantArray[defunits, dim];
						units = ConstantArray[units, dim];
					];
					If[ListQ[targetunits] && Length[targetunits] === dim,
						funits = targetunits,
						funits = ConstantArray[targetunits, dim]
					];
					funits = MapThread[If[#1 === Automatic, #2, #1]&, {funits, defunits}];
					numbers = MapThread[arrayUnitBlockConvert, {numbers, units, funits}];
					If[!ArrayQ[numbers], Throw[$Failed, "IncompatibleUnits"]];
				],
			compatibility === "Columnwise" && depth === 2,
				(* We want all elements of each column of the array to have the same unit,
				   but different columns could have different units *)
				If[!levelsIdentityQ[levels],
					(* Undo flattening and retry *)
					Return[getData[toIdentityLevels[qa], options]],
					numbers = Transpose[numbers];
					dim = Length[numbers];
					If[arrayDepth[units] === 2 && Length[units[[1]]] === dim,
						units = Transpose[units];
						defunits = compatibleUnit /@ units,
						If[unitQ[units], units = ConstantArray[units, dim]];
						defunits = units;
						If[Length[defunits] =!= dim, Throw[$Failed, "IncompatibleUnits"]];
					];
					If[ListQ[targetunits] && Length[targetunits] === dim,
						funits = targetunits,
						funits = ConstantArray[targetunits, dim]
					];
					funits = MapThread[If[#1 === Automatic, #2, #1]&, {funits, defunits}];
					numbers = MapThread[arrayUnitBlockConvert, {numbers, units, funits}];
					If[!ArrayQ[numbers], Throw[$Failed, "IncompatibleUnits"]];
					numbers = Transpose[numbers];
				],
			True,
				Throw[$Failed, "IncompatibleUnits"]
		];
		If[levelsIdentityQ[levels],
			{numbers, reduceArray[funits]},
			{numbers, funits, levels}
		]
	],
	"IncompatibleUnits"
];

(* Validation *)
QuantityArray /: StructuredArray`ValidateStructuredData[QuantityArray, dims_, qdata_] :=
	QAstructuredDataQ[qdata, dims];

QAstructuredDataQ[HoldPattern[StructuredArray`StructuredData[QuantityArray, numbers_, units_, levels_]], dims_] :=
	Quiet[Acheck[numbers] && Ucheck[units] && QAcheck[numbers, units] && levelsQ[levels]] &&
	dims === dimsFlatten[arrayDimensions[numbers], levels];

QuantityArrayQ[HoldPattern[StructuredArray[QuantityArray, dims_List, qdata_]]] :=
	dimensionsQ[dims] && QAstructuredDataQ[qdata, dims];
QuantityArrayQ[_] := False;

(* Normalization. Let Quantity do all the threading through levels without units.
   TODO: Generalize to destructure only some levels. It may be nontrivial to flatten then. *)
QuantityArray /: StructuredArray`Destructure[QuantityArray, qa_, _] := With[{
	numbers = Normal[getNumbers[qa], {StructuredArray, SparseArray}],  (* Handle nested normalization *)
	units = getUnits[qa],
	levels = getLevels[qa]
},
	If[unitQ[units],
		(* This is a particular case of the general case, but this way is faster *)
		Quantity[myFlatten[numbers, levels], units],
		(* This is slow, but currently I don't see a better way to do it *)
		myFlatten[mapThread[Quantity, {numbers, units}, -arrayDepth[units]], levels]
	]
];

(* Tests in ArrayQ, MatrixQ, etc *)
QuantityArray /: StructuredArray`TestStructuredElements[QuantityArray, qa_StructuredArray] := True;
QuantityArray /: StructuredArray`TestStructuredElements[QuantityArray, qa_StructuredArray, test_] :=
	Which[
		MemberQ[$QuantityPredicates, test],
			True,
		MemberQ[$NonQuantityPredicates, test],
			False,
		MemberQ[$MagnitudePredicates, test],
			ArrayQ[getNumbers[qa], _, testTransform[test]],
		True,
			(* Need to check every single element, so destructure.
			   TODO: We can do better than this if the answer is False by testing
			   elements one by one and aborting as soon as possible. Currently we test
			   only the first element for failed cases, the most frequent situation *)
			Module[{depth = ArrayDepth[qa], elem},
				elem = Function[Part[qa, ##]]@@ ConstantArray[1, depth];
				If[!test[elem],
					False,
					ArrayQ[Normal[qa, StructuredArray], depth, test]
				]
			]
	];

(* Predicates that would return True on any Quantity object *)
$QuantityPredicates = {
	QuantityQ,
	Internal`PossibleQuantityQ
};

(* Predicates that would return False on any Quantity object *)
$NonQuantityPredicates = {
	NumericQ,
	NumberQ,
	Internal`RealValuedNumberQ,
	Internal`RealValuedNumericQ,
	IntegerQ,
	DateObjectQ,
	StringQ, AtomQ, GraphQ, ImageQ, ColorQ
};

(* Predicates p for which p[Quantity[mag, unit]] === testTransform[p][mag] *)
$MagnitudePredicates = {
	Positive, Negative, NonPositive, NonNegative,
	QuantityUnits`Private`NumericQuantityQ,
	Predictions`Private`RealValuedNumericQuantityQ
};

testTransform[QuantityUnits`Private`NumericQuantityQ] := NumericQ;
testTransform[Predictions`Private`RealValuedNumericQuantityQ] := Internal`RealValuedNumericQ;

(* Extraction via methods *)
QuantityArray /: StructuredArray`TryStructuredArrayMethod[QuantityArray, qa_, "Magnitudes"] := getNumbers[qa];
QuantityArray /: StructuredArray`TryStructuredArrayMethod[QuantityArray, qa_, "UnitBlock"] := getUnits[qa];
QuantityArray /: StructuredArray`TryStructuredArrayMethod[QuantityArray, qa_, "Flattening"] := getLevels[qa];

(* Add new methods in StructuredArray using an upvalue *)
Unprotect[StructuredArray];
HoldPattern[sa: StructuredArray[structure_, _, _]][method_String] :=
	With[{res = Quiet@ StructuredArray`TryStructuredArrayMethod[structure, sa, method]},
		res /; res =!= $Failed
	];
Protect[StructuredArray];
StructuredArray`TryStructuredArrayMethod[_, _, _] := $Failed;


(*** Undoing flattenings ***)

toIdentityLevels[qa_StructuredArray] := With[{
	numbers = getNumbers[qa],
	units = getUnits[qa],
	levels = getLevels[qa]
},
	(* Block cleverness of quantityArray trying to choose back a better flattening for unit matrices *)
	Block[{unitMatrixLooksTransposedQ},
		quantityArray[
			myFlatten[numbers, levels],
			deFlatten[units, levels, Dimensions[numbers]],
			levelsIdentity[Length[levels]]
		]
	]
];

(* A QuantityArray` driver to change the flattening levels without modifying the array.
   Without a second argument it changes to the identityLevels representation *)
ChangeFlattening[qa_StructuredArray?QuantityArrayQ] := toIdentityLevels[qa];
(* TODO: Implement the general 2-arg form of this function *)


(*** Parts ***)

(* Part extraction *)
(* TODO: Note that StructuredArray[...][[{}]] does not work. It should, returning {} *)
QuantityArray /: StructuredArray`StructuredPart[QuantityArray, qa_StructuredArray, indices___] :=
	getPart[getNumbers[qa], getUnits[qa], getLevels[qa], {indices}];

getPart[numbers_, units_, levels_?levelsIdentityQ, inds_List] := getPartSimple[numbers, units, inds];

getPartSimple[numbers_, units_, inds_List] := quantityArray[
	Part[numbers, Sequence@@ inds],
	Part[units, Sequence@@ Drop[inds, Min[Length[inds], arrayDepth[numbers] - arrayDepth[units]]]]
];

getPart[numbers_, units_, levels_, inds_List] := Module[{
	indsrules = indsPartition[inds, levels, arrayDimensions[numbers]],
	part, ranges
},
	ranges = Cases[indsrules, _[_, All | _Span | _List]];
	part = getPartSimple[numbers, units, Last /@ Sort[indsrules]];
	(* TODO: This final transposition is ugly. Is there a better way to keep track of it? *)
	If[ranges =!= {},
		part = Transpose[part, Ordering[First /@ ranges]];
	];
	part
];

(* Numbers in mixed bases *)
indsPartition[inds_, levels_, dims_] :=
	Flatten@ MapThread[indPartition[dims], {PadRight[inds, Length[levels], All], levels}, 1];
indPartition[dims_][All, levels_List] := Thread[levels -> All];
indPartition[dims_][span_Span, levels_List] := Thread[levels -> span];
indPartition[dims_][list_List, levels_List] := (# -> list)& /@ levels;
indPartition[dims_][i_Integer, levels_List] := Thread[levels -> integerDigits[i, dims[[levels]]]];
integerDigits[i_, dims_] := Block[{n},
	Reap[
		Fold[
			Function[Sow[n = Quotient[#1, #2]]; #1 - n #2],
			i,
			Reverse@ FoldList[Times, 1, Reverse@ Rest[dims]]
		]
	][[2, 1]]
];


(*** Numerics ***)

(* Numerization *)
QuantityArray /: StructuredArray`StructuredN[QuantityArray, f_, qa_StructuredArray] :=
	quantityArray[f[getNumbers[qa]], getUnits[qa], getLevels[qa]];

(* Precision and Accuracy. TODO: I think this is not really needed now, but I'm not sure *)
QuantityArray /: StructuredArray`StructuredPA[QuantityArray, Internal`EffectivePrecision, qa_StructuredArray] :=
	getNumbers[qa];


(*** Typesetting ***)

(* TODO: This should be done using upvalues, to avoid loading it in StructuredArray *)
(* Always use a generic icon, because in general Quantity objects of different units are not comparable *)

(* Summary box *)
Unprotect[StructuredArray];
StructuredArray /: MakeBoxes[sa: StructuredArray[QuantityArray, dims_, qdata_], fmt_] /;
	BoxForm`UseIcons && System`Private`ValidQ[Unevaluated@ sa] := Module[
	{numbers, units, levels, scalarUnitQ, numericQ, minmax,
	 typeItem, dimsItem, unitsItem, levelsItem, minmaxItem,
	 icon, alwaysGrid, sometimesGrid},
	(* Extract data *)
	numbers = getNumbers[qdata];
	units = getUnits[qdata];
	levels = getLevels[qdata];
	numericQ = ArrayQ[numbers, _, NumericQ];
	scalarUnitQ = KnownUnitQ[Evaluate[units]];  (* Beware that KnownUnitQ is HoldAll *)
	minmax = Which[
		numericQ && scalarUnitQ,
			Quantity[MinMax[numbers], units],
		numericQ && VectorQ[units, KnownUnitQ],
			MapThread[Quantity, {CoordinateBounds[numbers], units}, 1],
		True,
			$Failed
	];
	(* Use the flattened numbers for plotting *)
	numbers = myFlatten[numbers, levels];
	(* Items to show *)
	dimsItem = BoxForm`SummaryItem[{"Dimensions: ", dims}];
	unitsItem = BoxForm`SummaryItem[{If[scalarUnitQ, "Unit: ", "Units: "], Short[units, 2]}];
	levelsItem = BoxForm`SummaryItem[{"Flattening: ", levels}];
	minmaxItem = BoxForm`SummaryItem[{"MinMax: ", minmax}];
	(* Always show the default icon *)
	icon = BoxForm`GenericIcon[QuantityArray];
	alwaysGrid = {dimsItem, unitsItem};
	(* Show flattening only when it is nontrivial, as a way to indicate when it is or not trivial *)
	If[minmax =!= $Failed,
		sometimesGrid = {minmaxItem},
		sometimesGrid = {}
	];
	If[!levelsIdentityQ[levels],
		AppendTo[sometimesGrid, levelsItem];
	];
	BoxForm`ArrangeSummaryBox[QuantityArray, sa, icon, alwaysGrid, sometimesGrid, fmt, "Interpretable" -> False]
];
Protect[StructuredArray];

(* This is used for OutputForm, as well as for $BoxForms when BoxForm`UseIcon is set to False *)
QuantityArray /: StructuredArray`FormatStructuredArrayThirdArgument[QuantityArray, qa_] := QAThirdArgument[qa];

SetAttributes[QAThirdArgument, HoldAll];
QAThirdArgument[StructuredArray[QuantityArray, dims_, data_]] := QAThirdArgument[data];
QAThirdArgument[StructuredArray`StructuredData[QuantityArray, numbers_, units_, flattenings_]] :=
	If[OrderedQ[Flatten[flattenings]],
		{Short[units]},
		{SequenceForm[Short[units], ", ", flattenings]}
	];



(********* Quantity functionality *********)

(* These are functions that need to be called from the respective public Quantity functions *)


(*** Extractors ***)

QuantityArrayMagnitude[qa_StructuredArray?QuantityArrayQ] := myFlatten[getNumbers[qa], getLevels[qa]];
QuantityArrayMagnitude[qa_StructuredArray?QuantityArrayQ, units_] := QuantityArrayMagnitude[QuantityArrayUnitConvert[qa, units]];
QuantityArrayMagnitude[_] := $Failed;
QuantityArrayMagnitude[_, _] := $Failed;

QuantityArrayUnit[qa_StructuredArray?QuantityArrayQ] := With[{
	numbers = getNumbers[qa],
	units = getUnits[qa],
	levels = getLevels[qa]
},
	myFlatten[replicateArray[units, arrayDimensions[numbers]], levels]
];
QuantityArrayUnit[_] := $Failed;


(*** Unit conversion ***)

(* This converts an array of magnitudes all with the same single oldunit into the newunit. *)
arrayUnitConvert[array_, unit_, unit_] := array;
arrayUnitConvert[array_, oldunit_, newunit_] := With[
	{res = QuantityUnits`UnitConvertArray[array, oldunit, newunit]},
	If[res === $Failed,
		Message[Quantity::compat, oldunit, newunit];
		Throw[$Failed, "UnitConvertError"],
		res
	]
];

(* This converts an array of magnitudes with (deep) oldunitblock into the newunitblock. *)
arrayUnitBlockConvert[array_, unitblock_, unitblock_] := array;
arrayUnitBlockConvert[array_, oldunitblock_, newunitblock_] := Catch[
	mapThread[arrayUnitConvert, {array, oldunitblock, newunitblock}, -arrayDepth[newunitblock]],
	"UnitConvertError"
] /; arrayDimensions[oldunitblock] === arrayDimensions[newunitblock];
arrayUnitBlockConvert[array_, oldunitblock_, newunitblock_] := With[
	{unitblocks = makeSameDimensions[oldunitblock, newunitblock]},
	If[ListQ[unitblocks] && Length[unitblocks] === 2,
		arrayUnitBlockConvert[array, Sequence@@ unitblocks],
		$Failed
	]
];

(* Intercept the special case of using a unit system instead of a unit *)
unitSystemQ["Metric"] := True;
unitSystemQ["Imperial"] := True;
unitSystemQ["SI"] := True;
unitSystemQ["SIBase"] := True;
unitSystemQ[_] := False;
UnitSystemToUnit[unit_, usystem_] := QuantityUnit[UnitConvert[Quantity[1, unit], usystem]];

(* Motivated by bug 288220, normalize when unit incompatibilities are found. This only affects UnitConvert.
   Introduce this variable to deactivate normalization, which may be bad for large arrays. *)
$NormalizeOnUnitConvertError = True;

(* This function would be trivial if the unit block uext was always given with the same dimensions
   of the internal unit block of the QuantityArray object. However we have the levels spec in between
   and that complicates matters very much. There are two things we need to do. First we need to undo
   the levels spec on the uext to 'move it inside'. Then the unit blocks could still be of different
   dimensions, so we need to either replicate one or take subarrays of the other one. The key idea is
   that the levels spec of the output is always identical to that of the input. *)
QuantityArrayUnitConvert[qa_StructuredArray?QuantityArrayQ, uext_] := Module[{
	numbers = getNumbers[qa],
	units = getUnits[qa],
	levels = getLevels[qa],
	tmp, usystemQ, euext, eunits, newnumbers, status
},
	(* Detect the special case of a unit system *)
	usystemQ = unitSystemQ[uext];
	If[usystemQ,
		euext = uext,
		(* Check dimensional consistency of the uext with the qa *)
		If[arrayDepth[uext] > Length[levels] || Take[Dimensions[qa], -arrayDepth[uext]] =!= arrayDimensions[uext],
			(* The given unit block is too deep for this array *)
			Message[UnitConvert::udims, Dimensions[qa], arrayDimensions[uext]];
			Return[$Failed]
		];
		euext = uext /. q_Quantity :> QuantityUnit[q];  (* Confusing, but documented in ref/UnitConvert *)
		euext = Catch[canonUnitArray[euext], "InvalidUnit"];  (* Convert "Foot" into "Feet" and similar *)
		If[euext === $Failed,
			Message[UnitConvert::unkunit, uext];
			Return[$Failed]
		];
	];
	If[True,
		(* Make unit blocks comparable at the level of the internal array of numbers *)
		euext = unFlatten[euext, levels, Dimensions[numbers]];
		status = UnsameQ[euext, $Failed]
	];
	If[status,
		(* Make both unit arrays have the same dimensions by replication or reduction *)
		tmp = makeSameDimensions[units, euext];
		status = ListQ[tmp] && Length[tmp] === 2
	];
	If[status && usystemQ,
		eunits = MapThread[UnitSystemToUnit, tmp, arrayDepth[tmp[[1]]]];
		status = SameQ[Dimensions[eunits], Dimensions[tmp[[1]]]];
		tmp = {tmp[[1]], eunits};
	];
	If[status,
		{eunits, euext} = tmp;
		(* Now convert from one unit block to the other one.
		   Unit incompatibility messages will be issued here, with head Quantity, as UnitConvert does *)
		newnumbers = arrayUnitBlockConvert[numbers, eunits, euext];
		status = arrayQ[newnumbers, arrayDepth[numbers]];
	];
	If[status,
		(* Constuct the final QuantityArray object. We might be able to reduce the unit block *)
		quantityArray[newnumbers, reduceArray[euext], levels],
		(* Issue a message if for whatever reason there wasn't one yet *)
		If[$MessageList === {}, Message[UnitConvert::conv, uext]];
		If[TrueQ[$NormalizeOnUnitConvertError],
			(* Redo the whole operation from scratch, but this time on the normalized array.
			   Quiet because error messages have been already issued *)
			Quiet[mapThread[UnitConvert, {Normal[qa, StructuredArray], uext}, -arrayDepth[uext]]],
			(* Return $Failed, as UnitConvert does for single quantity objects *)
			$Failed
		]
	]
];

commonUnitList[units_List] := QuantityUnit[CommonUnits[Quantity /@ units]];
commonUnitList[unit_] := {unit};

commonUnitListOrMessage[head_][units_List] := Module[{uunits = DeleteDuplicates[units], common},
	common = commonUnitList[uunits];
	If[SameQ@@ common,
		common,
		Message[head::lcompat, uunits];
		Throw[$Failed, "IncompatibleUnits"]
	]
];

QuantityArrayCommonUnits[qa_StructuredArray?QuantityArrayQ] := Module[{
	numbers = getNumbers[qa],
	units = getUnits[qa],
	levels = getLevels[qa],
	funits, commonfunits, commonunits
},
	funits = DeleteDuplicates@ flattenList[units];
	commonfunits = commonUnitList[funits];
	If[commonfunits === funits, Return[qa]];
	commonunits = replaceAll[units, Thread[funits -> commonfunits]];
	numbers = arrayUnitBlockConvert[numbers, units, commonunits];
	quantityArray[numbers, reduceArray[commonunits], levels]
];

compatibleUnitsQ[units_] := CompatibleUnitQ[Quantity /@ DeleteDuplicates[flattenList[units]]];
compatibleUnitsWithMessageQ[head_][units_] := If[compatibleUnitsQ[units],
	True,
	Message[head::lcompat, units];
	Throw[False, "IncompatibleUnits"]
];

(* Check compatibility of units in two arrays of the same dimensions. Currently not used *)
compatibleUnitsQ[units1_, units2_] :=
	arrayDimensions[units1] === arrayDimensions[units2] &&
		And@@ Inner[compunitQ, flattenList[units1], flattenList[units2], Hold];
compunitQ[unit1_, unit2_] := CompatibleUnitQ[Quantity[1, unit1], Quantity[1, unit2]];

(* Construct a common unit block starting from two unit blocks. Check compatibility in the process *)
commonUnits[units1_, units2_] := Module[{units},
	If[arrayDimensions[units1] === arrayDimensions[units2],
		Catch[communits[units1, units2], "IncompatibleUnits"],
		units = makeSameDimensions[units1, units2];
		If[ListQ[units] && Length[units] === 2,
			commonUnits@@ units,
			$Failed
		]
	]
];
communits[unit_, unit_] := unit;
communits[units1_List, units2_List] := MapThread[communits, {units1, units2}, 1];
communits[unit1_, unit2_] := With[{
	common = DeleteDuplicates[QuantityUnit[CommonUnits[{Quantity[1, unit1], Quantity[1, unit2]}]]]
},
	If[Length[common] === 1,
		First[common],
		Throw[$Failed, "IncompatibleUnits"]
	]
];


(********* StructuredArray algorithms *********)


(*** Plus ***)

(* Plus will try to add the arrays in all possible orders, so it is enough to have
   QuantityArray as first structure type *)

QuantityArray /: StructuredArray`StructuredArrayAlgorithm[Plus, QuantityArray, type_] := QAPlus;

(* This is needed to handle the case of a single unit *)
tensorProduct[units_?ArrayQ, ones_] := TensorProduct[units, ones];
tensorProduct[unit_, ones_] := unit ones;

QAPlus[Plus, qa1_StructuredArray, qa2_StructuredArray?QuantityArrayQ] := Module[{
	dims1 = Dimensions[qa1],
	dims2 = Dimensions[qa2],
	numbers1 = getNumbers[qa1],
	numbers2 = getNumbers[qa2],
	units1 = getUnits[qa1],
	units2 = getUnits[qa2],
	levels1 = getLevels[qa1],
	levels2 = getLevels[qa2],
	numbers, units, levels, qqa1, qqa2
},
	Which[
		dims1 === dims2,
			If[levels1 === levels2,
				levels = levels1;
				If[units1 === units2,
					units = units1,
					units = commonUnits[units1, units2];
					If[units === $Failed,
						(* TODO: Message *)
						 Return[$Failed]
					];
					numbers1 = arrayUnitBlockConvert[numbers1, units1, units];
					numbers2 = arrayUnitBlockConvert[numbers2, units2, units];
				],
				(* Convert levels into identity levels and retry *)
				Return@ QAPlus[Plus, toIdentityLevels[qa1], toIdentityLevels[qa2]]
			],
		compatibleDimsQ[dims1, dims2],
			If[levelsIdentityQ[levels1] && levelsIdentityQ[levels2],
				(* Convert into one of the previous cases by tensor product *)
				If[Length[dims1] > Length[dims2],
					qqa1 = qa1;
					ones = ConstantArray[1, Drop[dims1, Length[dims2]]];
					qqa2 = quantityArray[
						TensorProduct[numbers2, ones],
						tensorProduct[units2, ones],
						levels1
					],
					ones = ConstantArray[1, Drop[dims2, Length[dims1]]];
					qqa1 = quantityArray[
						TensorProduct[numbers1, ones],
						tensorProduct[units1, ones],
						levels2
					];
					qqa2 = qa2
				];
				Return@ QAPlus[Plus, qqa1, qqa2],
				(* Convert levels into identity levels and retry *)
				Return@ QAPlus[Plus, toIdentityLevels[qa1], toIdentityLevels[qa2]]
			],
		True,
			(* Use message with head Thread, as Plus does, though this is ugly *)
			Message[Thread::tdlen, qa1, qa2];
			Return[$Failed]
	];
	quantityArray[numbers1 + numbers2, units, levels]
];

QAPlus[Plus, qa_StructuredArray, q: HoldPattern[Quantity[_, _, __]]] := 
	QAPlus[Plus, qa, QuantityArray[q]];

QAPlus[Plus, qa_StructuredArray, HoldPattern[Quantity[mag_, unit_]]] := Module[{
	dims = Dimensions[qa],
	numbers = getNumbers[qa],
	units = getUnits[qa],
	levels = getLevels[qa],
	funits,
	newnumbers,
	newunits
},
	funits = Union@ flattenList[units];
	Which[
		funits === {unit},
			quantityArray[
				numbers + mag,
				units,
				levels
			],
		CompatibleUnitQ[Append[funits, unit]],
			quantityArray[
				arrayUnitBlockConvert[numbers, units, ConstantArray[unit, Dimensions[units]]] + mag,
				unit,
				levels
			],
		True,
			(* TODO: Message *)
			$Failed
	]
];


(*** Times ***)

(* Times by scalar *)
QuantityArray /: StructuredArray`StructuredArrayAlgorithm[Times, QuantityArray, StructuredArray`ScalarQ] := QATimesScalar;

QATimesScalar[Times, qa_StructuredArray, scalar_?QuantityQ] := quantityArray[
	QuantityMagnitude[scalar] getNumbers[qa],
	QuantityUnit[scalar] getUnits[qa],
	getLevels[qa]
];

QATimesScalar[Times, qa_StructuredArray, scalar_] := quantityArray[
	scalar getNumbers[qa],
	getUnits[qa],
	getLevels[qa]
];

(* Times of two QuantityArray objects. Currently only for the levelsIdentityQ case *)
QuantityArray /: StructuredArray`StructuredArrayAlgorithm[Times, QuantityArray, QuantityArray] := QATimesQA;

compatibleDimsQ[dims1_List, dims2_List] := With[{n = Min[Length[dims1], Length[dims2]]},
	Take[dims1, n] === Take[dims2, n]
];

QATimesQA[Times, qa1_StructuredArray, qa2_StructuredArray] := With[{
	dims1 = Dimensions[qa1],
	dims2 = Dimensions[qa2],
	numbers1 = getNumbers[qa1],
	numbers2 = getNumbers[qa2],
	units1 = getUnits[qa1],
	units2 = getUnits[qa2],
	levels1 = getLevels[qa1],
	levels2 = getLevels[qa2]
},
	If[compatibleDimsQ[dims1, dims2],
		If[levelsIdentityQ[levels1] && levelsIdentityQ[levels2],
			quantityArray[
				Times[numbers1, numbers2],
				(* The following can generate 1 as unit, so quantityArray must be prepared for that *)
				If[arrayDepth[units1] >= arrayDepth[units2],
					Times[units1, replicateArray[units2, arrayDimensions[units1]]],
					Times[replicateArray[units1, arrayDimensions[units2]], units2]
				],
				If[Length[levels1] >= Length[levels2],
					levels1,
					levels2
				]
			],
			(* Convert to the previous case *)
			Return@ QATimes[Times, toIdentityLevels[qa1], toIdentityLevels[qa2]]
		],
		(* Use message with head Thread, as Times does, though this is ugly *)
		Message[Thread::tdlen, qa1, qa2];
		$Failed
	]
];


(*** Power ***)

QuantityArray /: StructuredArray`StructuredArrayAlgorithm[Power, QuantityArray, StructuredArray`ScalarQ] := QAScalarPower;
QuantityArray /: StructuredArray`StructuredArrayAlgorithm[Power, _, QuantityArray] := AnyQAPower;

QAScalarPower[powerF_, base_StructuredArray, exponent_] :=
	quantityArray[
		Power[getNumbers[base], exponent],
		Power[getUnits[base], exponent],
		getLevels[base]
	];

AnyQAPower[powerF_, base_, exponent_StructuredArray] := With[{
	numbers = getNumbers[exponent],
	units = getUnits[exponent],
	levels = getLevels[exponent]
},
	If[flattenList[units] === {"DimensionlessUnit"},
		powerF[base, myFlatten[numbers, levels]],
		$Failed
	]
];

(* TODO: It should be possible to take advantage of the QuantityArray character of the base also in the
   case in which the exponent is another array, for threading. Power is Listable *)


(*** Equal ***)

QuantityArray /: StructuredArray`StructuredArrayAlgorithm[Equal, QuantityArray, QuantityArray] := QAEqual;

QAEqual[comp_, qa1_StructuredArray, qa2_StructuredArray] := Module[{
	numbers1 = getNumbers[qa1],
	numbers2 = getNumbers[qa2],
	units1 = getUnits[qa1],
	units2 = getUnits[qa2],
	levels1 = getLevels[qa1],
	levels2 = getLevels[qa2],
	units, bool
},
	If[Dimensions[qa1] =!= Dimensions[qa2], Return[False]];
	Which[
		levels1 === levels2 && units1 === units2,
			bool = comp[numbers1, numbers2],
		levelsIdentityQ[levels1] && levelsIdentityQ[levels2],
			If[units1 =!= units2,
				units = commonUnits[units1, units2];
				If[units === $Failed, Return[False]];
				numbers1 = arrayUnitBlockConvert[numbers1, units1, units];
				numbers2 = arrayUnitBlockConvert[numbers2, units2, units];
			];
			bool = comp[numbers1, numbers2],
		True,
			(* Convert to the previous case *)
			Return@ QAEqual[comp, toIdentityLevels[qa1], toIdentityLevels[qa2]]
	];
	If[TrueQ[bool || !bool],
		bool,
		Indeterminate
	]
];


(*** Flatten ***)

QuantityArray /: StructuredArray`StructuredArrayAlgorithm[Flatten, QuantityArray] := QAFlatten;

flattenFLevels[flattenF_] := flattenF[[1, 2]];

parseFLevels[Infinity, depth_] := {Range[depth]};
parseFLevels[n_Integer, depth_] := If[n < depth,
	Join[{Range[n + 1]}, List /@ Range[n + 2, depth]],
	{Range[depth]}
];
parseFLevels[list: {__Integer}, depth_] := Join[{list}, List /@ Complement[Range[depth], list]];
parseFLevels[levels_, depth_] := Join[levels, List /@ Complement[Range[depth], Flatten[levels]]];

QAFlatten[flattenF_, qa_StructuredArray] := quantityArray[
	getNumbers[qa],
	getUnits[qa],
	levelsFlatten[getLevels[qa], parseFLevels[flattenFLevels[flattenF], arrayDepth[qa]]]
];


(*** Partition ***)

(* Handle some simple cases *)
QuantityArray /: StructuredArray`StructuredArrayAlgorithm[Partition, QuantityArray] := QAPartition;

QAPartition[partitionF_, qa_StructuredArray] := Module[{
	numbers = getNumbers[qa],
	units = getUnits[qa],
	levels = getLevels[qa],
	lengths = Extract[partitionF, {1, 2}],
	offsets = Extract[partitionF, {1, 3}],
	knobs   = Extract[partitionF, {1, 4}],
	padding = Extract[partitionF, {1, 5}],
	nlengths, nlevels, plevels, dimpos
},
	If[offsets =!= lengths || knobs =!= None || padding =!= qa,
		(* We cannot yet handle arguments 3, 4, 5 of Partition *)
		Return[$Failed]
	];

	nlengths = Length[lengths];
	nlevels = Length[levels];
	Which[
		levelsIdentityQ[levels] && nlengths <= arrayDepth[numbers] - arrayDepth[units],
			(* The partitioning does not affect the unit blocks *)
			quantityArray[
				Partition[numbers, lengths],
				units,
				List /@ Range[nlevels + nlengths]
			],
		nlevels >= nlengths && Min[Length /@ (plevels = Take[levels, nlengths])] > 1 &&
			(Last /@ plevels) === (dimpos = Range[Max[plevels] - nlengths + 1, Max[plevels]]) &&
			Dimensions[numbers][[dimpos]] === lengths,
			(* The partitioning undoes a previous flattening *)
			quantityArray[
				numbers,
				units,
				Join[Most /@ plevels, List /@ Last /@ plevels, Drop[levels, nlengths]]
			],
		True,
			$Failed
	]
];


(*** Transpose ***)

QuantityArray /: StructuredArray`StructuredArrayAlgorithm[Transpose, QuantityArray] := QATranspose;

getTransposePerm[transposeF_] := transposeF[[1, 2]];

QATranspose[transposeF_, qa_StructuredArray] := quantityArray[
	getNumbers[qa],
	getUnits[qa],
	Permute[getLevels[qa], getTransposePerm[transposeF]]
];


(*** Outer and TensorProduct ***)

(* Right now Outer normalizes all StructuredArray objects.
   In general Outer will not be able to do better than this, except for Outer[Times, qa1, qa2],
   that is except for the TensorProduct case, in which we do want to keep the QuantityArray form.
   Here we specify how to do the Outer of two QuantityArray objects. This can be iterated for any
   flat product (like Times). *)

QuantityArray /: StructuredArray`StructuredArrayAlgorithm[Outer, QuantityArray, _] := QAOuter2;
QuantityArray /: StructuredArray`StructuredArrayAlgorithm[Outer, _, QuantityArray] := QAOuter2;

(* Handle "DimensionlessUnit" with care in the tensor product of units (always normalized) *)
unitsTimes["DimensionlessUnit", "DimensionlessUnit"] := "DimensionlessUnit";
unitsTimes["DimensionlessUnit", unit_] := unit;
unitsTimes[unit_, "DimensionlessUnit"] := unit;
unitsTimes[unit1_, unit2_] := Times[unit1, unit2];
unitsTensorProduct[units1_List, units2_List] := Outer[unitsTimes, units1, units2];
unitsTensorProduct["DimensionlessUnit", units_] := units;
unitsTensorProduct[units_, "DimensionlessUnit"] := units;
unitsTensorProduct[units1_, units2_] := Times[units1, units2];

(* TensorProduct (Outer[Times, ..., Infinity]) of two QuantityArray objects *)
QAOuter2[fun_Function, qa1_StructuredArray?QuantityArrayQ, qa2_StructuredArray?QuantityArrayQ] := Module[{
	numbers1 = getNumbers[qa1], units1 = getUnits[qa1], levels1 = getLevels[qa1], effrank1 = Extract[fun, {1, 4}],
	numbers2 = getNumbers[qa2], units2 = getUnits[qa2], levels2 = getLevels[qa2], effrank2 = Extract[fun, {1, 5}],
	f = Extract[fun, {1, 1}], d1, d2, u1, u2, perm, range
},
	If[f =!= Times, Return[$Failed]];
	d1 = arrayDepth[numbers1];
	d2 = arrayDepth[numbers2];
	u1 = arrayDepth[units1];
	u2 = arrayDepth[units2];
	If[effrank1 < d1, Return[$Failed]];
	If[effrank2 < d2, Return[$Failed]];
	perm = Range[d1 + d2];
	range = Range[d1 - u1 + 1, d1 + d2 - u2];
	perm[[range]] = RotateRight[range, u1];
	quantityArray[
		Transpose[TensorProduct[numbers1, numbers2], perm],
		unitsTensorProduct[units1, units2],
		PermutationReplace[Join[levels1, levels2 + d1], perm]
	]
];

QAOuter2[fun_, array1_, array2_] := Module[{
	qa1 = QuantityArray[array1],
	qa2 = QuantityArray[array2],
	qa
},
	If[QuantityArrayQ[qa1] && QuantityArrayQ[qa2],
		qa = QAOuter2[fun, qa1, qa2],
		qa = $Failed
	];
	If[QuantityArrayQ[qa],
		qa,
		$Failed
	]
];


(*** Mean ***)

QuantityArray /: StructuredArray`StructuredArrayAlgorithm[Mean, QuantityArray] := QAMean;

QAMean[fun_, qa_StructuredArray?QuantityArrayQ] := Module[{
	numbers = getNumbers[qa],
	units = reduceArray[getUnits[qa]],
	levels = getLevels[qa],
	newunits, newnumbers
},
	If[First[levels] === {1},
		If[arrayDepth[units] < arrayDepth[numbers],
			newnumbers = numbers;
			newunits = units,
			If[Catch[And@@ (compatibleUnitsWithMessageQ[Mean] /@ Transpose[Flatten /@ List /@ units]),
				"IncompatibleUnits"],
				newunits = First[units];
				newnumbers = MapThread[arrayUnitBlockConvert,
					{numbers, units, ConstantArray[newunits, Length[units]]}
				],
				Return[$Failed]
			]
		];
		quantityArray[Mean[newnumbers], newunits, Rest[levels] - 1],
		QAMean[fun, toIdentityLevels[qa]]
	]
];


(*** Min, Max, CoordinateBounds ***)

QuantityArray /: StructuredArray`StructuredArrayAlgorithm[Min, QuantityArray] := QAMinMax;
QuantityArray /: StructuredArray`StructuredArrayAlgorithm[Max, QuantityArray] := QAMinMax;
QuantityArray /: StructuredArray`StructuredArrayAlgorithm[MinMax, QuantityArray] := QAMinMax;
QuantityArray /: StructuredArray`StructuredArrayAlgorithm[CoordinateBounds, QuantityArray] := QACoordinateBounds;
QuantityArray /: StructuredArray`StructuredArrayAlgorithm[CoordinateBoundingBox, QuantityArray] := QACoordinateBoundingBox;

QAMinMax[fun_, qa_StructuredArray?QuantityArrayQ] := Module[{
	numbers = getNumbers[qa],
	units = getUnits[qa],
	funits
},
	funits = DeleteDuplicates[flattenList[units]];
	Which[
		Length[funits] === 1,
			Quantity@@ {fun[numbers], First[funits]},
		compatibleUnitsQ[funits],
			funits = commonUnitList[funits];
			QAMinMax[fun, QuantityArrayUnitConvert[qa, First[funits]]],
		True,
			Message[fun::lcompat, funits];
			$Failed
	]
];

QACoordinateBounds[fun_, qa_StructuredArray?QuantityArrayQ] := Catch[Module[{
	numbers = getNumbers[qa],
	units = getUnits[qa],
	levels = getLevels[qa],
	fnumbers, funits
},
	units = reduceArray[units];
	funits = DeleteDuplicates[flattenList[units]];
	If[Length[funits] === 1,
		fnumbers = myFlatten[numbers, levels];
		Return[ Quantity@@ {CoordinateBounds[fnumbers], First[funits]} ]
	];
	If[!levelsIdentityQ[levels],
		Return[ QACoordinateBounds[fun, toIdentityLevels[qa]] ]
	];
	units = Transpose[units, RotateLeft[Range[arrayDepth[units]]]];
	numbers = Transpose[numbers, RotateLeft[Range[arrayDepth[numbers]]]];
	funits = DeleteDuplicates /@ commonUnitList /@ (DeleteDuplicates[flattenList[#]]& /@ units);
	If[Length[#] =!= 1,
		Message[fun::lcompat, #];
		Throw[$Failed, "QACoordinateBoundsError"]
	]& /@ funits;
	funits = First /@ funits;
	numbers = MapThread[arrayUnitBlockConvert, {numbers, units, funits}, 1];
	MapThread[Quantity, {MinMax /@ numbers, funits}, 1]
], "QACoordinateBoundsError"];

QACoordinateBoundingBox[fun_, qa_StructuredArray?QuantityArrayQ] := With[{
	bounds = QACoordinateBounds[CoordinateBounds, qa]
},
	If[MatrixQ[bounds],
		Transpose[bounds],
		$Failed
	]
];

(*** Total ***)

QuantityArray /: StructuredArray`StructuredArrayAlgorithm[Total, QuantityArray] := QATotal;

getTotalLevels[Total] := {1};
getTotalLevels[func_Function, depth_] := Module[{levs = func[[1, 2]]},
	levs = levs /. l_Integer?Negative :> l + depth;
	levs = levs /. Infinity -> depth;
	Switch[levs,
		_Integer, Range[levs],
		{_Integer}, levs,
		{_Integer, _Integer}, Range@@ levs
	]
];

shiftdown[list_List] := With[{elems = Sort[Flatten[list]]},
	list /. Thread[Rule[elems, Range[Length[elems]]]]
];

(* Given a list of slots, construct the permutation that moves them to a different list of positions,
   with the same length *)
permutationFromTo[original_List, final_List] := With[{range = Range[Max[original, final]]},
	 Last /@ SortBy[ Join[
		Transpose[{original, final}],
		Transpose[{Complement[range, original], Complement[range, final]}]
	], First ]
];
(* Extension of Total to specify the actual list of levels, whatever they may be *)
myTotal[array_, {}] := array;
myTotal[array_, levels_List] := Module[{min = Min[levels], max = Max[levels], perm},
	If[levels === Range[min, max],
		(* We can use the notation Total[array, {m, n}] *)
		Total[array, {min, max}],
		(*  Total does not take a list of levels, so we need to transpose *)
		perm = permutationFromTo[levels, min - 1 + Range[Length[levels]]];
		Total[Transpose[array, perm], {min, min - 1 + Length[levels]}]
	]
];

QATotal[func_, qa_StructuredArray?QuantityArrayQ] := Module[{
	numbers = getNumbers[qa],
	units = getUnits[qa],
	levels = getLevels[qa],
	totallevels = getTotalLevels[func, ArrayDepth[qa]],
	adepth, udepth, audepth, unitlevels, itotallevels, intersection, newnumbers, newunits
},
	adepth = arrayDepth[numbers];
	udepth = arrayDepth[units];
	audepth = adepth - udepth;
	unitlevels = Range[audepth + 1, adepth];
	itotallevels = Sort[Join@@ Extract[levels, List /@ totallevels]];
	intersection = Intersection[itotallevels, unitlevels];
	If[intersection === {},
		newunits = units;
		newnumbers = myTotal[numbers, itotallevels],
		(* Flatten intersection levels at the bottom *)
		numbers = Flatten[numbers, Join[List /@ Complement[Range[adepth], intersection], {intersection}]];
		units = Flatten[units, Join[List /@ Complement[Range[udepth], intersection - audepth], {intersection - audepth}]];
		(* Check unit compatibility of intersection levels and convert to common unit *)
		newunits = Catch[Map[commonUnitListOrMessage[Total], units, {arrayDepth[units] - 1}], "IncompatibleUnits"];
		If[newunits === $Failed,
			(* Message has been emitted already *)
			Return[$Failed]
		];
		newnumbers = arrayUnitBlockConvert[numbers, units, newunits];
		newunits = Map[First, newunits, {arrayDepth[newunits] - 1}];  (* This is some kind of array deep-reduction *)
		(* Finally do Total of nonintersection levels and the deep intersection-flattened level *)
		newnumbers = myTotal[newnumbers, Append[Complement[itotallevels, intersection], arrayDepth[newnumbers]]]
	];
	quantityArray[
		newnumbers,
		newunits,
		shiftdown[Delete[levels, List /@ totallevels]]
	]
];

End[]

EndPackage[]
