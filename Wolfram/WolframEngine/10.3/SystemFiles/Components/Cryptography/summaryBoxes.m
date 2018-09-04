Package["Cryptography`"]


getLength[s_String] := StringLength[s];
getLength[l: _List | _ByteArray] := Row[{Length[l]*8, " bits"}];
getLength[n_Integer] := Row[{IntegerLength[n, 2], " bits"}];

getByteLength[b_ByteArray] := bytesToSize @ Length[b];
getByteLength[_] := Indeterminate;

bytesToSize[bytes_Integer] := Module[{i, k = 1000, sizes = {"bytes", "kB", "MB", "GB", "TB", "PB", "EB", "ZB", "YB"}},
	If[bytes == 0,
		Return[ToString[bytes]  <> " bytes"];
	];
	i = Floor[Log[bytes]/Log[k]];
	If[i == 0,
		Return[ToString[bytes]  <> " bytes"];
		,
		Return[ToString[NumberForm[N[bytes/k^i], {4, 1}]] <> " " <> sizes[[i + 1]]]
	]
];

tofield["InitializationVector", vec_] := {"IV length: ", getLength[vec]};
tofield["InitializationVector", None] := {"IV: ", None};
tofield["OriginalForm", type_] := {"original form: ", type};
tofield["Key", key_] := {"key length: ", getLength[key]};
tofield["Cipher", method_] := {"cipher: ", method};
tofield["BlockMode", mode_] := {"block mode: ", mode}
tofield["Data", ciphertext_] := {"data length: ", getByteLength[ciphertext]};
tofield["Padding", packing_] := {"padding method: ", packing};
tofield["PublicExponent", exponent_] := {"public exponent: ", exponent};
tofield["PrivateExponent", exponent_] := {"private exponent length: ", getLength[exponent]};
tofield["PublicModulus", exponent_] := {"public modulus length: ", getLength[exponent]};
tofield[fieldname_, _] := $Failed[fieldname];

makeKeyIcon[data_, colorFunction_] := ArrayPlot[
	Partition[IntegerDigits[Hash[data,"SHA256"],4,64],8],
		ImageSize -> 42,
		ColorFunction -> colorFunction, ColorFunctionScaling -> False,
		Frame -> None, PlotRangePadding -> None];

decamelize[str_String] := StringTrim @ StringReplace[str, s:LetterCharacter /; UpperCaseQ[s] :> (" " <> ToLowerCase[s])];
toextrafield[str_, value_] := {decamelize[str] <> ": ", value};
toextrafield[str_, value:_Integer] := {decamelize[str] <> ": ", Short[value, 0.25]};



SymmetricKey /: MakeBoxes[SymmetricKey[data_Association], StandardForm] :=
	BoxForm`ArrangeSummaryBox[
		SymmetricKey, 
		SymmetricKey[data],
		makeKeyIcon[data, 106],
		System`KeyValueMap[tofield /* BoxForm`SummaryItem, KeyDrop[data, {"InitializationVector"}]],
		{},
		StandardForm];
	
	
EncryptedObject /: MakeBoxes[EncryptedObject[data_Association], StandardForm] :=
	BoxForm`ArrangeSummaryBox[
		EncryptedObject, 
		EncryptedObject[data],
		makeKeyIcon[data, 47],
		(tofield /* BoxForm`SummaryItem) @@@ Normal[data],
		{},
		StandardForm, "Interpretable"->False];
	
PrivateKey /: MakeBoxes[PrivateKey[data_Association], StandardForm] :=
	BoxForm`ArrangeSummaryBox[
		PrivateKey, 
		PrivateKey[data],
		makeKeyIcon[data, 100],
		(tofield /* BoxForm`SummaryItem) @@@ DeleteCases[Normal[data], ("PublicExponent" -> _) | ("Padding" -> _)],
		(toextrafield /* BoxForm`SummaryItem) @@@ DeleteCases[Normal[data], ("Cipher" -> _)], 
		StandardForm];
	
PublicKey /: MakeBoxes[PublicKey[data_Association], StandardForm] :=
	BoxForm`ArrangeSummaryBox[
		PublicKey, 
		PublicKey[data],
		makeKeyIcon[data, 63],
		(tofield /* BoxForm`SummaryItem) @@@ DeleteCases[Normal[data], ("PublicExponent" -> _) | ("Padding" -> _)],
		(toextrafield /* BoxForm`SummaryItem) @@@ DeleteCases[Normal[data], ("Cipher" -> _)],
		StandardForm];