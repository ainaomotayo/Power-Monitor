Package["Cryptography`"]

PackageExport["Macros`"]

PackageExport["$LastEncryptionError"]

checkList[l_List] := l;
checkList[err_LibraryFunctionError] := ($LastEncryptionError = err; Throw[$Failed]); 
checkList[err_] := ($LastEncryptionError = err; Throw[$Failed]);

Clear[encryptInternal];

encryptInternal[SymmetricKey[info_Association], data_List, dir_Integer] := 
	llencryptSym[
		$SymmetricCipherNumbering[info["Cipher"], info["BlockMode"]],
		data,
		Normal[info["Key"]],
		Replace[Normal[info["InitializationVector"]], None -> {}],
		dir
	] // checkList;

encryptInternal[PublicKey[info_Association], data_List, dir_Integer] := (
	If[dir == 0 && !MatchQ[info["Padding"], "PKCS1" | "None" | None],
		Message[Decrypt::padding]; Throw[$Failed]];
	llrsaPublic[
		data, 
		ToCharacterCode[ToString[info["PublicModulus"]]],
		ToCharacterCode[ToString[info["PublicExponent"]]],
		$RSAPaddingModeNumbering[info["Padding"]],
		dir
	] // checkList
);

encryptInternal[PrivateKey[info_Association], data_, dir_Integer] := (
	If[dir == 1 && !MatchQ[info["Padding"], "PKCS1" | "None" | None],
		 Message[Encrypt::padding]; Throw[$Failed]];
	llrsaPrivate[
		data,
		ToCharacterCode[ToString[info["PublicModulus"]]],
		ToCharacterCode[ToString[info["PublicExponent"]]],
		ToCharacterCode[ToString[info["PrivateExponent"]]],
		$RSAPaddingModeNumbering[info["Padding"]],
		dir
	] // checkList
);

encryptInternal[args___] := (
	Message[Encrypt::failed]; 
	Throw[$Failed];
);

Encrypt::invkeyspec = Decrypt::invkeyspec = "Key should a string or valid SymmetricKey, PrivateKey, or PublicKey."
GenerateSymmetricKey::invcipher = "`1` is not a valid cipher."
GenerateSymmetricKey::invkeylen = "Key length `1` is not valid."
GenerateSymmetricKey::invivlen = "Initialization vector length `1` is not valid."
GenerateSymmetricKey::invblockmode = "`` is not a valid block mode."
GenerateSymmetricKey::inviv = "Initialization vector should be a valid ByteArray."

Encrypt::failed = "Data could not be encrypted.";
Decrypt::failed = "Data could not be decrypted."
Decrypt::key = "Key is not compatible with the encrypted data." 
Encrypt::keylen = Decrypt::keylen = "Key is not of suitable length."
Encrypt::ivlen = Decrypt::ivlen = "Initialization vector is not of suitable length."
Encrypt::padding = Decrypt::padding = "Specified padding could not be used."
Encrypt::len = Decrypt::len = "Input too long for this encryption method."
Encrypt::invdata = "Input is not a valid ByteArray.";
Decrypt::invdata = "`` is not a ByteArray or valid EncryptedObject.";

PackageExport["Encrypt"]

encryptForm[data_ByteArray] := {If[!System`ByteArrayQ[data], Message[Encrypt::invdata]; Throw[$Failed]]; Normal[data], ByteArray};
encryptForm[data_String] := {ToCharacterCode[data, "UTF8"], String};
encryptForm[expr_] := {Normal @ ByteArray[StringDrop[Compress[expr], 2]], Expression};

Clear[Encrypt];
Encrypt[key_, data_] := Module[
	{res, key2, data2, form, iv, encrypted},
	res = Catch[
		key2 = toKey[key, randomBytes];
		If[key2 === $Failed, 
			Message[MessageName[Encrypt, "invkeyspec"]];
			Throw[$Failed];
		];
		iv = key2["InitializationVector"];
		{data2, form} = encryptForm[data];
		encrypted = encryptInternal[key2, data2, 1];
		EncryptedObject[<|
			"Data" -> ByteArray[encrypted],
			If[System`ByteArrayQ[iv], "InitializationVector" -> iv, Sequence @@ {}],
			"OriginalForm" -> form
		|>]
	];
	res
];

Clear[toKey];

assocMatchQ[assoc_Association, rules_] := MatchQ[Sort @ Normal @ assoc, Sort @ rules];

toKey[SymmetricKey[assoc_Association], ivfunc_] := Module[
	{iv, cipher, blockmode, assoc2 = assoc, key, ivsize},
	
	{iv, cipher, blockmode, key} = Lookup[assoc, {"InitializationVector", "Cipher", "BlockMode", "Key"}];
	
	If[!MemberQ[$SymmetricCiphers, cipher], Return[$Failed]];
	
	If[!ValidBlockModeQ[blockmode, cipher], Return[$Failed]];
	
	If[CipherRequiresIVQ[cipher, blockmode],
		ivsize = $SymmetricIVSizes[cipher];
		Which[
			iv === None,
				assoc2["InitializationVector"] = ivfunc[ivsize/8],
			System`ByteArrayQ[iv],
				If[Length[iv]*8 =!= ivsize, Return[$Failed]],
			True,
				Return[$Failed]
		];

	];
	
	If[!System`ByteArrayQ[key] || !ValidKeySizeQ[Length[key], cipher], 
		Return[$Failed]
	];

	SymmetricKey[assoc2]
];

toKey[key:PublicKey[assoc_Association], _] /;
	assocMatchQ[assoc, {
		"Cipher" -> "RSA",
		"Padding" -> p_ /; MemberQ[$RSAPaddingModes, p],
		"PublicExponent" -> _Integer,
		"PublicModulus" -> _Integer
	}] := key;
	
toKey[key:PrivateKey[assoc_Association], _] /;
	assocMatchQ[assoc, {
		"Cipher" -> "RSA",
		"Padding" -> p_ /; MemberQ[$RSAPaddingModes, p],
		"PrivateExponent" -> _Integer,
		"PublicExponent" -> _Integer,
		"PublicModulus" -> _Integer
	}] := key;
	
toKey[s_String, ivfunc_] := 
	Replace[Quiet @ GenerateSymmetricKey[s], {
		k_SymmetricKey :> toKey[k, ivfunc],
		_ :> $Failed
	}];

toKey[head_, ___] := $Failed;


PackageExport["Decrypt"]

unwrapEncryptedData[e:EncryptedObject[data_Association]] := With[{keys = Keys[data]}, If[
	SubsetQ[keys, {"Data", "OriginalForm"}] && System`ByteArrayQ @ data["Data"] && MemberQ[{ByteArray, String, Expression}, data["OriginalForm"]],
	MapAt[Normal, 1] @ Lookup[data, {"Data", "OriginalForm", "InitializationVector"}],
	Message[Decrypt::invdata , e]; Throw[$Failed]]
];

unwrapEncryptedData[ba_ByteArray ? ByteArrayQ] := {Normal[ba], ByteArray, None&};

unwrapEncryptedData[e_] := (Message[Decrypt::invdata, e]; Throw[$Failed]);

wrapForm[ByteArray, data_List] := ByteArray[data];

wrapForm[String, data_List] := Quiet @ Check[
	FromCharacterCode[data, "UTF8"],
	Throw[Unevaluated[Message[Decrypt::failed]; $Failed]]];

wrapForm[Expression, data_List] := Quiet @ Check[
	Uncompress @ StringJoin["1:", Developer`EncodeBase64[data]], (* hate how ugly this is, but we don't yet have very good ByteArray support *)
	Throw[Unevaluated[Message[Decrypt::failed]; $Failed]]];
	
wrapForm[_, _] := (
	Message[Decrypt::failed];
	Throw[$Failed];
)

Clear[Decrypt];

Decrypt[key_, data_] := Module[
	{res, key2, data2, form, iv, decrypted},
	res = Catch[
		{data2, form, iv} = unwrapEncryptedData[data];
		key2 = toKey[key, iv&];
		If[key2 === $Failed, 
			Message[MessageName[Decrypt, "invkeyspec"]];
			Throw[$Failed];
		];
		decrypted = encryptInternal[key2, data2, 0];
		wrapForm[form, decrypted]
	];
	res
];