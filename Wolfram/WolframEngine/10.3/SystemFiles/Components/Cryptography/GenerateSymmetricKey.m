Package["Cryptography`"]


PackageExport["SymmetricKey"]

(*SymmetricKey::usage = "Represents a symmetric key"*)

SymmetricKey[data_Association][key_] := data[key]

PackageExport["GenerateSymmetricKey"]

Clear[GenerateSymmetricKey];

GenerateSymmetricKey::invcipher = GenerateAsymmetricKeyPair::invcipher = "`1` is not a valid cipher."
GenerateSymmetricKey::invmethodkey = GenerateAsymmetricKeyPair::invmethodkey = "The method key `1` is not one of ``."
GenerateSymmetricKey::invblockmode = "`1` is not a known block mode for this method."
GenerateSymmetricKey::invivlen = "Cipher requires initialization vector of `1` bytes; `2` bytes given."
GenerateSymmetricKey::inviv = "Initialization vector `` should be Automatic, None, or a valid ByteArray.";
GenerateSymmetricKey::invkeysize = "`1` is not a valid key size."
GenerateSymmetricKey::invpassword = "Password cannot be empty."
GenerateSymmetricKey::nocipher = "No cipher given."

Options[GenerateSymmetricKey] = {Method->"AES256"}

PackageScope["checkInvalidMethodKeys"]

checkInvalidMethodKeys[head_, assoc_, allowed_] := 
	Replace[Complement[Keys[assoc], allowed],
		{{} -> None,
		 l_List :> (
		 	Message[MessageName[head, "invmethodkey"], First[l], allowed];
		 	Throw[$Failed];
		 )}];


PackageScope["missingQ"]

missingQ[_Missing] := True;
missingQ[_] := False;

parseMethod[cipher_String] := parseMethod[<|"Cipher" -> cipher|>];

parseMethod[assoc_Association] := Module[
	{cipher, blockmode, keysize, iv, validLen},
	
	checkInvalidMethodKeys[GenerateSymmetricKey, assoc, {"Cipher", "BlockMode", "KeySize", "InitializationVector"}];
	
	{cipher, blockmode, keysize, iv} = Lookup[assoc, {"Cipher", "BlockMode", "KeySize", "InitializationVector"}];
		
	If[missingQ[cipher], 
		Message[GenerateSymmetricKey::nocipher]; 
		Throw[$Failed]];
		
	If[!MemberQ[$SymmetricCiphers, cipher], 
		Message[GenerateSymmetricKey::invcipher, cipher]; 
		Throw[$Failed]];
	
	If[missingQ[blockmode],
		blockmode = If[cipher === "RC4", None, "CBC"];
	,
		If[!ValidBlockModeQ[blockmode, cipher],
			Message[GenerateSymmetricKey::invblockmode, blockmode];
			Throw[$Failed]
		]
	];
	
	If[missingQ[keysize]  || keysize === Automatic,
		keysize = $SymmetricKeySizes[cipher];
	,
		If[!(MatchQ[keysize, _Integer] && keysize > 0) || !ValidKeySizeQ[keysize, cipher],
			Message[GenerateSymmetricKey::invkeysize, keysize];
			Throw[$Failed]
		];
	];
	
	validLen = $SymmetricIVSizes[cipher] / 8;
	iv = parseIV[iv, validLen];
	
	{cipher, blockmode, keysize, iv}
];

parseMethod[method_] := (
	Message[GenerateSymmetricKey::invmethod, method];
	Throw[$Failed]
); 

parseIV[None|_Missing, _] := None;
parseIV[Automatic, n_] := randomBytes[n];
parseIV[iv_ByteArray ? ByteArrayQ, n_] := If[Length[iv] === n, iv,
	Message[GenerateSymmetricKey::invivlen, n, Length[iv]];
	Throw[$Failed]
];
parseIV[iv_, _] := (
	Message[GenerateSymmetricKey::inviv, iv];
	Throw[$Failed]
);


toKey[Automatic, keysize_] := randomBytes[keysize];

toKey[password_String, keysize_] := (If[password === "", Message[GenerateSymmetricKey::invpassword];Throw[$Failed]];
									ByteArray[scrypt[ToCharacterCode[password, "UTF8"], ToCharacterCode["wolframKey", "UTF8"], keysize]])

toKey[ba_ByteArray, keysize_] := ByteArray[PadRight[ba, 0, keysize]];

toKey[__] := Throw[$Failed];

GenerateSymmetricKey[opts:OptionsPattern[]] := GenerateSymmetricKey[Automatic, opts];

GenerateSymmetricKey[keyspec_, OptionsPattern[]] := Module[{res, key, method, cipher, blockmode, keysize, iv},
	
	res = Catch[
	
		{cipher, blockmode, keysize, iv} = parseMethod[OptionValue[Method]];
			
		key = toKey[keyspec, keysize/8];
			
		SymmetricKey[<|
			"Cipher" -> cipher,
			"BlockMode" -> blockmode,
			"Key" -> key,
			"InitializationVector" -> iv
		|>]
		
	];
	
	res
];
