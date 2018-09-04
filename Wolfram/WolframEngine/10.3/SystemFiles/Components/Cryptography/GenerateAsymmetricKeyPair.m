Package["Cryptography`"]

PackageExport["PublicKey"]

(*PublicKey::usage = "Represents a public key"*)

PublicKey[data_Association][key_] := data[key]


PackageExport["PrivateKey"]

(*PrivateKey::usage = "Represents private key"*)

PrivateKey[data_Association][key_] := data[key]


PackageExport["GenerateAsymmetricKeyPair"]

(*GenerateAsymmetricKeyPair::usage = "Generates a PrivateKey and PublicKey."*)

GenerateAsymmetricKeyPair::failure = "Couldn't generate a key pair."
GenerateAsymmetricKeyPair::invkeysize = "`1` is not a valid key size."
GenerateAsymmetricKeyPair::invmethod = "The value of the option Method -> `` should be a string or an association."
GenerateAsymmetricKeyPair::invpexp = "The public exponent `1` should be an odd integer greater than 3.";
GenerateAsymmetricKeyPair::invpadding = "The padding `1` should be one of ``.";


(*GenerateRSAKey[bits_Integer, publicExponent_Integer] :=
 
 Module[{p, q, modulus, totient, privateExponent},
  
  {p, q} = RandomPrime[{2^((bits/2) - 1), 2^(bits/2) - 1}, 2];
  modulus = p*q;
  totient = (p - 1)*(q - 1);
  
  While[! CoprimeQ[totient, publicExponent],
   {p, q} = RandomPrime[{2^((bits/2) - 1), 2^(bits/2) - 1}, 2];
   modulus = p*q;
   totient = (p - 1)*(q - 1);
   ];
  privateExponent = PowerMod[publicExponent, -1, totient];
  <|"PublicExponent" -> publicExponent, "PublicModulus" -> modulus, "PrivateExponent" -> privateExponent|>]*)

PackageExport["GeneratePrivateKey"]

GeneratePrivateKey[bits_Integer, publicExponent_Integer, padding_] := Module[
	{pubmod, privmod},
	{pubmod, privmod} = ToExpression[FromCharacterCode[llgenerateRSAKey[bits, publicExponent]]];
	If[!IntegerQ[pubmod] || !IntegerQ[privmod], 
		Message[GenerateAsymmetricKeyPair::failure];
		Throw[$Failed]
	];
	<|
		"Cipher" -> "RSA",
		"Padding" -> padding,
		"PublicExponent" -> publicExponent, 
		"PublicModulus" -> pubmod, 
		"PrivateExponent" -> privmod
	|>
];
  
Options[GenerateAsymmetricKeyPair] = {Method -> "RSA"}

GenerateAsymmetricKeyPair[OptionsPattern[]] := Catch @ Module[
	{method, cipher, keysize, pexponent, padding, info},
	
	method = OptionValue[Method];
	
	If[StringQ[method],
		method = <|"Cipher" -> method|>;
	,
		If[!AssociationQ[method], 
			Message[GenerateAsymmetricKeyPair::invmethod, methodI]; 
			Throw[$Failed]
		];
	];

	checkInvalidMethodKeys[GenerateAsymmetricKeyPair, method, {"Cipher", "Padding", "KeySize", "PublicExponent"}];
		
	{cipher, keysize, pexponent, padding} = Lookup[method, {"Cipher", "KeySize", "PublicExponent", "Padding"}];
	
	If[!missingQ[cipher] && cipher =!= "RSA",
		Message[GenerateAsymmetricKeyPair::invcipher, cipher]; 
		Throw[$Failed];
	];
	
	If[missingQ[keysize],
		keysize = 2048;
	,
		If[!IntegerQ[keysize] || keysize < 17 || keysize > 65536,
			Message[GenerateAsymmetricKeyPair::invkeysize, keysize]; 
			Throw[$Failed];
		];
	];
	
	If[missingQ[pexponent],
		pexponent = 65537;
	,
		If[!IntegerQ[pexponent] || EvenQ[pexponent] || pexponent < 3,
			Message[GenerateAsymmetricKeyPair::invpexp, pexponent]; 
			Throw[$Failed];
		];
	];
	
	If[missingQ[padding],
		padding = "PKCS1";
	,
		If[!MemberQ[$RSAPaddingModes, padding],
			Message[GenerateAsymmetricKeyPair::invpadding, padding, $RSAPaddingModes];
			Throw[$Failed]
		];
	];
	
	info = GeneratePrivateKey[keysize, pexponent, padding];

	<|
		"PrivateKey" -> PrivateKey[info], 
		"PublicKey" -> PublicKey[KeyTake[info, {"Cipher", "Padding", "PublicExponent", "PublicModulus"}]]
	|>
];