Package["Cryptography`"]

Encrypt::libopenssl = "Couldn't load OpenSSL library."

SetAttributes[checkLibraryLoad, HoldAll];
checkedLibraryFunctionLoad[args___] := 
	Replace[
		Quiet[LibraryFunctionLoad[args]], 
		$Failed :> (Message[Encrypt::libopenssl]; Throw[$Failed])
	];

SetAttributes[checkedLibraryLoad, HoldAll];
checkedLibraryLoad[args___] := 
	Replace[
		Quiet[LibraryLoad[args]], 
		$Failed :> (Message[Encrypt::libopenssl]; Throw[$Failed])
	];

libraryPath := libraryPath = (
	If[$OperatingSystem === "Windows",
		checkedLibraryLoad["libeay32"];
		checkedLibraryLoad["ssleay32"];
	,
		checkedLibraryLoad["libcrypto"];
		checkedLibraryLoad["libssl"];
	];
	ToFileName[ PacletManager`PacletResource["Cryptography", "Libraries"], "OpenSSLLink" <> ToString[$SystemWordLength] ]
);

llscrypt := llscrypt = checkedLibraryFunctionLoad[libraryPath, "scrypt", {{Integer, 1}, {Integer, 1}, Integer, 
  Integer, Integer, Integer}, {Integer, 1}];
  
PackageScope["scrypt"]

scrypt[password:{__Integer}, salt:{__Integer}, outputLength_Integer, N_Integer:8192, r_Integer:8, p_Integer:16] :=
	llscrypt[password, salt, N, r, p, outputLength]
	
PackageScope["randomBytes"]

llrandomBytes := llrandomBytes = checkedLibraryFunctionLoad[libraryPath, "randomBytes", {Integer}, {Integer, 1}];

randomBytes[length_Integer] := ByteArray[llrandomBytes[length]]

PackageScope["llgenerateRSAKey"]
	
llgenerateRSAKey := llgenerateRSAKey = checkedLibraryFunctionLoad[libraryPath, "generateRSAKey", {Integer, Integer}, {Integer, 1}];

PackageScope["llrsaPublic"]

llrsaPublic := llrsaPublic = 
  checkedLibraryFunctionLoad[libraryPath, 
   "rsaPublic", {{Integer, 1}, {Integer, 1}, {Integer, 1}, Integer, 
    Integer}, {Integer, 1}];


PackageScope["llrsaPrivate"]

llrsaPrivate := llrsaPrivate =  
  checkedLibraryFunctionLoad[libraryPath, 
   "rsaPrivate", {{Integer, 1}, {Integer, 1}, {Integer, 1}, {Integer, 
     1}, Integer, Integer}, {Integer, 1}];


PackageScope["llencryptSym"]
llencryptSym := llencryptSym = 
  checkedLibraryFunctionLoad[libraryPath, 
   "encryptSym", {Integer, {Integer, 1}, {Integer, 1}, {Integer, 1}, 
    Integer}, {Integer, 1}];


PackageExport["$SymmetricCiphers"]

$SymmetricCiphers = {"Blowfish", "CAST5", "DES", "RC4", "IDEA", "AES128", "AES192", "AES256"};


PackageExport["$BlockModes"]

$BlockModes = {"ECB", "CBC", "CFB", "OFB"};


PackageExport["$SymmetricCipherNumbering"]

$SymmetricCipherNumbering = 
<|"Blowfish" -> <|"ECB" -> 0, "CBC" -> 1, "CFB" -> 2, "OFB" -> 3|>, 
 "CAST5" -> <|"ECB" -> 4, "CBC" -> 5, "CFB" -> 6, "OFB" -> 7|>, 
 "DES" -> <|"ECB" -> 8, "CBC" -> 9, "CFB" -> 10, "OFB" -> 11|>, 
 "RC4" -> <|None -> 12, "None" -> 12|>, 
 "IDEA" -> <|"ECB" -> 13, "CBC" -> 14, "CFB" -> 15, "OFB" -> 16|>, 
 "AES128" -> <|"ECB" -> 17, "CBC" -> 18, "CFB" -> 19, "OFB" -> 20|>, 
 "AES192" -> <|"ECB" -> 21, "CBC" -> 22, "CFB" -> 23, "OFB" -> 24|>, 
 "AES256" -> <|"ECB" -> 25, "CBC" -> 26, "CFB" -> 27, "OFB" -> 28|>|>;


PackageExport["CipherRequiresIVQ"]

CipherRequiresIVQ["RC4", _] = False;
CipherRequiresIVQ[_, "ECB"] = False;
CipherRequiresIVQ[_, _] = True;


PackageExport["$VariableSizeSymmetricKeys"]

$VariableSizeSymmetricKeys = {"Blowfish", "RC4", "CAST5"};


PackageExport["ValidKeySizeQ"]

ValidKeySizeQ[size_Integer, cipher_String] := 
	If[MemberQ[$VariableSizeSymmetricKeys, cipher],
		Mod[size, 8] == 0,
		$SymmetricKeySizes[cipher] / 8 === size
	];


PackageExport["ValidBlockModeQ"]

ValidBlockModeQ[block_, "RC4"] := block === None;
ValidBlockModeQ[None, cipher_] := cipher === "RC4";
ValidBlockModeQ[block_, _] := MemberQ[$BlockModes, block];

PackageExport["$SymmetricKeySizes"]

$SymmetricKeySizes = 
<|"Blowfish" -> 256, "CAST5" -> 256, "DES" -> 64, "RC4" -> 256, 
 "IDEA" -> 128, "AES128" -> 128, "AES192" -> 192, "AES256" -> 256|>


PackageExport["$SymmetricIVSizes"]

$SymmetricIVSizes =
<|"Blowfish" -> 64, "CAST5" -> 64, "DES" -> 64, "IDEA" -> 64, 
 "AES128" -> 128, "AES192" -> 128, "AES256" -> 128, "RC4" -> 0|>
 

PackageExport["$RSAPaddingModeNumbering"]

$RSAPaddingModeNumbering = <|
	"PKCS1" -> 1,
	"SSLV23" -> 2,
	None -> 3,
	"None" -> 3,
	"OAEP" -> 4
|>;

PackageExport["$RSAPaddingModes"]

$RSAPaddingModes = Keys[$RSAPaddingModeNumbering];

