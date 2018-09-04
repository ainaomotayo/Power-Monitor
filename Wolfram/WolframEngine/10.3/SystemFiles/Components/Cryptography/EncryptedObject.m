Package["Cryptography`"]


PackageExport["EncryptedObject"]

(*EncryptedObject::usage = "Represents a piece of encrypted information."*)

EncryptedObject[data_Association][key_] := data[key]
EncryptedObject /: Normal[enc_EncryptedObject] := enc["Data"]
