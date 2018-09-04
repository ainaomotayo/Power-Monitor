(* ::Package:: *)

Begin["System`Convert`HDF5Dump`"]


ImportExport`RegisterImport[
 "HDF5",
 {
 	{"DataEncoding"|"Dimensions"|"DataFormat"} :> HDF5GetMetadata,
 	{"Annotations"} :> getMetadata,
 	{"Attributes"} :> HDF5ImportAttributes[All],
 	{"Attributes", object_String} :> HDF5ImportAttributes[object],
 	"Datasets" :> HDF5ImportNames,
 	"Data" :> ImportAll,
 	{"Datasets", object_String } :> ImportObjectByName[object],
 	{"Datasets", object_Integer} :> ImportObjectByNumber[object],
 	"Groups" -> HDF5ImportGroupNames,
     HDF5ImportNames
 },
 "Sources" -> ImportExport`DefaultSources[{"HDF5","DataCommon"}],
 "AvailableElements" -> {"Annotations", "Attributes", "Data", "DataEncoding", "DataFormat", "Datasets", "Dimensions", "Groups"},
 "DefaultElement"->"Datasets",
 "BinaryFormat" -> True
]


End[]
