Paclet[
	Name -> "GeneralUtilities",
	Version -> "10.2.2",
	MathematicaVersion -> "10.2+",
	Description -> "General utilities",
	Loading -> Automatic,
	Extensions -> {
		{"Kernel", Context -> {"GeneralUtilitiesLoader`", "GeneralUtilities`"}, Symbols -> {
			"System`DeleteMissing",
			"System`DisjointQ", 
			"System`IntersectingQ", 
			"System`SubsetQ",
			"System`Failure",
			"System`PowerRange",
			"System`CountDistinct",
			"System`CountDistinctBy",
			"System`DeleteDuplicatesBy",
			"System`TextString",
			"System`AssociationMap",
			"System`InsertLinebreaks",
			"System`StringStartsQ",
			"System`StringEndsQ",
			"System`StringContainsQ",
			"System`StringDelete",
			"System`StringPadLeft",
			"System`StringPadRight",
			"System`StringExtract",
			"System`Capitalize",
			"System`Decapitalize",
			"System`StringRepeat",
			"System`StringRiffle",
			"System`StringPartition",
			"System`PrintableASCIIQ",
			(* Options for TextString -- I wish these didn't have autoload stubs, but there is no
			other way of getting PacletManager to make these system symbols *)
			"System`AssociationFormat","System`ListFormat",
			"System`BooleanStrings", "System`MissingString",
			"System`TimeFormat", "System`ElidedForms" 
		}}
	}
]
