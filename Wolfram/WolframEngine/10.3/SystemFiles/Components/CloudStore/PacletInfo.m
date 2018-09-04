Paclet[
	Name -> "CloudStore",
	Version -> "0.1.0",
	MathematicaVersion -> "10+",
	Description -> "Efficiently store expressions in the Wolfram Cloud",
	Loading -> Automatic,
	Extensions -> {
		{"Kernel", Context -> {"CloudStoreLoader`", "CloudStore`"}, Symbols -> {
		"System`CloudStore",
		"System`CreateCloudStore",
		"System`DeleteCloudStore",
		"System`CloudStores"
		}
		}
	}
]
