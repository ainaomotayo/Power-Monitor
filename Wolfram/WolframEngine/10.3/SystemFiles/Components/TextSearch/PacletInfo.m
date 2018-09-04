Paclet[
    Name -> "TextSearch",
    Version -> "1.0.0",
    MathematicaVersion -> "10.2+",
    Description -> "Text Search",
    Loading -> Automatic,
    Extensions -> {
        {"Resource",
            Resources -> {{"javalibs", "Resources/JavaLibs"}}
        },
        {"Resource",
            Resources -> {{"lucene48", "Resources/Lucene4.8"}}
        },
        {"Resource",
            Resources -> {{"lucene35", "Resources/Lucene3.5"}}
        },
        {"Resource", 
            Resources -> {{"ExampleData", "ExampleData/Text"}}
        },
        {"Resource",
            SystemID->"MacOSX-x86-64",
            Resources -> {{"binary", "Binaries/MacOSX-x86-64/TextSearch"}}
        },
        {"Resource",
            SystemID->"Windows",
            Resources -> {{"binary", "Binaries/Windows/TextSearch.exe"}}
        },
        {"Resource",
            SystemID->"Windows-x86-64",
            Resources -> {{"binary", "Binaries/Windows-x86-64/TextSearch.exe"}}
        },
        {"Resource",
            SystemID->"Linux", 
            Resources -> {{"binary", "Binaries/Linux/TextSearch"}}
        },
        {"Resource",
            SystemID->"Linux-x86-64",
            Resources -> {{"binary", "Binaries/Linux-x86-64/TextSearch"}}
        },  

        {"Resource",
            SystemID->"MacOSX-x86-64",
            Resources -> {{"libraryDir", "Binaries/MacOSX-x86-64"}}
        },
        {"Resource",
            SystemID->"Windows",
            Resources -> {{"libraryDir", "Binaries/Windows"}}
        },
        {"Resource",
            SystemID->"Windows-x86-64",
            Resources -> {{"libraryDir", "Binaries/Windows-x86-64"}}
        },
        {"Resource",
            SystemID->"Linux", 
            Resources -> {{"libraryDir", "Binaries/Linux"}}
        },
        {"Resource",
            SystemID->"Linux-x86-64",
            Resources -> {{"libraryDir", "Binaries/Linux-x86-64"}}
        },  
        {"Resource",
            SystemID->"Windows",
            Resources -> {{"ag", "Binaries/Windows/ag.exe"}}
        }, 
        {"Resource",
            SystemID->"Windows-x86-64",
            Resources -> {{"ag", "Binaries/Windows-x86-64/ag.exe"}}
        }, 
        {"Resource",
            SystemID->"MacOSX-x86-64",
            Resources -> {{"ag", "Binaries/MacOSX-x86-64/ag"}}
        }, 
        {"Resource",
            SystemID->"Linux",
            Resources -> {{"ag", "Binaries/Linux/ag"}}
        },
        {"Resource",
            SystemID->"Linux-x86-64",
            Resources -> {{"ag", "Binaries/Linux-x86-64/ag"}}
        },
        {"Path", 
    		Base -> "ExampleData",
    		Root -> "ExampleData"
    	},
        {"Kernel", Context -> {"TextSearchLoader`", "TextSearch`"}, Symbols -> {
            "System`Autocomplete",
            "System`AutocompletionFunction",
            "System`ContentObject",
            "System`CreateSearchIndex",
            "System`DeleteSearchIndex",
            "System`SearchIndexObject",
            "System`SearchIndices",
            "System`Snippet",
            "System`TextSearch",
            "System`TextSearchReport",
            "System`UpdateSearchIndex"
        }}
    }
]
