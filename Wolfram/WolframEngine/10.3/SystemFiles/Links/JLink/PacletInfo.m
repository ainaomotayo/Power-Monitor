Paclet[
	Name->"JLink",
	Version->"4.9.0",
	Extensions->{
	    {"Kernel", Context -> "JLink`"},
		(* This extension allows paclet:JLink to resolve to the root guide page. *)
		{"Documentation", Language->All, Resources->{"Guides/JavaInterface"}},
		(* This extension explicitly lists all symbol pages, which is necessary to allow
		   lookups like paclet:LoadJavaClass to suceeed (i.e., finding a paclet's symbol
		   from just the symbol name without the paclet's LinkBase).
		*)
		{"Documentation", Language->"English", Resources->{
			"ReferencePages/Symbols/$JavaExceptionHandler",
			"ReferencePages/Symbols/$RelaxedTypeChecking",
			"ReferencePages/Symbols/AddPeriodical",
			"ReferencePages/Symbols/AddToClassPath",
			"ReferencePages/Symbols/AllowRaggedArrays",
			"ReferencePages/Symbols/AppletViewer",
			"ReferencePages/Symbols/BeginJavaBlock",
			"ReferencePages/Symbols/ClassName",
			"ReferencePages/Symbols/Constructors",
			"ReferencePages/Symbols/DoModal",
			"ReferencePages/Symbols/EndJavaBlock",
			"ReferencePages/Symbols/EndModal",
			"ReferencePages/Symbols/Fields",
			"ReferencePages/Symbols/GetClass",
			"ReferencePages/Symbols/GetComplexClass",
			"ReferencePages/Symbols/GetJavaException",
			"ReferencePages/Symbols/ImplementJavaInterface",
			"ReferencePages/Symbols/InstallJava",
			"ReferencePages/Symbols/InstanceOf",
			"ReferencePages/Symbols/JavaBlock",
			"ReferencePages/Symbols/JavaClass",
			"ReferencePages/Symbols/JavaClassPath",
			"ReferencePages/Symbols/JavaLink",
			"ReferencePages/Symbols/JavaNew",
			"ReferencePages/Symbols/JavaObject",
			"ReferencePages/Symbols/JavaObjectQ",
			"ReferencePages/Symbols/JavaObjectToExpression",
			"ReferencePages/Symbols/JavaShow",
			"ReferencePages/Symbols/JavaThrow",
			"ReferencePages/Symbols/JavaUILink",
			"ReferencePages/Symbols/KeepJavaObject",
			"ReferencePages/Symbols/LoadJavaClass",
			"ReferencePages/Symbols/MakeJavaExpr",
			"ReferencePages/Symbols/MakeJavaObject",
			"ReferencePages/Symbols/Methods",
			"ReferencePages/Symbols/ParentClass",
			"ReferencePages/Symbols/ReinstallJava",
			"ReferencePages/Symbols/ReleaseJavaObject",
			"ReferencePages/Symbols/ReturnAsJavaObject",
			"ReferencePages/Symbols/SameObjectQ",
			"ReferencePages/Symbols/SetComplexClass",
			"ReferencePages/Symbols/SetInternetProxy",
			"ReferencePages/Symbols/ShowJavaConsole",
			"ReferencePages/Symbols/UninstallJava"
			}
		},
		{"Documentation", Language->"Japanese", Resources->{
			"ReferencePages/Symbols/$JavaExceptionHandler",
			"ReferencePages/Symbols/$RelaxedTypeChecking",
			"ReferencePages/Symbols/AddPeriodical",
			"ReferencePages/Symbols/AddToClassPath",
			"ReferencePages/Symbols/AllowRaggedArrays",
			"ReferencePages/Symbols/AppletViewer",
			"ReferencePages/Symbols/BeginJavaBlock",
			"ReferencePages/Symbols/ClassName",
			"ReferencePages/Symbols/Constructors",
			"ReferencePages/Symbols/DoModal",
			"ReferencePages/Symbols/EndJavaBlock",
			"ReferencePages/Symbols/EndModal",
			"ReferencePages/Symbols/Fields",
			"ReferencePages/Symbols/GetClass",
			"ReferencePages/Symbols/GetComplexClass",
			"ReferencePages/Symbols/GetJavaException",
			"ReferencePages/Symbols/ImplementJavaInterface",
			"ReferencePages/Symbols/InstallJava",
			"ReferencePages/Symbols/InstanceOf",
			"ReferencePages/Symbols/JavaBlock",
			"ReferencePages/Symbols/JavaClass",
			"ReferencePages/Symbols/JavaClassPath",
			"ReferencePages/Symbols/JavaLink",
			"ReferencePages/Symbols/JavaNew",
			"ReferencePages/Symbols/JavaObject",
			"ReferencePages/Symbols/JavaObjectQ",
			"ReferencePages/Symbols/JavaObjectToExpression",
			"ReferencePages/Symbols/JavaShow",
			"ReferencePages/Symbols/JavaThrow",
			"ReferencePages/Symbols/JavaUILink",
			"ReferencePages/Symbols/KeepJavaObject",
			"ReferencePages/Symbols/LoadJavaClass",
			"ReferencePages/Symbols/MakeJavaExpr",
			"ReferencePages/Symbols/MakeJavaObject",
			"ReferencePages/Symbols/Methods",
			"ReferencePages/Symbols/ParentClass",
			"ReferencePages/Symbols/ReinstallJava",
			"ReferencePages/Symbols/ReleaseJavaObject",
			"ReferencePages/Symbols/ReturnAsJavaObject",
			"ReferencePages/Symbols/SameObjectQ",
			"ReferencePages/Symbols/SetComplexClass",
			"ReferencePages/Symbols/SetInternetProxy",
			"ReferencePages/Symbols/ShowJavaConsole",
			"ReferencePages/Symbols/UninstallJava"
			}
		},
				{"Documentation", Language->"ChineseSimplified", Resources->{
			"ReferencePages/Symbols/$JavaExceptionHandler",
			"ReferencePages/Symbols/$RelaxedTypeChecking",
			"ReferencePages/Symbols/AddPeriodical",
			"ReferencePages/Symbols/AddToClassPath",
			"ReferencePages/Symbols/AllowRaggedArrays",
			"ReferencePages/Symbols/AppletViewer",
			"ReferencePages/Symbols/BeginJavaBlock",
			"ReferencePages/Symbols/ClassName",
			"ReferencePages/Symbols/Constructors",
			"ReferencePages/Symbols/DoModal",
			"ReferencePages/Symbols/EndJavaBlock",
			"ReferencePages/Symbols/EndModal",
			"ReferencePages/Symbols/Fields",
			"ReferencePages/Symbols/GetClass",
			"ReferencePages/Symbols/GetComplexClass",
			"ReferencePages/Symbols/GetJavaException",
			"ReferencePages/Symbols/ImplementJavaInterface",
			"ReferencePages/Symbols/InstallJava",
			"ReferencePages/Symbols/InstanceOf",
			"ReferencePages/Symbols/JavaBlock",
			"ReferencePages/Symbols/JavaClass",
			"ReferencePages/Symbols/JavaClassPath",
			"ReferencePages/Symbols/JavaLink",
			"ReferencePages/Symbols/JavaNew",
			"ReferencePages/Symbols/JavaObject",
			"ReferencePages/Symbols/JavaObjectQ",
			"ReferencePages/Symbols/JavaObjectToExpression",
			"ReferencePages/Symbols/JavaShow",
			"ReferencePages/Symbols/JavaThrow",
			"ReferencePages/Symbols/JavaUILink",
			"ReferencePages/Symbols/KeepJavaObject",
			"ReferencePages/Symbols/LoadJavaClass",
			"ReferencePages/Symbols/MakeJavaExpr",
			"ReferencePages/Symbols/MakeJavaObject",
			"ReferencePages/Symbols/Methods",
			"ReferencePages/Symbols/ParentClass",
			"ReferencePages/Symbols/ReinstallJava",
			"ReferencePages/Symbols/ReleaseJavaObject",
			"ReferencePages/Symbols/ReturnAsJavaObject",
			"ReferencePages/Symbols/SameObjectQ",
			"ReferencePages/Symbols/SetComplexClass",
			"ReferencePages/Symbols/SetInternetProxy",
			"ReferencePages/Symbols/ShowJavaConsole",
			"ReferencePages/Symbols/UninstallJava"
			}
		},
        {"Documentation", Resources->{
            {"ReferencePages/Java/BracketMatcher", "JavaDoc/com/wolfram/jlink/ui/BracketMatcher.html"},
            {"ReferencePages/Java/ConsoleWindow", "JavaDoc/com/wolfram/jlink/ui/ConsoleWindow.html"},
            {"ReferencePages/Java/Expr", "JavaDoc/com/wolfram/jlink/Expr.html"},
            {"ReferencePages/Java/ExprFormatException", "JavaDoc/com/wolfram/jlink/ExprFormatException.html"},
            {"ReferencePages/Java/Install", "JavaDoc/com/wolfram/jlink/Install.html"},
            {"ReferencePages/Java/InterruptDialog", "JavaDoc/com/wolfram/jlink/ui/InterruptDialog.html"},
            {"ReferencePages/Java/JLinkClassLoader", "JavaDoc/com/wolfram/jlink/JLinkClassLoader.html"},
            {"ReferencePages/Java/KernelLink", "JavaDoc/com/wolfram/jlink/KernelLink.html"},
            {"ReferencePages/Java/LoopbackLink", "JavaDoc/com/wolfram/jlink/LoopbackLink.html"},
            {"ReferencePages/Java/MathActionListener", "JavaDoc/com/wolfram/jlink/MathActionListener.html"},
            {"ReferencePages/Java/MathAdjustmentListener", "JavaDoc/com/wolfram/jlink/MathAdjustmentListener.html"},
            {"ReferencePages/Java/MathCanvas", "JavaDoc/com/wolfram/jlink/MathCanvas.html"},
            {"ReferencePages/Java/MathComponentListener", "JavaDoc/com/wolfram/jlink/MathComponentListener.html"},
            {"ReferencePages/Java/MathContainerListener", "JavaDoc/com/wolfram/jlink/MathContainerListener.html"},
            {"ReferencePages/Java/MathFocusListener", "JavaDoc/com/wolfram/jlink/MathFocusListener.html"},
            {"ReferencePages/Java/MathFrame", "JavaDoc/com/wolfram/jlink/MathFrame.html"},
            {"ReferencePages/Java/MathGraphicsJPanel", "JavaDoc/com/wolfram/jlink/MathGraphicsJPanel.html"},
            {"ReferencePages/Java/MathItemListener", "JavaDoc/com/wolfram/jlink/MathItemListener.html"},
            {"ReferencePages/Java/MathJFrame", "JavaDoc/com/wolfram/jlink/MathJFrame.html"},
            {"ReferencePages/Java/MathKeyListener", "JavaDoc/com/wolfram/jlink/MathKeyListener.html"},
            {"ReferencePages/Java/MathLink", "JavaDoc/com/wolfram/jlink/MathLink.html"},
            {"ReferencePages/Java/MathLinkException", "JavaDoc/com/wolfram/jlink/MathLinkException.html"},
            {"ReferencePages/Java/MathLinkFactory", "JavaDoc/com/wolfram/jlink/MathLinkFactory.html"},
            {"ReferencePages/Java/MathListener", "JavaDoc/com/wolfram/jlink/MathListener.html"},
            {"ReferencePages/Java/MathMouseListener", "JavaDoc/com/wolfram/jlink/MathMouseListener.html"},
            {"ReferencePages/Java/MathMouseMotionListener", "JavaDoc/com/wolfram/jlink/MathMouseMotionListener.html"},
            {"ReferencePages/Java/MathPropertyChangeListener", "JavaDoc/com/wolfram/jlink/MathPropertyChangeListener.html"},
            {"ReferencePages/Java/MathSessionPane", "JavaDoc/com/wolfram/jlink/ui/MathSessionPane.html"},
            {"ReferencePages/Java/MathTextListener", "JavaDoc/com/wolfram/jlink/MathTextListener.html"},
            {"ReferencePages/Java/MathVetoableChangeListener", "JavaDoc/com/wolfram/jlink/MathVetoableChangeListener.html"},
            {"ReferencePages/Java/MathWindowListener", "JavaDoc/com/wolfram/jlink/MathWindowListener.html"},
            {"ReferencePages/Java/MLFunction", "JavaDoc/com/wolfram/jlink/MLFunction.html"},
            {"ReferencePages/Java/PacketArrivedEvent", "JavaDoc/com/wolfram/jlink/PacketArrivedEvent.html"},
            {"ReferencePages/Java/PacketListener", "JavaDoc/com/wolfram/jlink/PacketListener.html"},
            {"ReferencePages/Java/PacketPrinter", "JavaDoc/com/wolfram/jlink/PacketPrinter.html"},
            {"ReferencePages/Java/StdLink", "JavaDoc/com/wolfram/jlink/StdLink.html"},
            {"ReferencePages/Java/SyntaxTokenizer", "JavaDoc/com/wolfram/jlink/ui/SyntaxTokenizer.html"},
            {"ReferencePages/Java/LinkSnooper", "JavaDoc/com/wolfram/jlink/util/LinkSnooper.html"},
            {"ReferencePages/Java/MathematicaTask", "JavaDoc/com/wolfram/jlink/util/MathematicaTask.html"},
            {"ExamplePages/Part1/BouncingBalls", "../Examples/Part1/BouncingBalls.nb"},
            {"ExamplePages/Part1/FileChooserDialog", "../Examples/Part1/FileChooserDialog.nb"},
            {"ExamplePages/Part1/GetURL", "../Examples/Part1/GetURL.nb"},
            {"ExamplePages/Part1/ModalInputDialog", "../Examples/Part1/ModalInputDialog.nb"},
            {"ExamplePages/Part1/Palette", "../Examples/Part1/Palette.nb"},
            {"ExamplePages/Part1/Piano", "../Examples/Part1/Piano.nb"},
            {"ExamplePages/Part1/ProgressBar", "../Examples/Part1/ProgressBar.nb"},
            {"ExamplePages/Part1/RealTimeAlgebra", "../Examples/Part1/RealTimeAlgebra.nb"},
            {"ExamplePages/Part1/RealTimePlotting", "../Examples/Part1/RealTimePlotting.nb"},
            {"ExamplePages/Part1/SimpleModal", "../Examples/Part1/SimpleModal.nb"},
            {"ExamplePages/Part1/Spirograph", "../Examples/Part1/Spirograph.nb"},
            {"ExamplePages/Part1/Scribble", "../Examples/Part1/Scribble.nb"}
            }
        }
	}
]