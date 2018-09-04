(* ::Package:: *)

(* Paclet Info File *)

(* created 2015.01.23*)

Paclet[
  Name -> "ArduinoLink",
  Version -> "1.1.0",
  MathematicaVersion -> "10+",
  Creator ->"Ian Johnson <ijohnson@wolfram.com>",
  Loading->Automatic,
  Internal->True,
  
  Extensions -> {
  	{"Kernel",
  		Root -> "Kernel",
  		Context -> 
  			{
	  			"ArduinoLink`",
	  			"ArduinoCompile`",
	  			"ArduinoUpload`",
	  			"AVRCCompiler`",
	  			"SketchTemplate`",
	  			"Firmata`"
  			}
  	},
    {"Documentation",
		MainPage->"ReferencePages/Devices/Arduino",
		Language->All
	},
    {"Resource",
    	Root->"Resources",
    	Resources->{
	    	{"Sketch","CSource/SketchTemplate.cpp"},
	    	{"Logo","Bitmap/community_logo.png"}
    	}
    }
}]
