(* ::Package:: *)

(* Wolfram Language Package *)

(*==========================================================================================================
			
					ARDUINO COMPILE
			
Author: Ian Johnson
			
Copyright (c) 2015 Wolfram Research. All rights reserved.			


Arduino Compile is a package to take a valid C/C++ file and produce a compiled .o file for subsequent upload
to an Arduino.

CURRENT SUPPORTED BOARDS:
~Arduino Uno

USER ACCESSIBLE FUNCTIONS:
arduinoCompile

==========================================================================================================*)



BeginPackage["ArduinoCompile`",{"ArduinoLink`"}]
(* Exported symbols added here with SymbolName::usage *)  

arduinoCompile::usage="arduinoCompile takes the location c/c++ program in a text file and will compile it in this location. It returns the output from the compilation, as well as the "



Begin["`Private`"] (* Begin Private Context *) 


$thisFileDir = DirectoryName@$InputFileName

Get[FileNameJoin[{$thisFileDir,"AVRCCompiler.m"}]]

Needs[ "CCompilerDriver`"];
Needs["CCompilerDriver`CCompilerDriverBase`"];




(*==========================================================================================================
arduinoCompile will compile the c/c++ file at the given location. The directory that the file is located
in is taken to be the folder where compilation output is put. No output files in this directory are deleted.

The arduino install directory must be passed to this function, it is not the responsibility of the compiler
to find the install location.

arduinoCompile uses CreateObjectFile from CCompilerDriver and the custom AVRCCompiler file to compile the
program and libraries with the avr-gcc and avr-g++ compilers found in the Arduino software.



algorithm for compiling:

first get all the libraries in order. Need to have a list of the library locations

then use CreateObjectFile with g++ to create the output file of the SketchTemplate.cpp

then use CreateObjectFile with g++ to create the output file of the libfile.cpp for all the libraries(if necessary)

then use CreateObjectFile with gcc to create the output file of the utillibfile.c for all the libraries(if necessary)

finally, compile all these together to produce an .elf file






PARAMETERS:
	arduinoInstallLocation - the location of Arduino software
	buildLocation - the location of the temp folder to look for files and the firmware file
	fileName - the name of the file to be compiled

RETURNS: 
	association of input command to output from each command, if fully successful
	$Failed, if any part of the compilation process fails


OPTIONS:
	"Debug" - whether or not debugging info should be output
	"CleanIntermediate" - whether or not to clean up all the intermediate files, like the object files and such
	"ArduinoVersion" - the version of the arduino software to define when compiling it
	"AVRGCCLocation" - the location to find the avr-gcc utilities (setting this to default causes it to use the ones that are supposed to be in the arduino install location)
	"StandardArduinoLibrariesLocation" - the location to find all the standard arduino libraries that need to also be compiled


==========================================================================================================*)

Options[arduinoCompile]=
	{
		"Debug"->False,
		"CleanIntermediate"->True,
		"ArduinoVersion"->"16300",
		"AVRGCCLocation"->Default,
		"StandardArduinoLibrariesLocation"->Default
	};

arduinoCompile::filenotfound="Unable to import `1` as program text file";
arduinoCompile::mainfail="Compilation of the sketch failed in the first stage";
arduinoCompile::execFailed="Compilation of the sketch failed in the last stage";
arduinoCompile::cLibraryFail="Compilation of the C library `1` failed";
arduinoCompile::cppLibraryFail="Compilation of the C++ library `1` failed";
arduinoCompile::arduinoLibraryFail="Compilation of the Arduino standard library `1` failed"

arduinoCompile[arduinoInstallLocation_,buildLocation_,fileName_,OptionsPattern[]]:=Module[
	{
		(*constant strings for CCompilerDriver, avr-gcc, and avr-g++*)
		highVerbose= "-v -v -v -v",
		lowVerbose = "-v",
		OptimizeForSize = "-Os",
		allWarnings = "-Wall",
		noExceptions = "-fno-exceptions",
		functionSectioning = "-ffunction-sections",
		dataSectioning = "-fdata-sections",
		noThreading="-fno-threadsafe-statics",
		microcontrollerUnitSpec = "-mmcu=",
		arduinoUnoMicrocontrollerUnit = "atmega328p",
		clockCrystalSpeedDef = "F_CPU=",
		arduinoUnoClockCrystalSpeed ="16000000L",
		onlyHeaderFileOutput ="-MMD",
		arduinoVersionDef = "ARDUINO=",
		(*default to 16300 if the passed in option is anything other than a string*)
		arduinoVersion = If[StringQ[OptionValue["ArduinoVersion"]],OptionValue["ArduinoVersion"],"16300"],
		linkerDirectiveOnlyUsedCode = "--gc-sections",
		linkerDirectiveVerboseOutput="--verbose",
		arduinoBoard="ARDUINO_AVR_UNO",
		arduinoArchitectureAVR="ARDUINO_ARCH_AVR",
		originalPathEnvironment=Environment["PATH"],
		debugNativeOutput="-g",
		compilerLocation,
		arduinoStandardLibraryDirectories
	},
	(
		compilerLocation = If[OptionValue["AVRGCCLocation"]===Default,
			(*THEN*)
			(*use the default directory path inside the arduino install location*)
			FileNameJoin[{arduinoInstallLocation,"hardware","tools","avr","bin"}],
			(*ELSE*)
			(*a different location was specified, try and use that one instead*)
			(
				If[FileExistsQ[OptionValue["AVRGCCLocation"]],
					(*THEN*)
					(*the location specified exists, so use that one instead*)
					OptionValue["AVRGCCLocation"],
					(*ELSE*)
					(*the location specified was incorrect, so try using default*)
					FileNameJoin[{arduinoInstallLocation,"hardware","tools","avr","bin"}]
				]
			)
		];
		
		arduinoStandardLibraryDirectories = If[OptionValue["StandardArduinoLibrariesLocation"]===Default,
			(*THEN*)
			(*use the standard locations*)
			{
				FileNameJoin[{arduinoInstallLocation, "hardware", "arduino","avr", "cores","arduino"}],
				FileNameJoin[{arduinoInstallLocation,"hardware", "arduino","avr", "variants", "standard"}]
			},
			(*ELSE*)
			(*a different set of locations was specified so make sure that both locations exist*)
			If[And@@(FileExistsQ[FileNameJoin[#]]&/@OptionValue["StandardArduinoLibrariesLocation"]),
				(*THEN*)
				(*the files exists, so use those*)
				FileNameJoin/@OptionValue["StandardArduinoLibrariesLocation"],
				(*ELSE*)
				(*they don't exist, try default*)
				{
					FileNameJoin[{arduinoInstallLocation, "hardware", "arduino","avr", "cores","arduino"}],
					FileNameJoin[{arduinoInstallLocation,"hardware", "arduino","avr", "variants", "standard"}]
				}
			]
		];
		
		(*first add the compiler location to the environment path so the commands can find the cygwin dll on windows for version 1.6.0 of arduino*)
		If[$OperatingSystem === "Windows",
			SetEnvironment["PATH"->originalPathEnvironment<>";"<>arduinoInstallLocation];
		];
		
		(*reset $output for returning at the end*)
		$output=<||>;
		
		(*get a list of the libraries using libFinder function*)
		libs = libFinder[buildLocation];
		
		programText = Import[FileNameJoin[{buildLocation,fileName}],"Text"];
		
		(*if the file isn't found, issue message and return $Failed*)
		If[programText === $Failed, 
			Message[arduinoCompile::filenotfound,FileNameJoin[{buildLocation,fileName}]];
			Return[$Failed]
		];
		
		If[OptionValue["Debug"]===True,
			(
				Print["Starting compilation..."];
				$startCompileTime = AbsoluteTime[];
			)
		];

		If[ libs === {},
			(*THEN*)
			(*we don't have libraries, so we can just jump to the final compilation*)
			(
				If[OptionValue["Debug"]===True,
					(
						Print["No libraries found by arduinoCompile"];
					)
				];
				
				(*compile the main program file first*)
				CreateObjectFile[
					programText,
					fileName,
					"Compiler"->AVRCCompiler,
					"CompilerInstallation"->compilerLocation,
					"CompilerName"->"avr-g++",
					"TargetDirectory"->buildLocation,
					"CompileOptions"->CommandJoin[
						Riffle[
							{
								highVerbose,
								OptimizeForSize,
								debugNativeOutput,
								allWarnings,
								noExceptions,
								functionSectioning,
								dataSectioning,
								noThreading,
								onlyHeaderFileOutput,
								microcontrollerUnitSpec<>arduinoUnoMicrocontrollerUnit
							},
							" "]
						],
					"Defines"->
					{
						"SERIAL_RX_BUFFER_SIZE="<>"128",
						"SERIAL_TX_BUFFER_SIZE="<>"64",
						clockCrystalSpeedDef<>arduinoUnoClockCrystalSpeed,
						arduinoBoard,
						arduinoArchitectureAVR,
						arduinoVersionDef<>arduinoVersion
					},
					"LibraryDirectories" -> arduinoStandardLibraryDirectories,
					"IncludeDirectories" -> arduinoStandardLibraryDirectories,
					"ShellOutputFunction"->(compilationProgressCatcher[#,"output"]&),
					"ShellCommandFunction"->(compilationProgressCatcher[#,"command","Debug"->OptionValue["Debug"]]&),
					"CleanIntermediate"->False,
					(*SystemIncludeDirectories, SystemLibraryDirectories, and SystemLibraries are for
				  	Wolfram C Libraries, so they can be discarded*)
					"SystemIncludeDirectories" -> {}
				];
				
				(*increment the progress indiciator for the progress bar*)
				ArduinoLink`Private`$compilationProgressIndicator++;
				
				(*THE FOLLOWING IS A WORKAROUND FOR A WIERD BUG WHERE CCOMPILERDRIVER THINKS IT FAILED EVEN WHEN 
				IT SUCCESSFULLY COMPLETES SO HERE WE MANUALLY CHECK TO SEE IF IT FAILED*)
				If[fileCheck[getWolfWorkDir[buildLocation],fileName<>".o"],
					(*THEN*)
					(*the file was compiled and the output exists*)
					moveFiles[getWolfWorkDir[buildLocation]],
					(*ELSE*)
					(*the file doesn't exist, so the compilation failed*)
					(*Raise Message and throw $Failed*)
					(
						Message[arduinoCompile::mainfail];
						If[$OperatingsSystem === "Windows",
							SetEnvironment["PATH"->originalPathEnvironment];
						];
						Return[$Failed]
					)
				];
				
				(*now compile the standard arduino library files*)
				$arduinoStandardLibraryFiles=compileArduinoStandardLibraries[
					arduinoInstallLocation,
					buildLocation,
					"AVRGCCLocation"->OptionValue["AVRGCCLocation"],
					"StandardArduinoLibrariesLocation"->OptionValue["StandardArduinoLibrariesLocation"],
					"ArduinoVersion"->OptionValue["ArduinoVersion"]
				];
				
				(*check to make sure the standard arduino libraries all compiled okay*)
				If[$arduinoStandardLibraryFiles===$Failed,
					(*THEN*)
					(*the libraries failed, but a message should already have been generated, so just reset the path environment variable and exit*)
					(
						I[$OperatingSystem === "Windows",
							SetEnvironment["PATH"->originalPathEnvironment];
						];
						Return[$Failed];
					)
				];
				
				CreateExecutable[
					{FileNameJoin[{buildLocation,fileName<>".o"}]},
					fileName,
					"Compiler"->AVRCCompiler,
					"CompilerInstallation"->compilerLocation,
					"CompilerName"->"avr-gcc",
					"TargetDirectory"->buildLocation,
					"CompileOptions" ->CommandJoin[
						Riffle[
							{
								lowVerbose,
								OptimizeForSize,
								allWarnings,
								microcontrollerUnitSpec<>arduinoUnoMicrocontrollerUnit,
								"-Wl,"<>linkerDirectiveOnlyUsedCode<>","<>linkerDirectiveVerboseOutput
							},
							" "]
						],
					"ShellOutputFunction"->(compilationProgressCatcher[#,"output"]&),
					"ShellCommandFunction"->(compilationProgressCatcher[#,"command","Debug"->OptionValue["Debug"]]&),
					"CleanIntermediate" -> False,
					"LibraryDirectories"->buildLocation,
					(*SystemIncludeDirectories, SystemLibraryDirectories, and SystemLibraries are for
				  	Wolfram C Libraries, so they can be discarded*)
					"SystemIncludeDirectories" -> {},
					"SystemLibraryDirectories" -> {},
					"SystemLibraries" -> {},
					"ExtraObjectFiles"->$arduinoStandardLibraryFiles,
					"Libraries"->"m"
				];
				
				(*increment the progress indiciator for the progress bar*)
				ArduinoLink`Private`$compilationProgressIndicator++;
				
				If[fileCheck[getWolfWorkDir[buildLocation],fileName<>".elf"],
					(*THEN*)
					(*the file was compiled and the output exists*)
					moveFiles[getWolfWorkDir[buildLocation]],
					(*ELSE*)
					(*the file doesn't exist, so the compilation failed*)
					(
						(*Raise Message and return $Failed*)
						Message[arduinoCompile::execFailed];
						If[$OperatingSystem === "Windows",
							SetEnvironment["PATH"->originalPathEnvironment];
						];
						Return[$Failed]
					)
				]
			),
			(*ELSE*)
			(*we have libraries, so we need to compile those, then do the final compilation*)
			(	
				(*print off some debgging info if requested*)
				If[OptionValue["Debug"]===True,
					(
						Print["The libraries ",libs," were found by arduinoCompile"];
					)
				];
				
				
				(*compile the main program file first*)
				CreateObjectFile[
					programText,
					fileName,
					"Compiler"->AVRCCompiler,
					"CompilerInstallation"->compilerLocation,
					"CompilerName"->"avr-g++",
					"TargetDirectory"->buildLocation,
					"CompileOptions"->CommandJoin[
						Riffle[
							{
								highVerbose,
								OptimizeForSize,
								debugNativeOutput,
								allWarnings,
								noExceptions,
								functionSectioning,
								dataSectioning,
								noThreading,
								onlyHeaderFileOutput,
								microcontrollerUnitSpec<>arduinoUnoMicrocontrollerUnit
							},
							" "]
						],
					"Defines"->
					{
						"SERIAL_RX_BUFFER_SIZE="<>"128",
						"SERIAL_TX_BUFFER_SIZE="<>"64",
						clockCrystalSpeedDef<>arduinoUnoClockCrystalSpeed,
						arduinoBoard,
						arduinoArchitectureAVR,
						arduinoVersionDef<>arduinoVersion
					},
					"LibraryDirectories" -> Join[arduinoStandardLibraryDirectories,{FileNameJoin[{buildLocation,"libs"}]}],
					"IncludeDirectories" -> Join[arduinoStandardLibraryDirectories,{FileNameJoin[{buildLocation,"libs"}]}],
					"ShellOutputFunction"->(compilationProgressCatcher[#,"output"]&),
					"ShellCommandFunction"->(compilationProgressCatcher[#,"command","Debug"->OptionValue["Debug"]]&),
					"CleanIntermediate"->False,
					(*SystemIncludeDirectories, SystemLibraryDirectories, and SystemLibraries are for
				  	Wolfram C Libraries, so they can be discarded*)
		  			"SystemIncludeDirectories" -> {}
				];
				
				If[fileCheck[getWolfWorkDir[buildLocation],fileName<>".o"],
					(*THEN*)
					(*the file was compiled and the output exists*)
					moveFiles[getWolfWorkDir[buildLocation]],
					(*ELSE*)
					(*the file doesn't exist, so the compilation failed*)
					(*Raise Message and throw $Failed*)
					(
						Message[arduinoCompile::mainfail];
						If[$OperatingSystem === "Windows",
							SetEnvironment["PATH"->originalPathEnvironment];
						];
						Return[$Failed]
					)
				];
				
				(*increment the progress indiciator for the progress bar*)
				ArduinoLink`Private`$compilationProgressIndicator++;
				
				(*for each library we found compile it with CreateOjectFile, switching on which kind of library it is*)
				Do[
					(
						(*increment the progress indiciator for the progress bar*)
						ArduinoLink`Private`$compilationProgressIndicator++;
					
						If[ToLowerCase[FileExtension[lib]]==="c",
							(*THEN*)
							(*the file is a c library, so have to compile it with gcc*)
							(
								If[OptionValue["Debug"],
									Print[lib," is a c library"];
								];
								CreateObjectFile[
									{FileNameJoin[{buildLocation,"libs",lib}]},
									lib,
									"Compiler"->AVRCCompiler,
									"CompilerInstallation"->compilerLocation,
									"CompilerName"->"avr-gcc",
									"TargetDirectory"->buildLocation,
									"CompileOptions"->CommandJoin[
										Riffle[
											{
												highVerbose,
												OptimizeForSize,
												debugNativeOutput,
												allWarnings,
												functionSectioning,
												dataSectioning,
												onlyHeaderFileOutput,
												microcontrollerUnitSpec<>arduinoUnoMicrocontrollerUnit
											},
											" "]
										],
									"Defines"->
									{
										"SERIAL_RX_BUFFER_SIZE="<>"128",
										"SERIAL_TX_BUFFER_SIZE="<>"64",
										clockCrystalSpeedDef<>arduinoUnoClockCrystalSpeed,
										arduinoVersionDef<>arduinoVersion,
										arduinoBoard,
										arduinoArchitectureAVR
									},
									"LibraryDirectories" -> Join[arduinoStandardLibraryDirectories,{FileNameJoin[{buildLocation,"libs"}]}],
									"IncludeDirectories" -> Join[arduinoStandardLibraryDirectories,{FileNameJoin[{buildLocation,"libs"}]}],
									"ShellOutputFunction"->(compilationProgressCatcher[#,"output"]&),
									"ShellCommandFunction"->(compilationProgressCatcher[#,"command","Debug"->OptionValue["Debug"]]&),
									"CleanIntermediate"->False,
									(*SystemIncludeDirectories, SystemLibraryDirectories, and SystemLibraries are for
						  			Wolfram C Libraries, so they can be discarded*)
						  			"SystemIncludeDirectories" -> {}
								];
							),
							(*ELSE*)
							(*the library is a C++ library, so compile it with g++*)
							(
								If[OptionValue["Debug"],
									Print[lib," is a c++ library"];
								];
								CreateObjectFile[
									{
										FileNameJoin[{buildLocation,"libs",lib}]
									},
									lib,
									"Compiler"->AVRCCompiler,
									"CompilerInstallation"->compilerLocation,
									"CompilerName"->"avr-g++",
									"TargetDirectory"->buildLocation,
									"CompileOptions"->CommandJoin[
										Riffle[
											{
												highVerbose,
												OptimizeForSize,
												debugNativeOutput,
												allWarnings,
												noExceptions,
												functionSectioning,
												dataSectioning,
												noThreading,
												onlyHeaderFileOutput,
												microcontrollerUnitSpec<>arduinoUnoMicrocontrollerUnit
											},
											" "]
										],
									"Defines"->
									{
										"SERIAL_RX_BUFFER_SIZE="<>"128",
										"SERIAL_TX_BUFFER_SIZE="<>"64",
										clockCrystalSpeedDef<>arduinoUnoClockCrystalSpeed,
										arduinoBoard,
										arduinoArchitectureAVR,
										arduinoVersionDef<>arduinoVersion
									},
									"LibraryDirectories" -> Join[arduinoStandardLibraryDirectories,{FileNameJoin[{buildLocation,"libs"}]}],
									"IncludeDirectories" -> Join[arduinoStandardLibraryDirectories,{FileNameJoin[{buildLocation,"libs"}]}],
									"ShellOutputFunction"->(compilationProgressCatcher[#,"output"]&),
									"ShellCommandFunction"->(compilationProgressCatcher[#,"command","Debug"->OptionValue["Debug"]]&),
									"CleanIntermediate"->False,
									(*SystemIncludeDirectories, SystemLibraryDirectories, and SystemLibraries are for
						  			Wolfram C Libraries, so they can be discarded*)
						  			"SystemIncludeDirectories" -> {}
								]
							)
						];
						(*after compiling the library, check the file to see if it exists and if it does move it*)
						If[fileCheck[getWolfWorkDir[buildLocation],lib<>".o"],
							(*THEN*)
							(*the file was compiled and the output exists*)
							moveLibraryFiles[getWolfWorkDir[buildLocation],lib],
							(*ELSE*)
							(*the file doesn't exist, so the compilation failed*)
							(*Raise Message and throw $Failed*)
							(
								Message[arduinoCompile::cLibraryFail,lib];
								If[$OperatingSystem === "Windows",
									SetEnvironment["PATH"->originalPathEnvironment];
								];
								Return[$Failed];
							)
						];
						
					),
					{lib,libs}
				];
				
				(*now compile the standard libraries*)				
				$arduinoStandardLibraryFiles=compileArduinoStandardLibraries[
					arduinoInstallLocation,
					buildLocation,
					"AVRGCCLocation"->OptionValue["AVRGCCLocation"],
					"StandardArduinoLibrariesLocation"->OptionValue["StandardArduinoLibrariesLocation"],
					"ArduinoVersion"->OptionValue["ArduinoVersion"]
				];
				
				(*check the resultof the standard libraries*)
				If[$arduinoStandardLibraryFiles===$Failed,
					(*THEN*)
					(*the libraries failed, but a message should already have been generated, so just reset the path environment variable and exit*)
					(
						If[$OperatingSystem === "Windows",
							SetEnvironment["PATH"->originalPathEnvironment];
						];
						Return[$Failed];
					)
				];
				
				(*increment the progress indiciator for the progress bar*)
				ArduinoLink`Private`$compilationProgressIndicator++;
				
				(*finally compile all of the files into the .elf file*)
				
				CreateExecutable[
					Join[{FileNameJoin[{buildLocation,fileName<>".o"}]},FileNameJoin[{buildLocation,"liboutput",#<>".o"}]&/@Reverse[libs]],
					fileName,
					"Compiler"->AVRCCompiler,
					"CompilerInstallation"->compilerLocation,
					"CompilerName"->"avr-gcc",
					"TargetDirectory"->buildLocation,
					"CompileOptions" ->CommandJoin[
						Riffle[
							{
								lowVerbose,
								OptimizeForSize,
								allWarnings,
								microcontrollerUnitSpec<>arduinoUnoMicrocontrollerUnit,
								"-Wl,"<>linkerDirectiveOnlyUsedCode<>","<>linkerDirectiveVerboseOutput
							},
							" "]
						],
					"ShellOutputFunction"->(compilationProgressCatcher[#,"output"]&),
					"ShellCommandFunction"->(compilationProgressCatcher[#,"command","Debug"->OptionValue["Debug"]]&),
					"CleanIntermediate" -> False,
					"LibraryDirectories"->{buildLocation,FileNameJoin[{buildLocation,"liboutput"}]},
					(*SystemIncludeDirectories, SystemLibraryDirectories, and SystemLibraries are for
				  	Wolfram C Libraries, so they can be discarded*)
					"SystemIncludeDirectories" -> {},
					"SystemLibraryDirectories" -> {},
					"SystemLibraries" -> {},
					"ExtraObjectFiles"->$arduinoStandardLibraryFiles,
					"Libraries"->"m"
				];
				
				(*increment the progress indiciator for the progress bar*)
				ArduinoLink`Private`$compilationProgressIndicator++;
				
				If[fileCheck[getWolfWorkDir[buildLocation],fileName<>".elf"],
					(*THEN*)
					(*the file was compiled and the output exists*)
					moveFiles[getWolfWorkDir[buildLocation]],
					(*ELSE*)
					(*the file doesn't exist, so the compilation failed*)
					(*Raise Message and throw $Failed*)
					(
						Message[arduinoCompile::execFailed];
						If[$OperatingSystem === "Windows",
							SetEnvironment["PATH"->originalPathEnvironment];
						];
						Return[$Failed];
					)
				]
			)		
		];
		
		If[OptionValue["Debug"]===True,
			(
				Print["Finished compiling..."];
				Print["Took ",AbsoluteTime[] - $startCompileTime," seconds to compile"];
			)
		];
		
		(*finally reset the environment path before exiting*)
		If[$OperatingSystem === "Windows",
			SetEnvironment["PATH"->originalPathEnvironment];
		];

		Return[$output]
		
	)
];

Options[compileArduinoStandardLibraries]=
	{
		"LibraryCoreLocation"->Default,
		"AVRGCCLocation"->Default,
		"StandardArduinoLibrariesLocation"->Default,
		"ArduinoVersion"->Default
	};
(*this function will compile all of the standard arduino libraries and return a list of the filenames of the object files from all of the libraries*)
compileArduinoStandardLibraries[arduinoInstallLocation_,buildLocation_,OptionsPattern[]]:=Module[
	{
		highVerbose= CommandJoin[Table["-v ",{4}]],
		OptimizeForSize = "-Os",
		debugNativeOutput="-g",
		allWarnings = "-Wall",
		noExceptions = "-fno-exceptions",
		functionSectioning = "-ffunction-sections",
		dataSectioning = "-fdata-sections",
		noThreading="-fno-threadsafe-statics",
		microcontrollerUnitSpec = "-mmcu=",
		arduinoUnoMicrocontrollerUnit = "atmega328p",
		clockCrystalSpeedDef = "F_CPU=",
		arduinoUnoClockCrystalSpeed ="16000000L",
		onlyHeaderFileOutput ="-MMD",
		arduinoVersionDef = "ARDUINO=",
		arduinoVersion = If[StringQ[OptionValue["ArduinoVersion"]],OptionValue["ArduinoVersion"],"16300"],
		arduinoBoard="ARDUINO_AVR_UNO",
		arduinoArchitectureAVR="ARDUINO_ARCH_AVR",
		libraryCoreLocation,
		compilerLocation,
		arduinoStandardLibraryDirectories
	},
	(
	
		compilerLocation = If[OptionValue["AVRGCCLocation"]===Default,
			(*THEN*)
			(*use the default directory path inside the arduino install location*)
			FileNameJoin[{arduinoInstallLocation,"hardware","tools","avr","bin"}],
			(*ELSE*)
			(*a different location was specified, try and use that one instead*)
			(
				If[FileExistsQ[OptionValue["AVRGCCLocation"]],
					(*THEN*)
					(*the location specified exists, so use that one instead*)
					OptionValue["AVRGCCLocation"],
					(*ELSE*)
					(*the location specified was incorrect, so try using default*)
					FileNameJoin[{arduinoInstallLocation,"hardware","tools","avr","bin"}]
				]
			)
		];
		
		arduinoStandardLibraryDirectories = If[OptionValue["StandardArduinoLibrariesLocation"]===Default,
			(*THEN*)
			(*use the standard locations*)
			{
				FileNameJoin[{arduinoInstallLocation, "hardware", "arduino","avr", "cores","arduino"}],
				FileNameJoin[{arduinoInstallLocation,"hardware", "arduino","avr", "variants", "standard"}]
			},
			(*ELSE*)
			(*a different set of locations was specified so make sure that both locations exist*)
			If[And@@(FileExistsQ[FileNameJoin[#]]&/@OptionValue["StandardArduinoLibrariesLocation"]),
				(*THEN*)
				(*the files exists, so use those*)
				FileNameJoin/@OptionValue["StandardArduinoLibrariesLocation"],
				(*ELSE*)
				(*they don't exist, try default*)
				{
					FileNameJoin[{arduinoInstallLocation, "hardware", "arduino","avr", "cores","arduino"}],
					FileNameJoin[{arduinoInstallLocation,"hardware", "arduino","avr", "variants", "standard"}]
				}
			]
		];
	
		(*the files we need to compile are located in the first directory with all the standard libraries*)
		libraryCoreLocation = First[arduinoStandardLibraryDirectories];
	
		compiledLibs={};
		(*dynamically get the names of the files to compile, this is generated from the libraryCoreLocation*)
		arduinoLibraries = FileNameTake[#,-1]&/@Union[
			FileNames["*.c", libraryCoreLocation],
			FileNames["*.cpp",libraryCoreLocation]
		];
		(*compile all the files necessary*)
		(*two different cases, one with gcc, the other with g++*)
		(*SetDirectory[compileLocation];*)
		For[libNum = 1, libNum <= Length[arduinoLibraries], libNum++,
			(
				(*increment the progress indiciator for the progress bar*)
				ArduinoLink`Private`$compilationProgressIndicator++;
				lib = arduinoLibraries[[libNum]];
				If[ToUpperCase[FileExtension[lib]]==="C",
					(*THEN*)
					(*it is a c library, so compile it with gcc*)
					(
						CreateObjectFile[
							{
								FileNameJoin[{libraryCoreLocation,lib}]
							},
							lib,
							"Compiler"->AVRCCompiler,
							"CompilerInstallation"->compilerLocation,
							"CompilerName"->"avr-gcc",
							"TargetDirectory"->buildLocation,
							"CompileOptions"->CommandJoin[
								Riffle[
									{
										highVerbose,
										OptimizeForSize,
										debugNativeOutput,
										allWarnings,
										functionSectioning,
										dataSectioning,
										onlyHeaderFileOutput,
										microcontrollerUnitSpec<>arduinoUnoMicrocontrollerUnit
									},
									" "]
								],
							"Defines"->
								{
									"SERIAL_RX_BUFFER_SIZE="<>"128",
									"SERIAL_TX_BUFFER_SIZE="<>"64",
									clockCrystalSpeedDef<>arduinoUnoClockCrystalSpeed,
									arduinoVersionDef<>arduinoVersion,
									arduinoBoard,
									arduinoArchitectureAVR
								},
							"LibraryDirectories" -> arduinoStandardLibraryDirectories,
				  			"IncludeDirectories" -> arduinoStandardLibraryDirectories,
					  		"ShellOutputFunction"->(compilationProgressCatcher[#,"output"]&),
							"ShellCommandFunction"->(compilationProgressCatcher[#,"command","Debug"->False]&),
							"CleanIntermediate"->False,
							(*SystemIncludeDirectories, SystemLibraryDirectories, and SystemLibraries are for
						  	Wolfram C Libraries, so they can be discarded*)
				  			"SystemIncludeDirectories" -> {}
						]
					),
					(*ELSE*)
					(*it is a c++ library, so compile it with g++*)
					(
						CreateObjectFile[
							{
								FileNameJoin[{libraryCoreLocation,lib}]
							},
							lib,
							"Compiler"->AVRCCompiler,
							"CompilerInstallation"->compilerLocation,
							"CompilerName"->"avr-g++",
							"TargetDirectory"->buildLocation,
							"CompileOptions"->CommandJoin[
								Riffle[
									{
										highVerbose,
										OptimizeForSize,
										debugNativeOutput,
										allWarnings,
										noExceptions,
										functionSectioning,
										dataSectioning,
										noThreading,
										onlyHeaderFileOutput,
										microcontrollerUnitSpec<>arduinoUnoMicrocontrollerUnit
									},
									" "]
								],
							"Defines"->
									{
										"SERIAL_RX_BUFFER_SIZE="<>"128",
										"SERIAL_TX_BUFFER_SIZE="<>"64",
										clockCrystalSpeedDef<>arduinoUnoClockCrystalSpeed,
										arduinoVersionDef<>arduinoVersion,
										arduinoBoard,
										arduinoArchitectureAVR
									},
							"LibraryDirectories" -> arduinoStandardLibraryDirectories,
				  			"IncludeDirectories" -> arduinoStandardLibraryDirectories,
				  			"ShellOutputFunction"->(compilationProgressCatcher[#,"output"]&),
							"ShellCommandFunction"->(compilationProgressCatcher[#,"command","Debug"->False]&),
							"CleanIntermediate"->False,
							(*SystemIncludeDirectories, SystemLibraryDirectories, and SystemLibraries are for
						  	Wolfram C Libraries, so they can be discarded*)
				  			"SystemIncludeDirectories" -> {}
						]
					)
				];
				(*THE FOLLOWING IS A WORKAROUND FOR A WIERD BUG WHERE CCOMPILERDRIVER THINKS IT FAILED EVEN WHEN 
				IT SUCCESSFULLY COMPLETES SO HERE WE MANUALLY CHECK TO SEE IF IT FAILED*)
				If[fileCheck[getWolfWorkDir[buildLocation],lib<>".o"],
					(*THEN*)
					(*the file was compiled and the output exists*)
					(
						moveFiles[getWolfWorkDir[buildLocation]];
						AppendTo[compiledLibs,FileNameJoin[{buildLocation,lib<>".o"}]]
					),
					(*ELSE*)
					(*the file doesn't exist, so the compilation failed*)
					(*Raise Message and throw $Failed*)
					(
						Message[arduinoCompile::arduinoLibraryFail,lib];
						Return[$Failed];
					)
				];
			)
		];
		Return[compiledLibs];
	)
]



(*cLibFinder will take as an argument the folder to search, and will return a list of all of the files with .cpp or .h extensions*)
libFinder[libFolder_]:=Module[{},
	(
		files=FileNames[{"*.c","*.cpp"},FileNameJoin[{libFolder,"libs"}]];
		Return[Last[FileNameSplit[#]]&/@files]
	)
];


fileCheck[locationToCheck_,fileToCheckFor_]:=Module[
	{
		dirFiles = (Last@FileNameSplit[#])&/@FileNames["*",locationToCheck]
	},
	(
		MemberQ[dirFiles,fileToCheckFor]
	)
]



(*go and get the actual folder name from CCompilerDriverBase for this*)
getWolfWorkDir[location_]:=FileNameJoin[{location,
	StringJoin["Working-", 
		$MachineName, "-", 
		ToString[$ProcessID], "-",
		ToString[Developer`ThreadID[]], "-",
		ToString[CCompilerDriver`CCompilerDriverBase`Private`$WorkingDirCount]]}];



(*first copy all of the files to the normal working directory, then delete the directory passed*)
moveFiles[wolfFolder_]:=Module[{buildLocation = FileNameJoin@Most@FileNameSplit@wolfFolder},
	(
		Quiet[CopyFile[#,FileNameJoin[{buildLocation,Last@FileNameSplit@#}]] & /@
				(Select[(FileExtension[#] != "bat")&]@FileNames["*", wolfFolder])];
		DeleteDirectory[wolfFolder,"DeleteContents"->True];
	)
];


(*does just about the same thing as the regular move Files, but this will be smart about handling the 
libraries by copying the files into the correct library*)
moveLibraryFiles[wolfFolder_,libName_]:=Module[{buildLocation = FileNameJoin@Most@FileNameSplit@wolfFolder},
	(
		Quiet[CopyFile[#, FileNameJoin[{buildLocation,"liboutput",Last@FileNameSplit@#}]] & /@
				(Select[(FileExtension[#] != "bat")&]@FileNames["*", wolfFolder])];
		DeleteDirectory[wolfFolder,"DeleteContents"->True];
	)
];



(*set $output to be empty initially, then add to it rules of the form input -> output as they arrive*)
$output= <||>;
$prevCommand = "";
Options[compilationProgressCatcher]={"Debug"->False}
compilationProgressCatcher[string_,type_,OptionsPattern[]]:=Module[{},
	(
		If[ToLowerCase[type]==="command",
			(*THEN*)
			(*the string is a command, so set the $prevCommand to it, and if debugging is on, print it off*)
			(
				$prevCommand = string;
				If[TrueQ[OptionValue["Debug"]],
					(*THEN*)
					(*print off the command*)
					Print["Compiling with command :\n",string];
				]
			),
			(*ELSE*)
			(*the string is the output of the previous command stored in $prevCommand, so add the pair to the output*)
			(*check to make sure it is output though*)
			(
				If[ToLowerCase[type]==="output",
					(*THEN*)
					(*it is good, add it to the $output*)
					(*Print["Ouput is: \n",string];*)
					AppendTo[$output,$prevCommand->string];
					(*ELSE*)
					(*don't do anything*)
				]
			)
		]
	)
];
echo = (Print[#];#)&

(*compilationProgressCatcher=Print;*)



End[] (* End Private Context *)

EndPackage[]
