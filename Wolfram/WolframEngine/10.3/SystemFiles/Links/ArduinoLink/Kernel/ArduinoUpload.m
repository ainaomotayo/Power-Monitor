(* ::Package:: *)

(* Wolfram Language Package *)

(*=========================================================================================================
===========================================================================================================
================ ARDUINO UPLOAD ===========================================================================
===========================================================================================================
===========================================================================================================

Author: Ian Johnson

Version: 1.00

Copyright (c) 2015 Wolfram Research. All rights reserved.			



Arduino Upload is a package to upload and compile custom C/C++ code to an Arduino.
It will take as input the options for the sketch, including libraries to include
and functions to define. 

Arduino Upload will first use the package SketchTemplate to create the program 
file and then use ArduinoCompile to actually compile the program. After this compilation,
Arduino Upload will convert the file into a .hex file, and use the AVR utility, AVRDUDE
to physically deploy the .hex sketch file to the Arduino. 

Arduino Upload will by default delete all directories it outputs and will also by default
not output any debugging information. Debugging information can be accessed by setting
the option "Debug" to True. This will number one print off useful information about the 
process and the values of relevant variables etc., as well as produce a nicely formatted
message dialog box with all of the output from the compilation process, as well as another
message dialog with just the raw program c/c++ file in it. Eventually, it is hoped to combine
all of this information into one standard interface that is launched and dynamically updated.

Arduino Upload also will reset the Directory stack to whatever it was when the function was
called.

Arduino Upload also provides a function to upload a .hex file that has already been compiled,
in which case no files or directories are created.

Arduino Upload functions also have the option for Flash Verification if the user wants to
upload their function quickly and not verify that it was actually uploaded. This is not
advised, but may save a couple seconds. Typical uploads, including compilation, take about
20-30 seconds.


CURRENT SUPPORTED BOARDS:
~Arduino Uno

USER ACCESSIBLE FUNCTIONS:
~arduinoIndividualUpload
~arduinoUpload


=========================================================================================================*)

BeginPackage["ArduinoUpload`"]
(* Exported symbols added here with SymbolName::usage *)  

arduinoIndividualUpload::usage="uploads the specified file to the arduino located on the serial port";
arduinoUpload::usage="creates, compiles, and uploads the standard sketch with the given options to the arduino located on the serial port"
arduinoReset::usage="issues a reset command to the arduino on the specified serial port";


Begin["`Private`"] (* Begin Private Context *) 

(*=========================================================================================================
======================== NEEDS PACKAGES ===================================================================
===========================================================================================================
This package needs the following packages:
ArduinoCompile - for compiling the sketch for the function arduinoCompile
SketchTemplate - for creating the sketch with the function sketchSetup
CCompilerDriverBase - for the function CommandJoin
CCompilerDriver - for the function QuoteFile
SymbolicC - if the user passes a custom c function in the form of a SymbolicC expression. 
=========================================================================================================*)

$thisFileDir = DirectoryName@$InputFileName;


auxiliaryPackages={"ArduinoCompile.m","SketchTemplate.m"};


Get[FileNameJoin[{$thisFileDir,#}]]&/@auxiliaryPackages;

(*
Needs["ArduinoLink`ArduinoCompile`"]
Needs["ArduinoLink`SketchTemplate`"]
*)
Needs["CCompilerDriver`"];
Needs["CCompilerDriver`CCompilerDriverBase`"];
Needs["SymbolicC`"];



(*=========================================================================================================
============================== arduinoIndividualUpload ====================================================
===========================================================================================================

arduinoIndividualUpload will upload an individual file to the Arduino. It requires the user to pass as a
string a valid file location, and that the file must end in .hex $Failed will be returned as well as a
message raised if the file ends in a .hex but the upload fails for some reason. 

arduinoIndividualUpload also expects the location of avrdude. The User configuration file can also be 
passed in, but defaults to being the empty string as it if not typically used or needed.

===========================================================================================================
===============================ALGORITHM===================================================================
===========================================================================================================


The general algorithm for this function is as follows:
===========================================================================================================
Step 1. Validate the file passed to make sure it is a .hex
===========================================================================================================
Step 2. Set the avrdude options
===========================================================================================================
Step 3. Call AVRDUDE with Import
===========================================================================================================


===========================================================================================================
=============================PARAMETERS====================================================================
===========================================================================================================

Parameters - 
	program - String corresponding to the location of the .hex file that should be uploaded
	serialPort - String corresponding to the serial port to be used for uploading, it is assumed that no
		other programs are using this serial port.
	avrdudeLocation - String corresponding to the location of avrdude
	avrdudeConfLocation - Stirng corresponding to the location of avrdude user configuration file

===========================================================================================================
==================================RETURN=================================================================== 
===========================================================================================================

Return Value - 
	Rule of String input to String output if successful
	$Failed if incorrect file passed

===========================================================================================================
===================================OPTIONS=================================================================
===========================================================================================================


	"Debug" - normally False, directs whether or not to put up the output from AVRDUDE
	


=============================FUNCTION CODE FOLLOWS=========================================================
===========================================================================================================
=========================================================================================================*)

Options[arduinoIndividualUpload]=
{
	"Debug"->False,
	"FlashVerify"->True
};

arduinoIndividualUpload::invalidftype="the file needs to be a hex file and is a `1` file";

arduinoIndividualUpload[program_String,serialPort_String,avrdudeLocation_String,avrdudeConfLocation_String,OptionsPattern[]]:= Module[
	{
		avrdudeLoc = avrdudeLocation,
		avrdudeConfLoc = avrdudeConfLocation,
		originalPathEnvironment = Environment["PATH"]
	},
	(

		avrdudeConfigSpecOption="-C";
		(*there are varying levels of verbose output for avrdude, we just use the most verbose
		users can simply ignore the rest of the output they don't care about*)
		verbose = CommandJoin[Table["-v ",{2}]];
		avrdudeOptions1 = Null;
		(*windows is weird...*)
		windowsComPortPrefix = "\\\\.\\";
		programmerSpecOption="-c";
		arduinoUnoProgrammer="arduino";
		partSpecOption="-p";
		(*this option will change for other arduinos that don't have an atmega328p*)
		arduinoUnoChip="atmega328p";
		serialPortSpecOption="-P";
		baudRateSpecOption = "-b";
		baudRateSpeed = "115200";
		(*because the atmega328p has eeprom, 
		we don't have to erase everything first, so this saves time*)
		autoEraseDisable = "-D";
		(*flash verify will instruct avrdude to read in te flash after it is done to verify that
		the program sent and the program uploaded are the same*)
		flashVerifyDisable = "-V";
		memoryOperationSpec="-U";
		memoryTypeSpec="flash:";
		(*w is to read in a hex file and write it to the memory on the atmega chip*)
		memoryOperationTypeSpec="w:";
		(*i is for intel hex file*)
		fileTypeSpec=":i";
		If[ToLowerCase@FileExtension[program]==="hex",
			(*THEN it is valid, upload it*)
			(
				command = CommandJoin[
					Riffle[
						{
							avrdudeLoc,
							avrdudeConfigSpecOption,
							avrdudeConfLoc,
							verbose,
							partSpecOption,
							arduinoUnoChip,
							programmerSpecOption,
							arduinoUnoProgrammer,
							serialPortSpecOption,
							serialPort,
							baudRateSpecOption,
							baudRateSpeed,
							autoEraseDisable,
							If[TrueQ[!OptionValue["FlashVerify"]],flashVerifyDisable,""],
							memoryOperationSpec,
							memoryTypeSpec<>memoryOperationTypeSpec<>QuoteFile@program<>fileTypeSpec
						},
						" "]
					];
				
				(*before actually running the command, set the environment variable so that the avrdude utility finds cygwin dll properly on windows*)
				If[$OperatingSystem === "Windows",
					SetEnvironment["PATH"->originalPathEnvironment<>";"<>StringDrop[FileNameTake[avrdudeLocation,{1,-6}],1]];
				];
				
				If[OptionValue["Debug"]===True,
					(
						Print["Starting uploading..."];
						$startUploadTime = AbsoluteTime[];
					)
				];
				
				(*on windows we need to quote the entire command, all other OS's don't care*)
				output = Import["!"<>
					If[$OperatingSystem==="Windows",QuoteFile@command,command]<>
					" 2>&1","Text"];
					
				(*increment the progress indiciator for the progress bar*)
				ArduinoLink`Private`$compilationProgressIndicator++;
					
				If[OptionValue["Debug"]===True,
					(
						Print["Finished uploading..."];
						Print["Took ",AbsoluteTime[] - $startUploadTime," seconds to upload"];
					)
				];
				(*now reset the path variable*)
				If[$OperatingSystem === "Windows",
					SetEnvironment["PATH"->originalPathEnvironment];
				];
				
				(*TODO: check the output to see whether or not the upload failed,
				if it did in fact fail, then issue message regarding what probably happened, and either notify the
				calling newArduinoUpload that if failed, or return the output as if it had failed*)
				
				(*if the length of the ouput is around 500-600 characters, it probably failed because
				it was unable to open the serial port*)
				
				(*if the length of the output is more than around 20000 characters, it probably worked*)
				(*more definitively, if the output contains the string avrdude: <<number of bytes, typically larger than 10000 for the sketch>> 
				bytes of flash written*)
				
				(*if the length of the output is around 10000, it probably failed due to not being able
				to find the file to upload*)
				(*more definitively, if the output contains the string avrdude: can't open input file ... : no such file or directory*)
				
				(*Print[Length[Characters@output]];*)
				
				Return[Association[command -> output]]
			),
			(*ELSE*)
			(*issue message regarding the fact that a non-hex file was passed to it, and return $Failed*)
			Message[arduinoIndividualUpload::invalidftype,ToLowerCase@FileExentsion[program]];
			$Failed
		]
	)
]

(*=========================================================================================================
============================ arduinoUpload ================================================================
===========================================================================================================

arduinoUpload will take as input options for the standard sketch to be customized and will create the
program file, compile it, convert it to .hex, then upload it over serial to the Arduino. arduinoUpload 
will also make some temporary directories for this purposes, these are put in $TemporaryDirectory. 

arduinoUpload also performs verification that the libraries passed to it are valid libraries, mainly that
they follow the arduino convention and also that they are somehwere in the folders/zip files passed. For
more on this process, see the function librarySetup.


===========================================================================================================
=====================ALGORITHM=============================================================================
===========================================================================================================

The general algorithm for this function is as follows:
===========================================================================================================
Step 1.	Setup a temp folder to be used.
===========================================================================================================
Step 2. 	Populate the temp folder with the empty output files.
===========================================================================================================
Step 3. 	Put the possibly valid libraries in the folder plibs.
===========================================================================================================
Step 4. 	Validate the libraries in plibs, putting the valid libraries in the folder libs.
===========================================================================================================
Step 5. 	Determine from the libraries in plibs what the actual libraries to include are.
===========================================================================================================
Step 6. 	Pass the validated library information along with the relevant options to SketchTemplate to
			create the program file.
===========================================================================================================
Step 7. 	Save the program file into the temp folder.
===========================================================================================================
Step 8. 	Print out debugging info if requested
===========================================================================================================
Step 9. 	Compile the program file with arduinoCompile
===========================================================================================================
Step 10.	Upload the .hex program with arduinoIndividualUpload
===========================================================================================================
Step 11.	Clean intermediate files and folders if requested
===========================================================================================================
Step 12. 	Reset the Directory Stack
===========================================================================================================
Step 13. 	Output the information from the process if requested with the debugging interface
===========================================================================================================


===========================================================================================================
=============================PARAMETERS====================================================================
===========================================================================================================

	mainDirectory - String corresponding to the directory where the Arduino software is installed
	serialPort - String corresponding to the serial port to be used for uploading, it is assumed that no
		other programs are using this serial port.

===========================================================================================================


===========================================================================================================
================================RETURN=====================================================================
===========================================================================================================
	
	Null - arduinoUpload will always return Null because it is primarily used by DeviceConfigure, which 
			doesn't return anything anyways.
	$Failed - if something failed in the process

===========================================================================================================


===========================================================================================================
==================================OPTIONS==================================================================
===========================================================================================================

	"Debug" - normally False, will direct arduinoUpload to output useful debugging information as well as
		put up message dialogs with the program produced and with the compilation input and output
	"Libraries" - normally {}, will direct arduinoUpload what libraries to include in the program file
		and to compile
	"CleanIntermediate" - normally True, will direct arduinoUpload whether or not to delete all the
		intermediate files and directories created
	Initialization - normally "", will direct arduinoUpload what code to include as initialization
		code in the program
	"Functions" - normally <||>, will use SketchTemplate to put the specified functions into
		the program 
	"FlashVerify" - normally True, directs AVRDUDE whether or not to verify the upload
	"AVRGCCLocation" - location to find the avr-gcc utilities, defaults to using the ones located in the 
		arduino install (if not default, should be a list corresponding to the directory path, i.e. 
		{"/usr","bin"}
	"AVRDUDELocation" - location to find the avrdude utility, defaults to using the one in the arduino
		software location passed in
	"AVRDUDEConfLocation" - location to find the avrdude configuration file, defaults to the one in the 
		arduino software install
	"ArduinoVersion" - the version of the software to define when compiling the code
	"StandardArduinoLibrariesLocation" - the location of the standard arduino libraries, defaults to the 
		ones inside the arduino software install

===========================================================================================================


=================================FUNCTION CODE FOLLOWS=====================================================
===========================================================================================================
=========================================================================================================*)

Options[arduinoUpload]={
		"Debug"->False,
		"Libraries" -> {},
		"CleanIntermediate"->True,
		"FlashVerify"->True,
		Initialization->"",
		"Functions"-><||>,
		"AVRDUDELocation"->Default,
		"AVRDUDEConfLocation"->Default,
		"AVRGCCLocation"->Default,
		"ArduinoVersion"->Default,
		"StandardArduinoLibrariesLocation"->Default
	};


DeviceConfigure::sketchfail = "Sketch failed to be generated. Halting";
DeviceConfigure::compilefail="Compilation of the sketch failed. Halting";
DeviceConfigure::objcopyfail="Conversion to intel hex file failed. Halting";
DeviceConfigure::uploadfail="Uploading of the sketch failed. Halting";
arduinoUpload::invalidArduinoVersion="The version of Arduino software `1` specified is invalid";
arduinoUpload::invalidavrdudeloc="The location `1` is not a valid location of the avrdude utility";
arduinoUpload::invalidGCCloc="The location `1` is not a valid location of the avr-gcc utilities";

arduinoUpload[ serialPort_String, arduinoInstallLocation_String,sketchTemplateLocation_, OptionsPattern[]] := Module[
	{
		prevDirectoryStack=DirectoryStack[],
		arduinoVersion,
		tempFolderName,
		objCopyLocation,
		avrdudeLocation,
		avrdudeConfLocation,
		avrgccLocation
	},
	(
		If[OptionValue["Debug"],
			Print["Arduino install directory is ", arduinoInstallLocation];
			Print["Arduino is on serial port ", serialPort];
			Print["Flash verify is "<>If[TrueQ[OptionValue["FlashVerify"]],"turned on","turned off"]];
		];
		
		(*setup will create the folder for us, as well as validate the libraries and 
		put them into a subfolder called libs in the temp folder*)
		tempFolderName = setup[
			(*only take the strings from the list*)
			Cases[_String]@Flatten[{OptionValue["Libraries"]}],
			arduinoInstallLocation
		];
			
		(*get the header file names from the temp folder*)
		SetDirectory[FileNameJoin@{$TemporaryDirectory,tempFolderName,"libs"}];
		$libraries=FileNames["*.h"];
			
		If[OptionValue["Debug"],
			Print["Temp folder name is ",If[$FrontEnd=!=Null,Button[tempFolderName,SystemOpen[FileNameJoin[{$TemporaryDirectory,tempFolderName}]]],tempFolderName]];
			Print["Include library files are: ",$libraries];
		];
		
		(*this is for the UserFuncSource in sketchSetup*)
		$funcSources=StringJoin@@Riffle[Values[OptionValue["Functions"]][[All,2]],"\n"];
		
		(*this is for the UserFuncTypeInfo option in sketchSetup*)
		$userFuncType={Values[OptionValue["Functions"]][[All,1]],parseCFuncName/@Values[OptionValue["Functions"]][[All,2]]};
		
		(*create the sketch text with SketchTemplate*)
		$sketchText = sketchSetup[
			sketchTemplateLocation,
			"Libraries"->$libraries,
			Initialization->OptionValue[Initialization],
			"UserFuncSource"->If[OptionValue["Functions"]===<||>,None,$funcSources],
			"UserFuncTypeInfo"->If[OptionValue["Functions"]===<||>,None,$userFuncType],
			"Debug"->OptionValue["Debug"]
		];
		
		(*increment the progress indicator for creating the sketch text*)
		ArduinoLink`Private`$compilationProgressIndicator++;	
			
		(*if $sketchText is not a string, issue message, reset DirectoryStack and return $Failed*)
		If[Head[$sketchText]=!=String,
			(
				Message[DeviceConfigure::sketchfail];
				dirStackReset[prevDirectoryStack];
				Return[$Failed]
			)
		];
		
		(*save the sketch into a text file*)
		sketchFileCreate[$sketchText,tempFolderName];
	
		(*next check the version of arduino software passed in as an option*)
		arduinoVersion = Which[
			(*user passed in a string*)
			StringQ[OptionValue["ArduinoVersion"]],
			(
				OptionValue["ArduinoVersion"]
			),
			(*it's the default version, use 16300 for version 1.6.3*)
			OptionValue["ArduinoVersion"]===Default,
			(
				"16300"
			),
			(*anything else, raise a message about it and use default value*)
			True,
			(
				Message[arduinoUpload::invalidArduinoVersion,OptionValue["ArduinoVersion"]];
				"16300"
			)
		];
	
		(*figure out where the avr-gcc utilities are location from the option passed in*)
		avrgccLocation = With[
			{locationOption=OptionValue["AVRGCCLocation"]},
			If[locationOption === Default,
				(*THEN*)
				(*use default for the location*)
				Default,
				(*ELSE*)
				(*a different location was specified, so try that one*)
				If[FileExistsQ[FileNameJoin[locationOption]],
					(*THEN*)
					(*it exists, use that*)
					FileNameJoin[locationOption],
					(*ELSE*)
					(*it doesn't exist, so return $Failed*)
					(
						Message[arduinoUpload::invalidGCCloc,locationOption];
						dirStackReset[prevDirectoryStack];
			 			Return[$Failed]
					)
				]
			]
		];
		
		(*also figure out which standard library dirrectories we are using*)
		ardStdLibs = With[
			{locationOption=OptionValue["StandardArduinoLibrariesLocation"]},
			If[locationOption === Default,
				(*THEN*)
				(*use default for the location*)
				Default,
				(*ELSE*)
				(*a different location was specified, so try that one*)
				If[And@@(FileExistsQ[FileNameJoin[#]]&/@locationOption),
					(*THEN*)
					(*it exists, use that*)
					locationOption,
					(*ELSE*)
					(*it doesn't exist, so return $Failed*)
					(
						Message[arduinoUpload::invalidStdLibs,locationOption];
						dirStackReset[prevDirectoryStack];
			 			Return[$Failed]
					)
				]
			]
		];
		
		(*now compile the program with ArduinoCompile*)
		
		compilationOutput = arduinoCompile[
			arduinoInstallLocation,
			FileNameJoin[{$TemporaryDirectory,tempFolderName}],
			"SketchTemplate.cpp",
			"CleanIntermediate"->OptionValue["CleanIntermediate"],
			"Debug"->OptionValue["Debug"],
			"ArduinoVersion"->arduinoVersion,
			"AVRGCCLocation"->avrgccLocation,
			"StandardArduinoLibrariesLocation"->ardStdLibs
		];
		
		(*if the compilation failed, issue message, reset DirectoryStack and return $Failed*)
		If[compilationOutput === $Failed, 
			(
				Message[DeviceConfigure::compilefail];
				dirStackReset[prevDirectoryStack];
				Return[$Failed]
			)
		];
		
		objCopyLocation = If[avrgccLocation === Default,
			(*THEN*)
			(*use default location inside arduino install*)
			QuoteFile@FileNameJoin[{arduinoInstallLocation, "hardware", "tools", "avr", "bin","avr-objcopy"}],
			(*ELSE*)
			(*a non default location was specified, so append avr-objcopy onto the directory where avr-gcc is located*)
			QuoteFile@FileNameJoin[{avrgccLocation,"avr-objcopy"}]
		];
		
		(*convert the elf/executable file to a hex file with avrobjcopy*)
		
		objectCopyOutput = avrObjectCopy[
			objCopyLocation,
			arduinoInstallLocation,
			tempFolderName,
			"SketchTemplate.cpp"
		];
		(*if the object compilation failed, issue message, reset DirectoryStack, and return $Failed*)
		If[objectCopyOutput === $Failed,
			(
				Message[DeviceConfigure::objcopyfail];
				dirStackReset[prevDirectoryStack];
				Return[$Failed]
			)
		];
		
		(*get the avrdude location and avrdude configuration file location*)
		
		avrdudeLocation = With[
			{locationOption=OptionValue["AVRDUDELocation"]},
			If[locationOption===Default,
				(*THEN*)
				(*use the default location inside the arduino install directory*)
				QuoteFile@FileNameJoin[{arduinoInstallLocation,"hardware", "tools", "avr", "bin","avrdude"}],
				(*ELSE*)
				(*a different location was specified*)
				If[FileExistsQ[FileNameJoin[locationOption]],
					(*THEN*)
					(*it exists, so use it*)
					QuoteFile@FileNameJoin[locationOption],
					(*ELSE*)
					(*it doesn't exist, so raise a message and return $Failed*)
					(
						Message[arduinoUpload::invalidavrdudeloc,locationOption];
						dirStackReset[prevDirectoryStack];
				 		Return[$Failed]
					)
				]
			]
		];

		avrdudeConfLocation = With[
			{locationOption=OptionValue["AVRDUDEConfLocation"]},
			If[locationOption===Default,
				(*THEN*)
				(*use the default location inside the arduino install directory*)
				QuoteFile@FileNameJoin[{arduinoInstallLocation, "hardware", "tools", "avr", "etc","avrdude.conf"}],
				(*ELSE*)
				(*a different location was specified*)
				If[FileExistsQ[FileNameJoin[locationOption]],
					(*THEN*)
					(*it exists, so use it*)
					QuoteFile@FileNameJoin[locationOption],
					(*ELSE*)
					(*it doesn't exist, so raise a message and return $Failed*)
					(
						Message[arduinoUpload::invalidavrdudeloc,locationOption];
						dirStackReset[prevDirectoryStack];
				 		Return[$Failed]
					)
				]
			]
		];
		
		(*finally upload the hex file with arduinoIndividualUpload*)
		
		uploadOutput = arduinoIndividualUpload[
			FileNameJoin[{$TemporaryDirectory,tempFolderName,"SketchTemplate.cpp.hex"}],
			serialPort,
			avrdudeLocation,
			avrdudeConfLocation,
			"Debug"->OptionValue["Debug"]
		];
		
		If[uploadOutput === $Failed,
			(
				Message[DeviceConfigure::uploadfail];
				dirStackReset[prevDirectoryStack];
			 	Return[$Failed]
			)
		];
		
		(*create the association output*)	
		(*if any are not associations, don't add them, else outputViewer will get confused*)
		(*also add the sketch text to the association as the first command*)
		output=Join@@Flatten[
			{
				<|"Sketch File"->$sketchText|>,
				(If[Head[#]===Association,#,<||>]&/@{compilationOutput,objectCopyOutput,uploadOutput})
			}
		];
		
		(*delete directories and intermediate files by default*)
		If[!(TrueQ[OptionValue["Debug"]]),
			(*then delete the directory and all the files inside*)
			(
				If[OptionValue["Debug"],Print["deleting files in temp directory"]];
				SetDirectory[FileNameJoin[{$TemporaryDirectory}]];
				DeleteDirectory[tempFolderName,DeleteContents->True];
			)
			(*ELSE*)
			(*don't delete the files, so just don't do anything*)
		];
		
		(*Clear the DirectoryStack*)
		dirStackReset[prevDirectoryStack];
		If[OptionValue["Debug"]===True,
			(
				Print["Finished successfully"];
				MessageDialog[outputViewer[tempFolderName,output],WindowSize->{1250,750}];
			)
		];
	)
]


(*=========================================================================================================
============================ AVR OBJECT COPY ==============================================================
===========================================================================================================
avrObjectCopy will convert the elf file from the compilation process into a .hex file for avrdude to 
upload. It basically is just two calls to the command line.

===========================================================================================================
=====================ALGORITHM=============================================================================
===========================================================================================================

The general algorithm for this function is as follows:
===========================================================================================================
Step 1.		Generate the command
===========================================================================================================
Step 2. 	Run the command in the system shell
===========================================================================================================

===========================================================================================================
=============================PARAMETERS====================================================================
===========================================================================================================

	arduinoInstallLocation - the location of the arduino software on this machine
	tempFolderName - the name of the temporary folder created by arduino upload to be used as the build 
		location
	fileName - the name of the .elf file to be copied

===========================================================================================================


===========================================================================================================
================================RETURN=====================================================================
===========================================================================================================
	
	Association of the input commands to the output from those commands if successful
	$Failed if unsuccessful

===========================================================================================================


===========================================================================================================
==================================OPTIONS==================================================================
===========================================================================================================

	N/A, not much to change, so there are no options

===========================================================================================================


=================================FUNCTION CODE FOLLOWS=====================================================
===========================================================================================================
=========================================================================================================*)


avrObjectCopy[objExecutableLocation_,arduinoInstallLocation_,tempFolderName_,fileName_]:= Module[
	{
		fName = fileName,
		temp = tempFolderName,
		originalPathEnvironment = Environment["PATH"],
		outputTargetSpec = "-O",
		outputTarget = "ihex",
		sectionNameSpec = "-j",
		sectionName = ".eeprom",
		sectionFlagsSpec = "--set-section-flags",
		sectionFlags = "=.eeprom=alloc,load",
		noChangeWarnings = "--no-change-warnings",
		changeSectionLMASpec = "--change-section-lma",
		changeSectionLMA = ".eeprom=0",
		verbose = "-v",
		removeSection = "-R"
	},
	(
		(*WORKAROUND FOR QUOTEFILE*)
		
		(*note that for the two commands, blank spaces are appended and prepended to "trick" the QuoteFile
		used from CCompilerDriver, because it won't quote files that have quotes at the beginning and end
		of the name. This does have quotes at the start and the end, but it is not because the entire 
		command has already been quoted, but because the command consists of files that have already been
		quoted individually at the start and the end*)
		
		commandEEP = CommandJoin[
			Riffle[
				{
					" ",
					objExecutableLocation,
					outputTargetSpec,
					outputTarget,
					sectionNameSpec,
					sectionName,
					sectionFlagsSpec<>sectionFlags,
					noChangeWarnings,
					changeSectionLMASpec,
					changeSectionLMA,
					verbose,
					QuoteFile@FileNameJoin[{$TemporaryDirectory, temp, fName <> ".elf"}],
					QuoteFile@FileNameJoin[{$TemporaryDirectory, temp, fName <> ".eep"}],
					" "
				}," "]
		];
		commandHEX = CommandJoin[
			Riffle[	
				{
					" ",
					objExecutableLocation,
					outputTargetSpec,
					outputTarget,
					removeSection,
					sectionName,
					verbose,
					QuoteFile@FileNameJoin[{$TemporaryDirectory, temp, fName <> ".elf"}],
					QuoteFile@FileNameJoin[{$TemporaryDirectory, temp, fName <> ".hex"}],
					" "
				},
				" "]
		];
		
		If[$OperatingSystem==="Windows",
			SetEnvironment["PATH"->originalPathEnvironment<>";"<>arduinoInstallLocation];
		];

		eepOutput = Import["!"<>If[$OperatingSystem==="Windows",QuoteFile@commandEEP,commandEEP]<>" 2>&1","Text"];
		
		(*increment the progress bar for running object copy*)
	    ArduinoLink`Private`$compilationProgressIndicator++;
	    
		hexOutput = Import["!"<>If[$OperatingSystem==="Windows",QuoteFile@commandHEX,commandHEX]<>" 2>&1","Text"];

		If[$OperatingSystem === "Windows",
			SetEnvironment["PATH"->originalPathEnvironment];
		];

		(*increment the progress bar for running object copy*)
	    ArduinoLink`Private`$compilationProgressIndicator++;

		(*return the input/output association*)
		Association[{commandEEP -> eepOutput, commandHEX -> hexOutput}]
	)
];



Options[arduinoReset]=
	{
		"AVRDUDELocation"->Default,
		"AVRDUDEConfLocation"->Default
	};

arduinoReset[serialPort_,arduinoInstallLocation_,OptionsPattern[]]:=Module[
	{
		avrdudeConfLocation,
		avrdudeLocation,
		originalPathEnvironment = Environment["PATH"]
	},
	(
		avrdudeLocation = With[
			{locationOption=OptionValue["AVRDUDELocation"]},
			If[locationOption===Default,
				(*THEN*)
				(*use the default location inside the arduino install directory*)
				QuoteFile@FileNameJoin[{arduinoInstallLocation,"hardware", "tools", "avr", "bin","avrdude"}],
				(*ELSE*)
				(*a different location was specified*)
				If[FileExistsQ[FileNameJoin[locationOption]],
					(*THEN*)
					(*it exists, so use it*)
					QuoteFile@FileNameJoin[locationOption],
					(*ELSE*)
					(*it doesn't exist, so raise a message and return $Failed*)
					(
						Message[arduinoReset::invalidavrdudeloc,locationOption];
						dirStackReset[prevDirectoryStack];
				 		Return[$Failed]
					)
				]
			]
		];

		avrdudeConfLocation = With[
			{locationOption=OptionValue["AVRDUDEConfLocation"]},
			If[locationOption===Default,
				(*THEN*)
				(*use the default location inside the arduino install directory*)
				QuoteFile@FileNameJoin[{arduinoInstallLocation, "hardware", "tools", "avr", "etc","avrdude.conf"}],
				(*ELSE*)
				(*a different location was specified*)
				If[FileExistsQ[FileNameJoin[locationOption]],
					(*THEN*)
					(*it exists, so use it*)
					QuoteFile@FileNameJoin[locationOption],
					(*ELSE*)
					(*it doesn't exist, so raise a message and return $Failed*)
					(
						Message[arduinoReset::invalidavrdudeloc,locationOption];
						dirStackReset[prevDirectoryStack];
				 		Return[$Failed]
					)
				]
			]
		];
	
		(*for arduino 1.6.0, the install location has to be added to the environment path for any*)
		(* utilities to work, as they depend on cygwin*)
		If[$OperatingSystem === "Windows", 
			SetEnvironment["PATH"->originalPathEnvironment<>";"<>arduinoInstallLocation];
		];
		resetOutput = Import["!"<>If[$OperatingSystem==="Windows","\"",""]<>
				avrdudeLocation<>" -C "<>
				avrdudeConfLocation<>" -p atmega328p -c arduino -P "<>
				If[$OperatingSystem==="Windows","\\\\.\\",""]<>
				serialPort<>" -b 115200 2>&1"<>If[$OperatingSystem==="Windows","\"",""],
			"Text"
		];
		(*now reset the PATH environment variable to whatever it was before*)
		If[$OperatingSystem === "Windows",
			SetEnvironment["PATH"->originalPathEnvironment];
		];
	)
];



(*=========================================================================================================
============================ LIBRARY SETUP ================================================================
===========================================================================================================

This funciton will validate any libraries passed to it, and put the valid ones in one of two folders, 
either the cpp folder, for C++ libraries, or the c folder for C libraries.
There are three different types of libraries that this function will allow.
	1. A library that is just a name, which corresponds to a builtin arduino software library.
	2. A library that is an archive file, hypothetically containing library files (here it may be possible 
		to find online libraries, as if the string is an url, we can just download the url and treat it as
		a normal archive file 
	3. A library that is a directory, hypothetically containing library files
	4. A library that is an URL


The main purpose of this function is to make the compiling and including of these libraries as simple as 
possible for the future functions. More work here means less complicated compiling code for the compilation.

This function ensures that all of the valid include directories are just the libs directory as
well as the fact that the names of the libraries to be included will be given by the FileNames["*",...] of
that directory.


This function and its helper functions expect that the temp folder passed to it contains the following 
folders:
	"libs", with subdirectories of "c", "h", and "cpp"
	"plibs", can be (and will most likely be) empty

===========================================================================================================
=====================ALGORITHMS============================================================================
===========================================================================================================

There are different algorithms for how to handle each kind of library. First, we will handle a user-passed
directory.

The general algorithm for this function is as follows:
===========================================================================================================
Step 1.		First, we will "flatten" out the directory, moving all the internally stored files in any 
			subdirectories, and put them all in the root directory.
===========================================================================================================
Step 2. 		Now, we make 3 lists. One of all the .h files, one of all of the .cpp files, and one of all the
			.c files.
===========================================================================================================
Step 4.		Now, we then move all of the .h files inside the h folder in libs, all the .c files inside 
			the c folder inside libs, and the same for the .cpp files. 
===========================================================================================================


Next, we will handle the case of a built in library from the arduino software.

The general algorithm for this function is as follows:
===========================================================================================================
Step 1.		First, check to make sure that the passed string is in fact a member of the built in libraries.
===========================================================================================================
Step 2.		If it is, then we need to copy the entire directory from the arduino software location, and put
			the entire directory inside the plibs folder. Now, it can be treated like a normal user-passed
			directory to be handled with the above algorithm.
===========================================================================================================


Now, we handle the case of an archive file.
The general algorithm for this function is as follows:
===========================================================================================================
Step 1.		First, check to make sure that the archive is in fact a valid file.
===========================================================================================================
Step 2.		If it is, then we need to extract the entire archive into a folder inside plibs named whatever
			the name of the archive file is. Now we can handle this library as if it was a normal
			user-passed directory with the first algorithm. 
===========================================================================================================


Finally, we handle the case of an URL library (which must itself be an archive).
===========================================================================================================
Step 1.		First, make sure that the URL is indeed a valid internet location that can be downloaded from.
===========================================================================================================
Step 2.		Now, we can download the file in a blocking way to plibs, because we can't do anything else 
			until the library is downloaded. Once the library is downloaded, we treat it as if it was a 
			user-passed archive with the above algorithm. 
===========================================================================================================


===========================================================================================================
=============================PARAMETERS====================================================================
===========================================================================================================

	libraries - the raw option that the user passed declaring what libraries to be included.
	tempFolder - the name of the temp folder location
	arduinoInstallLocation - the location of the arduino software to grab the builtin libraries from

===========================================================================================================


===========================================================================================================
================================RETURN=====================================================================
===========================================================================================================
	
	Null - because this function will never "fail", it will simply not do anything if the libraries are 
			invalid. 

===========================================================================================================


===========================================================================================================
==================================OPTIONS==================================================================
===========================================================================================================

	N/A - no options because there isn't really anything to handle here 

===========================================================================================================


=================================FUNCTION CODE FOLLOWS=====================================================
===========================================================================================================
=========================================================================================================*)


librarySetup[libraries_List, tempFolder_, arduinoInstallLocation_]:=Module[ 
	{
		setupLocation = FileNameJoin[{$TemporaryDirectory,tempFolder}],
		(*if we are on the Raspberry Pi, we need to replace some built-in libraries with the actual location*)
		(*this is because the version of arduino on the Raspberry Pi is older and doesn't have the same directory structure*)
		libs = If[$MachineID === "4801-62204-12672", libraries/.raspiLibraryLocations[arduinoInstallLocation] , libraries]
	},
	(
		(*iterate over the libraries*)
		Do[
			(
			(*switch on which case we are handling*)
			Switch[libraryType[lib],
				"Directory",
				(
					CopyDirectory[lib,FileNameJoin[{setupLocation,"plibs",Last@FileNameSplit@lib}]];
					dirLibHandle[FileNameJoin[{setupLocation,"plibs",Last@FileNameSplit@lib}],setupLocation]
				),
				"Archive",
					(
						If[FileExistsQ[lib],
							(*THEN*)
							(*extract the lib*)
							(
								(*FixedPoint here is used to get rid of all file extensions so that for 
								example library.tar.gz will be extracted into the folder /libs/library*)
								ExtractArchive[
									lib,
									libLocationName = FileNameJoin@
										{
											setupLocation,
											"plibs",
											FixedPoint[FileBaseName,Last@FileNameSplit@lib]
										},
									CreateIntermediateDirectories -> True
								];
								(*now we can handle this library normally*)
								dirLibHandle[libLocationName,setupLocation];
							),
							(*ELSE*)
							(*issue message and move on to the next library*)
							(
								Message[librarySetup::invalidziplib,lib];
								Null;
							)
						] (*end if*)
					),(*end case*)
				"URL",
					(
						(*first check the status code of the location using URLFetch*)
						If[IntegerDigits[URLFetch[lib,"StatusCode"]][[1]]===2,
							(*THEN*)
							(*the request is probably a success, it is returning a 2xx success code at least*)
							(
								URLSave[lib,archiveLocation = FileNameJoin@{setupLocation,"plibs",Last@FileNameSplit@lib}];
								(*assume it was successful, and go ahead and try to extract it*)
								ExtractArchive[
									archiveLocation,
									libLocationName = FileNameJoin@
										{
											setupLocation,
											"plibs",
											FixedPoint[FileBaseName,Last@FileNameSplit@lib]
										},
									CreateIntermediateDirectories -> True
								];
								(*now we can handle this library normally*)
								dirLibHandle[libLocationName,setupLocation];
							),
							(*ELSE*)
							(*the request is probably bad because it is not returning a 2xx code,
							so just issue a message and go to the next one*)
							(
								Message[librarySetup::cloudLib,lib];
								Null;
							)
						](*end if*)
					),(*end case*)
				"BuiltIn",
					(
						(*first check if it is indeed a builtin library*)
						If[MemberQ[arduinoBuiltInLibrariesList,lib],
							(*THEN*)
							(*it is a member of the arduino built in libraries, so check which kind*)
							(*there are two locations for the built in libraries, one in the libraries 
							folder, and the other inside the hardware/arduino/avr/arduino/libraries folder*)
							(
								If[MemberQ[arduinoHardwareLibraries,lib],
									(*THEN*)
									(*the library has to be copied from a different location*)
									(
										CopyDirectory[
											FileNameJoin@
												{
													arduinoInstallLocation,
													"hardware",
													"arduino",
													"avr",
													"libraries",
													lib
												},
											libLocationName = FileNameJoin@
												{
													setupLocation,
													"plibs",
													lib
												}
										];
										(*now we can handle this library normally*)
										dirLibHandle[libLocationName,setupLocation];
									),
									(*ELSE*)
									(*the library can be copied from the normal libraries location*)
									(
										CopyDirectory[
											FileNameJoin@
												{
													arduinoInstallLocation,
													"libraries",
													lib,
													"src"
												},
											libLocationName = FileNameJoin@
												{
													setupLocation,
													"plibs",
													lib
												}
										];
										(*now we can handle this library normally*)
										dirLibHandle[libLocationName,setupLocation];
									)
								];	
							),
							(*ELSE*)
							(*it isn't a member, so just issue a message and go to the next one*)
							(
								Message[librarySetup::libnotfound,lib];
								Null;
							)
						](*end If loop*)
					),(*end case*)
					_,
					(*default case, don't do anything, but issue a message and go to the next one*)
					(
						Message[librarySetup::libnotfound,lib];
						Null
					)
				](*end Swtich*)
			),
			{lib,libs}
		](*end Do loop*)
	)
]



(*this is the helper function to librarySetup that copies the relevant files and directories into libs*)
dirLibHandle[libraryDirectory_String,mainDirectory_]:=Module[{},
	(
		(*first set the current directory to the working plibs directory*)
		SetDirectory[libraryDirectory];
		(*flatten the directory out*)
		DirectoryFlatten[];
		(*get the names of all of the .c, .cpp, and .h files, as well as all the directories*)
		cLibs = FileNames["*.c"];
		cPlusPlusLibs = FileNames["*.cpp"];
		headers = FileNames["*.h"];
		directories = Select[DirectoryQ]@FileNames["*"];
		(*copy all those files into the libs directory*)
		CopyFile[FileNameJoin@{libraryDirectory,#},FileNameJoin@{mainDirectory,"libs",#}]&/@cLibs;
		CopyFile[FileNameJoin@{libraryDirectory,#},FileNameJoin@{mainDirectory,"libs",#}]&/@headers;
		CopyFile[FileNameJoin@{libraryDirectory,#},FileNameJoin@{mainDirectory,"libs",#}]&/@cPlusPlusLibs;
		CopyDirectory[FileNameJoin@{libraryDirectory,#},FileNameJoin@{mainDirectory,"libs",#}]&/@directories;
		(*THE FOLLOWING IS A WORKAROUND TO GET THE SERVO LIBRARY TO WORK*)
		(*The Servo.h file includes another file, ServoTimers.h, that doesn't have an #ifndef SERVOTIMERS_H block around the source code, to prevent being*) 
		(*included twice, so it gets included into the file twice, so we just delete the file that would be included in the normal sketch, so that*)
		(*the one included in the Servo.h doesn't get overridden.*)
		Quiet[DeleteFile[FileNameJoin[{mainDirectory,"libs","ServoTimers.h"}]]]
	)
]


(*there's a bug with this function, it goes into an infinite recursive loop*)
(*smart directory copy will merge directories if the dest dir already exists*)
smartCopyDirectory[sourceDir_,destDir_]:=Module[{},
	(
		If[FileExistsQ[destDir],
			(*THEN*)
			(*the directories exists, make sure it's not a file before merging the two*)
			(
				If[DirectoryQ[destDir],
					(*THEN*)
					(*the source and destination directories should be merged*)
					(
						$all = FileNames["*",{sourceDir}];
						$allSubDirectories = Select[DirectoryQ]@$all;
						$allFiles = Complement[$all,$allSubDirectories];
						(*do a normal copy of all the files*)
						Quiet[CopyFile[#,destDir]&/@$allFiles,CopyFile::filex];
						(*do a smart directory copy of all the sub directories*)
						Quiet[smartCopyDirectory[#,destDir]&/@$allDirectories,CopyDirectory::filex];
					)
					(*ELSE*)
					(*the destination directory isn't a directory, so don't do anything*)
				]
			),
			(*ELSE*)
			(*the directory doesn't exist, so we can do a normal copy*)
			(
				CopyDirectory[sourceDir,destDir];
			)
		]
	)
]


(*directory will go into all subfolders and copy all files into the root directory*)
(*USE CAUTION WITH THIS FUNCTION, IT SHOULD ONLY BE CALLED FROM INSIDE plibs FOLDER*)
(*TODO: enforce that this function is only called from inside the plibs folder*)
DirectoryFlatten[]:=Module[
	{
		$currentDirectory = Directory[]
	},
	(
		(*get the names of all the files and subfiles*)
		$allFiles = FileNames["*",{"*"},Infinity];
		(*rename all these to have absolute file locations*)
		$allFiles = FileNameJoin[{$currentDirectory,#}]&/@$allFiles;
		$allDirectories = Select[DirectoryQ]@$allFiles;
		$allFiles = Complement[$allFiles, $allDirectories];
		(*now copy all of them, using quiet to ignore any overwrite warnings*)
		Quiet[CopyFile[#,FileNameJoin[{$currentDirectory,Last@FileNameSplit@#}]]&/@$allFiles,CopyFile::filex];
		Quiet[CopyDirectory[#,FileNameJoin[{$currentDirectory,Last@FileNameSplit@#}]]&/@$allDirectories,CopyDirectory::filex];
	)
];


(*libraryType will take as an argument the library and it will try to determine what kind, if any, of library it is*)
(*note that this won't validate that the library exists, just if it does, what kind it is. The handling of whether or not 
it exists is handled inside librarySetup*)
libraryType[library_]:=Module[{},
	(
		If[Head[library]===String,
			(*THEN*)
			(*the library is at least a string*)
			(*still need to determine which case it fits*)
			(
				If[Length[FileNameSplit[library]]===1,
					(*THEN*)
					(*the library is definitely not a valid file name, so it is probably a builtin library*)
					Return["BuiltIn"],
					(*ELSE*)
					(*the library is definitely a file name, so let's see if it is a valid file*)
					If[FileExistsQ[library]===True,
						(*THEN*)
						(*the library is a file or a directory*)
						(*still need to determine if it is an archive file or a directory*)
						If[DirectoryQ[library]===True,
							(*THEN*)
							(*library is a directory*)
							Return["Directory"],
							(*ELSE*)
							(*library is not a directory, so it must be an archive file*)
							Return["Archive"]
						],
						(*ELSE*)
						(*the library is not a valid file, so let's assume it is an URL*)
						Return["URL"]
					]
				]
			),
			(*ELSE*)
			(*the library is not a string, so it is not a valid library*)
			"Invalid"
		]
			
	)
	
];




(*library setup messages*)
librarySetup::invalidziplib="the zip library `1` is invalid";
librarySetup::notrecognized="the file library `1` is not recognized as a file";
librarySetup::libnotfound="the library `1` was not found";
librarySetup::invalidfoldlib="the folder library `1` is invalid";
librarySetup::invalidftype="the file `1` is not a zip";
librarySetup::cpfilefail="the file `1` failed to copy";
librarySetup::cpfolderfail="the folder `1` failed to copy";
librarySetup::cloudLib="the library `1` failed to download";



(*=========================================================================================================
====================================CONSTANTS==============================================================
=========================================================================================================*)

(*arduinoBuiltInLibraries is used by the librarySetup function*)
arduinoBuiltInLibrariesList=
{
	"EEPROM", 
	"Ethernet", 
	"Firmata", 
	"GSM", 
	"LiquidCrystal", 
	"SD", 
	"Servo", 
	"SoftwareSerial",
	"SPI", 
	"Stepper", 
	"TFT" ,
	"Temboo",
	"WiFi", 
	"Wire"
};

(*these libraries are libraries that are found inside a different location in the arduino software install*)
arduinoHardwareLibraries=
{
	"EEPROM",
	"SoftwareSerial",
	"SPI",
	"Wire"
};

(*these libraries aren't on the version of the arduino software that's on the raspberry pi*)
arduinoPiMissingLibraries=
{
	"Bridge",
	"Esplora",
	"GSM",
	"Robot_Control",
	"RobotIRremote",
	"Robot_Motor",
	"SpacebrewYun",
	"Temboo",
	"TFT",
	"WiFi"
};

(*these are the locations to use in place of the hardware locations on the raspberry pi, as the version of the arduino software on the raspberry pi is older than the 
supported version, and so it doesn't have all the same locations*)
raspiLibraryLocations[installLocation_]:=
<|
	#->FileNameJoin[{installLocation,"libraries",#}]&/@Fold[DeleteCases,arduinoBuiltInLibrariesList,arduinoPiMissingLibraries]
|>;


(*=========================================================================================================
=======================================INTERNAL UTILITIES==================================================
=========================================================================================================*)


(*=========================================================================================================
=======================================OUTPUT VIEWER=======================================================
outputViewer creates a mini-debugging interface to view the output of the compilation/upload process

It expects an association and the temp folder name used in the process. It will figure out where certain 
commands are and places buttons on the window pane to jump to those commands.
It also has a button to open up the folder that was used to manually look at the output

=========================================================================================================*)

outputViewer[tempFolderName_String,association_Association] :=Module[{},
	If[$FrontEnd === Null,
		(*THEN*)
		(*we don't have a front end so just make it a string*)
		(
			(*because we don't have a front end, and this result will likely just be printed off, leave out the sketch text, as it is huge*)
			(*TODO: implement this so that it is actually useful or readable*)
			""
		),
		(*ELSE*)
		(*we have a front end to display, so use the manipulate*)
		(
			Manipulate[
				Labeled[
					Pane[
						Column[
							{
								"Input", 
								Null, 
								Pane[Keys[association][[index]],{1150,125},Scrollbars->True],
								Null, 
								"Output:", 
								Null, 
								Pane[Values[association][[index]],{1150,375},Scrollbars->True,LineBreakWithin -> False]
							}
						]
					],
					Row[
						{
							Button["Forward", If[index < Length[association], index++, index = 1]], "  ", 
							Button["Backward",If[index > 1, index--, index = Length[association]]], "  ", 
							Button["Sketch text", index = 1], "  ", 
							Button["Final GCC", index = Length[association] - 3], "  ", 
							Button["Initial sketch compile", index = 2], "  ", 
							Button["First library", index = 3], "  ", 
							Button["AVRDUDE", index = Length[association]]," ",
							Button["Open folder",SystemOpen[FileNameJoin[{$TemporaryDirectory,tempFolderName}]]]
						}
					],
					Top
				],
				{{index, 1, "Command number"}, 1, Length[association], 1},
				ContentSize->{1200,650}
			]
		)
	]
]


(*=========================================================================================================
====================================SKETCH FILE CREATE===================================================
sketchFileCreate will write the text passed to a file named SketchTemplate.cpp in the folder in 
$TemporaryDirectory with the given name

===========================================================================================================
=========================================================================================================*)


sketchFileCreate[text_,temporaryFolderName_]:= Module[
	{
		tempFolder = temporaryFolderName,
		fileName="SketchTemplate.cpp"
	},
	(
		SetDirectory[FileNameJoin[{$TemporaryDirectory,tempFolder}]];
		$file = OpenWrite[fileName];
		WriteString[ $file, text];
		Close[ $file ];
	)
];





(*============================================SETUP========================================================
setup will create a new folder with a UUID determined name, create the subdirectories libs, and plibs
as well as call librarySetup which will go through and perform the necessary actions to validate and add
the libraries. It returns the name of the folder it created
=========================================================================================================*)

setup[userArdLibs_,arduinoInstallLocation_] := Module[{tempFolderName,libs=userArdLibs},
    (
    	SetDirectory[$TemporaryDirectory];
   	tempFolderName = FileNameTake@CreateDirectory@createFolderName[];
    	SetDirectory[FileNameJoin[{Directory[], tempFolderName}]];
    	CreateDirectory["libs"];
    	CreateDirectory["plibs"];
    	CreateDirectory["liboutput"];
    	librarySetup[libs,tempFolderName,arduinoInstallLocation];
    	tempFolderName
	)
];


(*createFolderName uses CreateUUID to create a unique code for the folder's name, but drops the non-alphanumerics*)
createFolderName[] := Module[{},
	(
		"arduinocompile"<>StringJoin@Delete[{{9}, {14}, {19}, {24}}]@Characters@CreateUUID[]
	)
];


(*dirStackReset will reset the DirectoryStack to whatever the user had previously, quietly*)
dirStackReset[prevDirStack_]:=
(
	Quiet[While[DirectoryStack[]!=prevDirStack,ResetDirectory[]],ResetDirectory::cdir]
);


(*for parsing the name of the function out of the source*)
parseCFuncName[cFunction_String]:=Module[{},
	(
		firstParenth = (FirstPosition["("]@Characters[cFunction])[[1]];
		firstSubString = StringTake[cFunction,firstParenth];
		reversed = StringReverse[firstSubString];
		firstSpace = StringLength[reversed] - (FirstPosition[" "]@Characters[reversed])[[1]];
		name = StringTrim[StringTake[cFunction,{firstSpace+1,firstParenth-1}]];
		name
	)
]


echo=(Print[#];#)&;


End[] (* End Private Context *)

EndPackage[]
