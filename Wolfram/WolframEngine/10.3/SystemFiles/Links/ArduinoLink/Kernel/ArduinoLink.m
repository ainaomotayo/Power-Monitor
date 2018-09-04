(* ::Package:: *)

(*==========================================================================================================
			
					ARDUINO LINK
			
Author: Ian Johnson
			
Copyright (c) 2015 Wolfram Research. All rights reserved.			


ArduinoLink is a package with DeviceFramework functionality setup to interface the Wolfram Language with 
an Arduino. 

CURRENT SUPPORTED BOARDS:
~Arduino Uno

USER ACCESSIBLE FUNCTIONS:

==========================================================================================================*)


BeginPackage["ArduinoLink`"]
(* Exported symbols added here with SymbolName::usage *) 

(*no public functions, as all functionality is through the device driver framework*)

Begin["`Private`"]
(* Implementation of the package *)

(*the arduino driver is an extension of the firmata driver*)
(*arduino upload is used for DeviceConfigure*)


$thisFileDir = DirectoryName@$InputFileName;

Needs["ArduinoUpload`"];
Needs["Firmata`"];

(*SymbolicC is for if any functions specified in DeviceConfigure are SymbolicC functions*)
Needs["SymbolicC`"];


(*paclet manager is for managing the paclet directory and such*)
Needs["PacletManager`"];

(*this is the only part where any of the packages needs to know where it is located*)
(*all other locations are passed as arguments from this package*)


$arduinoInternalInstallDirectory= FileNameJoin[{$UserBaseDirectory,"ApplicationData","Arduino"}]




(*BOOK KEEPING VARIABLES*)

$lastConfigCall=AbsoluteTime[];
$pinConfigs = <||>;
$previousFunctions=<||>;
$functionCalls=<||>;

ports={{0,1,2,3,4,5,6,7},{8,9,10,11,12,13},{"A0","A1","A2","A3","A4","A5"}};

$DeviceStates=<||>;


(*for checking the pins, these are all the possible valid pins that can be used for an arduino uno*)
arduinoUnoPWMPins={3,5,6,9,10,11};
arduinoUnoPins={2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,"A0","a0","A1","a1","A2","a2","A3","a3","A4","a4","A5","a5"};
arduinoUnoAnalogPins={14,15,16,17,18,19,"A0","a0","A1","a1","A2","a2","A3","a3","A4","a4","A5","a5"};
arduinoPinToKey=<|
	14->"A0",15->"A1",16->"A2",17->"A3",18->"A4",19->"A5",
	"A0"->"A0","A1"->"A1","A2"->"A2","A3"->"A3","A4"->"A4","A5"->"A5",
	"a0"->"A0","a1"->"A1","a2"->"A2","a3"->"A3","a4"->"A4","a5"->"A5",
	0->"D0",1->"D1",2->"D2",3->"D3",4->"D4",5->"D5",6->"D6",7->"D7",8->"D8",9->"D9",10->"D10",11->"D11",12->"D12",13->"D13",
	(*this is added for additional security, just in case $pinConfiguration is ever accessed with "D8" or something*)
	"D0"->"D0","D1"->"D1","D2"->"D2","D3"->"D3","D4"->"D4","D5"->"D5","D6"->"D6","D7"->"D7","D8"->"D8","D9"->"D9","D10"->"D10","D11"->"D11","D12"->"D12","D13"->"D13"
|>;
pinToPort=<|14->2,"A0"->2,15->2,"A1"->2,16->2,"A2"->2,17->2,"A3"->2,18->2,"A4"->2,19->2,"A5"->2,0->0,1->0,2->0,3->0,4->0,5->0,6->0,7->0,8->1,9->1,10->1,11->1,12->1,13->1|>;

analogNumericPin=<|"A0"->0,"a0"->0,"A1"->1,"a1"->1,"A2"->2,"a2"->2,"A3"->3,"a3"->3,"A4"->4,"a4"->4,"A5"->5,"a5"->5,14->0,15->1,16->2,17->3,18->4,19->5|>;


$compilationProgressIndicator = 0;

$arduinoSoftwarePresent=False;
$arduinoInstallLocation=None;


(*MESSAGES*)
DeviceWrite::serialPin="Pins 0 and 1 are required for Serial communication";
DeviceWrite::notPWMPin="Pin `1` is not a PWM pin";
DeviceWrite::invalidPin="The pin `1` is not a valid Arduino Uno pin";
DeviceWrite::config="Pin `1` configured as an input, cannot write to it";
DeviceWrite::pwmresolution="The value `1` is not within the resolution supported, a value of `2` was used instead";
DeviceWrite::nonBooleanWrite="The value `1` is not a boolean (0 or 1), using `2` instead";
DeviceWrite::numericValue="The value `1` is not numeric";
DeviceRead::invalidPin="The pin `1` is not a valid Arduino Uno pin";
DeviceRead::config="Pin `1` configured as an output, cannot read from it";
DeviceExecute::funcName="Function name `1` not found";
DeviceExecute::past="Cannot execute the function `1` seconds in the past";
DeviceExecute::invalidTiming="The time specification `1` is invalid";
DeviceExecute::noFunc="Function name `1` not found";
DeviceExecute::invalidFunc="Not a valid form of a function name";
DeviceExecute::invalidArgs="The arguments `1` are invalid for this function";
DeviceExecute::taskRunning="There is already a task running on the arduino, either wait for it to finish or delete the task";
DeviceExecute::needsArgs="The function `1` needs arguments";
DeviceConfigure::invalidPin="The pin `1` is not a valid pin";
DeviceConfigure::invalidAnalogPin="The pin `1` is not a valid analog input pin";
DeviceConfigure::invalidMode="The mode `1` is not a valid pin configuration mode";
DeviceConfigure::invalidArgs="The argument `1` does not match the required type";
DeviceConfigure::noArduinoInstall="The Arduino software is not installed";
DeviceConfigure::invalidOptions="The option `1` is not a valid configuration option";
DeviceSetProperty::invalidInstallLocation="The location `1` is not a valid Arduino installation";
DeviceOpen::port="The name of the Serial port the Arduino is attached to must be specified";
DeviceOpen::serialDrivers="Serial drivers for the Arduino were not installed, install and reopen ";
DeviceOpen::raspiAutoInstall="The Arduino software was not found on this machine. On the Raspberry Pi, the Arduino software cannot be automatically installed, please install it with \"sudo apt-get update && sudo apt-get install -y arduino-core\"";
DeviceOpen::invalidInput="Please enter either True or False";
validArduinoInstallLocation::version="The version of Arduino installed at `1` is version `2`, only version `3` is supported";
validArduinoInstallLocation::unusableInstall="The version of Arduino installed at `1` is insufficient for ArduinoLink. Try reinstalling version 1.6.3";
validArduinoInstallLocation::version162="The location `1` is Arduino version 1.6.2, please install 1.6.3 to use ArduinoLink.";
validArduinoInstallLocation::arduinoCore="The aptitude package arduino-core is required for usage; install it with \"sudo apt-get update && sudo apt-get install -y arduino-core\"";

(*FUNCTIONS*)


driversInstalled[]:=Module[{},
	(
		TrueQ[Quiet[FileExistsQ[First[FileNames["arduino*",FileNameJoin[{Environment["windir"],"System32","DriverStore","FileRepository"}]]]]]]
	)	
];


(*manager handle driver is called in the device open sequence of events, and returns a unique uuid for each device that is used as the ihandle*)
ManagerHandleDriver[args___]:=CreateUUID[];



(*downloads and installs the arduino software into $UserBaseDirectory/arduino*)
(*returns the location of the base arduino directory*)
installArduino[]:=Module[
	{
		windowsURL = "http://arduino.cc/download.php?f=/arduino-1.6.3-windows.zip",
		linux32URL = "http://arduino.cc/download.php?f=/arduino-1.6.3-linux32.tar.xz",
		linux64URL = "http://arduino.cc/download.php?f=/arduino-1.6.3-linux64.tar.xz",
		macURL = "http://arduino.cc/download.php?f=/arduino-1.6.3-macosx.zip"
	},
	(
		Switch[$OperatingSystem,
			"Windows",
			(
				tempPrintCell = PrintTemporary["Downloading Arduino IDE from "<>windowsURL];
				tempLocation=URLSave[windowsURL,FileNameJoin[{$TemporaryDirectory,Last@FileNameSplit@windowsURL}],"ConnectTimeout"->5];
				NotebookDelete[tempPrintCell];
				tempPrintCell = PrintTemporary["Extracting archive into "<>$arduinoInternalInstallDirectory];
				(*extract the downloaded archive file and make the arduino ide folder inside $UserBaseDirectory if it doesn't already exist*)
				ExtractArchive[tempLocation,$arduinoInternalInstallDirectory,CreateIntermediateDirectories->True];
				(*also rename the directory to fit with convention*)
				RenameDirectory[FileNameJoin[{$arduinoInternalInstallDirectory,"arduino-1.6.3"}],FileNameJoin[{$arduinoInternalInstallDirectory,"arduino"}]];
				NotebookDelete[tempPrintCell];
				DeleteFile[tempLocation];
				(*before we're done with installing the arduino software, we need to check the driver situation*)
				(*if the drivers are installed, then we don't need to do anything and can just return*)
				(*if the drivers are not installed, opening the serial port will always fail. However, we don't want to be responsible for installing drivers,*)
				(*so we instead open up the path to the driver install utility in the driver, and make the user do it*)
				If[driversInstalled[],
					(*THEN*)
					(*drivers are installed, just return the directory we installed the software to back*)
					(
						Return[FileNameJoin[{$arduinoInternalInstallDirectory,"arduino"}]]
					),
					(*ELSE*)
					(*drivers are not installed, so we need to open up the directory for the user to download the drivers*)
					(
						driverOpen=ChoiceDialog[
							Column[
								{
									TextCell[
										"The Arduino software has been installed but the Arduino USB driver " <>
										"is still required in order to use ArduinoLink. You can install the " <> "driver by running " <> 
										Switch[$SystemID, "Windows-x86-64", "dpinst-amd64.exe", "Windows", "dpinst-x86.exe"]<>".\n\n"
									],
									Style["Note: Administrative privileges will be required to install the driver.", Bold], 
									TextCell[
										"\nPress \"Take me there\" to open the directory containing this driver or \"Cancel\" "<>
										"to return to the Wolfram System."
									]
								}
							],
							{"Take me there" -> True,"Cancel" -> False}, "WindowSize" -> {500, 250}
						];
						If[TrueQ[driverOpen],
							SystemOpen[FileNameJoin[{$arduinoInternalInstallDirectory, "arduino", "drivers"}]]
						];
						(*now return the directory that was just installed*)
						(*raise a message about serial drivers and return $Failed, because the drivers weren't installed*)
						Message[DeviceOpen::serialDrivers];
						Return[$Failed];
					)
				];
			),
			"MacOSX",
			(
				tempPrintCell = PrintTemporary["Downloading Arduino IDE from "<>macURL];
				tempLocation=URLSave[macURL,FileNameJoin[{$TemporaryDirectory,Last@FileNameSplit@macURL}],"ConnectTimeout"->5];
				NotebookDelete[tempPrintCell];
				tempPrintCell = PrintTemporary["Extracting archive into "<>$arduinoInternalInstallDirectory];
				(*extract the downloaded archive file and make the arduino ide folder inside $UserBaseDirectory if it doesn't already exist*)
				ExtractArchive[tempLocation,$arduinoInternalInstallDirectory,CreateIntermediateDirectories->True];
				NotebookDelete[tempPrintCell];
				DeleteFile[tempLocation];
				Return[FileNameJoin[{$arduinoInternalInstallDirectory,"Arduino.app","Contents","Java"}]];
			),
			"Unix",
			(
				(*if we're on the Raspberry Pi, we can't automatically install anything*)
				If[$MachineID === "4801-62204-12672",
					(
						Message[DeviceOpen::raspiAutoInstall];
						Return[$Failed];
					)
				];
			
				(*switch on 64-bit or 32-bit operating sysyem, there are two different archives*)
				tempPrintCell = Switch[$ProcessorType,
					"x86",PrintTemporary["Downloading Arduino IDE from "<>linux32URL],
					"x86-64",PrintTemporary["Downloading Arduino IDE from "<>linux64URL]
				];
				tempLocation=Switch[$ProcessorType,
					"x86",URLSave[linux32URL,FileNameJoin[{$TemporaryDirectory,Last@FileNameSplit@linux32URL}],"ConnectTimeout"->5],
					"x86-64",URLSave[linux64URL,FileNameJoin[{$TemporaryDirectory,Last@FileNameSplit@linux64URL}],"ConnectTimeout"->5]
				];
				NotebookDelete[tempPrintCell];
				(*because we won't use ExtractArchive, and are using the terminal tar command instead, we should make sure the directory doesn't exist first, then we can just rename the extracted archive*)
				If[Not[DirectoryQ[$arduinoInternalInstallDirectory]],
					(*THEN*)
					(*the directory doesn't exist, so we need to create it*)
					(
						CreateDirectory[$arduinoInternalInstallDirectory];
					),
					(*ELSE*)
					(*the directory already exists, so we should delete it first and then make it fresh*)
					(
						DeleteDirectory[$arduinoInternalInstallDirectory,DeleteContents->True];
						CreateDirectory[$arduinoInternalInstallDirectory];
					)
				];
				tempPrintCell = PrintTemporary["Extracting archive into "<>$arduinoInternalInstallDirectory];
				(*ExtractArchive doesn't support xz file format, so use the terminal and do it manually instead*)
				Switch[$ProcessorType,
					"x86",Import["!tar xf "<>FileNameJoin[{$TemporaryDirectory,Last@FileNameSplit@linux32URL}]<>" -C "<>$arduinoInternalInstallDirectory,"Text"],
					"x86-64",Import["!tar xf "<>FileNameJoin[{$TemporaryDirectory,Last@FileNameSplit@linux64URL}]<>" -C "<>$arduinoInternalInstallDirectory,"Text"]
				];
				(*now rename the Directory*)
				RenameDirectory[FileNameJoin[{$arduinoInternalInstallDirectory,"arduino-1.6.3"}],FileNameJoin[{$arduinoInternalInstallDirectory,"arduino"}]];
				NotebookDelete[tempPrintCell];
				DeleteFile[tempLocation];
				Return[FileNameJoin[{$arduinoInternalInstallDirectory,"arduino"}]];
			),
			_,
			(
				(*any other kind of $OperatingSystem, don't do anything*)
				Return[$Failed];
			)
		]
	)
];


(*validArduinoInstallLocation will verify that the given location is a usable installation of the arduino software*)
(*note: searches the $UserBaseDirectory directory first, then searches the user's possible install, such as Program Files, etc.*)
validArduinoInstallLocation[location_String]:=Module[
	{
		avrdude=FileNameJoin[{location,"hardware","tools","avr","bin",
			Switch[$OperatingSystem,
				"Windows","avrdude.exe",
				"MacOSX"|"Unix","avrdude"]}],
		avrBin=FileNameJoin[{location,"hardware","tools","avr","bin",
			Switch[$OperatingSystem,
				"Windows","avr-gcc.exe",
				"MacOSX"|"Unix","avr-gcc"]}],
		libraries=FileNameJoin[{location,"libraries"}],
		coreLibraries=FileNameJoin[{location,"hardware","arduino","avr","cores","arduino"}]
	},
	(
		(*need to check for the three things, avrdude, avr-gcc, and the libraries, and the core libraries folder*)
		valid=FileExistsQ[avrdude]&&FileExistsQ[avrBin]&&FileExistsQ[libraries]&&FileExistsQ[coreLibraries];
		(*if it wasn't valid, and we are on the raspberry pi, perform a different check*)
		If[Not[valid]&&$MachineID === "4801-62204-12672",
			(*THEN*)
			(*we are on a raspberry pi*)
			(
				(*overwrite coreLibraries with the location on the pi*)
				coreLibraries = FileNameJoin[{location,"hardware","arduino","cores","arduino"}];
				(*load the RaspberryPiTools package since we're on Raspberry Pi*)
				Needs["RaspberryPiTools`"];
				(*now check if the aptitude packages are installed as well as if the core package exists exist*)
				If[Not[RaspberryPiTools`Private`aptitudePackageInstalledQ["arduino-core"]],
					(*THEN*)
					(*the package isn't installed*)
					(
						Message[validArduinoInstallLocation::arduinoCore];
						Return[False];
					)
				];
				(*now check that the core libraries exist in the install itself*)
				If[Not[FileExistsQ[coreLibraries]],
					(*THEN*)
					(*the package must be installed to get to here, but the install is missing the libraries*)
					(
						Message[validArduinoInstallLocation::unusableInstall,location];
						Return[False];
					)
				];
				
				(*if we made it here, the install on the pi is good*)
				Return[True];
			)
			(*ELSE*)
			(*we're either not on a raspberry pi, or the install is fine, but regardless we don't have to do anything*)
		];
		(*if it isn't valid, then see if it is an older version of arduino, if so raise a message about 1.6 support*)
		If[Not[valid],
			(*THEN*)
			(*it isn't valid, so check to see if the arduino software exists at all*)
			(
				If[FileExistsQ[location],
					(*THEN*)
					(*then the location exists, check if the version.txt file exists anywhere inside the location*)
					(
						(*search for any version.txt files in the all sub directories of the one passed*)
						versionFiles = FileNames["version.txt",{location},Infinity];
						If[versionFiles=!={},
							(*THEN*)
							(*there is at least one file called version.txt, so read the first one in the list in and check if the string is equal to "1.6"~~*)
							(
								(*open up the file as a binary format, read the entire file as one string, then close the file*)
								versionFile = OpenRead[First@versionFiles,BinaryFormat->True];
								version = ReadString[versionFile];
								Close[versionFile];
								(*just make sure that the version is 1.6, don't worry about minor releases*)
								If[StringMatchQ[version,"1.6"~~___],
									(*THEN*)
									(*somehow the user has a 1.6 installed, but it doesn't have the right files, so it's probably 1.6.2*)
									(
										If[version==="1.6.2",
											(*THEN*)
											(*issue a message specific to 1.6.2*)
											(
												Message[validArduinoInstallLocation::version162,location];
												Return[False];
											),
											(*ELSE*)
											(*something wierd is going on, so raise a message about the install being unusable*)
											(
												Message[validArduinoInstallLocation::unusableInstall,location];
												Return[False];
											)
										];
									),
									(*ELSE*)
									(*the user has a different version, so raise a message*)
									(
										Message[validArduinoInstallLocation::version,location,StringTake[version,5],"1.6.0"];
										Return[False];
									)
								]
							),
							(*ELSE*)
							(*no version.txt file was found, so again just return False*)
							(
								Return[False];
							)
						];
						
					),
					(*ELSE*)
					(*the location doesn't even exist at all, so definitely return false*)
					(
						Return[False];
					)
				]
			),
			(*ELSE*)
			(*it is valid, just return True*)
			(
				Return[True];
			)
		]
	)
];



(*arduinoSoftwarePresent checks a few different locations for the arduino IDE software*)
arduinoSoftwarePresent[]:=Module[
	{
		(*locations to check*)
		macLocation=FileNameJoin[{"/Applications","Arduino.app","Contents","Java"}],
		programFilesLocation = FileNameJoin[
			{
				(*this is the current drive*)
				FileNameSplit[$InstallationDirectory][[1]],
				"Program Files (x86)",
				"Arduino"
			}],
		linuxLocation=FileNameJoin[{"/usr","share","arduino"}],
		(*for mac, can't just check the base directory, need to check the app folder inside that*)
		internalLocation=FileNameJoin[{$arduinoInternalInstallDirectory,"arduino"}],
		macInternalLocation = FileNameJoin[{$arduinoInternalInstallDirectory,"Arduino.app","Contents","Java"}]
	},
	(
		(*this directory is different on Mac, on mac it is inside the .app Contents folder, then inside the Java folder*)
		If[$OperatingSystem==="MacOSX",
			(*THEN*)
			(*this is a mac, so check the mac locations*)
			If[FileExistsQ[macInternalLocation]&&validArduinoInstallLocation[macInternalLocation],
				Return[True];
			],
			(*ELSE*)
			(*check the normal locations*)
			If[FileExistsQ[internalLocation]&&validArduinoInstallLocation[internalLocation],
				Return[True];
			]
		];
		Switch[$OperatingSystem,
			"MacOSX",
			(
				(*on mac check the Arduino.app folder*)
				If[FileExistsQ[macLocation],
					(*THEN*)
					(*it exists, so return wether or not it is a valid location*)
					(
						Return[validArduinoInstallLocation[macLocation]];
					),
					(*ELSE*)
					(*it doesn't exist, so return False*)
					(
						Return[False];
					)
				]
			),
			"Windows",
			(
				(*on windows check the Arduino program files directory folder*)
				If[FileExistsQ[programFilesLocation],
					(*THEN*)
					(*it exists, so return wether or not it is a valid location*)
					(
						Return[validArduinoInstallLocation[programFilesLocation]];
					),
					(*ELSE*)
					(*it doesn't exist, so return False*)
					(
						Return[False];
					)
				]
			),
			"Unix",
			(
				(*this is the confirmed location from apt-get install on debian and ubuntu*)
				(*on linux, check the Arduino usr/share/arduino directory folder*)
				If[FileExistsQ[linuxLocation],
					(*THEN*)
					(*it exists, so return wether or not it is a valid location*)
					(
						Return[validArduinoInstallLocation[linuxLocation]];
					),
					(*ELSE*)
					(*it doesn't exist, so return False*)
					(
						Return[False];
					)
				]
			),
			_,
			(*not sure what other $OperatingSystem this would be, so just return False*)
			(
				Return[False];
			)
		]
	)
];


(*getArduinoSoftwareLocation does the exact same thing as arduinoSoftwarePresent, except it returns the location*) 
(*instead of a boolean*)
getArduinoSoftwareLocation[]:=Module[
	{
		macLocation=FileNameJoin[{"/Applications","Arduino.app","Contents","Java"}],
		windowsLocation = FileNameJoin[
			{
				(*this is the current drive*)
				FileNameSplit[$InstallationDirectory][[1]],
				"Program Files (x86)",
				"Arduino"
			}],
		linuxLocation=FileNameJoin[{"/usr","share","arduino"}],
		internalLocation=FileNameJoin[{$arduinoInternalInstallDirectory,"arduino"}],
		macInternalLocation = FileNameJoin[{$arduinoInternalInstallDirectory,"Arduino.app","Contents","Java"}]
	},
	(
		If[$OperatingSystem==="MacOSX",
			(*THEN*)
			(*check the mac internal location inside the Arduino.app first before checking anything else*)
			If[FileExistsQ[macInternalLocation]&&validArduinoInstallLocation[macInternalLocation],
				(*THEN*)
				(*the internal applications data folder exists on mac and it is valid, so use that one*)
				Return[macInternalLocation];
				(*ELSE*)
				(*go on to check the other locations*)
			],
			(*ELSE*)
			(*check the normal location first*)
			(
				If[FileExistsQ[internalLocation]&&validArduinoInstallLocation[internalLocation],
					(*THEN*)
					(*it does exist in the Mac internal directory, so return that*)
					(
						Return[internalLocation];
					)
					(*ELSE*)
					(*go on to check the other locations*)
				]
			)
		];
		Switch[$OperatingSystem,
			"MacOSX",
			(
				(*on mac check the Arduino.app folder*)
				If[FileExistsQ[macLocation],
					(*THEN*)
					(*it exists, so return wether or not it is a valid location*)
					(
						If[validArduinoInstallLocation[macLocation],
							(*THEN*)
							(*it is valid, return that directory*)
							(
								Return[macLocation];
							),
							(*ELSE*)
							(*it doesn't exist, return None*)
							(
								Return[None];
							)
						]
					),
					(*ELSE*)
					(*it doesn't exist, so return False*)
					(
						Return[None];
					)
				]
			),
			"Windows",
			(
				(*on windows check the Arduino program files directory folder*)
				If[FileExistsQ[windowsLocation],
					(*THEN*)
					(*it exists, so return wether or not it is a valid location*)
					(
						If[validArduinoInstallLocation[windowsLocation],
							(*THEN*)
							(*it is valid, return that directory*)
							(
								Return[windowsLocation];
							),
							(*ELSE*)
							(*it isn't valid, return None*)
							(
								Return[None];
							)
						];
					),
					(*ELSE*)
					(*it doesn't exist, so return False*)
					(
						Return[None];
					)
				]
			),
			"Unix",
			(
				(*this is the confirmed location from apt-get install on debian and ubuntu*)
				(*on linux, check the Arduino usr/share/arduino directory folder*)
				If[FileExistsQ[linuxLocation],
					(*THEN*)
					(*it exists, so return wether or not it is a valid location*)
					(
						If[validArduinoInstallLocation[linuxLocation],
							(*THEN*)
							(*the location is valid, return that directory*)
							(
								Return[linuxLocation];
							),
							(*ELSE*)
							(*it isn't valid, return None*)
							(
								Return[None];
							)
						];
					),
					(*ELSE*)
					(*it doesn't exist, so return None*)
					(
						Return[None];
					)
				]
			),
			_,
			(*not sure what this case is, but return None for any other kind of $OperatingSystem*)
			(
				Return[None];
			)
		]
	)
];


ArduinoOpenDriver[ihandle_,___,OptionsPattern[]]:=Module[{},
	(
		Message[DeviceOpen::port];
		Return[$Failed];
	)
];


(*ArduinoOpenDriver will make sure that the arduino software is installed, and if it isn't,
it will prompt the user to install it.*)
Options[ArduinoOpenDriver]=
	{
		"InitialUpload"->True
	};
ArduinoOpenDriver[ihandle_,serialPort_String,OptionsPattern[]]:=Module[
	{
		defaultDeviceState=<|
			"D0"-><|"Direction"->"Reserved","LastWriteValue"->"Reserved","PWM"->"Reserved","ADC"->"Reserved","LastReadValue"->"Reserved"|>,
			"D1"-><|"Direction"->"Reserved","LastWriteValue"->"Reserved","PWM"->"Reserved","ADC"->"Reserved","LastReadValue"->"Reserved"|>,
			"D2"-><|"Direction"->Default,"LastWriteValue"->None,"PWM"->False,"ADC"->False,"LastReadValue"->None|>,
			"D3"-><|"Direction"->Default,"LastWriteValue"->None,"PWM"->True,"ADC"->False,"LastReadValue"->None|>,
			"D4"-><|"Direction"->Default,"LastWriteValue"->None,"PWM"->False,"ADC"->False,"LastReadValue"->None|>,
			"D5"-><|"Direction"->Default,"LastWriteValue"->None,"PWM"->True,"ADC"->False,"LastReadValue"->None|>,
			"D6"-><|"Direction"->Default,"LastWriteValue"->None,"PWM"->True,"ADC"->False,"LastReadValue"->None|>,
			"D7"-><|"Direction"->Default,"LastWriteValue"->None,"PWM"->False,"ADC"->False,"LastReadValue"->None|>,
			"D8"-><|"Direction"->Default,"LastWriteValue"->None,"PWM"->False,"ADC"->False,"LastReadValue"->None|>,
			"D9"-><|"Direction"->Default,"LastWriteValue"->None,"PWM"->True,"ADC"->False,"LastReadValue"->None|>,
			"D10"-><|"Direction"->Default,"LastWriteValue"->None,"PWM"->True,"ADC"->False,"LastReadValue"->None|>,
			"D11"-><|"Direction"->Default,"LastWriteValue"->None,"PWM"->True,"ADC"->False,"LastReadValue"->None|>,
			"D12"-><|"Direction"->Default,"LastWriteValue"->None,"PWM"->False,"ADC"->False,"LastReadValue"->None|>,
			"D13"-><|"Direction"->Default,"LastWriteValue"->None,"PWM"->False,"ADC"->False,"LastReadValue"->None|>,
			"A0"-><|"Direction"->Default,"LastWriteValue"->None,"PWM"->False,"ADC"->True,"LastReadValue"->None|>,
			"A1"-><|"Direction"->Default,"LastWriteValue"->None,"PWM"->False,"ADC"->True,"LastReadValue"->None|>,
			"A2"-><|"Direction"->Default,"LastWriteValue"->None,"PWM"->False,"ADC"->True,"LastReadValue"->None|>,
			"A3"-><|"Direction"->Default,"LastWriteValue"->None,"PWM"->False,"ADC"->True,"LastReadValue"->None|>,
			"A4"-><|"Direction"->Default,"LastWriteValue"->None,"PWM"->False,"ADC"->True,"LastReadValue"->None|>,
			"A5"-><|"Direction"->Default,"LastWriteValue"->None,"PWM"->False,"ADC"->True,"LastReadValue"->None|>
		|>
	},
	(	
		If[arduinoSoftwarePresent[],
			(*THEN*)
			(*software is present, so save the main location in a variable which the property will gets*)
			(
				$arduinoInstallLocation = getArduinoSoftwareLocation[];
				
			),
			(*ELSE*)
			(*it's not present, so prompt the user to install it*)
			(
				install = If[$FrontEnd =!= Null,
						(*THEN*)
						(*there is a front end, so display the message normally*)
						ChoiceDialog[
							"The Arduino software package was not found on your "<>
							"computer.\n\nArduinoLink requires the Arduino software package, "<>
							"portions of which may be under separate license.\n\nBy proceeding, "<>
							"you understand and agree that Arduino is a separate software package "<>
							"with separate licensing. \n\nNote:  You may manually install the "<>
							"Arduino software and provide the install directory with the Device "<>
							"property \"ArduinoInstallLocation\".", 
							{"Install" -> True,"Do Not Install" -> False}, 
							WindowTitle -> "Arduino Software Install", WindowFloating -> True, 
							WindowSize -> {700, 225}, WindowFrame -> "ModalDialog"
						],
						(*ELSE*)
						(*there isn't a front end, so we have to print off the message, then take input as to whether or not the user accepts*)
						(
							(*on the raspberry pi, we can't automatically install the software for the user, so just issue a message on how the user*)
							(*can install it*)
							If[$MachineID === "4801-62204-12672",
								(*THEN*)
								(*we're on a raspberry pi, so issue a message and return False*)
								(
									Message[DeviceOpen::raspiAutoInstall];
									False
								),
								(*ELSE*)
								(*we're not on a pi, and can actually install the software*)
								(
									(*use WriteString to stdout so that it is formatted properly for whatever window size the user has*)
									Print["The Arduino software package was not found on your computer."<>
										"ArduinoLink requires the Arduino software package, portions of which may be under separate license."<>
										"By proceeding, you understand and agree that Arduino is a separate software package with separate licensing."<>
										" Note:  You may manually install the Arduino software and provide the install directory with the Device property"<>
										" \"ArduinoInstallLocation\".\nWould you like to install the Arduino software now?\nPlease enter True or False\n\n"];
									While[True,
										(
											(*now take the user's input as a string with InputString*)
											input = InputString[];
											If[MemberQ[{"true","yes","false","no"},ToLowerCase[input]],
												(*THEN*)
												(*input is good, we can exit*)
												Break[],
												(*ELSE*)
												(*input is invalid, continue again, but issue a message first*)
												Message[DeviceOpen::invalidInput]
											]
										)
									];
									MemberQ[{"true","yes"},ToLowerCase[input]]
								)
							]
						)
					];
				If[TrueQ[install],
					(*THEN*)
					(*user wants the software installed, so run the install subroutine*)
					(
						installLocation=installArduino[];
						If[installLocation===$Failed,
							(*THEN*)
							(*it failed to install, so return $Failed*)
							Return[$Failed],
							(*ELSE*)
							(*it didn't fail, so set it to the $arduinoInstallLocation*)
							$arduinoInstallLocation=installLocation
						]
					),
					(*ELSE*)
					(*user doesn't want the software installed, so set it to none*)
					(
						$arduinoInstallLocation = None;
					)
				]
			)
		];
		$DeviceStates[ihandle]=<||>;
		$DeviceStates[ihandle,"PinConfigurations"]=defaultDeviceState;
		$DeviceStates[ihandle,"SerialPort"]=serialPort;
		$DeviceStates[ihandle,"UploadOnOpen"]=OptionValue["InitialUpload"];
		(*finally, before actually opening the port, issue a reset command to the device*)
		(*note that doing this now before the DeviceFramework opens up the device can save almost 5 seconds on calls to DeviceOpen on windows platforms*)
		(*if we did this in the pre configure stage, then we would have to close the serial port through the device framework*)
		(*which can take up to 5 seconds on windows*)
		(*check if we are on a Pi, as AVRDUDE locations are different on the pi*)
		If[$MachineID === "4801-62204-12672",
			(*THEN*)
			(*we need to specify a different location for avrdude*)
			(
				avrdudeLocation = {"/usr","bin","avrdude"};
				avrdudeConfLocation = {"/etc", "avrdude.conf"};
			),
			(*ELSE*)
			(*just use defaults*)
			(
				avrdudeLocation = Default;
				avrdudeConfLocation = Default;
			)
		];
		arduinoReset[
			serialPort,
			$arduinoInstallLocation,
			"AVRDUDELocation"->avrdudeLocation,
			"AVRDUDEConfLocation"->avrdudeConfLocation
		];
		portObject=DeviceFramework`DeviceDriverOption["Firmata","OpenFunction"][Null,serialPort,"BaudRate"->115200];
		portObject
	)
];


(*TODO: re-add the pre configure function to upload the sketch if requested on initially opening the device*)
ArduinoPreConfigureDriver[dev_]:=Module[
	{
		ihandle = DeviceFramework`DeviceManagerHandle[dev],
		dhandle = DeviceFramework`DeviceHandle[dev]
	},
	(
		If[TrueQ[$DeviceStates[ihandle]["UploadOnOpen"]],
			(*THEN*)
			(*we need to upload the sketch to the device before returning to the user*)
			(
				DeviceConfigure[dev,"Upload"];
			)
			(*ELSE*)
			(*we don't need to do anything*)
		];
		All
	)
];


ArduinoCommunityLogo[{ihandle_,dhandle_},___,OptionsPattern[]]:=Module[{},
	(
		Import[PacletResource["ArduinoLink","Logo"]]
	)
];

Options[ArduinoWriteDriver]={};
ArduinoWriteDriver[{ihandle_,dhandle_},pin_->value_,OptionsPattern[]]:=Module[
	{
		writeValue=value
	},
	(
		If[MemberQ[arduinoUnoPins,pin],
			(*THEN*)
			(*check if the pin can be written to*)
			If[okayToWrite[$DeviceStates[ihandle]["PinConfigurations"][arduinoPinToKey[pin]]["Direction"]],
				(*THEN*)
				(*it can be written to, check if the pin is PWM*)
				(
					If[MemberQ[arduinoUnoPWMPins,pin],
						(*THEN*)
						(*the pin is a pwm pin, so do check if the value is a compatible unit first*)
						(
							If[CompatibleUnitQ[value,"Volts"],
								(*THEN*)
								(*the value is an actual unit that can be converted to volts, so convert it and check the magnitude*)
								(
									writeValue = UnitConvert[value,"Volts"];
									If[QuantityMagnitude[writeValue]>=0 && QuantityMagnitude[writeValue] <=5,
										(*THEN*)
										(*the value is within the range, so just convert it to the bits*)
										(
											arduinoAnalogWrite[{ihandle,dhandle},pin,Floor[QuantityMagnitude[writeValue]/5*255]];
										),
										(*ELSE*)
										(*the value is not within the range, so raise a message and normalize it*)
										(
											Message[DeviceWrite::voltMagnitude,value,
												writeValue=Quantity[5*booleanize[QuantityMagnitude[writeValue]/5],"Volts"]];
											arduinoDigitalWrite[{ihandle,dhandle},pin,Floor[QuantityMagnitude[writeValue]/5]];
										)
									]
								),
								(*ELSE*)
								(*the value is not a unit, so check if it is numeric*)
								(
									If[NumericQ[value],
										(*THEN*)
										(*the value can be normalized, so check if it is between 0 and 1*)
										(
											If[value >0 && value <1,
												(*THEN*)
												(*the value is already within the correct range, so convert it, then write it*)
												(
													arduinoAnalogWrite[{ihandle,dhandle},pin,Floor[value*255]];
												),
												(*ELSE*)
												(*check if the value is equal to 1 or 0*)
												If[value ==0 || value ==1,
													(*THEN*)
													(*the value is boolean, so use arduinoDigitalWrite*)
													(
														arduinoDigitalWrite[{ihandle,dhandle},pin,Round[value]];
													),
													(*ELSE*)
													(*the value is not within the correct range, so raise a message and normalize it*)
													(
														Message[DeviceWrite::pwmresolution,value,writeValue=pwmize[255*writeValue]/255];
														arduinoDigitalWrite[{ihandle,dhandle},pin,writeValue];
													)
												]
											]
										),
										(*ELSE*)
										(*the value cannot be normalized, raise a message and return $Failed*)
										(
											Message[DeviceWrite::numericValue,value];
											Return[$Failed];
										)
									]
								)
							];
						),
						(*ELSE*)
						(*the pin is a normal digital out pin, so check if the value is 1 or 0*)
						(
							If[value===1||value===0,
								(*THEN*)
								(*the pin is good to write to*)
								(
									arduinoDigitalWrite[{ihandle,dhandle},pin,value];
								),
								(*ELSE*)
								(*the pin is not good to write to, so see if the value can be normalized*)
								If[NumericQ[value],
									(*THEN*)
									(*the value is numeric, and it can be normalized*)
									(
										Message[DeviceWrite::nonBooleanWrite,value,booleanize[value]];
										arduinoDigitalWrite[{ihandle,dhandle},numericalPin[pin],writeValue=booleanize[value]];
									),
									(*ELSE*)
									(*the value is not numeric and can't be normalized, so raise message and return $Failed*)
									(
										Message[DeviceWrite::numericValue,value];
										Return[$Failed];
									)
								]
							]
						)
					];
					(*finally update the configuration association with the write value and the direction*)
					$DeviceStates[ihandle]["PinConfigurations"][arduinoPinToKey[pin]]["LastWriteValue"]=(DateObject[]->writeValue);
					If[$DeviceStates[ihandle]["PinConfigurations"][arduinoPinToKey[pin]]["Direction"]===Default||$DeviceStates[ihandle]["PinConfigurations"][arduinoPinToKey[pin]]["Direction"]==="SoftInput",
						(*THEN*)
						(*the pin hasn't been configured or written to yet, or it was a soft input previously, and we just wrote to it,*)
						(*so change it to "SoftOutput"*)
						$DeviceStates[ihandle]["PinConfigurations"][arduinoPinToKey[pin]]["Direction"]="SoftOutput";
						(*ELSE*)
						(*to have written to it and have it not be default or "SoftInput", it must have been "HardOutput", so don't change it*)
					]
				),
				(*ELSE*)
				(*the pin has already been configured as a HardInput, so we can't write to it*)
				(
					Message[DeviceWrite::config,pin];
					Return[$Failed];
				)
			],
			(*ELSE*)
			(*the pin is not a valid arduino uno pin, check if it is a serial communication pin and raise a seperate message for that*)
			(
				If[pin===0||pin===1,
					(*THEN*)
					(*the pins are reserved for serial, raise a special message for that*)
					(
						Message[DeviceWrite::serialPin];
						Return[$Failed];
					),
					(*ELSE*)
					(*the pin just isn't a pin on the arduino uno at all*)
					(
						Message[DeviceWrite::invalidPin,pin];
						Return[$Failed];
					)
				]
				
			)
		]
	)
]


(*the following are for association or lists of rules*)
ArduinoWriteDriver[{ihandle_,dhandle_},pins_List,OptionsPattern[]]:=Module[{},
	(
		ArduinoWriteDriver[{ihandle,dhandle},#]&/@pins
	)
];

ArduinoWriteDriver[{ihandle_,dhandle_},pins_Association,OptionsPattern[]]:=Module[{},
	(
		ArduinoWriteDriver[{ihandle,dhandle},#]&/@Normal[pins]
	)
];


(*note this function does not do any checking of the pin or value, so that must be done previously*)
arduinoAnalogWrite[{ihandle_,dhandle_},pin_,value_]:=Module[{},
	(
		DeviceFramework`DeviceDriverOption["Firmata","WriteFunction"][{ihandle,dhandle},{pin,value},"WriteMode"->"Analog"]
	)
];


arduinoDigitalWrite[{ihandle_,dhandle_},pin_,value_]:=Module[
	{
		valueBitMask=Table[0,{8}],
		port=pinToPort[pin]
	},
	(
		(*any pin can be a digital output, so we don't have to do any pin checking*)
		Switch[port,
			0, (*PORT 0 - digital pins 0-7*)
				(
					valueBitMask=(
						If[okayToWrite[$DeviceStates[ihandle]["PinConfigurations"][arduinoPinToKey[#]]["Direction"]],
							(*THEN*)
							(*the pin is okay to write to*)
							If[$DeviceStates[ihandle]["PinConfigurations"][arduinoPinToKey[#]]["LastWriteValue"]===None,
								(*THEN*)
								(*the pin has either never been written to before, so use None as the value*)
								None,
								(*ELSE*)
								(*it has been written to before, write whatever value was last written there as the value*)
								$DeviceStates[ihandle]["PinConfigurations"][arduinoPinToKey[#]]["LastWriteValue"][[2]]
							],
							(*ELSE*)
							(*the pin isn't okay to write to, so use a zero*)
							0
						]
					)&/@{2,3,4,5,6,7};
					valueBitMask=Flatten[Prepend[valueBitMask,{0,0}]];
				),
			1,(*PORT 1 - digital pins 8-13*)
				(
					valueBitMask=(
						If[okayToWrite[$DeviceStates[ihandle]["PinConfigurations"][arduinoPinToKey[#]]["Direction"]],
							(*THEN*)
							(*the pin is okay to write to*)
							If[$DeviceStates[ihandle]["PinConfigurations"][arduinoPinToKey[#]]["LastWriteValue"]===None,
								(*THEN*)
								(*the pin has either never been written to before, so use None as the value*)
								None,
								(*ELSE*)
								(*it has been written to before, write whatever value was last written there as the value*)
								$DeviceStates[ihandle]["PinConfigurations"][arduinoPinToKey[#]]["LastWriteValue"][[2]]
							],
							(*ELSE*)
							(*the pin isn't okay to write to, so use a zero*)
							0
						]
					)&/@{8,9,10,11,12,13};
					valueBitMask=Flatten[Append[valueBitMask,{0,0}]];
				),
			2, (*PORT 2 - analog pins*)
				(
					valueBitMask=(
						If[okayToWrite[$DeviceStates[ihandle]["PinConfigurations"][arduinoPinToKey[#]]["Direction"]],
							(*THEN*)
							(*the pin is okay to write to*)
							If[$DeviceStates[ihandle]["PinConfigurations"][arduinoPinToKey[#]]["LastWriteValue"]===None,
								(*THEN*)
								(*the pin has either never been written to before, so use None as the value*)
								None,
								(*ELSE*)
								(*it has been written to before, write whatever value was last written there as the value*)
								$DeviceStates[ihandle]["PinConfigurations"][arduinoPinToKey[#]]["LastWriteValue"][[2]]
							],
							(*ELSE*)
							(*the pin isn't okay to write to, so use a zero*)
							0
						]
					)&/@{14,15,16,17,18,19};
					valueBitMask=Flatten[Append[valueBitMask,{0,0}]];
				),
			_,(*all other ports, return $Failed*)
			Return[$Failed]
		];
		(*None is for pins that have never been written to, so put those as 0*)
		valueBitMask = valueBitMask/.None->0;
		(*now set the pin we want to configure to have the value the user requested*)
		If[Head[pin]=!=String&&Not[MemberQ[Range[14,19],pin]],
			(*THEN*)
			(*the pin number is a normal integer, use that*)
			valueBitMask=ReplacePart[valueBitMask,(Mod[pin,8]+1)->value],
			(*ELSE*)
			(*the pin number is a string, so convert it and use that instead*)
			valueBitMask=ReplacePart[valueBitMask,Mod[analogNumericPin[pin]+1,8]->value]
		];
		(*if any of the pins previously had values that weren't 1 or 0, make those zero*)
		valueBitMask = If[#===0||#===1,#,0]&/@valueBitMask;
		(*reverse the bit mask and make it into a binary number*)
		finalPortValue = FromDigits[Reverse[valueBitMask],2];
		DeviceFramework`DeviceDriverOption["Firmata","WriteFunction"][{ihandle,dhandle},{port,finalPortValue},
			"PinAddressing"->"Port",
			"WriteMode"->"Digital",
			(*if the pin we are writing to is a pwm pin, then we are digitally writing to a pwm pin,*)
			(* so we need to enforce disabling of the pwm timer on that pin with a hidden bit in the firmata packet*)
			If[MemberQ[arduinoUnoPWMPins,pin],
				(*THEN*)
				(*then it's a member, which one*)
				Which[
					(*pack the first hidden bit if it is pin 3 or pin 9*)
					pin === 3 || pin === 9,"HiddenBits"->FromDigits["1",2],
					(*pack the second hidden bit if it is pin 5 or pin 10*)
					pin === 5 || pin === 10,"HiddenBits"->FromDigits["10",2],
					(*pack the third hidden bit if it is pin 6 or pin 11*)
					pin === 6 || pin === 11, "HiddenBits"->FromDigits["100",2],
					(*default case of none*)
					True, "HiddenBits"->None
				],
				(*ELSE*)
				(*it's not a member so set this option to None*)
				"HiddenBits"->None
			]
		];
	)
];


Options[ArduinoReadDriver]=
{
	"ReadMode"->Automatic,
	"ReturnFunction"->Automatic
};
(*this is for debugging, it will read the entire buffer for the serial port*)
ArduinoReadDriver[{ihandle_,dhandle_},"Raw",OptionsPattern[]]:=Module[{},
	(
		DeviceFramework`DeviceDriverOption["Firmata","ReadFunction"][{ihandle,dhandle},"raw"]
	)
];

ArduinoReadDriver[{ihandle_,dhandle_},OptionsPattern[]]:=Module[{},
	(
		If[OptionValue["ReadMode"]==="Analog",
			(*THEN*)
			(*the user specified an analog read of Arduino, so just return the analog pins*)
			(
				Return[
					ArduinoReadDriver[
						{ihandle,dhandle},
						(*this gets a list of all valid arduino pins (but not duplicates)*)
						{"A0","A1","A2","A3","A4","A5"},
						Sequence[#->OptionValue[#]&/@Options[ArduinoReadDriver][[All,1]]]
					]
				]
			),
			(*ELSE*)
			(*didn't specify analog read mode, so we can safely read from all pins*)
			(
				Return[
					ArduinoReadDriver[
						{ihandle,dhandle},
						(*this gets a list of all valid arduino pins (but not duplicates)*)
						Join[Complement[arduinoUnoPins,arduinoUnoAnalogPins],{"A0","A1","A2","A3","A4","A5"}],
						Sequence[#->OptionValue[#]&/@Options[ArduinoReadDriver][[All,1]]]
					]
				]
			)
		]
		
	)
]


ArduinoReadDriver[{ihandle_,dhandle_},pin_,OptionsPattern[]]:=Module[{},
	(
		(*first check if the pin exists at all*)
		If[MemberQ[arduinoUnoPins,pin],
			(*THEN*)
			(*it exists, now check if we can read from it*)
			If[okayToRead[$DeviceStates[ihandle]["PinConfigurations"][arduinoPinToKey[pin]]["Direction"]],
				(*THEN*)
				(*we are good to read*)
				(
					value=DeviceFramework`DeviceDriverOption["Firmata","ReadFunction"][{ihandle,dhandle},pin,
						"ReadMode"->OptionValue["ReadMode"],
						(*this check is if we are reading from a pwm pin, then we need to specify whether to the *)
						(*arduino to turn off PWM on that pin, else the PWM timer will override the read and will just return 0*)
						If[MemberQ[arduinoUnoPWMPins,pin],
							(*THEN*)
							(*then it's a member, which one*)
							Which[
								(*pack the first hidden bit if it is pin 3 or pin 9*)
								pin === 3 || pin === 9,"HiddenBits"->FromDigits["1",2],
								(*pack the second hidden bit if it is pin 5 or pin 10*)
								pin === 5 || pin === 10,"HiddenBits"->FromDigits["10",2],
								(*pack the third hidden bit if it is pin 6 or pin 11*)
								pin === 6 || pin === 11, "HiddenBits"->FromDigits["100",2],
								(*default case of none*)
								True, "HiddenBits"->None
							],
							(*ELSE*)
							(*it's not a member so set this option to None*)
							"HiddenBits"->None
						]
						];
					(*if value is $Failed, don't do anything else and just return $Failed*)
					If[value===$Failed,
						Return[$Failed]
					]
				),
				(*ELSE*)
				(*user hard configured this pin to be an output, so we can't read from it*)
				(
					Message[DeviceRead::config,pin];
					Return[$Failed];
				)
			];
			Switch[OptionValue["ReturnFunction"],
				Automatic,
				(
					(*for automatic return function, this will depend on the type of pin*)
					Switch[OptionValue["ReadMode"],
						Automatic,
						(
							(*the behavior for automatic read mode is that if the pin is an analog pin, an analog read was performed, if it is digital,
							we can just return the value*)
							If[MemberQ[arduinoUnoAnalogPins,pin],
								(*then the pin is an analog pin, and we should convert it to a voltage*)
								(
									$DeviceStates[ihandle]["PinConfigurations"][arduinoPinToKey[pin]]["LastReadValue"]=DateObject[]->Quantity[N[5*value/1023,6],"Volts"];
									Return[Quantity[N[5*value/1023,6],"Volts"]]
								),
								(*ELSE*)
								(*the pin isn't an analog pin, so just return the pin*)
								(
									$DeviceStates[ihandle]["PinConfigurations"][arduinoPinToKey[pin]]["LastReadValue"]=DateObject[]->value;
									Return[value];
								)
							]
						),
						"Analog",
						(
							(*the pin must have been an analog pin, or else we are going to convert $Failed*)
							$DeviceStates[ihandle]["PinConfigurations"][arduinoPinToKey[pin]]["LastReadValue"]=DateObject[]->Quantity[N[5*value/1023,6],"Volts"];
							Return[Quantity[N[5*value/1023,6],"Volts"]];
						),
						"Digital",
						(
							(*just return normal value*)
							$DeviceStates[ihandle]["PinConfigurations"][arduinoPinToKey[pin]]["LastReadValue"]=DateObject[]->value;
							Return[value];
						),
						_,
						(
							(*anything else, just return the value normally*)
							$DeviceStates[ihandle]["PinConfigurations"][arduinoPinToKey[pin]]["LastReadValue"]=DateObject[]->value;
							Return[value];
						)
					]
				),
				_,
				(
					(*the user specified the function, so try applying their function to it*)
					$DeviceStates[ihandle]["PinConfigurations"][arduinoPinToKey[pin]]["LastReadValue"]=DateObject[]->OptionValue["ReturnFunction"][value];
					Return[OptionValue["ReturnFunction"][value]]
				)
			],
			(*ELSE*)
			(*the pin doesn't exist, issue a message and return $Failed*)
			(
				Message[DeviceRead::invalidPin,pin];
				Return[$Failed];
			)
		]
	)
];


ArduinoReadDriver[{ihandle_,dhandle_},pins_List,OptionsPattern[]]:=Module[{},
	(
		Association[
			Function[pin,
				(pin->ArduinoReadDriver[{ihandle,dhandle},pin,"ReadMode"->OptionValue["ReadMode"],"ReturnFunction"->OptionValue["ReturnFunction"]])]/@pins]
	)
];



(*this is to check whether or not there are any scheduled tasks running on the arduino at any given time*)
$scheduledTaskRunning=<|"Running"->False,"startTime"->AbsoluteTime[],"endTime"->AbsoluteTime[]|>;


(*the execute driver will basically just use the internal association for functions currently uploaded to 
the arduino to build the packet to send to Firmata, noting to Firmata if there are return packets expected,
if there are those will be interpreted and sent back to the user*)
Options[ArduinoExecute]={};
ArduinoExecute[{ihandle_,dhandle_},"DeleteTask",functionName_,OptionsPattern[]]:=Module[{},
	(
		(*first confirm that the function requested actually exists*)
		If[MemberQ[Keys[$functionCalls],functionName],
			(*THEN*)
			(*the function exists, so send the delete request*)
			(
				(*TODO: in the sketch, change the delete task specified from 1 back to 6*)
				packet={FromDigits["F0",16],FromDigits["01",16],$functionCalls[functionName][[1,3]],FromDigits["f7",16]};
				DeviceFramework`DeviceDriverOption["Firmata","ExecuteFunction"][{ihandle,dhandle},packet];
				(*lastly, update the scheduledTask association to not be running*)
				$scheduledTaskRunning["Running"]=False;
				$scheduledTaskRunning["startTime"]=AbsoluteTime[];
				$scheduledTaskRunning["endTime"]=AbsoluteTime[];
			),
			(*ELSE*)
			(*the function doesn't exist, so issue a message and return $Failed*)
			(
				Message[DeviceExecute::funcName,functionName];
				Return[$Failed];
			)
		]
	)
];


(*Software reset uses AVRDUDE to issue a null command to the device, effectively resetting the device*)
ArduinoExecute[{ihandle_,dhandle_},"SoftwareReset",OptionsPattern[]]:=Module[{},
	(*reset the arduino before returning it to the user*)
	(*check if we are on a Pi*)
	If[$MachineID === "4801-62204-12672",
		(*THEN*)
		(*we need to specify a different location for avrdude*)
		(
			avrdudeLocation = {"/usr","bin","avrdude"};
			avrdudeConfLocation = {"/etc", "avrdude.conf"};
		),
		(*ELSE*)
		(*just use defaults*)
		(
			avrdudeLocation = Default;
			avrdudeConfLocation = Default;
		)
	];
	(*before we reset the device, we have to close the serial port so it can be accessed by AVRDUDE*)
	DeviceFramework`DeviceDriverOption["Firmata","CloseFunction"][{ihandle,dhandle}];
	arduinoReset[
		$DeviceStates[ihandle,"SerialPort"],
		$arduinoInstallLocation,
		"AVRDUDELocation"->avrdudeLocation,
		"AVRDUDEConfLocation"->avrdudeConfLocation
	];
	(*now we re-open the serial port*)
	DeviceFramework`DeviceDriverOption["Firmata","OpenFunction"][{ihandle,dhandle},$DeviceStates[ihandle]["SerialPort"],"BaudRate"->115200];
	
]


ArduinoScheduleExecute[{ihandle_,dhandle_},functionTask_,OptionsPattern[]]:=Module[
	{
		functionName=functionTask[[1]],
		args=functionTask[[2]],
		timespec=functionTask[[3]],
		funcID=Quiet[$functionCalls[functionTask[[1]]][[1,3]]],
		function=Quiet[$functionCalls[functionTask[[1]]][[2]]]
	},
	(
		(*first confirm that the function requested actually exists*)
		If[MemberQ[Keys[$functionCalls],functionName],
			(*THEN*)
			(*the function does exist*)
			(*TODO: implement the grabber functionality with a ScheduledTask on the Mathematica side for the return value of arduino scheduled tasks*)
			(*TODO: expand timespec to all cases the arduino supports*)
			(*right now it just ignores it and doesn't send them if it doesn't have to*)
			(*make sure that a scheduled task isn't already running*)
			If[TrueQ[$scheduledTaskRunning["Running"]],
				(*THEN*)
				(*one was running, see if it expired*)
				(
					If[AbsoluteTime[] >= $scheduledTaskRunning["endTime"],
						(*THEN*)
						(*it expired, so we are good to go*)
						(
							$scheduledTaskRunning["Running"]=False;
							$scheduledTaskRunning["endTime"]=AbsoluteTime[];
							$scheduledTaskRunning["startTime"]=AbsoluteTime[];
						),
						(*ELSE*)
						(*it hasn't expired so return $Failed and raise a message*)
						(
							Message[DeviceExecute::taskRunning];
							Return[$Failed]
						)
					]
				)
				(*ELSE*)
				(*no else case, just allow to continue normally*)
			];
			Switch[timespec,
				_Integer|_Real,
				(
					(*this is for the case of ScheduledTask[expr, syncTime], where the task is just
					run every syncTime seconds infinitely*)
					(*a run time length and iteration count of 0 represents an infinite task to the arduino*)
					(*also, the time specified by the user is in seconds, but the arduino needs in milliseconds, so multiply the seconds by 1000*)
					functionPacket=functionCallPacketSend[funcID,
						"SyncTime"->Floor[timespec*1000],"RunTimeLength"->0,"IterationCount"->0,
						"LongArgumentNumber"->longArgsNum[function],
						"FloatArgumentNumber"->floatArgsNum[function],
						"StringArgumentNumber"->stringArgsNum[function],
						"LongArrayArgumentNumber"->longArrayArgsNum[function],
						"FloatArrayArgumentNumber"->floatArrayArgsNum[function]];
					(*check if there are any arguments to send*)
					If[hasArgs[function],
						(*THEN*)
						(*the function has args, so send those too*)
						(
							(*now check to make sure that the arguments sent are good to go*)
							argPackets = sendArgs[$functionCalls[functionName][[2]],args];
							If[ argPackets =!= $Failed,
								(*THEN*)
								(*the arguments are valid, we can schedule the function now*)
								(
									(*always default to not waiting for a return value*)
									DeviceFramework`DeviceDriverOption["Firmata","ExecuteFunction"][{ihandle,dhandle},
										Flatten[Join[functionPacket,argPackets]],
										"ReturnValue"->False];
									(*a function is now running on the arduino, so don't let the user run anymore until this one is done running or deleted*)
									$scheduledTaskRunning["Running"]=True;
									$scheduledTaskRunning["startTime"]=AbsoluteTime[];
									(*this is an infinite task*)
									$scheduledTaskRunning["endTime"]=Infinity
								),
								(*ELSE*)
								(*the arguments are invalid, so raise message and return $Failed*)
								(
									Message[DeviceExecute::invalidArgs,args];
									Return[$Failed];
								)
							]
						),
						(*ELSE*)
						(*the function doesn't have args, so don't send those, but still check if the user specified arguments*)
						(
							If[args=!={},
								(*THEN*)
								(*the user specified arguments, when this function doesn't take arguments*)
								(
									Message[DeviceExecute::invalidArgs,args];
									Return[$Failed];
								),
								(*ELSE*)
								(*the user didn't specify arguments, so run normally*)
								(
									(*always default to not waiting for a return value*)
									DeviceFramework`DeviceDriverOption["Firmata","ExecuteFunction"][{ihandle,dhandle},functionPacket,"ReturnValue"->False];
									(*a function is now running on the arduino, so don't let the user run anymore until this one is done running or deleted*)
									$scheduledTaskRunning["Running"]=True;
									$scheduledTaskRunning["startTime"]=AbsoluteTime[];
									(*this is an infinite task*)
									$scheduledTaskRunning["endTime"]=Infinity
								)
							];
							
						)
					];
					
				),
				{_Integer|_Real,_Integer},
				(
					(*this is for the case of ScheduledTask[expr,{syncTime, count}], where the task is
					run every syncTime seconds for a total number of count times*)
					functionPacket=functionCallPacketSend[funcID,
						"SyncTime"->Floor[timespec[[1]]*1000],"RunTimeLength"->0,"IterationCount"->timespec[[2]],
						"LongArgumentNumber"->longArgsNum[function],
						"FloatArgumentNumber"->floatArgsNum[function],
						"StringArgumentNumber"->stringArgsNum[function],
						"LongArrayArgumentNumber"->longArrayArgsNum[function],
						"FloatArrayArgumentNumber"->floatArrayArgsNum[function]];
					(*now send the packets to the arduino*)
					(*first check if there are any arguments to send*)
					If[hasArgs[function],
						(*THEN*)
						(*the function has args, so send those too*)
						(
							(*now let's check the arguments the user passed and see if those are good to go*)
							argPackets = sendArgs[$functionCalls[functionName][[2]],args];
							(*always default to not waiting for a return value*)
							If[argPackets =!= $Failed,
								(*THEN*)
								(*the arguments are valid, so send the task along*)
								(
									DeviceFramework`DeviceDriverOption["Firmata","ExecuteFunction"][{ihandle,dhandle},
										Flatten[Join[functionPacket,argPackets]],
										"ReturnValue"->False];
									(*a function is now running on the arduino, so don't let the user run anymore until this one is done running or deleted*)
									$scheduledTaskRunning["Running"]=True;
									$scheduledTaskRunning["startTime"]=AbsoluteTime[];
									(*this isn't an infinite task, so set the end time to be count * waitTime + 1*)
									$scheduledTaskRunning["endTime"]=AbsoluteTime[]+1+timespec[[1]]*timespec[[2]];
								),
								(*ELSE*)
								(*the arguments are invalid, so raise message and return $Failed*)
								(
									Message[DeviceExecute::invalidArgs,args];
									Return[$Failed];
								)
							]
						),
						(*ELSE*)
						(*the function doesn't have arguments, so don't send those*)
						(
							If[args=!={},
								(*THEN*)
								(*the user specified arguments, when this function doesn't take arguments*)
								(
									Message[DeviceExecute::invalidArgs,args];
									Return[$Failed];
								),
								(*ELSE*)
								(*the user didn't specify arguments, so run normally*)
								(
									DeviceFramework`DeviceDriverOption["Firmata","ExecuteFunction"][{ihandle,dhandle},functionPacket,"ReturnValue"->False];
									(*a function is now running on the arduino, so don't lwt the user run anymore until this one is done running or deleted*)
									$scheduledTaskRunning["Running"]=True;
									$scheduledTaskRunning["startTime"]=AbsoluteTime[];
									(*this isn't an infinite task, so set the end time to be count * waitTime + 1*)
									$scheduledTaskRunning["endTime"]=AbsoluteTime[]+1+timespec[[1]]*timespec[[2]];
								)
							]
						)
					];
				),
				{_Integer|_Real},
				(
					(*this is for the case of ScheduledTask[expr, {delayTime}], where the task is to be 
					run in delayTime seconds*)
					functionPacket=functionCallPacketSend[funcID,"InitialDelayTime"->Floor[First[timespec]*1000],
						"LongArgumentNumber"->longArgsNum[function],
						"FloatArgumentNumber"->floatArgsNum[function],
						"StringArgumentNumber"->stringArgsNum[function],
						"LongArrayArgumentNumber"->longArrayArgsNum[function],
						"FloatArrayArgumentNumber"->floatArrayArgsNum[function]];
					(*now send the packets to the arduino*)
					(*first check if there are args to send as well*)
					If[hasArgs[function],
						(*THEN*)
						(*the function has args, so send those too*)
						(
							(*now validate the arguments before sending them*)
							argPackets = sendArgs[$functionCalls[functionName][[2]],args];
							If[argPackets =!= $Failed,
								(*THEN*)
								(*the arguments are valid, so schedule the task*)
								(
									(*always default to not waiting for a return value*)
									DeviceFramework`DeviceDriverOption["Firmata","ExecuteFunction"][{ihandle,dhandle},
										Flatten[Join[functionPacket,argPackets]],
										"ReturnValue"->False];
									(*a function is now running on the arduino, so update the association for that*)
									$scheduledTaskRunning["Running"]=True;
									$scheduledTaskRunning["startTime"]=AbsoluteTime[];
									(*this task will be run in timespec seconds, so set the end time to be that*)
									$scheduledTaskRunning["endTime"]=First[AbsoluteTime[]+timespec+1];
								),
								(*ELSE*)
								(*the arguments are invalid, so raise a message and return $Failed*)
								(
									Message[DeviceExecute::invalidArgs,args];
									Return[$Failed];
								)
							]
						),
						(*ELSE*)
						(*the function doesn't have arguments, so don't send those*)
						(
							If[args=!={},
								(*THEN*)
								(*the user specified arguments, when this function doesn't take arguments*)
								(
									Message[DeviceExecute::invalidArgs,args];
									Return[$Failed];
								),
								(*ELSE*)
								(*the user didn't specify arguments, so run normally*)
								(
									DeviceFramework`DeviceDriverOption["Firmata","ExecuteFunction"][{ihandle,dhandle},functionPacket,"ReturnValue"->False];
									(*a function is now running on the arduino, so update the association for that*)
									$scheduledTaskRunning["Running"]=True;
									$scheduledTaskRunning["startTime"]=AbsoluteTime[];
									(*this task will be run in timespec seconds, so set the end time to be that*)
									$scheduledTaskRunning["endTime"]=First[AbsoluteTime[]+timespec+1];
								)
							]
						)
					];
				),(*
				_String,
				(
					(*TODO: implement this case for Hourly, Monthly, etc.*)
					(*TODO: implement this case for cron tab spec*)
					Null
				),*)
				_DateObject,
				(
					(*this case is for running once at the time specified by the DateObject*)
					(*for this case, we just calculate the amount of seconds between now and when the date object is for*)
					(*if it is positive (or less than a second ago), throw it in, if it is not, then issue a message*)
					If[(waitTime=AbsoluteTime[timespec]-AbsoluteTime[Now]>-1),
						(*THEN*)
						(*it is in fact in the future, so run it normally*)
						(
							functionPacket=functionCallPacketSend[funcID,"InitialDelayTime"->waitTime,
								"LongArgumentNumber"->longArgsNum[function],
								"FloatArgumentNumber"->floatArgsNum[function],
								"StringArgumentNumber"->stringArgsNum[function],
								"LongArrayArgumentNumber"->longArrayArgsNum[function],
								"FloatArrayArgumentNumber"->floatArrayArgsNum[function]];
							(*Print["sending ",functionPacket];*)
							(*now send the packets to the arduino*)
							(*first check if there are arguments to send*)
							If[hasArgs[function],
								(*THEN*)
								(*the function has args, so send those too*)
								(
									(*then check the arguments the user passed to see if they are good*)
									argPackets= sendArgs[$functionCalls[functionName][[2]],args];
									If[ argPacket =!= $Failed,
										(*THEN*)
										(*the arguments are good, so send it along*)
										(
											(*always default to not waiting for a return value*)
											DeviceFramework`DeviceDriverOption["Firmata","ExecuteFunction"][{ihandle,dhandle},
												Flatten[Join[functionPacket,argPackets]],
												"ReturnValue"->False];
												(*update the scheduledTask association because a function is now running on the arduino*)
												$scheduledTaskRunning["Running"]=True;
												$scheduledTaskRunning["startTime"]=AbsoluteTime[];
												$scheduledTaskRunning["endTime"]=AbsoluteTime[]+waitTime+1;
										),
										(*ELSE*)
										(*arguments are invalid, so raise a message and return $Failed*)
										(
											Message[DeviceExecute::invalidArgs,args];
											Return[$Failed];
										)
									];
								),
								(*ELSE*)
								(*the function doesn't have arguments, so don't send those*)
								(
									If[args=!={},
										(*THEN*)
										(*the user specified arguments, when this function doesn't take arguments*)
										(
											Message[DeviceExecute::invalidArgs,args];
											Return[$Failed];
										),
										(*ELSE*)
										(*the user didn't specify arguments, so run normally*)
										(
											DeviceFramework`DeviceDriverOption["Firmata","ExecuteFunction"][{ihandle,dhandle},functionPacket,"ReturnValue"->False];
											(*update the scheduledTask association because a function is now running on the arduino*)
											$scheduledTaskRunning["Running"]=True;
											$scheduledTaskRunning["startTime"]=AbsoluteTime[];
											$scheduledTaskRunning["endTime"]=AbsoluteTime[]+waitTime+1;
										)
									]
								)
							];
						),
						(*ELSE*)
						(*not in the future, so not much we can do here*)
						(
							Message[DeviceExecute::past,waitTime];
							Return[$Failed];
						)
					]
				),
				_,
				(
					(*any other kind of timespec should be invalid, so issue message and return $Failed*)
					Message[DeviceExecute::invalidTiming,timespec];
					Return[$Failed];
				)
			],
			(*ELSE*)
			(*the function doesn't exist, issue message and reutrn $Failed*)
			(
				Message[DeviceExecute::noFunc,functionName];
				Return[$Failed];
			)
		]
	)
];


ArduinoExecute[{ihandle_,dhandle_},functionName_,args__,OptionsPattern[]]:=Module[{},
	(
		(*before execution, make sure the requested function exists*)
		If[MemberQ[Keys[$functionCalls],functionName],
			(*THEN*)
			(*function exists, so make sure a function isn't already running on the arduino*)
			(
				(*make sure that a scheduled task isn't already running*)
				If[TrueQ[$scheduledTaskRunning["Running"]],
					(*THEN*)
					(*one was running, see if it expired*)
					(
						If[AbsoluteTime[] >= $scheduledTaskRunning["endTime"],
							(*THEN*)
							(*it expired, so we are good to go*)
							(
								$scheduledTaskRunning["Running"]=False;
								$scheduledTaskRunning["endTime"]=AbsoluteTime[];
								$scheduledTaskRunning["startTime"]=AbsoluteTime[];
							),
							(*ELSE*)
							(*it hasn't expired so return $Failed and raise a message*)
							(
								Message[DeviceExecute::taskRunning];
								Return[$Failed]
							)
						]
					)
					(*ELSE*)
					(*no else case, just allow to continue normally*)
				];
				(*get the arg packets, and check to make sure that didn't fail*)
				argPackets = sendArgs[$functionCalls[functionName][[2]],args];
				If[argPackets =!= $Failed,
					(*THEN*)
					(*it worked, argPackets didn't fail*)
					(
						Return[DeviceFramework`DeviceDriverOption["Firmata","ExecuteFunction"]
							[{ihandle,dhandle},
								Flatten[Join[$functionCalls[functionName][[1]],argPackets]],
								"ReturnValue"->ReturnQ[$functionCalls[functionName][[2]]]]]
					),
					(*ELSE*)
					(*it failed, so raise a message about the arguments*)
					(
						Message[DeviceExecute::invalidArgs,args];
						Return[$Failed];
					)
				]
			),
			(*ELSE*)
			(*the function doesn't exist, issue message and return $Failed*)
			(
				Message[DeviceExecute::noFunc,functionName];
				Return[$Failed];
			)
		]
	)
];


(*TODO: deprecate this, the wrapper should take care of this...*)
ArduinoExecute[{ihandle_,dhandle_},functionName_List,OptionsPattern[]]:=Module[{},
	(
		Message[DeviceExecute::invalidFunc];
		Return[$Failed];
	)
];



ArduinoExecute[{ihandle_,dhandle_},functionName_,OptionsPattern[]]:=Module[{},
	(
		(*before execution, make sure that the requested function exists*)
		If[MemberQ[Keys[$functionCalls],functionName],
			(*THEN*)
			(*the function exists, so make sure that one isn't already running before running a new one*)
			(
				If[TrueQ[$scheduledTaskRunning["Running"]],
					(*THEN*)
					(*one was running, see if it expired*)
					(
						If[AbsoluteTime[] >= $scheduledTaskRunning["endTime"],
							(*THEN*)
							(*it expired, so we are good to go, but need to reset the $scheduledTaskRunning*)
							(
								$scheduledTaskRunning["Running"]=False;
								$scheduledTaskRunning["endTime"]=AbsoluteTime[];
								$scheduledTaskRunning["startTime"]=AbsoluteTime[];
							),
							(*ELSE*)
							(*it hasn't expired so return $Failed and raise a message*)
							(
								Message[DeviceExecute::taskRunning];
								Return[$Failed]
							)
						]
					)
					(*ELSE*)
					(*no else case, just allow to continue normally*)
				];
				(*function exists, so make sure it doesn't need args, then call it*)
				(*TracePrint[hasArgs[$functionCalls[functionName][[2]]]];*)
				If[hasArgs[$functionCalls[functionName][[2]]],
					(*THEN*)
					(*it needs arguments, and we didn't get any, so raise message and return $Failed*)
					(
						Message[DeviceExecute::needsArgs,functionName];
						Return[$Failed];
					),
					(*ELSE*)
					(*it doesn't need arguments, and we didn't get any, so don't send any*)
					(
						Return[DeviceFramework`DeviceDriverOption["Firmata","ExecuteFunction"]
							[{ihandle,dhandle},$functionCalls[functionName][[1]],
								"ReturnValue"->ReturnQ[$functionCalls[functionName][[2]]]]]
					)
				]
			),
			(*ELSE*)
			(*the function doesn't exist, issue message and return $Failed*)
			(
				Message[DeviceExecute::noFunc,functionName];
				Return[$Failed];
			)
		]
	)
];


(*this is basically a wrapper function that converts the user's raw input from DeviceExecte into the individual arguments necessary for it*)
(*possible forms of DeviceExecute supported:*)
(*DeviceExecute["Arduino","DeleteTask","func"]*)
(*DeviceExecute["Arduino","func"]*)
(*DeviceExecute["Arduino","func",args]*)
(*DeviceExecute["Arduino","func",{args,"Scheduling"->timespec}]*)
(*DeviceExecute["Arduino","func","Scheduling"->timespec]*)
Options[ArduinoExecuteDriver]={"Scheduling"->None};
ArduinoExecuteDriver[{ihandle_,dhandle_},args__,OptionsPattern[]]:=Module[
	{
		allArgs={args}
	},
	(	
		Switch[First[allArgs],
			"SoftwareReset",
			(
				Return[ArduinoExecute[{ihandle,dhandle},allArgs[[1]]]];
			),
			"DeleteTask",
			(
				Return[ArduinoExecute[{ihandle,dhandle},allArgs[[1]],allArgs[[2]]]];
			),
			_,
			Switch[Length[allArgs],
				1,
				(*this case is where a function does not have any arguments, so args is just the function's name*)
				(
					(*check if this execution is scheduled*)
					If[OptionValue["Scheduling"]===None,
						(*THEN*)
						(*no scheduling necessary, so run normally*)
						(
							Return[ArduinoExecute[{ihandle,dhandle},First@allArgs]];
						),
						(*ELSE*)
						(*scheduling isn't none, so use ArduinoScheduleExecute with a list of {func name, args, scheduling}*)
						(
							Return[ArduinoScheduleExecute[{ihandle,dhandle},{First@allArgs,{},OptionValue["Scheduling"]}]];
						)
					]
				),
				_Integer,
				(*for case of more than one argument, include the args as a list as the second argument*)
				(
					(*check if this execution is scheduled*)
					If[OptionValue["Scheduling"]===None,
						(*THEN*)
						(*no scheduling necessary, so run normally*)
						(
							Return[ArduinoExecute[{ihandle,dhandle},First[allArgs],allArgs[[2;;]]]];
						),
						(*ELSE*)
						(*scheduling isn't none, so use ArduinoScheduleExecute with a list of {func name, args, scheduling}*)
						(
							Return[ArduinoScheduleExecute[{ihandle,dhandle},{First@allArgs,allArgs[[2;;]],OptionValue["Scheduling"]}]];
						)
					]
				)
			]
		]
	)
];


ArduinoConfigureDriverWrapper[{ihandle_,dhandle_},args__]:=Module[{},
	(
		Switch[Head[args],
			Association,(*this is for Pin Configure*)ArduinoConfigureDriver[{ihandle,dhandle},args],
			Rule,(*need to switch on the first one*)
			(
				Switch[args[[1]],
					"Upload",ArduinoConfigureDriver[{ihandle,dhandle},"Upload",args[[2]]],
					"PinConfigure"|"PinConfigurations",ArduinoConfigureDriver[{ihandle,dhandle},"PinConfigure"->args[[2]]]
				]
			),
			String,(*this might be just upload with no options, but check*)
			(
				If[args==="Upload",
					(*THEN*)
					(*perform a default upload*)
					ArduinoConfigureDriver[{ihandle,dhandle},"Upload"],
					(*ELSE*)
					(*the string is something, else raise a message*)
					(
						Message[DeviceConfigure::invalidOptions,args];
					)
				]
			),
			_,(*this is for anything else*)
			(
				Message[DeviceConfigure::invalidOptions,args];
			)
		]
	)
];


(*the configure driver basically will just pass the options for the configuration to the arduinoUpload 
function if the user wants to add a new function, or if the user just wants to configure pin mode,
then we just send those packets and update the internal association *)
Options[ArduinoConfigureDriver]=
{
		"Debug"->False,
		"Libraries" -> {},
		"CleanIntermediate"->True,
		"FlashVerify"->True,
		Initialization->"",
		"Functions"-><||>
};
ArduinoConfigureDriver[{ihandle_,dhandle_},"Upload",OptionsPattern[]]:=Module[
	{
		compilerLocation=$arduinoInstallLocation
	},
	(
		(*first though, check if an upload has been previously done in the past 1.5 seconds*)
		If[AbsoluteTime[]-$lastConfigCall>1.5,
			(*THEN*)
			(*we are okay to call the function, enough time has passed since last call*)
			(
				(*first check to make sure we have the arduino software installed*)
				If[compilerLocation === None,
					(*THEN*)
					(*the compiler location isn't configured, so we can't do anything else*)
					(
						Message[DeviceConfigure::noArduinoInstall];
						Return[$Failed];
					)
					(*ELSE*)
					(*it's not none, so assume that it is alright to use*)
				];
				(*reset the progress bar indicator*)
				$compilationProgressIndicator = 0;
				(*also print off the progress bar for the upload process*)
				(*only print it off if we have a front end*)
				If[$FrontEnd =!= Null, PrintTemporary[ProgressIndicator[Dynamic[$compilationProgressIndicator],{0,30}]]];
				If[TrueQ[OptionValue["Debug"]],Print["Debugging turned on"]];
				$serialPort = $DeviceStates[ihandle]["SerialPort"];
				If[OptionValue["Debug"]===True,Print["Closing the serial connection"]];
				$timeToCloseSerialPort=AbsoluteTime[];
				(*close the connection to the device before uploading*)
				DeviceFramework`DeviceDriverOption["Firmata","CloseFunction"][{ihandle,dhandle}];
				If[OptionValue["Debug"]===True,Print["Took "<>ToString[AbsoluteTime[]-$timeToCloseSerialPort]<>" seconds to close the serial port"]];
				(*TODO: implement remembering*)
				(*this is a workaround to prevent the "remembering" feature from being utilized, will be enabled in a future release*)
				$previousFunctions=<||>;
				$functionCalls=<||>;
				(*we need to convert symbolic c to the ExternalFunction type of specification before sending it to arduinoUpload*)
				customFunctions = OptionValue["Functions"];
				newCustomFunctions=Table[0,{Length[customFunctions]}];
				For[functionIndex=1,functionIndex<=Length[customFunctions],functionIndex++,
					(
						(*for each function in the list, check if it is a SymbolicC function*)
						If[Head[Normal[customFunctions][[functionIndex,2]]]===SymbolicC`CFunction,
							(*THEN*)
							(*this function is a SymbolicC function, so we have to compile it down to a c code string*)
							(
								newCustomFunctions[[functionIndex]]=
									Keys[customFunctions][[functionIndex]]->symbolicCFunctionFull[Values[customFunctions][[functionIndex]]]
							),
							(*ELSE*)
							(*the function is just a normal ArduinoCode function, so wrap it into the old ExternalFunction*)
							newCustomFunctions[[functionIndex]]=Keys[customFunctions][[functionIndex]]->externalFunctionConvert[Values[customFunctions][[functionIndex]]];
						]
					)
				];
				customFunctions=Association[newCustomFunctions];
				(*check if we are on the raspberry pi, if we are then don't use the default avr util options*)
				If[$MachineID === "4801-62204-12672",
					(*THEN*)
					(*we are on the Raspberry Pi*)
					(
						avrdudeLocation = {"/usr","bin","avrdude"};
						avrgccLocation = {"/usr","bin"};
						avrdudeConfLocation = {"/etc","avrdude.conf"};
						stdLibsLocation = {
							{$arduinoInstallLocation,"hardware","arduino","cores","arduino"},
							{$arduinoInstallLocation,"hardware", "arduino", "variants", "standard"}
						};
						arduinoVersion = "101";
					),
					(*ELSE*)
					(*we're not on the pi, use default*)
					(
						avrdudeLocation = Default;
						avrgccLocation = Default;
						avrdudeConfLocation = Default;
						stdLibsLocation = Default;
						arduinoVersion = Default;
					)
				];
				
				(*finally actually perform the upload*)
				uploadResult=arduinoUpload
				[
					$serialPort,
					compilerLocation,
					PacletResource["ArduinoLink","Sketch"],
					"Debug"->OptionValue["Debug"],
					"FlashVerify"->OptionValue["FlashVerify"],
					"Libraries"->OptionValue["Libraries"],
					Initialization->OptionValue[Initialization],
					"CleanIntermediate"->OptionValue["CleanIntermediate"],
					"Functions"->Join[$previousFunctions,customFunctions],
					"AVRDUDELocation"->avrdudeLocation,
					"AVRDUDEConfLocation"->avrdudeConfLocation,
					"AVRGCCLocation"->avrgccLocation,
					"ArduinoVersion"->arduinoVersion,
					"StandardArduinoLibrariesLocation"->stdLibsLocation
				];
				If[TrueQ[OptionValue["Debug"]],Print["Reopening the serial connection"]];
				(*now reopen the connection to the device*)
				DeviceFramework`DeviceDriverOption["Firmata","OpenFunction"][{ihandle,dhandle},$serialPort,"BaudRate"->115200];
				(*finally, if a new function was uploaded, add it's information to the internal association storing information on calling the functions*)
				If[uploadResult===$Failed,
					(*THEN*)
					(*the upload failed, so print off message if debug is on, and return $Failed*)
					(
						If[TrueQ[OptionValue["Debug"]],
							Print["Upload failed"];
						];
						Return[$failed];
					)
					(*ELSE*)
					(*the upload succeeded*)
				];
				If[customFunctions===<||>,
					(*THEN*)
					(*there wasn't a function uploaded, so we don't need to add it*)
					Null,
					(*ELSE*)
					(*there was a function uploaded, we need to add it*)
					(
						$previousFunctions = Join[$previousFunctions,customFunctions];
						(*now need to update the function call options*)
						$functionCalls = addNewFunctionCalls[$functionCalls,customFunctions];
					)
				];
				(*finally, because we re-uploaded data to it, there will no longer be a scheduled function on it running, so set the association for that*)
				$scheduledTaskRunning["Running"]=False;
				$scheduledTaskRunning["endTime"]=AbsoluteTime[];
				$scheduledTaskRunning["startTime"]=AbsoluteTime[];
			),
			(*ELSE*)
			(*don't call the function, this is the front end being weird and calling the configure function automatically*)
			(
				(*also this is a specific return so that we don't set the last time called, as that should only be updated when the upload is actually run*)
				Return[];
			)
		];
		(*we set the time that the function was last called as a workaround to prevent the front end from trying to call the function automatically*)
		$lastConfigCall=AbsoluteTime[];
	)
];


ArduinoConfigureDriver[{ihandle_,dhandle_},"PinConfigure"->config_,OptionsPattern[]]:=Module[{},
	(
		setPinConfigurations[{ihandle,dhandle},config];
	)
];


ArduinoConfigureDriver[{ihandle_,dhandle_},pinConfigs_Association,OptionsPattern[]]:=Module[{},
	(
		setPinConfigurations[{ihandle,dhandle},pinConfigs];
	)
];


ArduinoConfigureDriver[{ihandle_,dhandle_},pin_->mode_,OptionsPattern[]]:=Module[{},
	(
		setPinConfigurations[{ihandle,dhandle},{pin->mode}];
	)
];


setPinConfigurations[{ihandle_,dhandle_},pin_->mode_]:=Module[{},
	(
		setPinConfigurations[{ihandle,dhandle},{pin->mode}];
	)
];



setPinConfigurations[{ihandle_,dhandle_},config_List]:=Module[{},
	(
		For[configIndex=1,configIndex<=Length[config],configIndex++,
			(*for each config passed in, first verify the configuration and then actually send the configuration to the arduino first*)
			pin=config[[configIndex,1]];
			mode=config[[configIndex,2]];
			Switch[mode,
				"Input"|"DigitalInput",
					(*any pin can be an input or digital input, so just make sure the pin is a valid pin*)
					(
						If[MemberQ[arduinoUnoPins,pin],
							(*THEN*)
							(*the pin is a valid analog pin, so update the internal configuration and configure it on the hardware*)
							(
								$DeviceStates[ihandle]["PinConfigurations"][arduinoPinToKey[pin]]["Direction"]="HardInput";
								DeviceFramework`DeviceDriverOption["Firmata","ConfigureFunction"][{ihandle,dhandle},config[[configIndex]]];
							),
							(*ELSE*)
							(*the pin isn't an analog pin, so issue a message and return $Failed*)
							(
								Message[DeviceConfigure::invalidPin,pin];
								Return[$Failed];
							)
						]
					),
				"AnalogInput",
					(*only analog pins can be analog input, but functionally analog input is the same configuration as any other kind of input*)
					(
						If[MemberQ[arduinoUnoAnalogPins,pin],
							(*THEB*)
							(*the pin is a valid analog pin, so update the internal configuration and configure it on the hardware*)
							(
								$DeviceStates[ihandle]["PinConfigurations"][arduinoPinToKey[pin]]["Direction"]="HardInput";
								DeviceFramework`DeviceDriverOption["Firmata","ConfigureFunction"][{ihandle,dhandle},config[[configIndex]]];
							),
							(*ELSE*)
							(*the pin isn't an analog pin, so issue a message and return $Failed*)
							(
								Message[DeviceConfigure::invalidAnalogPin,pin];
								Return[$Failed];
							)
						]
					),
				"Output"|"DigitalOutput",
					(*any pin can be an output or digital output, so just make sure that the pin is a valid pin*)
					(
						If[MemberQ[arduinoUnoPins,pin],
							(*THEN*)
							(*the pin is valid, so configure it and set the $DeviceStates association*)
							(
								$DeviceStates[ihandle]["PinConfigurations"][arduinoPinToKey[pin]]["Direction"]="HardOutput";
								DeviceFramework`DeviceDriverOption["Firmata","ConfigureFunction"][{ihandle,dhandle},config[[configIndex]]];
							),
							(*ELSE*)
							(*the pin isn't a valid pin to begin with*)
							(
								Message[DeviceConfigure::invalidPin,pin];
								Return[$Failed];
							)
						]
					),
				"PWMOutput"|"AnalogOutput",
					(*only pwm pins can be PWMOutput or analog output, but functionally, analog output is the same configuration as any other kind of output*)
					(
						If[MemberQ[arduinoUnoPWMPins,pin],
							(*THEN*)
							(*the pin is valid, so configure it and set the $DeviceStates association*)
							(
								$DeviceStates[ihandle]["PinConfigurations"][arduinoPinToKey[pin]]["Direction"]="HardOutput";
								DeviceFramework`DeviceDriverOption["Firmata","ConfigureFunction"][{ihandle,dhandle},config[[configIndex]]];
							),
							(*ELSE*)
							(*the pin isn't a valid pin to begin with*)
							(
								Message[DeviceConfigure::notPWMPin,pin];
								Return[$Failed];
							)
						]
					),
				_,
					(
						Message[DeviceConfigure::invalidMode,mode];
						Return[$Failed];
					)
			];
		]
	)
];



setPinConfigurations[{ihandle_,dhandle_},config_Association]:=Module[{},
	(
		setPinConfigurations[{ihandle,dhandle},Normal[config]];
	)
];




ArduinoPropertyGetDriver[dev_,property_]:=Module[{},
	(
		Switch[property,
			"PinConfigurations",
			(
				Return[Dataset[$DeviceStates[DeviceFramework`DeviceManagerHandle[dev]]["PinConfigurations"]]];
			),
			"SerialPort",
			(
				Return[$DeviceStates[DeviceFramework`DeviceManagerHandle[dev]]["SerialPort"]];
			),
			"ArduinoInstallLocation",
			(
				Return[$arduinoInstallLocation];
			)
		]
	)
];



(*unimplemented, there aren't any properties the user can set*)
ArduinoPropertySetDriver[dev_,property_,value_]:=Module[{},
	(
		Switch[property,
			"ArduinoInstallLocation",
			(
				(*need to check the location first*)
				If[validArduinoInstallLocation[ExpandFileName[value]],
					(*THEN*)
					(*it is valid, set the internal variable*)
					(
						$arduinoInstallLocation=ExpandFileName[value];
					),
					(*ELSE*)
					(*it's not valid, raise a message and don't change it, and lastly return $Failed*)
					(
						Message[DeviceSetProperty::invalidInstallLocation,value];
						Return[$Failed];
					)
				];
			)
		];
	)
];





(*addNewFunctionCalls will create the firmata packets necessary for all the functions inside the association passed to it*)
addNewFunctionCalls[allFunctions_,functionInfo_Association]:=Module[{},
	(
		functionIDStart=Length[allFunctions];
		Join[allFunctions,Association[individualFunctionPacket[functionIDStart++,#]&/@Normal[functionInfo]]]
	)
];

(*individualFunctionPacket will make a packet from the function information*)
individualFunctionPacket[functionNumber_,functionName_->function_]:=Module[{},
	(
		(*need to call functionCallPacketSend with the new function numer (aka it's ID), as well as information about the arguments*)
		functionName->
			{
				functionCallPacketSend[functionNumber,
					"LongArgumentNumber"->longArgsNum[function],
					"FloatArgumentNumber"->floatArgsNum[function],
					"StringArgumentNumber"->stringArgsNum[function],
					"LongArrayArgumentNumber"->longArrayArgsNum[function],
					"FloatArrayArgumentNumber"->floatArrayArgsNum[function]
				],
				function
			}
	)
];


(*gets the number of *)
longArgsNum[function_]:=Module[{},
	(
		If[Head[function[[1]]]===Rule,
			(*THEN*)
			(*function has a return type, ignore it*)
			Return[Association[Rule@@@Tally[function[[1,1]]]][Integer]/.Missing[___]->0],
			(*ELSE*)
			(*function doesn't have a return type*)
			Return[Association[Rule@@@Tally[function[[1]]]][Integer]/.Missing[___]->0]
		]
	)	
];

floatArgsNum[function_]:=Module[{},
	(
		If[Head[function[[1]]]===Rule,
			(*THEN*)
			(*function has a return type, ignore it*)
			Return[Association[Rule@@@Tally[function[[1,1]]]][Real]/.Missing[___]->0],
			(*ELSE*)
			(*function doesn't have a return type*)
			Return[Association[Rule@@@Tally[function[[1]]]][Real]/.Missing[___]->0]
		]
	)	
];

stringArgsNum[function_]:=Module[{},
	(
		If[Head[function[[1]]]===Rule,
			(*THEN*)
			(*function has a return type, ignore it*)
			Return[Association[Rule@@@Tally[function[[1,1]]]][String]/.Missing[___]->0],
			(*ELSE*)
			(*function doesn't have a return type*)
			Return[Association[Rule@@@Tally[function[[1]]]][String]/.Missing[___]->0]
		]
	)	
];

longArrayArgsNum[function_]:=Module[{},
	(
		If[Head[function[[1]]]===Rule,
			(*THEN*)
			(*function has a return type, ignore it*)
			Return[Association[Rule@@@Tally[function[[1,1]]]][{Integer}]/.Missing[___]->0],
			(*ELSE*)
			(*function doesn't have a return type*)
			Return[Association[Rule@@@Tally[function[[1]]]][{Integer}]/.Missing[___]->0]
		]
	)	
];

floatArrayArgsNum[function_]:=Module[{},
	(
		If[Head[function[[1]]]===Rule,
			(*THEN*)
			(*function has a return type, ignore it*)
			Return[Association[Rule@@@Tally[function[[1,1]]]][{Real}]/.Missing[___]->0],
			(*ELSE*)
			(*function doesn't have a return type*)
			Return[Association[Rule@@@Tally[function[[1]]]][{Real}]/.Missing[___]->0]
		]
	)
];


(*sendArgs will confirm the arguments are correctly typed and then package them up in a packet*)
sendArgs[function_,arg_]:=Module[{},
	(
		If[ReturnQ[function],
			(*THEN*)
			(*has a return value*)
			If[function[[1,1]]==={},
				(*THEN*)
				(*the function doesn't have any arguments, and thus nothing to send*)
				Return[$Failed],
				(*ELSE*)
				(*check to make sure that the single argument is of the right type by comparing it to the original type signature from the ExternalFunction object*)
				If[function[[1,1,1]]===Head[arg],
					(*THEN*)
					(*it is valid, so return the correct packet*)
					Return[
						(*switch on which type*)
						Switch[Head[arg],
							Real,sendFloatPacket[arg],
							Integer,sendLongPacket[arg],
							String,sendStringPacket[arg],
							(*list means array, so have to check which kind of array*)
							List,Switch[Head[arg[[1]]],
								Real,sendFloatArrayPacket[arg],
								Integer,sendLongArrayPacket[arg]
							],
							(*default case for any other kind of type, return $Failed*)
							_,
							(
								Return[$Failed];
							)
						]
					],
					(*ELSE*)
					(*invalid type for this argument*)
					(
						Return[$Failed]
					)
				]
			],
			(*ELSE*)
			(*doesn't have a return value*)
			If[function[[1]]==={},
				(*THEN*)
				(*the function doesn't have any arguments and thus nothing to send*)
				Return[$Failed],
				(*ELSE*)
				(*the function has arguments, so check tomake sure that the user's argument matches the expected arg type*)
				If[function[[1,1]]===Head[arg],
					(*THEN*)
					(*it does, so return the correct packet*)
					Return[
						Switch[Head[arg],
							Real,sendFloatPacket[arg],
							Integer,sendLongPacket[arg],
							String,sendStringPacket[arg],
							(*list means array, so we need to check which kind of array*)
							List,Switch[Head[arg[[1]]],
								Real,sendFloatArrayPacket[arg],
								Integer,sendLongArrayPacket[arg]
							],
							(*default case of any other head, return $Failed*)
							_,
							(
								Return[$Failed]
							)
						]
					],
					(*ELSE*)
					(*the arg doesn't have the right type, so return $Failed*)
					(
						Return[$Failed]
					)
				]
			]
		]
	)
];


(*sendArgs will confirm the arguments are correctly typed and then package them up in a packet*)
sendArgs[function_,args_List]:=Module[{},
	(
		packet={};
		signature=function[[1]];
		If[ReturnQ[function],
			(*THEN*)
			(*there is a return from the function*)
			If[signature[[1]]==={},
				(*THEN*)
				(*there are no arguments passed to send, so return $Failed*)
				Return[$Failed],
				(*ELSE*)
				(*there are arguments, check to see if the number of arguments is correct first*)
				If[Length[signature[[1]]]===Length[args],
					(*THEN*)
					(*the right number of arguments was passed*)
					(
						(*there are arguments, so for each argument, check it against the expected type defined in DeviceConfigure*)
						For[argNum=1,argNum<=Length[args],argNum++,
							If[Head[args[[argNum]]] === (signature[[1,argNum]]/.{{Real}->List,{Integer}->List}),
								(*THEN*)
								(*the arg is good, we can append it to the list*)
								Switch[Head[args[[argNum]]],
									Real,AppendTo[packet,sendFloatPacket[args[[argNum]]]],
									Integer,AppendTo[packet,sendLongPacket[args[[argNum]]]],
									String,AppendTo[packet,sendStringPacket[args[[argNum]]]],
									List,Switch[Head[args[[argNum,1]]],
										Real,AppendTo[packet,sendFloatArrayPacket[args[[argNum]]]],
										Integer,AppendTo[packet,sendLongArrayPacket[args[[argNum]]]]
									]
								],
								(*ELSE*)
								(*return failed, the user tried to pass invalid args*)
								(
									Return[$Failed];
								)
							]
						]
					),
					(*ELSE*)
					(*wrong number of packets*)
					(
						Return[$Failed];
					)
				]
			],
			(*ELSE*)
			(*the function doesn't have a return type*)
			If[signature==={},
				(*THEN*)
				(*there are no arguments to send for this function, so return $Failed, this function shouldn't have been called in the first place*)
				Return[$Failed],
				(*ELSE*)
				(*there are arguments, check to see if the number of arguments is correct first*)
				If[Length[signature]===Length[args],
					(*THEN*)
					(*the right number of arguments was passed*)
					(
						(*there are arguments, so for each argument, check it against the expected type defined in DeviceConfigure*)
						For[argNum=1,argNum<=Length[args],argNum++,
							If[Head[args[[argNum]]] === (signature[[argNum]]/.{{Real}->List,{Integer}->List}),
								(*THEN*)
								(*the arg is good, we can append it*)
								Switch[Head[args[[argNum]]],
									Real,AppendTo[packet,sendFloatPacket[args[[argNum]]]],
									Integer,AppendTo[packet,sendLongPacket[args[[argNum]]]],
									String,AppendTo[packet,sendStringPacket[args[[argNum]]]],
									List,Switch[Head[args[[argNum,1]]],
										Real,AppendTo[packet,sendFloatArrayPacket[args[[argNum]]]],
										Integer,AppendTo[packet,sendLongArrayPacket[args[[argNum]]]]
									]
								],
								(*ELSE*)
								(*return failed, the user tried to pass invalid args*)
								(
									Return[$Failed];
								)
							]
						]
					),
					(*ELSE*)
					(*wrong number of packets, return $Failed*)
					(
						Return[$Failed];
					)
				]
			]
		];
		(*finally return the packet*)
		Return[packet];
	)
];



(*boolean of whether or not the ExternalFunction has a return type or not*)
ReturnQ[function_]:=Module[{},
	(
		Return[Head[function[[1]]]===Rule]
	)
];

(*takes an ExternalFunction object and returns whether or not that object has any arguments*)
hasArgs[function_]:=Module[{},
	(
		(*return whether the args of the object is equal to {} or not*)
		If[ReturnQ[function],
			(*THEN*)
			(*the function has a return type*)
			(
				Return[Flatten[function[[1,1]]]=!={}];
			),
			(*ELSE*)
			(*the function doesn't have a return type*)
			(
				Return[Flatten[function[[1]]]=!={}];
			)
		]
	)
];


(*FIRMATA PACKET FUNCTIONS*)
(*this will build the packet for a function call, given the function id and the associated paramaters*)
Options[functionCallPacketSend]=
{
	"LongArgumentNumber"->0,
	"FloatArgumentNumber"->0,
	"StringArgumentNumber"->0,
	"LongArrayArgumentNumber"->0,
	"FloatArrayArgumentNumber"->0,
	"SyncTime"->0,
	"IterationCount"->1,
	"RunTimeLength"->0,
	"InitialDelayTime"->0
};
functionCallPacketSend[funcID_,OptionsPattern[]]:=Module[
	{
		start=FromDigits["f0",16],
		functionCall=FromDigits["02",16],
		end=FromDigits["f7",16]
	},
	Flatten[
		{
			(*first byte is sysex start*)
			start,
			(*next byte is the function call add*)
			functionCall,
			(*then the function id*)
			funcID,
			(*then the number of long and float arguments*)
			BitOr[BitShiftLeft[BitAnd[OptionValue["LongArgumentNumber"],15],4],BitAnd[OptionValue["FloatArgumentNumber"],15]],
			(*then the number of string and long array, as well as float array arguments*)
			BitOr[BitShiftLeft[BitAnd[OptionValue["StringArgumentNumber"],15],4],
				BitAnd[BitOr[BitShiftLeft[BitAnd[OptionValue["LongArrayArgumentNumber"],3],2],BitAnd[OptionValue["FloatArrayArgumentNumber"],3]],15]],
			(*next is the timing byte, more info in each individual bit*)
			timingByte= FromDigits[
				{
					(*the top 4 bits of this byte are currently unused for anything*)
					0,
					0,
					0,
					0,
					(*for the low nibble, each bit will tell the arduino to expect another packet with the value of each of the timing information bits*)
					(*then the next bit represents whether or not the iteration count is 0 or not*)
					If[OptionValue["IterationCount"]===1,0,1],
					(*the next bit is whether or not the run time length is 0 or not*)
					If[OptionValue["RunTimeLength"]===0,0,1],
					(*then the time to wait in between calls*)
					If[OptionValue["SyncTime"]===0,0,1],
					(*finally how long to wait after the task is recieved*)
					If[OptionValue["InitialDelayTime"]===0,0,1]
				},2],
			(*finally end the function call packet with a sysex end byte*)
			end,
			(*these last values are only populated if the corresponding bit is high*)
			(*if it is then a long number packet of the number that is to be sent is sent immeadiately after the function call packet*)
			If[OptionValue["InitialDelayTime"]!=0,sendLongPacket[OptionValue["InitialDelayTime"]],{}],
			If[OptionValue["SyncTime"]!=0,sendLongPacket[OptionValue["SyncTime"]],{}],
			If[OptionValue["RunTimeLength"]!=0,sendLongPacket[OptionValue["RunTimeLength"]],{}],
			If[OptionValue["IterationCount"]===1,{},sendLongPacket[OptionValue["IterationCount"]]
			]
	}
	]
];


(*builds a firmata formatted long array packet*)
sendLongArrayPacket[nums_List]:=Module[{},
	Flatten[{
		(*first is the sysex start byte*)
		FromDigits["f0",16],
		(*then the long array identifier byte*)
		FromDigits["07",16],
		(*then the length of the array*)
		Length[nums],
		(*then the actual data bytes for all the numbers in order*)
		ToCharacterCode[ExportString[#,"Integer32",ByteOrdering->1]]&/@nums,
		(*finally the sysex end byte*)
		FromDigits["f7",16]
		}
	]
];

(*builds a firmata formatted float array packet*)
sendFloatArrayPacket[nums_List]:=Module[{},
	Flatten[{
		(*first is the sysex start byte*)
		FromDigits["f0",16],
		(*the next byte is the float array identifier*)
		FromDigits["06",16],
		(*then the length of the array*)
		Length[nums],
		(*then the actual data bytes, for all the numbers in order*)
		ToCharacterCode[ExportString[#,"Real32",ByteOrdering->1]]&/@nums,
		(*then the sysex end byte*)
		FromDigits["f7",16]
		}
	]
];

(*builds a firmata formatted long packet*)
sendLongPacket[num_Integer]:=Module[{},
	Flatten[{
		(*first byte is the sysex start*)
		FromDigits["f0",16],
		(*then the long number byte identifier*)
		FromDigits["05",16],
		(*the next four bytes are actually the data bytes for the number, the arduino is big endian, so we use ByteOrdering->1*)
		ToCharacterCode@ExportString[num,"Integer32",ByteOrdering->1],
		(*finally the sysex end byte*)
		FromDigits["f7",16]
		}
	]
	(*only run the function if the number is within the limits of the arduino*)
]/;Abs[num]<=2^31-1;


(*builds a firmata formatted float packet*)
sendFloatPacket[num_Real]:=Module[{},
	Flatten[{
		(*first byte is the sysex start byte*)
		FromDigits["f0",16],
		(*next is the float number byte identifier*)
		FromDigits["04",16],
		(*then the actual data bytes, with big endian ordering for the arduino*)
		ToCharacterCode@ExportString[num,"Real32",ByteOrdering->1],
		(*finally the sysex end byte*)
		FromDigits["f7",16]
		}
	]
];

(*TODO: check the string for non-ascii values before it gets this far*)
(*builds a firmata formatted string packet*)
sendStringPacket[string_String] := Module[{},
	Flatten[{
		(*first the sysex start byte*)
		FromDigits["f0", 16],
		(*then a string identifier byte*)
		FromDigits["71", 16],
		(*then send the length of this string*)
		Length[Characters[string]],
		(*then send each character in the string as an ascii byte*)
		(*note this does mean that the user should only send strings with ascii bytes, but this is not enforced anywhere*)
		ToCharacterCode[string, "ASCII"],
		(*then finally the sysex end*)
		FromDigits["f7", 16]
		}
	]
];


(*this just checks to make sure that the direction is not "HardInput"*)
okayToWrite[dirValue_]:=Module[{},
	(
		dirValue==="SoftOutput"||dirValue===Default||dirValue==="HardOutput"||dirValue==="SoftInput"
	)
];

(*this just checks to make sure that the direction is not "HardOutput"*)
okayToRead[dirValue_]:=Module[{},
	(
		dirValue==="SoftInput"||dirValue===Default||dirValue==="HardInput"||dirValue==="SoftOutput"
	)
];



pwmize[val_]:=Module[{},
	If[TrueQ[val>255],
		(*THEN*)
		(*upper limit is 255*)
		255,
		(*ELSE*)
		(*check the lower limit of 0*)
		If[TrueQ[val<0],
			(*THEN*)
			(*the value is negative, so just make it 0*)
			0,
			(*the value is within the correct range, so just make it an integer*)
			Floor[val]
		]
	]
];


booleanize[val_]:=Module[{},
	If[val<=0,
		0,
		If[val>=1,
			1,
			(*ELSE*)
			(*it is within 0 to 1, so round it*)
			Round[val]
		]
	]
];


(*this converts the string representation for a analog pin into the numberical version*)
(*TODO: deprecate this, it shouldn't be necessary*)
numericalPin[pin_]:=Module[{},pin/.{"A0"->14,"a0"->14,"a1"->15,"A1"->15,"a2"->16,"A2"->16,"a3"->17,"A3"->17,"a4"->18,"A4"->18,"a5"->19,"A5"->19}];


echo=(Print[#];#)&;


(*SYMBOLIC C FUNCTIONS*)

(*gets the name of the function*)
symcolicCFuncName[function_CFunction]:=function[[2]];

(*gets the return type of the function, in an ArduinoLink friendly format*)
symbolicCFuncReturnType[function_CFunction]:=function[[1]]/.{
		"long"->Integer,
		"int"->Integer,
		"short"->Integer,
		"byte"->Integer,
		"char"->Integer,
		"float"->Real,
		"double"->Real,
		(*number arrays are not implemented as return types*)
		(*CPointerType["double"]->{Real},
		CPointerType["float"]->{Real},*)
		CPointerType["char"]->String
		(*CPointerType["long"]->{Integer},
		CPointerType["int"]->{Integer},
		CPointerType["short"]->{Integer},
		CPointerType["byte"]->{Integer}*)
		};

(*gets the list of argument types for the function, in an ArduinoLink friendly format*)
symbolicCFuncArgumentTypes[function_CFunction]:=
	function[[3,All,1]]/.{
		"long"->Integer,
		"int"->Integer,
		"short"->Integer,
		"byte"->Integer,
		"char"->Integer,
		"float"->Real,
		"double"->Real,
		CPointerType["double"]->{Real},
		CPointerType["float"]->{Real},
		CPointerType["char"]->String,
		CPointerType["long"]->{Integer},
		CPointerType["int"]->{Integer},
		CPointerType["short"]->{Integer},
		CPointerType["byte"]->{Integer}
		};
		
(*basically just a rule of the argument types to the return type*)
symbolicCFunctionArgType[function_CFunction]:=Module[{},
	(
		symbolicCFuncArgumentTypes[function]->symbolicCFuncReturnType[function]
	)
];

(*wraps all the function information inside the ExternalFunction wrapper that is necessary for ArduinoLink*)
symbolicCFunctionFull[function_CFunction]:=Module[{},
	(
		ExternalFunction[symbolicCFunctionArgType[function],ToCCodeString[function]]
	)
];


(*this function is a wrapper to convert ArduinoCode style specification to ExternalFunction way of converting that the rest of the package depends on*)
(*input will be of the type: ArduinoCode[<|"ArgumentTypes"->{Integer,{Integer},...},"ReturnType"->Integer,"Code"->codeString|>]*)
(*output will be of the type: ExternalFunction[{Integer,{Integer}}->Integer,codeString]*)
externalFunctionConvert[arduinoFunc_]:=Module[
	{
		function=arduinoFunc[[1]]
	},
	(
		If[KeyExistsQ["ReturnType"][function]&&function["ReturnType"]=!={},
			(*THEN*)
			(*it exists, and it's not an empty list, so include a return type*)
			(
				Return[ExternalFunction[Flatten[{function["ArgumentTypes"]},1]->function["ReturnType"],function["Code"]]/.{_Missing->{}}]
			),
			(*ELSE*)
			(*it doesn't exist, or it is an empty list, so don't include a return type*)
			(
				Return[ExternalFunction[Flatten[{function["ArgumentTypes"]},1],function["Code"]]/.{_Missing->{}}]
			)
		]
	)
];

(*check to see if an Arduino driver is already registered before registering the class*)
If[Not[Devices`DeviceAPI`DeviceDump`knownClassQ["Arduino"]],
	(*THEN*)
	(*there aren't any arduino drivers registered, so we should register it one*)
	DeviceFramework`DeviceClassRegister["Arduino",
		"Firmata",
		"ReadFunction"->ArduinoLink`Private`ArduinoReadDriver,
		"WriteFunction"->ArduinoLink`Private`ArduinoWriteDriver,
		"ConfigureFunction"->ArduinoLink`Private`ArduinoConfigureDriverWrapper,
		"PreconfigureFunction"->ArduinoLink`Private`ArduinoPreConfigureDriver,
		"DeregisterOnClose"->False,
		"ExecuteFunction"->ArduinoLink`Private`ArduinoExecuteDriver,
		"Properties"->{
			"PinConfigurations"->ArduinoLink`Private`$DeviceStates,
			"ArduinoInstallLocation"->None,
			"SerialPort"->""
			(*TODO: Implement the scheduled task handler property*)
			(*"ScheduledTaskValues"->{}*)
		},
		"GetPropertyFunction"->ArduinoLink`Private`ArduinoPropertyGetDriver,
		"SetPropertyFunction"->ArduinoLink`Private`ArduinoPropertySetDriver,
		"OpenFunction"->ArduinoLink`Private`ArduinoOpenDriver,
		"DeviceIconFunction"->ArduinoLink`Private`ArduinoCommunityLogo,
		"MakeManagerHandleFunction"->ArduinoLink`Private`ManagerHandleDriver,
		"DriverVersion"->1.1,
		"ReadTimeSeriesFunction"->DeviceRead
	]
	(*ELSE*)
	(*there already is a driver registered, so don't register anything*)
]




End[];

EndPackage[]


