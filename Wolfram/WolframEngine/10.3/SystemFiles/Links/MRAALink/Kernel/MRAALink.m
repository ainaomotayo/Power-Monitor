(* Wolfram Language Package *)

(* Created by the Wolfram Workbench Apr 3, 2015 *)

(*Copyright Wolfram Research 2015*)
(*Author: Ian Johnson <ijohnson@wolfram.com>*)

BeginPackage["MRAALink`"]
(* Exported symbols added here with SymbolName::usage *) 


Begin["`Private`"]
(* Implementation of the package *)

Needs["RaspberryPiTools`"];
Needs["PacletManager`"];

(*messages*)
DeviceRead::invalidArgs="DeviceRead for I2C requires an address to read from";
DeviceOpen::noAddress="No device found with address `1` on I2C bus";
DeviceOpen::needApt="The apt package libi2c-dev was not found. It needs to be installed, it can be installed by running \"sudo apt-get update && sudo apt-get install -y libi2c-dev\"";
DeviceOpen::unknownFail="The I2C kernel modules are set to load in \"/etc/modules\", but are not currently loaded. Try rebooting and running \"sudo modprobe i2c-dev && sudo modprobe i2c-bcm2708\"";
DeviceOpen::i2cBlacklist="The I2C kernel modules are currently being blacklisted from running in the file \"/etc/modprobe.d/raspi-blacklist.conf\", remove or comment out the line \" `1` \" from this file and reboot"
DeviceOpen::needModule="The module `1` is not loaded and needs to be loaded. Load it by adding the line \" `1` \" to the file \"/etc/modules\" with a text editor and reboot.";
DeviceOpen::deviceTree="The device tree overlay for I2C has not been turned on. Turn it on by editing the file \"/boot/config.txt\" to add the line \n`1`\n and reboot.";
DeviceOpen::noDeviceTreeItem="The device tree item for I2C was not found. Try rebooting";
DeviceOpen::i2cBus="The I2C bus must be specified";
DeviceOpen::mraaInitFailure="Failed to initialize the MRAA library";

DeviceRead::notPin="`1` is not a pin on the Raspberry Pi";
DeviceRead::gpiopins="The pins `1` are not configurable for GPIO.";
DeviceRead::gpioargs="A pin number of list of pin numbers is expected.";
DeviceRead::nonZeroRead="The number of bytes to read off of an I2C bus must be non-zero.";
DeviceRead::unknownMRAAReadError="An unknown error occured reading from the I2C bus.";
DeviceRead::invalidAddress="The address `1` is invalid, valid address are 8 through 119."
DeviceRead::invalidNumberBytes="Can only read a positive number of bytes.";

DeviceConfigure::gpioargs="A rule or list of rules specifying pin direction p->dir is expected.";
DeviceConfigure::gpioPins="The pins `1` are not configurable for GPIO.";
DeviceConfigure::gpioConfigValues="Valid directions are \"Input\" or \"Output\".";

DeviceWrite::detachedDevice="The device on address `1` is no longer attached.";
DeviceWrite::unknownMRAAWriteError="An unknown error occured writing to the I2C bus.";
DeviceWrite::I2Cbytes="Only Integers between 0 and 255 can be written to the I2C bus.";


$MRAALinkNoError = 0;
$MRAALinkGPIOInvalidPin = -1;
$MRAALinkGPIOInitFailure = -2;
$MRAALinkGPIOInvalidDirection = -3;
$MRAALinkGPIOSetDirectionFailure = -4;
$MRAALinkGPIOInvalidPinValue = -5;
$MRAALinkGPIOWriteFailure = -6;
$MRAALinkGPIOCloseFailure = -7;
$MRAALinkI2CWriteInvalidRank = -8;
$MRAALinkI2CWriteAddressSetFailure = -9;
$MRAALinkI2CWriteFailure = -10;
$MRAALinkI2CFileOpenError = -11;
$MRAALinkI2CioctlError = -12;
$MRAALinkI2CReadFailure = -13;
$MRAALinkLibInitFailure = -14;


validMRAARaspi2Pins=    {3 ,5 ,7 ,11,12,13,15,16,18,21,22,29,31,32,33,35,36,37,38,40};
validBCMRaspi2Pins=     {2 ,3 ,4 ,17,18,27,22,23,24,9 ,25,5 ,6 ,12,13,19,16,26,20,21};
validWiringPiRaspi2Pins={8 ,9 ,7 ,0 ,1 ,2 ,3 ,4 ,5 ,13,6 ,21,22,26,23,24,27,25,28,29};

$CurrentPlatform=None;

$CurrentPinMapping=None;

(*the user will input bcm pins*)
BCMPinToMRAAPin=AssociationThread[validBCMRaspi2Pins,validMRAARaspi2Pins];

WiringPi2PinToMRAAPin=AssociationThread[validWiringPiRaspi2Pins,validMRAARaspi2Pins];

$Pins =
<|
	(*compute module not supported at this time*)
	"Raspberry Pi Compute Module Rev 1"-><||>,
	"Raspberry Pi Model A+ Rev 1"->
		<|
			"PinMappings"->
				<|
					"WiringPi"->
						<|0 -> 11, 1 -> 12, 2 -> 13, 3 -> 15, 4 -> 16, 5 -> 18, 6 -> 22, 7 -> 7, 8 -> 3, 9 -> 5, 10 -> 24, 11 -> 26, 12 -> 19, 13 -> 21, 14 -> 23, 15 -> 8, 16 -> 10, 21 -> 29, 22 -> 31, 23 -> 33, 24 -> 35, 25 -> 37, 26 -> 32, 27 -> 36, 28 -> 38, 29 -> 40|>,
					"BCM"->
						<|2 -> 3, 3 -> 5, 4 -> 7, 7 -> 26, 8 -> 24, 9 -> 21, 10 -> 19, 11 -> 23, 14 -> 8, 15 -> 10, 17 -> 11, 18 -> 12, 27 -> 13, 22 -> 15, 23 -> 16, 24 -> 18, 9 -> 21, 25 -> 22, 5 -> 29, 6 -> 31, 12 -> 32, 13 -> 33, 19 -> 35, 16 -> 36, 26 -> 37, 20 -> 38, 21 -> 40|>,
					"Physical"->
						<|3 -> 3, 5 -> 5, 7 -> 7, 8 -> 8, 10 -> 10, 11 -> 11, 12 -> 12, 13 -> 13, 15 -> 15, 16 -> 16, 18 -> 18, 19 -> 19, 21 -> 21, 22 -> 22,23 -> 23, 24 -> 24, 26 -> 26, 29 -> 29, 31 -> 31, 32 -> 32, 33 -> 33, 35 -> 35, 36 -> 36, 37 -> 37, 38 -> 38, 40 -> 40|>,
					(*this association is for the intermediate design where Device functions have to handle "WiringPi2", etc. as well as BCM*)
					(*it's DelaySet so it can reference itself*)
					"BCMWithWiringPiString":>
						Join[
							$Pins["Raspberry Pi Model A+ Rev 1"]["PinMappings"]["BCM"],
							AssociationThread[
								Map[
									(StringJoin["WiringPi",ToString[#]])&,
									Keys[$Pins["Raspberry Pi Model A+ Rev 1"]["PinMappings"]["WiringPi"]]
								],
								Values[$Pins["Raspberry Pi Model A+ Rev 1"]["PinMappings"]["WiringPi"]]
							]
						]
				|>,
			"DefaultPinMapping"->"BCMWithWiringPiString"
		|>,
	"Raspberry Pi Model A Rev 2"->
		<|
			"PinMappings"->
				<|
					"WiringPi"->
						<|0 -> 11, 1 -> 12, 2 -> 13, 3 -> 15, 4 -> 16, 5 -> 18, 6 -> 22, 7 -> 7, 8 -> 3, 9 -> 5, 10 -> 24, 11 -> 26, 12 -> 19, 13 -> 21, 14 -> 23, 15 -> 8, 16 -> 10|>,
					"BCM"->
						<|2 -> 3, 3 -> 5, 4 -> 7, 7 -> 26, 8 -> 24, 9 -> 21, 10 -> 19, 11 -> 23, 14 -> 8, 15 -> 10, 17 -> 11, 18 -> 12, 22 -> 15, 23 -> 16, 24 -> 18, 25 -> 22, 27 -> 13|>,
					"Physical"->
						<|3 -> 3, 5 -> 5, 7 -> 7, 8 -> 8, 10 -> 10, 11 -> 11, 12 -> 12, 13 -> 13, 15 -> 15, 16 -> 16, 18 -> 18, 19 -> 19, 21 -> 21, 22 -> 22,23 -> 23, 24 -> 24, 26 -> 26|>,
					(*this association is for the intermediate design where Device functions have to handle "WiringPi2", etc. as well as BCM*)
					(*it's DelaySet so it can reference itself*)
					"BCMWithWiringPiString":>
						Join[
							$Pins["Raspberry Pi Model A Rev 2"]["PinMappings"]["BCM"],
							AssociationThread[
								Map[
									(StringJoin["WiringPi",ToString[#]])&,
									Keys[$Pins["Raspberry Pi Model A Rev 2"]["PinMappings"]["WiringPi"]]
								],
								Values[$Pins["Raspberry Pi Model A Rev 2"]["PinMappings"]["WiringPi"]]
							]
						]
				|>,
			"DefaultPinMapping"->"BCMWithWiringPiString"
		|>,
	"Raspberry Pi Model B Rev 1"->
		<|
			"PinMappings"->
				<|
					"WiringPi"->
						<|0 -> 11, 1 -> 12, 2 -> 13, 3 -> 15, 4 -> 16, 5 -> 18, 6 -> 22, 7 -> 7, 8 -> 3, 9 -> 5, 10 -> 24, 11 -> 26, 12 -> 19, 13 -> 21, 14 -> 23, 15 -> 8, 16 -> 10|>,
					"BCM"->
						<|2 -> 3, 3 -> 5, 4 -> 7, 7 -> 26, 8 -> 24, 9 -> 21, 10 -> 19, 11 -> 23, 14 -> 8, 15 -> 10, 17 -> 11, 18 -> 12, 22 -> 15, 23 -> 16, 24 -> 18, 25 -> 22, 27 -> 13|>,
					"Physical"->
						<|3 -> 3, 5 -> 5, 7 -> 7, 8 -> 8, 10 -> 10, 11 -> 11, 12 -> 12, 13 -> 13, 15 -> 15, 16 -> 16, 18 -> 18, 19 -> 19, 21 -> 21, 22 -> 22,23 -> 23, 24 -> 24, 26 -> 26|>,
					(*this association is for the intermediate design where Device functions have to handle "WiringPi2", etc. as well as BCM*)
					(*it's DelaySet so it can reference itself*)
					"BCMWithWiringPiString":>
						Join[
							$Pins["Raspberry Pi Model B Rev 2"]["PinMappings"]["BCM"],
							AssociationThread[
								Map[
									(StringJoin["WiringPi",ToString[#]])&,
									Keys[$Pins["Raspberry Pi Model B Rev 2"]["PinMappings"]["WiringPi"]]
								],
								Values[$Pins["Raspberry Pi Model B Rev 2"]["PinMappings"]["WiringPi"]]
							]
						]
				|>
			,"DefaultPinMapping"->"BCMWithWiringPiString"
		|>,
	"Raspberry Pi Model B Rev 2"->
		<|
			"PinMappings"->
				<|
					"WiringPi"->
						<|0 -> 11, 1 -> 12, 2 -> 13, 3 -> 15, 4 -> 16, 5 -> 18, 6 -> 22, 7 -> 7, 8 -> 3, 9 -> 5, 10 -> 24, 11 -> 26, 12 -> 19, 13 -> 21, 14 -> 23, 15 -> 8, 16 -> 10|>,
					"BCM"->
						<|2 -> 3, 3 -> 5, 4 -> 7, 7 -> 26, 8 -> 24, 9 -> 21, 10 -> 19, 11 -> 23, 14 -> 8, 15 -> 10, 17 -> 11, 18 -> 12, 22 -> 15, 23 -> 16, 24 -> 18, 25 -> 22, 27 -> 13|>,
					"Physical"->
						<|3 -> 3, 5 -> 5, 7 -> 7, 8 -> 8, 10 -> 10, 11 -> 11, 12 -> 12, 13 -> 13, 15 -> 15, 16 -> 16, 18 -> 18, 19 -> 19, 21 -> 21, 22 -> 22,23 -> 23, 24 -> 24, 26 -> 26|>,
					(*this association is for the intermediate design where Device functions have to handle "WiringPi2", etc. as well as BCM*)
					(*it's DelaySet so it can reference itself*)
					"BCMWithWiringPiString":>
						Join[
							$Pins["Raspberry Pi Model B Rev 2"]["PinMappings"]["BCM"],
							AssociationThread[
								Map[
									(StringJoin["WiringPi",ToString[#]])&,
									Keys[$Pins["Raspberry Pi Model B Rev 2"]["PinMappings"]["WiringPi"]]
								],
								Values[$Pins["Raspberry Pi Model B Rev 2"]["PinMappings"]["WiringPi"]]
							]
						]
				|>
			,"DefaultPinMapping"->"BCMWithWiringPiString"
		|>,
	"Raspberry Pi Model B+ Rev 1"->
		<|
			"PinMappings"->
				<|
					"WiringPi"->
						<|0 -> 11, 1 -> 12, 2 -> 13, 3 -> 15, 4 -> 16, 5 -> 18, 6 -> 22, 7 -> 7, 8 -> 3, 9 -> 5, 10 -> 24, 11 -> 26, 12 -> 19, 13 -> 21, 14 -> 23, 15 -> 8, 16 -> 10, 21 -> 29, 22 -> 31, 23 -> 33, 24 -> 35, 25 -> 37, 26 -> 32, 27 -> 36, 28 -> 38, 29 -> 40|>,
					"BCM"->
						<|2 -> 3, 3 -> 5, 4 -> 7, 7 -> 26, 8 -> 24, 9 -> 21, 10 -> 19, 11 -> 23, 14 -> 8, 15 -> 10, 17 -> 11, 18 -> 12, 27 -> 13, 22 -> 15, 23 -> 16, 24 -> 18, 9 -> 21, 25 -> 22, 5 -> 29, 6 -> 31, 12 -> 32, 13 -> 33, 19 -> 35, 16 -> 36, 26 -> 37, 20 -> 38, 21 -> 40|>,
					"Physical"->
						<|3 -> 3, 5 -> 5, 7 -> 7, 8 -> 8, 10 -> 10, 11 -> 11, 12 -> 12, 13 -> 13, 15 -> 15, 16 -> 16, 18 -> 18, 19 -> 19, 21 -> 21, 22 -> 22,23 -> 23, 24 -> 24, 26 -> 26, 29 -> 29, 31 -> 31, 32 -> 32, 33 -> 33, 35 -> 35, 36 -> 36, 37 -> 37, 38 -> 38, 40 -> 40|>,
					(*this association is for the intermediate design where Device functions have to handle "WiringPi2", etc. as well as BCM*)
					(*it's DelaySet so it can reference itself*)
					"BCMWithWiringPiString":>
						Join[
							$Pins["Raspberry Pi Model B+ Rev 1"]["PinMappings"]["BCM"],
							AssociationThread[
								Map[
									(StringJoin["WiringPi",ToString[#]])&,
									Keys[$Pins["Raspberry Pi Model B+ Rev 1"]["PinMappings"]["WiringPi"]]
								],
								Values[$Pins["Raspberry Pi Model B+ Rev 1"]["PinMappings"]["WiringPi"]]
							]
						]
				|>,
			"DefaultPinMapping"->"BCMWithWiringPiString"
		|>,
	"Raspberry Pi 2 Model B Rev 1"->
		<|
			"PinMappings"->
				<|
					"WiringPi"->
						<|0 -> 11, 1 -> 12, 2 -> 13, 3 -> 15, 4 -> 16, 5 -> 18, 6 -> 22, 7 -> 7, 8 -> 3, 9 -> 5, 10 -> 24, 11 -> 26, 12 -> 19, 13 -> 21, 14 -> 23, 15 -> 8, 16 -> 10, 21 -> 29, 22 -> 31, 23 -> 33, 24 -> 35, 25 -> 37, 26 -> 32, 27 -> 36, 28 -> 38, 29 -> 40|>,
					"BCM"->
						<|2 -> 3, 3 -> 5, 4 -> 7, 7 -> 26, 8 -> 24, 9 -> 21, 10 -> 19, 11 -> 23, 14 -> 8, 15 -> 10, 17 -> 11, 18 -> 12, 27 -> 13, 22 -> 15, 23 -> 16, 24 -> 18, 9 -> 21, 25 -> 22, 5 -> 29, 6 -> 31, 12 -> 32, 13 -> 33, 19 -> 35, 16 -> 36, 26 -> 37, 20 -> 38, 21 -> 40|>,
					"Physical"->
						<|3 -> 3, 5 -> 5, 7 -> 7, 8 -> 8, 10 -> 10, 11 -> 11, 12 -> 12, 13 -> 13, 15 -> 15, 16 -> 16, 18 -> 18, 19 -> 19, 21 -> 21, 22 -> 22,23 -> 23, 24 -> 24, 26 -> 26, 29 -> 29, 31 -> 31, 32 -> 32, 33 -> 33, 35 -> 35, 36 -> 36, 37 -> 37, 38 -> 38, 40 -> 40|>,
					(*this association is for the intermediate design where Device functions have to handle "WiringPi2", etc. as well as BCM*)
					(*it's DelaySet so it can reference itself*)
					"BCMWithWiringPiString":>
						Join[
							$Pins["Raspberry Pi 2 Model B Rev 1"]["PinMappings"]["BCM"],
							AssociationThread[
								Map[
									(StringJoin["WiringPi",ToString[#]])&,
									Keys[$Pins["Raspberry Pi 2 Model B Rev 1"]["PinMappings"]["WiringPi"]]
								],
								Values[$Pins["Raspberry Pi 2 Model B Rev 1"]["PinMappings"]["WiringPi"]]
							]
						]
				|>,
			"DefaultPinMapping"->"BCMWithWiringPiString"
		|>
|>


makeManagerHandle[___]:=CreateUUID[];


$Devices = <||>;

$PinConfigurations=<||>;

(*initially nothing is setup*)
$librarySetupQ = False;
$gpioSetupQ = False;
$I2CSetup = False;

gpioSetup[]:=Module[
	{
		mraaLinkLibrary = FindLibrary["MRAALink"]
	},
	(
		(*this symbol is for future functionality relating to custom gpio initialization options*)
		GPIOInitialize := Null;
		
		GetPlatform = LibraryFunctionLoad[mraaLinkLibrary,"GetPlatform",{},String];
		
		GPIOSetup = LibraryFunctionLoad[mraaLinkLibrary,"InitializePin",{Integer},Integer];
		
		GPIOSetDirection = LibraryFunctionLoad[mraaLinkLibrary,"SetDirection",{Integer,Integer},Integer];
		
		GPIOWrite = LibraryFunctionLoad[mraaLinkLibrary,"WritePin",{Integer,Integer},Integer];
		
		GPIORead = LibraryFunctionLoad[mraaLinkLibrary,"ReadPin",{Integer},Integer];
		
		GPIOClose = LibraryFunctionLoad[mraaLinkLibrary,"ClosePin",{Integer},Integer];
		
		$gpioSetupQ = True;
	)
];

gpioSetup[];


librarySetup[]:=Module[
	{
		etcModules = FileNameJoin[{"/etc","modules"}],
		bootConfig = FileNameJoin[{"/boot","config.txt"}],
		raspiBlacklist = FileNameJoin[{"/etc","modprobe.d","raspi-blacklist.conf"}],
		mraaLinkLibrary = FindLibrary["MRAALink"]
	},
	(
		(*initialize the messages*)
		(*we don't print off messages in this function, so we save the messages*)
		messages={};

		(*now before loading the i2c functions, we need to check on the state of I2C on this machine*)
		bootParameters = RaspberryPiTools`Private`configFileParse[bootConfig];
		
		modulesToLoad = RaspberryPiTools`Private`configFileParse[etcModules];
		
		blacklistedModules = If[FileExistsQ[raspiBlacklist],RaspberryPiTools`Private`configFileParse[raspiBlacklist],{}];
		
		(*get the loaded modules from lsmod*)
		(*this one-liner will split the output by lsmod by new lines (which seperates the different modules),*)
		(*then splits the individual module strings by whitespace, and the first of each of those lists are the module names*)
		(*note we take the second element on, as the first element is a header*)
		loadedModules = (StringSplit/@StringSplit[Import["!lsmod", "Text"], "\n"][[2 ;;]])[[All, 1]];
		
		(*cleaner implementation below with SemanticImport, when SemanticImport works on the Pi...*)
		(*loadedModules = Normal[SemanticImport["!lsmod"][All,"Module"]];*)
		
		needsUserConfig = False;
		
		If[Not[MemberQ[loadedModules,"i2c_dev"]&&MemberQ[loadedModules,"i2c_bcm2708"]],
			(*THEN*)
			(*one of the modules isn't loaded*)
			(
				Which[
					(*neither of the modules were loaded*)
					Not[MemberQ[loadedModules,"i2c_bcm2708"]]&&Not[MemberQ[loadedModules,"i2c_dev"]],
					(
						(*because neither of the modules are loaded, check to see if the modules are set to load in /etc/modules*)
						Which[
							(*both modules are in /etc/modules*)
							MemberQ[modulesToLoad,"i2c-dev"]&&MemberQ[modulesToLoad,"i2c-bcm2708"],
							(
								(*check the blacklist files for the modules*)
								If[MemberQ[blacklistedModules,"blacklist i2c-bcm2708"],
									(*THEN*)
									(*the module is being blacklisted*)
									(
										AppendTo[messages,{DeviceOpen::i2cBlacklist,"blacklist i2c-bcm2708"}];
										needsUserConfig = True;
									),
									(*ELSE*)
									(*the module isn't being blacklisted, but the modules still aren't loaded*)
									(
										AppendTo[messages,{DeviceOpen::unknownFail}];
										needsUserConfig = True;
									)
								]
							),
							(*just the i2c-dev module is in /etc/modules*)
							MemberQ[modulesToLoad,"i2c-dev"],
							(
								(*prompt the user that the i2c-bcm2708 module needs to be loaded*)
								AppendTo[messages,{DeviceOpen::needModule,"i2c-bcm2708"}];
								(*also worth checking the blacklist file*)
								If[MemberQ[blacklistedModules,"blacklist i2c-bcm2708"],
									(*THEN*)
									(*the module is being blacklisted*)
									(
										AppendTo[messages,{DeviceOpen::i2cBlacklist,"blacklist i2c-bcm2708"}];
										needsUserConfig = True;
									),
									(*ELSE*)
									(*the module isn't being blacklisted*)
									(
										(*there will already have been a message about the i2c-bcm2708 module, so just return $Failed*)
										needsUserConfig = True;
									)
								];
							),
							MemberQ[modulesToLoad,"i2c-bcm2708"],
							(
								(*prompt the user that the i2c-dev module needs to be loaded*)
								AppendTo[messages,{DeviceOpen::needModule,"i2c-dev"}];
								(*also worth checking the blacklist file*)
								If[MemberQ[blacklistedModules,"blacklist i2c-bcm2708"],
									(*THEN*)
									(*the module is being blacklisted*)
									(
										AppendTo[messages,{DeviceOpen::i2cBlacklist,"blacklist i2c-bcm2708"}];
										needsUserConfig = True;
									),
									(*ELSE*)
									(*the module isn't being blacklisted*)
									(
										(*there will already have been a message about the i2c-dev module, so just return $Failed*)
										needsUserConfig = True;
									)
								];
							),
							(*neither of the modules are in /etc/modules*)
							True,
							(
								(*here because neither of the files are in there we can just check the blacklist,*)
								(*then prompt user to append modules to /etc/modules and return $Failed*)
								If[MemberQ[blacklistedModules,"blacklist i2c-bcm2708"],AppendTo[messages,{DeviceOpen::i2cBlacklist,"blacklist i2c-bcm2708"}]];
								AppendTo[messages,{DeviceOpen::needModule,"i2c-dev"}];
								AppendTo[messages,{DeviceOpen::needModule,"i2c-bcm2708"}];
								needsUserConfig = True;
							)
						]
					),
					(*just i2c-dev isn't loaded*)
					Not[MemberQ[loadedModules,"i2c-dev"]],
					(
						(*because this means that the i2c-bcm2708 module is loaded fine, which means we don't need to check the blacklist*)
						If[MemberQ[modulesToLoad,"i2c-dev"],
							(*THEN*)
							(*the i2c-dev module should be loaded, but it isn't for some reason*)
							(
								AppendTo[messages,{DeviceOpen::unknownFail}];
								needsUserConfig = True;
							),
							(*ELSE*)
							(*the i2c-dev module was never told to be loaded, so issue message for user to do that*)
							(
								AppendTo[messages,{DeviceOpen::needModule,"i2c-dev"}];
								needsUserConfig = True;
							)
						];
					),
					(*any other case*)
					(*note that i2c-dev can't be loaded without i2c_bcm2708, so we don't have to worry aobut the case of just i2c-dev being loaded*)
					True,
					(
						Null
					)
				]
			)
		];
		
		(*finally check that the /dev/i2c* device exists for this device*)
		
		If[FileNames["i2c*","/dev"]==={},
			(*THEN*)
			(*the device tree is missing i2c*)
			(
				(*finally check the boot parameters in /boot/config.txt to make sure the device tree overlay exists*)
				If[Not[MemberQ[bootParameters,"dtparam=i2c=on"]],
					(*THEN*)
					(*it's missing, so prompt the user to modify that*)
					(
						AppendTo[messages,{DeviceOpen::deviceTree,"dtparam=i2c=on"}];
						needsUserConfig = True;
					),
					(*ELSE*)
					(*it's not missing, so issue a message*)
					(
						AppendTo[messages,{DeviceOpen::noDeviceTreeItem}];
						needsUserConfig = True;
					)
				]
			)
		];
		
		If[TrueQ[needsUserConfig],
			(*THEN*)
			(*the user needs to do stuff, so return $Failed and don't load any of the library, and also reset the options for the driver*)
			(
				$I2CSetup = False;
				DeviceFramework`DeviceDriverOption["I2C","OpenFunction"]=I2COpenFail;
				DeviceFramework`DeviceDriverOption["I2C","FindDevices"]=I2CFindDevicesFail;
				Return[$Failed]
			),
			(*ELSE*)
			(*no use config, but initialize the library*)
			(
				InitializeI2C = LibraryFunctionLoad[mraaLinkLibrary,"InitializeI2C",{},Integer];
				
				Switch[InitializeI2C[],
					$MRAALinkNoError, (*no errors - continue*)
					(
						$I2CSetup = True;
					),
					$MRAALinkLibInitFailure, (*failed to initialize*)
					(
						$I2CSetup = False;
						DeviceFramework`DeviceDriverOption["I2C","OpenFunction"]=I2COpenFail;
						DeviceFramework`DeviceDriverOption["I2C","FindDevices"]=I2CFindDevicesFail;
						AppendTo[messages,{DeviceOpen::mraaInitFailure}];
						Return[$Failed];
					)
				];
				
			)
		];
		
		I2CClose = LibraryFunctionLoad[mraaLinkLibrary,"I2CClose",{},"Void"];
		
		I2CWrite = LibraryFunctionLoad[mraaLinkLibrary,"I2CSlaveWrite",{Integer,{Integer,1}},Integer];
		
		I2CReadNumBytes = LibraryFunctionLoad[mraaLinkLibrary,"I2CReadNumBytes",{Integer,Integer},{Integer,1}];
		
		I2CReadByte = LibraryFunctionLoad[mraaLinkLibrary,"I2CReadByte",{Integer},Integer];
		
		I2CReadByteData = LibraryFunctionLoad[mraaLinkLibrary,"I2CReadByteData",{Integer,Integer},Integer];
		
		I2CFindDevices = LibraryFunctionLoad[mraaLinkLibrary,"I2CFindDevices",{},{Integer,1}];
		
		$librarySetupQ = True;
		
		Return[];
	)
];

librarySetup[];


getMRAAPlatformName[]:=Module[{},
	(
		(*first check to make sure the library was setup properly, if it wasn't then setup it up*)
		If[Not[TrueQ[$gpioSetupQ]],
			(*THEN*)
			(*load the library, then initialize the library*)
			(
				If[gpioSetup[]===$Failed,Return[$Failed]];
			)
		];
		Return[GetPlatform[]];
	)
]

(*load the library as soon as the package is loaded*)


(********************************************************************)
(*******************************I2C**********************************)
(********************************************************************)

WriteI2C[addr_Integer->data_]:=Module[
	{
		(*this ensures all data is in a list*)
		bytes = Flatten[{data},Infinity],
		writeResult
	},
	(
		(*TODO: determine if it is necessary to check if bytes is empty by seeing what happens when one tries to send an empty list*)
		(*(*next check to make sure that bytes isn't empty*)
		If[bytes==={},
			(*THEN*)
			(*the write will do nothing so, raise a message and return $Failed*)
			(
				(*TODO: add message here*)
				Return[$Failed];
			)
		];*)
		(*first make sure that all of the numbers are actual bytes*)
		If[AllTrue[bytes,(IntegerQ[#]&&#>=0&&#<=255)&],
			(*THEN*)
			(*the bytes are good, write them*)
			(
				writeResult = I2CWrite[addr,bytes];
				Switch[writeResult,
					$MRAALinkNoError, (*no error*)
					(
						Return[addr->bytes];
					),
					$MRAALinkI2CWriteInvalidRank, (*invalid rank passed in - should never happen*)
					(
						(*this should never happen as we check the list before it's passed in*)
						Return[$Failed];
					),
					$MRAALinkI2CWriteAddressSetFailure, (*setting the address failed*)
					(
						(*this should only ever happen if the I2C device is detached after it was attached and connected*)
						Message[DeviceWrite::detachedDevice,addr];
						Return[$Failed];
					),
					$MRAALinkI2CWriteFailure, (*writing to the bus with mraa failed*)
					(
						(*unlikely this would happen*)
						Message[DeviceWrite::unknownMRAAWriteError];
						Return[$Failed];
					),
					_, (*any other value just return $Failed*)
					(
						Return[$Failed];
					)
				];
				
			),
			(*ELSE*)
			(*at least one of the bytes aren't good, raise a message about that one and return $Failed*)
			(
				Message[DeviceWrite::I2Cbytes];
				Return[$Failed];
			)
		];
	)
];


ReadI2C[addr_Integer,numBytes_Integer]:=Module[
	{
		readResult
	},
	(
		(*next check to make sure that numBytes isn't 0*)
		If[numBytes==0,
			(*THEN*)
			(*the read will do nothing so, raise a message and return $Failed*)
			(
				Message[DeviceRead::nonZeroRead];
				Return[$Failed];
			)
		];
		(*now read the bytes*)
		readResult = I2CReadNumBytes[addr,numBytes];
		(*switch on the first element of the result*)
		Switch[First[readResult],
			$MRAALinkNoError,(*no error, just return the actual data*)
			(
				(*the rest of the list is the data*)
				Return[Rest[readResult]];
			),
			$MRAALinkI2CReadFailure, (*the read failed*)
			(
				Message[DeviceRead::unknownMRAAReadError];
				Return[$Failed];
			)
		];
	)
];


ReadByteI2C[addr_Integer]:=Module[{},
	(
		(*the bus has been initialized, so we can just read the bytes*)
		result = I2CReadByte[addr];
		If[result=!=-1,
			(*THEN*)
			(*the read didn't fail, return the result*)
			(
				Return[result]
			),
			(*ELSE*)
			(*the read failed, so return $Failed*)
			(
				Message[DeviceRead::unknownMRAAReadError];
				Return[$Failed]
			)
		];
	)
];



ReadByteDataI2C[addr_Integer,command_Integer]:=Module[{},
	(
		(*the bus has been initialized, so we can just read the bytes*)
		result = I2CReadByteData[addr,command];
		If[result=!=-1,
			(*THEN*)
			(*the read didn't fail, return the result*)
			(
				Return[result]
			),
			(*ELSE*)
			(*the read failed, so return $Failed*)
			(
				Message[DeviceRead::unknownMRAAReadError];
				Return[$Failed]
			)
		];
	)
];


FindI2CDevices[]:=Module[{},
	(
		(*note we don't have to check if I2C has been setup or not, as this function doesn't use the MRAA library utilties*)
		result=I2CFindDevices[];
		Switch[First[result],
			$MRAALinkNoError,(*no error*)
			(
				Return[Rest[result]];
			),
			$MRAALinkI2CFileOpenError,
			(
				(*with the design, we don't print any messages off in FindDevices, but here we know that the device tree overlay is missing*)
				Return[$Failed];
			),
			$MRAALinkI2CioctlError,
			(
				(*failed to try to open the file with i2c*)
				Return[$Failed];
			)
		];
	)
]

(*this function is in case initializing i2c fails, so that FindDevices silently fails as designed*)
I2CFindDevicesFail[___]:=Module[{},
	(
		Return[{{}}];
	)
];

FindI2CDevicesDriver[]:=Module[{},
	(
		Return[List/@FindI2CDevices[]];
	)
];


(*this function is in case initializing i2c fails, so that DeviceOpen fails but prints off all the messages from librarySetup*)
I2COpenFail[___]:=Module[{},
	(
		(*just print off the relevant messages, and return $Failed*)
		Message@@@messages;
		Return[$Failed];
	)
];

i2cOpenDriver[ihandle_,___]:=Module[{},
	(
		Message[DeviceOpen::i2cBus];
		Return[$Failed];
	)
];

i2cOpenDriver[ihandle_,address_Integer,OptionsPattern[]]:=Module[
	{
		deviceHandle = address
	},
	(
		(*here we try and check to see if the address exists on the bus*)
		If[MemberQ[FindI2CDevices[],address],
			(*THEN*)
			(*the address exists on the bus, so good to open*)
			(
				Return[deviceHandle]
			),
			(*ELSE*)
			(*the address doesn't exist, so return $Failed*)
			(
				Message[DeviceOpen::noAddress,address];
				Return[$Failed]
			)
		]
	)
];

(*this is for future implementation where more than one i2c bus exists, but for now just use default bus*)
i2cOpenDriver[ihandle_,address_Integer,bus_Integer,opts_:OptionsPattern[]]:=i2cOpenDriver[ihandle,address,opts];


i2cWriteDriver[{ihandle_,dhandle_},data__]:=Module[
	{
		addr = dhandle
	},
	(
		(*make sure that all the data that was passed is just a list of integers*)
		If[AllTrue[Flatten[{data},Infinity],IntegerQ],
			(*THEN*)
			(*all the arguments passed were integers*)
			(
				WriteI2C[addr->{data}]
			),
			(*ELSE*)
			(*at least some of the arguments were not integers*)
			(
				Message[DeviceWrite::I2Cbytes];
				Return[$Failed]
			)
		]
	)
]


i2cWriteDriver[{ihandle_,dhandle_},data_List]:=Module[
	{
		addr = dhandle
	},
	(
		WriteI2C[addr->data];
	)
]


i2cWriteDriver[{ihandle_,dhandle_},data_Integer]:=Module[
	{
		addr = dhandle
	},
	(
		WriteI2C[addr->{data}];
	)
]

i2cReadDriver[{ihandle_,dhandle_},byteNum_Integer]:=Module[
	{
		addr = dhandle
	},
	(
		(*first make sure that addr is within the appropriate limits*)
		If[addr>=8&&addr<=119,
			(*THEN*)
			(*the address is valid*)
			(
				Which[
					byteNum==1, (*call single byte function*)
					(
						Return[ReadByteI2C[addr]];
					),
					byteNum>0, (*all other valid number of bytes*)
					(
						Return[ReadI2C[addr,byteNum]];
					),
					True, (*less than zero, return $Failed*)
					(
						Message[DeviceRead::invalidNumberBytes];
						Return[$Failed];
					)
				]
			),
			(*ELSE*)
			(*the address is invalid, raise a message and return $Failed*)
			(
				Message[DeviceRead::invalidAddress,addr];
				Return[$Failed];
			)
		];
		
	)
];



i2cReadDriver[{ihandle_,dhandle_}]:=Module[
	{
		addr = dhandle
	},
	(
		(*first make sure that addr is within the appropriate limits*)
		If[addr>=8&&addr<=119,
			(*THEN*)
			(*the address is valid*)
			(
				Return[ReadByteI2C[addr]];
			),
			(*ELSE*)
			(*the address is invalid, raise a message and return $Failed*)
			(
				Message[DeviceRead::invalidAddress,addr];
				Return[$Failed];
			)
		];
		
	)
];


i2cReadDriver[{ihandle_,dhandle_},args___]:=Module[{},
	(
		(*unsupported arguments, raise a message and return $Failed*)
		(*TODO: add message here about unsupported arguments*)
		Message[DeviceRead::invalidArgs];
		Return[$Failed];
	)
];


(***************************************************************)
(*************************GPIO**********************************)
(***************************************************************)


gpioOpenDriver[ihandle_,args___]:=Module[{},
	(
		(*first set the current platform to this platform*)
		$CurrentPlatform = getMRAAPlatformName[];
		(*the default pin mapping enabled is in $Pins*)
		$CurrentPinMapping = $Pins[$CurrentPlatform]["DefaultPinMapping"];
		(*now generate $PinConfigurations for this platform*)
		$PinConfigurations=AssociationThread[
			(*use the Values as the keys for this association, because $PinConfiguration uses the hardware pins as keys*)
			Union[Values[$Pins[$CurrentPlatform]["PinMappings"][$CurrentPinMapping]]],
			Table[
				(*PWM and ADC are commented out for now, as the RPi only has digital IO*)
				<|"Setup"->False,"Direction"->Default,"LastWriteValue"->None,(*"PWM"->False,"ADC"->False,*)"LastReadValue"->None|>,
				{Union[Values[$Pins[$CurrentPlatform]["PinMappings"][$CurrentPinMapping]]]}
			]
		];
		(*for this implementation, we have to setup the gpio library*)
		If[Not[TrueQ[$gpioSetupQ]],
			(*THEN*)
			(*load the library, then initialize the library*)
			(
				If[gpioSetup[]===$Failed,Return[$Failed]];
			)
		];
		(*next initialize the GPIO library, and use that as the dhandle*)
		(*note that for the current implementation of GPIOInitialize, Null will be used*)
		Return[GPIOInitialize[]];
	)
];

gpioConfigureDriver[{ihandle_,dhandle_},args_Association]:=gpioConfigureDriver@@FlattenAt[{{ihandle,dhandle},Normal[args]},2]


gpioConfigureDriver[{ihandle_,dhandle_}]:=Module[{},
	(
		Message[DeviceConfigure::gpioargs];
		Return[$Failed];
	)
];


gpioConfigureDriver[{ihandle_,dhandle_},Automatic]:=Module[{},
	(
		Message[DeviceConfigure::gpioargs];
		Return[$Failed];
	)
]

gpioConfigureDriver[{ihandle_,dhandle_},args___]:=Module[
	{
		argsList = {args},
		pinMapping = $Pins[$CurrentPlatform]["PinMappings"][$CurrentPinMapping],
		hardwarePins
	},
	(
		If[AllTrue[argsList,Head[#]===Rule&],
			(*THEN*)
			(*all arguments are rules*)
			(
				(*next ensure that the pins actually exist*)
				If[AllTrue[argsList[[All,1]],MemberQ[Keys[pinMapping],#]&],
					(*THEN*)
					(*all of arguments are valid pins*)
					(
						If[AllTrue[argsList[[All,2]],MemberQ[{"Input","Output"},#]&],
							(*THEN*)
							(*all of the arguments' values are good*)
							(
								(*now get the actual hardware pins*)
								hardwarePins = pinMapping/@(argsList[[All,1]]);
								(*next setup any pins that haven't been previously setup*)
								pinsToSetup = Select[$PinConfigurations[#]["Setup"]=!=True&]@hardwarePins;
								GPIOSetup/@pinsToSetup;
								(*next mark all setup we setup as having been setup*)
								($PinConfigurations[#]["Setup"]=True)&/@pinsToSetup;
								(*now actually configure the pins*)
								MapThread[GPIOSetDirection,{hardwarePins,argsList[[All,2]]/.{"Input"->0,"Output"->1}}];
								Do[
									(
										pin=hardwarePins[[pinNumber]];
										pinConfigValue=argsList[[pinNumber,2]];
										(*now mark the pins as HardInput or HardOutput*)
										If[pinConfigValue==="Input",
											(*THEN*)
											(*the pin is an input*)
											(
												$PinConfigurations[pin]["Direction"]="HardInput";
											),
											(*ELSE*)
											(*the pin is an output*)
											(
												$PinConfigurations[pin]["Direction"]="HardOutput";
											)
										];
									),
									{pinNumber,Length[hardwarePins]}
								]
							),
							(*ELSE*)
							(*at least one of the arguments' values is bad*)
							(
								(*TODO: raise message about which specific values are bad*)
								Message[DeviceConfigure::gpioConfigValues];
								Return[$Failed];
							)
						]
					),
					(*ELSE*)
					(*at least one of the pins is bad*)
					(
						Message[DeviceConfigure::gpioPins];
						Return[$Failed];
					)
				]
			),
			(*ELSE*)
			(*at least one argument isn't a rule*)
			(
				Message[DeviceConfigure::gpioargs];
				Return[$Failed];
			)
		];
	)
]


(*for associations, just convert it to a list of rules and call the normal function*)
gpioWriteDriver[{ihandle_,dhandle_},args_Association]:=gpioWriteDriver@@FlattenAt[{{ihandle,dhandle},Normal[args]},2]


gpioWriteDriver[{ihandle_,dhandle_},args___]:=Module[
	{
		argsList = {args},
		pinMapping = $Pins[$CurrentPlatform]["PinMappings"][$CurrentPinMapping],
		hardwarePins
	},
	(
		If[argsList =!= {},
			(*THEN*)
			(*arguments exist*)
			(
				(*now need to check to make sure that the arguments are valid rules*)
				If[AllTrue[argsList,Head[#]===Rule&],
					(*THEN*)
					(*the args are all rules*)
					(
						(*now check the pins*)
						If[AllTrue[argsList[[All,1]],MemberQ[Keys[pinMapping],#]&],
							(*THEN*)
							(*all the pins are good*)
							(
								(*now check all of the values*)
								If[AllTrue[argsList[[All,2]],MemberQ[{0,1},#]&],
									(*THEN*)
									(*all of the values are either 0 or 1*)
									(
										(*now that we know all of the pins and values are good,*)
										(*we need to check to make sure that all of the pins have been setup*)
										(*get the actual pin numbers that MRAA uses*)
										hardwarePins = pinMapping/@argsList[[All,1]];
										(*now setup all the pins that aren't already setup*)
										pinsToSetup = Select[$PinConfigurations[#]["Setup"]=!=True&]@hardwarePins;
										GPIOSetup/@pinsToSetup;
										(*now mark all the pins we setup as setup*)
										($PinConfigurations[#]["Setup"]=True)&/@pinsToSetup;
										(*next, check the direction configurations of the pins to make sure they are all writeable*)
										If[NoneTrue[hardwarePins,$PinConfigurations[#]["Direction"]==="HardInput"&],
											(*THEN*)
											(*all of the pins are writeable*)
											(
												(*set all the pins to be output*)
												GPIOSetDirection[#,1]&/@hardwarePins;
												(*now we can actually write to the pins*)
												results=MapThread[GPIOWrite,{hardwarePins,argsList[[All,2]]}];
												(*next we update $PinConfigurations with this*)
												Do[
													(
														pin=hardwarePins[[pinNumber]];
														value=argsList[[pinNumber,2]];
														(*check if the pin was previously a hard output*)
														If[$PinConfigurations[pin]["Direction"]=!="HardOutput",
															(*THEN*)
															(*the pin wasn't a hard output, so we should set it to SoftOutput*)
															(
																$PinConfigurations[pin]["Direction"]="SoftOutput";
															)
															(*ELSE*)
															(*the pin was a hard output, we shouldn't change that, so don't do anything*)
														];
														(*also set the most recent written value*)
														$PinConfigurations[pin]["LastWriteValue"]= DateObject -> value;
													),
													{pinNumber,Length[hardwarePins]}
												]
											),
											(*ELSE*)
											(*at least one of the pins isn't writeable*)
											(
												(*we need to get the pins the user passed in, so first get all the pin keys that were configured for output*)
												hardInputPins = Select[$PinConfigurations[#]["Direction"]==="HardInput"&]@hardwarePins;
												(*TODO: get better way to do this, as with the "BCMWithWiringPiString" pin mapping, as*)
												(*it might not correctly identify the pin the user passed*)
												(*then, invert the association to do an inverse lookup for the pins*)
												hardInputUserPins = Association[Reverse/@Normal[pinMapping]]/@hardInputPins;
												(*finally, print off a message and return $Failed*)
												Message[DeviceWrite::gpioconfig,hardInputUserPins];
												Return[$Failed];
											)
										]
									),
									(*ELSE*)
									(*at least one of the values isn't 0 or 1*)
									(
										(*TODO: raise message about which specific values are bad*)
										Message[DeviceWrite::gpioValues];
										Return[$Failed];
									)
								]
							),
							(*ELSE*)
							(*at least one of the pins is bad*)
							(
								(*TODO: raise message about which specific pins are bad*)
								Message[DeviceWrite::gpioPins];
								Return[$Failed];
							)
						]
					),
					(*ELSE*)
					(*at least one is bad*)
					(
						(*NOTE: for the redesign, we should only return $Failed for the invalid pins*)
						Message[DeviceWrite::gpioargs,args];
						Return[$Failed];
					)
				]
			),
			(*ELSE*)
			(*no arguments were passed in*)
			(
				(*raise message and return $Failed*)
				Message[DeviceWrite::gpioargs];
				Return[$Failed];
			)
		]
	)
];


gpioReadDriver[{ihandle_,dhandle_},args___]:=Module[
	{
		argsList={args},
		pinMapping = $Pins[$CurrentPlatform]["PinMappings"][$CurrentPinMapping],
		hardwarePins,
		pinValues
	},
	(
		(*before anything else make sure that the arguments exist*)
		If[argsList =!= {},
			(*THEN*)
			(*arguments exist*)
			(
				(*first ensure that all the arguments passed in are valid pins on this platform*)
				If[AllTrue[argsList,MemberQ[Keys[pinMapping],#]&],
					(*THEN*)
					(*all of the arguments are valid pins*)
					(
						(*now get the actual pin numbers that MRAA uses*)
						hardwarePins = pinMapping/@argsList;
						(*now setup all the pins that aren't already setup*)
						pinsToSetup = Select[$PinConfigurations[#]["Setup"]=!=True&]@hardwarePins;
						GPIOSetup/@pinsToSetup;
						(*now log the pins that were setup as having been setup in $PinConfigurations*)
						Do[$PinConfigurations[pin]["Setup"]=True,{pin,pinsToSetup}];
						(*next make sure that all of the pins are readable*)
						If[NoneTrue[hardwarePins,$PinConfigurations[#]["Direction"]==="HardOutput"&],
							(*THEN*)
							(*none of the pins are configured as hard output, so they can all be read from*)
							(
								(*set all the pins to be input*)
								GPIOSetDirection[#,0]&/@hardwarePins;
								(*now read the values from those pins*)
								pinValues=GPIORead/@hardwarePins;
								(*now update $PinConfigurations*)
								Do[
									(
										pin=hardwarePins[[pinPosition]];
										(*first update the pin directions*)
										If[$PinConfigurations[pin]["Direction"]=!="HardInput",
											(*THEN*)
											(*we should set the direction to be SoftInput*)
											(
												$PinConfigurations[pin]["Direction"]="SoftInput";
											)
											(*ELSE*)
											(*it's already hard configured as an input*)
											(*so we shouldn't remove HardInput in place of SoftInput*)
										];
										(*now update the most recently read value*)
										$PinConfigurations[pin]["LastReadValue"]=DateObject[]->pinValues[pinPosition];
									),
									{pinPosition,Length[hardwarePins]}
								];
								(*thread the pins and values together in an association and return that*)
								Return[AssociationThread[argsList,pinValues]];
							),
							(*ELSE*)
							(*at least one of the pins is configured as a hard output and we can't write to it*)
							(
								(*we need to get the pins the user passed in, so first get all the pin keys that were configured for output*)
								hardOutputPins = Select[$PinConfigurations[#]["Direction"]==="HardOutput"&]@hardwarePins;
								(*TODO: get better way to do this, as with the "BCMWithWiringPiString" pin mapping,*)
								(*it might not correctly identify the pin the user passed*)
								(*then, invert the association to do an inverse lookup for the pins*)
								hardOutputUserPins = Association[Reverse/@Normal[pinMapping]]/@hardOutputPins;
								(*finally, print off a message and return $Failed*)
								Message[DeviceRead::gpioconfig,hardOutputUserPins];
								Return[$Failed];
							)
						];
						
					),
					(*ELSE*)
					(*at least one of the arguments is invalid*)
					(
						(*NOTE: for the redesign, we should only return $Failed for the invalid pins*)
						Message[DeviceRead::gpiopins,args];
						Return[$Failed];
					)
				]
			),
			(*ELSE*)
			(*there aren't any arguments*)
			(
				(*raise message and return $Failed*)
				Message[DeviceRead::gpioargs];
				Return[$Failed];
			)
		]
	)
];


gpioPropertyGetDriver[dev_,property_]:=Module[
	{
		pinMapping = $Pins[$CurrentPlatform]["PinMappings"][$CurrentPinMapping]
	},
	(
		If[property === "PinConfigurations",
			(*THEN*)
			(*correct property, generate the dataset*)
			(
				(*we need to replace the hardware pin numbers with GPIOn with n being whatever the current pin mapping is using*)
				Return[
					Dataset[
						AssociationThread[
							ReplaceAll[
								Normal[$PinConfigurations][[All,1]],
								(*need to replace the hardware pin number with "GPIOn", where n is the pin mapping number currently used*)
								(*use MapAt to only replace the second value of all the rules we get from the association*)
								Association@MapAt[
									StringJoin[{"GPIO",ToString[#]}]&,
									(*invert the association so it goes hardwarePin -> PinMappingName*)
									Reverse@Sort@(Reverse/@Normal[pinMapping]),
									{All,2}
								]
							],
							Values[$PinConfigurations]
						]
					]
				]
			),
			(*ELSE*)
			(*incorrect, return $Failed and issue message*)
			(
				Return[$Failed];
			)
		]
	)
];

echo=(Print[#];#)&

i2cIconFunction[___]:=ImageResize[Import[PacletResource["MRAALink","I2CLogo"]],26];

(*icon function from previous GPIO implementation*)
gpioIconFunction[___]:=Graphics[{Thickness[0.038461538461538464`], 
  Style[{FilledCurve[{{{1, 4, 3}, {0, 1, 0}, {1, 3, 3}, {0, 1, 0}, {1,
         3, 3}, {0, 1, 0}, {1, 3, 3}, {0, 1, 0}}}, {{{25.5, 
        2.5}, {25.5, 1.395}, {24.605, 0.5}, {23.5, 0.5}, {2.5, 
        0.5}, {1.395, 0.5}, {0.5, 1.395}, {0.5, 2.5}, {0.5, 
        23.5}, {0.5, 24.605}, {1.395, 25.5}, {2.5, 25.5}, {23.5, 
        25.5}, {24.605, 25.5}, {25.5, 24.605}, {25.5, 23.5}, {25.5, 
        2.5}}}]}, FaceForm[RGBColor[0.941, 0.961, 0.957, 1.]]], 
  Style[{JoinedCurve[{{{1, 4, 3}, {0, 1, 0}, {1, 3, 3}, {0, 1, 0}, {1,
         3, 3}, {0, 1, 0}, {1, 3, 3}, {0, 1, 0}}}, {{{25.5, 
        2.5}, {25.5, 1.395}, {24.605, 0.5}, {23.5, 0.5}, {2.5, 
        0.5}, {1.395, 0.5}, {0.5, 1.395}, {0.5, 2.5}, {0.5, 
        23.5}, {0.5, 24.605}, {1.395, 25.5}, {2.5, 25.5}, {23.5, 
        25.5}, {24.605, 25.5}, {25.5, 24.605}, {25.5, 23.5}, {25.5, 
        2.5}}}, CurveClosed -> {1}]}, JoinForm[{"Miter", 10.}], 
   RGBColor[0.7, 0.7, 0.7, 1.]], 
  Style[{FilledCurve[{{{1, 4, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 
        3}}}, {{{7.717, 13.}, {7.717, 12.26}, {7.116999999999999, 
        11.66}, {6.377, 11.66}, {5.637, 11.66}, {5.0360000000000005`, 
        12.26}, {5.0360000000000005`, 13.}, {5.0360000000000005`, 
        13.739999999999998`}, {5.637, 14.34}, {6.377, 
        14.34}, {7.116999999999999, 14.34}, {7.717, 
        13.739999999999998`}, {7.717, 13.}}}], 
    FilledCurve[{{{1, 4, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 
        3}}}, {{{20.853, 13.}, {20.853, 12.26}, {20.252, 
        11.66}, {19.512, 11.66}, {18.772, 11.66}, {18.172, 
        12.26}, {18.172, 13.}, {18.172, 13.739999999999998`}, {18.772,
         14.34}, {19.512, 14.34}, {20.252, 14.34}, {20.853, 
        13.739999999999998`}, {20.853, 13.}}}]}, 
   FaceForm[RGBColor[0.941, 0.961, 0.957, 1.]]], 
  Style[{FilledCurve[{{{1, 4, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 
        3}}}, {{{17.150000000000002`, 12.803}, {17.150000000000002`, 
        10.559000000000001`}, {15.331, 8.740999999999998}, {13.088, 
        8.740999999999998}, {10.844, 8.740999999999998}, {9.025, 
        10.559000000000001`}, {9.025, 12.803}, {9.025, 
        15.046999999999999`}, {10.844, 16.866}, {13.088, 
        16.866}, {15.331, 16.866}, {17.150000000000002`, 
        15.046999999999999`}, {17.150000000000002`, 12.803}}}], 
    FilledCurve[{{{0, 2, 0}, {0, 1, 0}, {0, 1, 
        0}}}, {{{18.483999999999998`, 17.316000000000003`}, {21.165, 
        17.316000000000003`}, {21.165, 15.337}, {18.483999999999998`, 
        15.337}}}]}, FaceForm[RGBColor[0.938, 0.961, 0.952, 1.]]], 
  Style[{FilledCurve[{{{0, 2, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 
        0}}, {{0, 2, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 
        0}}}, {{{6.299284375, 17.66166875}, {4.31883125, 
        17.66166875}, {4.31883125, 8.08566875}, {6.299284375, 
        8.08566875}, {6.299284375, 
        17.66166875}}, {{7.5078387499999995`, 8.08566875}, {9.02487, 
        8.08566875}, {12.73640125, 17.954684375}, {11.21937, 
        17.954684375}, {7.5078387499999995`, 8.08566875}}}], 
    FilledCurve[{{{1, 4, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 
        3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}}, {{1, 4, 3}, {1, 3, 
        3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1,
         3, 3}}}, {{{17.708440000000003`, 9.496715625}, {16.91044, 
        9.496715625}, {16.272455625, 9.789731249999999}, {15.79240875,
         10.373684375}, {15.326908750000001`, 
        10.959715625000001`}, {15.087924375, 11.7972}, {15.087924375, 
        12.87366875}, {15.087924375, 
        13.952215625000001`}, {15.326908750000001`, 
        14.7897}, {15.79240875, 15.375731250000001`}, {16.272455625, 
        15.959684375000002`}, {16.91044, 
        16.2527}, {17.708440000000003`, 16.2527}, {18.50644, 
        16.2527}, {19.131955625000003`, 
        15.959684375000002`}, {19.597455625000002`, 
        15.375731250000001`}, {20.062955625, 
        14.77723125}, {20.301940000000002`, 
        13.952215625000001`}, {20.301940000000002`, 
        12.87366875}, {20.301940000000002`, 11.7972}, {20.062955625, 
        10.959715625000001`}, {19.597455625000002`, 
        10.373684375}, {19.131955625000003`, 
        9.789731249999999}, {18.50644, 
        9.496715625}, {17.708440000000003`, 
        9.496715625}}, {{22.284471250000003`, 
        12.87366875}, {22.284471250000003`, 
        14.629684375}, {21.804424375000004`, 
        15.947215625}, {20.833940000000002`, 
        16.824184375}, {20.11490875, 
        17.57023125}, {19.065455625000002`, 
        17.942215625000003`}, {17.69597125, 
        17.942215625000003`}, {16.32440875, 
        17.942215625000003`}, {15.287424375, 
        17.57023125}, {14.555924375, 16.824184375}, {13.58544, 
        15.947215625}, {13.10747125, 14.629684375}, {13.10747125, 
        12.87366875}, {13.10747125, 11.159215625}, {13.58544, 
        9.841684375}, {14.555924375, 8.92523125}, {15.287424375, 
        8.179184375}, {16.32440875, 7.8072}, {17.69597125, 
        7.8072}, {19.065455625000002`, 7.8072}, {20.11490875, 
        8.179184375}, {20.833940000000002`, 
        8.92523125}, {21.804424375000004`, 
        9.841684375}, {22.284471250000003`, 
        11.159215625}, {22.284471250000003`, 12.87366875}}}]}, 
   FaceForm[RGBColor[0.7, 0.7, 0.7, 1.]]]}, 
 ImageSize -> {26., 26.}, PlotRange -> {{0., 26.}, {0., 26.}}, 
 AspectRatio -> Automatic];


(*finally register the classes*)
If[Not[Devices`DeviceAPI`DeviceDump`knownClassQ["GPIO"]],
	(*THEN*)
	(*there aren't any arduino drivers registered, so we should register it one*)
	DeviceFramework`DeviceClassRegister["GPIO",
		"OpenFunction"->MRAALink`Private`gpioOpenDriver,
		"ReadFunction"->MRAALink`Private`gpioReadDriver,
		"WriteFunction"->MRAALink`Private`gpioWriteDriver,
		"FindFunction" -> ({{}}&),
		"GetPropertyFunction"->MRAALink`Private`gpioPropertyGetDriver,
		"Properties"->{"PinConfigurations"->Null},
		"ConfigureFunction"->MRAALink`Private`gpioConfigureDriver,
		"MakeManagerHandleFunction"->MRAALink`Private`makeManagerHandle,
		"DeviceIconFunction"->MRAALink`Private`gpioIconFunction,
		"DriverVersion"->2.0
	]
	(*ELSE*)
	(*there already is a driver registered, so don't register anything*)
];

(*also for I2C*)
If[Not[Devices`DeviceAPI`DeviceDump`knownClassQ["I2C"]],
	(*THEN*)
	(*there aren't any i2c drivers registered, so we should register one*)
	DeviceFramework`DeviceClassRegister["I2C",
		"OpenFunction"->MRAALink`Private`i2cOpenDriver,
		"ReadFunction"->MRAALink`Private`i2cReadDriver,
		"WriteFunction"->MRAALink`Private`i2cWriteDriver,
		"FindFunction"->MRAALink`Private`FindI2CDevicesDriver,
		"MakeManagerHandleFunction"->MRAALink`Private`makeManagerHandle,
		(*"DeviceIconFunction"->MRAALink`Private`i2cIconFunction,*)
		"DriverVersion"->1.0
	]
	(*ELSE*)
	(*there already is a driver registered, so don't register anything*)
];




End[]

EndPackage[]

