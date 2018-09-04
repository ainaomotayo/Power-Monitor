(* ::Package:: *)

(* RaspberryPiTools Paclet *)
(* Brett Haines, July 2015 *)
(* This paclet adds functionality for Raspberry Pi addons.  It currently supports the Pi Camera and 
the Sense HAT addons. *)

BeginPackage["RaspberryPiTools`"]


Begin["`Private`"]

(* Needs Statements *)
Needs["PacletManager`"]
Needs["MRAALink`"];
Needs["Quaternions`"]

(* Messages *)
DeviceOpen::missingMMAL="The MMAL libraries are missing.  Please update your Pi.";
DeviceOpen::noDevice="No SenseHAT device was detected.";
DeviceRead::unknownFunction="Unknown sensor function given, please enter one of the following:\ntemperature, humidity, pressure, acceleration, rotation, magnetic field, orientation";


(*this function uses apt-cache to check if an aptitude package exists or not*)
(*it is guaranteed to always return true or false for any given string*)
(*note that apt-cache may print off some stuff to stderr if the package doesn't exist at all*)
aptitudePackageInstalledQ[package_String] := Not[
	StringTrim[
		First[
			StringSplit[
				StringTrim[
					Join[
						StringSplit[
							(*we use apt-cache policy to see if a package is installed or not*)
							Import["!apt-cache policy " <> package, "Text"],
							"\n"
						],
						(*we join this with the following strings because if the package doesn't exist at all, the empty string is returned*)
						{"","Installed:(none)"}
					][[2]]
				],
				"Installed:"
			]
		]
	] === "(none)"
];


(*configFileParse parses out from a file the lines that don't start with a comment, and also cleans out comments from lines with other valid data in them*)
(*it returns a list of the lines, all cleaned of trailing and beginning whitespace characters*)
configFileParse[fName_,commentCharacter_String:"#"]:=Module[
	{
		line,
		modules = {},
		fileName = AbsoluteFileName[fName],
		file
	},
	(
		(*first check to make sure the file exists*)
		If[Not[FileExistsQ[fileName]],Return[$Failed]];
		(*open the file with BinaryFormat set to True so we can use ReadLine properly*)
		file=OpenRead[fileName,BinaryFormat->True];
		(*initially, read a line before entering the while loop*)
		line = ReadLine[file];
		(*now read the entirety of the file, one line at a time, and if the line is a comment or empty,*)
		(*ignore it, else parse it out*)
		While[line =!= EndOfFile,
			If[line =!= "" && StringTake[StringTrim[line], 1] =!= commentCharacter,
				(*THEN*)
				(*the line isn't a comment*)
				If[MemberQ[Characters[line], commentCharacter],
					(*THEN*)
					(*there is a comment somewhere im this line, so we have to trim it*)
					AppendTo[modules,StringTrim[StringDrop[#,First@StringPosition[#,commentCharacter~~___]]&[line]]],
					(*ELSE*)
					(*there isn't a comment, so we can just trim out the whitespace it*)
					AppendTo[modules, StringTrim[line]]
				]
				(*ELSE*)
				(*this line is either empty or a comment, so ignore it*)
			];
			line = ReadLine[file];
		];
		Return[modules];
	)
]/;StringLength[commentCharacter]===1
(*only match the pattern if commentCharacter is actually a single character*)


(*============================================================================*)
(* RaspiCam Code *)
(*============================================================================*)

(* Load the dynamic library RaspiSillLink *)
LibraryLoad["libRaspberryPiTools"];
lib1="libRaspberryPiTools";

(* Link to function in library that gets an image from the camera *)
GetImageBytes = LibraryFunctionLoad[lib1, "GetByteData", {Integer, Integer}, {_Integer, 1, "Automatic"}];

(* Create wrapper function for library function *)
RaspiCam[{ihandle_,dhandle_},args___]:=(Module[{argsList,mult,size,scaledSize},

	argsList = Flatten[{args}];
	If[Length[argsList]==2, (* IF argument list is correct length... *)
		(* THEN *)
		size=argsList,
		(* ELSE *)
		size={2592,1944}
	];
	scaledSize=size;

	(* Deal with too-small images bug by scaling up, taking the image, then scaling back down *)
	If[ size[[1]]<300 || size[[2]]<300, 
		(*THEN*) 
		mult = 300 / Min[ size[[1]], size[[2]] ];
		scaledSize = { mult*size[[1]], mult*size[[2]] }
	];
	

	Return[ImageResize[ ImportString[FromCharacterCode[GetImageBytes[ scaledSize[[1]], scaledSize[[2]] ]]], size[[1]] ]];
]);

(* Ensure the MMAL libraries are present on the Pi.  This function runs when DeviceOpen is called. *)
CheckMMAL[ihandle_]:=Module[{lib},
	lib=FileNameJoin[{"","opt","vc","lib","libmmal.so"}];
	If[!FileExistsQ[lib],                                   (* If the library does not exist... *)
		Message[DeviceOpen::MissingMMAL]; Return[$Failed],  (* ...Notify the user and return Failed... *)
		Return[1];                                          (* ...Else, return the device handle. *)
	];
	
];


(*============================================================================*)
(* SenseHAT Code *)
(*============================================================================*)

(* Helper function to get pixel map to display a character that can be Joined with other chars*)
GetCharData[char_String]:=(Module[{img,data,charList,pos},

	(* Get list of characters in the image *)
	charList=" +-*/!\"#$><0123456789.=)(ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz?,;:|@%[&_']\\~^";

	(* Search for desired character's position in list.  If it isn't in the list, return Failed.  *)
	If[Length[StringPosition[charList,char]]==0,Return[$Failed]];
	pos=StringPosition[charList,char][[1]][[1]];

	(* Get character images from Resources folder *)
	img=Binarize[Import[PacletResource["RaspberryPiTools","TextImage"],"PNG"]];
	data=ImageData[img,"Binary"];

	(* Isolate that character from character images *)
	data=Take[data,{(pos-1)*5+1,pos*5}];

	Return[data];
]);

(* Helper function to turn 3 color values from 0-255 into 2 bytes that will be written to the output stream *)
EncodePixel[color_List]:=(Module[{r,g,b,bits},
	(* Ensure 3 values are given for R,G,B *)
	If[Length[color]!=3,Return[$Failed]];

	(* Condense the 3 values to a single 16 bit integer *)
	r=BitAnd[BitShiftRight[color[[1]],3],FromDigits["1F",16]];
	g=BitAnd[BitShiftRight[color[[2]],2],FromDigits["3F",16]];
	b=BitAnd[BitShiftRight[color[[3]],3],FromDigits["1F",16]];
	bits=BitShiftLeft[r,11]+BitShiftLeft[g,5]+b;

	(* Convert integer into 2 byte values *)
	bits=Reverse[IntegerDigits[bits,256,2]];
	Return[bits];
]);

(* Helper function to turn off all LEDs *)
ClearLEDArray[]:=(Module[{stream},
	stream=OpenWrite["/dev/fb1","BinaryFormat"->True];
	BinaryWrite[stream,Table[EncodePixel[{0,0,0}],{64}]];
	Close[stream];
]);

(* Display an image with 64 pixels; used in DisplayMessage *)
DisplayImage[img_Image,color_List]:=(Module[{data,display,stream},
	(* Error checking *)
	data=Flatten[ImageData[img]];
	If[Length[data]!=64,Return[$Failed]];
	If[Length[color]!=3,Return[$Failed]];

	(* Color 1's, don't color 0's, then encode colored pixels *)
	display=If[#==1,color,{0,0,0}]&/@data;
	display=Flatten[EncodePixel/@display];

	(* Open stream to LED array *)
	stream=OpenWrite["/dev/fb1","BinaryFormat"->True];

	(* Display pixels *)
	BinaryWrite[stream,display];

	(* Close the stream *)
	Close[stream];
]);

(* Display an image with 192 pixels; used in DisplayMessage *)
DisplayImage[img_List]:=(Module[{display,stream},
	(* Convert each  *)
	display=Flatten[EncodePixel/@img];

	(* Open stream to LED array *)
	stream=OpenWrite["/dev/fb1","BinaryFormat"->True];

	(* Display pixels *)
	BinaryWrite[stream,display];

	(* Close the stream *)
	Close[stream];
]);

(* Display a given pattern on the LED array *)
Options[DisplayMessage]={"Color"->{255,255,255}, "ScrollSpeed"->0.05};

DisplayMessage[{ihandle_,dhandle_},args___,opts:OptionsPattern[]]:=(Module[{argsList,color,chars,fullimg,frames,data,i,scroll},
	argsList={Flatten[{args}]};	

	(* Error Checking *)
	If[Length[OptionValue["Color"]]!=3, color={255,255,255}, color=OptionValue["Color"]];
	If[Head[OptionValue["ScrollSpeed"]]=!=Real, scroll=0.05, scroll=OptionValue["ScrollSpeed"] ];

	(* If input is a list of length 64, display the image.  Else, isolate the message string. *)
	If[ Head[argsList]===List, 
		(* If the list has 64 elements (one for each light)... *)
		If[Length[Flatten[argsList]]==64, 
			(* Binarize and display the list, using color to light 1s and not lighting 0s  *)
			data=Partition[argsList[[1]], 8];
			DisplayImage[Binarize[Image[data]], color];
		];
		(* Else, if the list has 192 elements (3 for each light)... *)
		If[Length[Flatten[argsList]]==192, 
			(* Display the list, lighting each light using the color defined in the corresponding list position  *)
			DisplayImage[Partition[ argsList[[1]], 3]];
		];
	];
	(* Finally, the only other option is a string. *)
	Quiet[If[ Head[args]=!=String, Return[$Failed] ]];

	(* If string is empty or all whitespace, just clear the LED array *)
	If[StringTrim[args]=="", ClearLEDArray[]; Return[]; ];

	(* Else, display the message... *)
	(* Get pixel arrays for each character in message and combine them *)
	chars=GetCharData/@Characters[args];
	data=Partition[Flatten[Join[chars]],8];

	(* Add buffer space to left/right of message *)
	data=ArrayPad[data, {{8,8}}, {{0,0,0,0,0,0,0,0}} ];

	(* Determine height of array for use in loop *)
	frames=Dimensions[data][[1]]-8;

	(* Display top 8 rows, rotated to correct orientation, then delete first row to simulate scrolling motion *)
	For[i=0,i<=frames,i++,
		DisplayImage[ImageRotate[Image[Take[data,8]]], color];
		Pause[scroll];
		data=Drop[data,1];
	];

	(* Clear out the array *)
	ClearLEDArray[];
]);


(* SENSOR FUNCTIONS AND HELPERS *)

(* Helper function to convert a binary string to a number using 2's complement *)
TwosComplement[digits_List]:=(Module[{len,sum,i},
	len=Length[digits];
	sum=0;
	For[i=1,i<=len,i++,
		sum=sum+(Times[digits[[i]], 2^(len-i)]);
	];
	If[digits[[1]]==1, sum=sum-(2^len)];
	Return[sum];
]);

(* Helper function to cleanly read from a specific register *)
I2CRegisterRead[sensor_DeviceObject, register_String]:=(
	DeviceWrite[sensor,FromDigits[register,16]];
	Return[DeviceRead[sensor]];
);

(* Helper function to project a 3D vector to a 2D space *)
AccelToEuler[vector_List] := Module[{x,y,z},
	Normalize[vector];
	x=vector[[1]]; y=vector[[2]]; z=vector[[3]];
	Return[{ ArcTan[y,z], ArcTan[x,Sqrt[y^2 + z^2]], 0 }];
];

SenseHatReadTemperature[]:=Module[{binaryDigits,pressureSensor,temp,tempOutHigh,tempOutLow},
	pressureSensor=DeviceOpen["I2C",92];
	
	(* Initialize the sensor *)
	DeviceWrite[pressureSensor,{FromDigits["20",16],FromDigits["C4",16]}];
	DeviceWrite[pressureSensor,{FromDigits["10",16],FromDigits["05",16]}];
	DeviceWrite[pressureSensor,{FromDigits["2E",16],FromDigits["C0",16]}];
	DeviceWrite[pressureSensor,{FromDigits["21",16],FromDigits["40",16]}];

	(* Read data from the temperature registers *)
	tempOutLow=I2CRegisterRead[pressureSensor,"2B"];
	tempOutHigh=I2CRegisterRead[pressureSensor,"2C"];

	(* Convert this binary string to degrees Celsius, formula taken form sensor manual, page 34 *)
	binaryDigits=Join[IntegerDigits[tempOutHigh,2,8],IntegerDigits[tempOutLow,2,8]];
	temp=(TwosComplement[binaryDigits]/480)+42.5;

	(* Close the device to prevent memory leaks *)
	DeviceClose[pressureSensor];

	(* Return the temperature *)
	Return[temp];
];

SenseHatReadPressure[]:=Module[{binaryDigits,pressureSensor,pres,presOutHigh,presOutLow,presOutXLow},
	pressureSensor=DeviceOpen["I2C",92];
	
	(* Initialize the sensor *)
	DeviceWrite[pressureSensor,{FromDigits["20",16],FromDigits["C4",16]}];
	DeviceWrite[pressureSensor,{FromDigits["10",16],FromDigits["05",16]}];
	DeviceWrite[pressureSensor,{FromDigits["2E",16],FromDigits["C0",16]}];
	DeviceWrite[pressureSensor,{FromDigits["21",16],FromDigits["40",16]}];

	(* Read data from the pressure registers *)
	presOutXLow=I2CRegisterRead[pressureSensor,"28"];
	presOutLow=I2CRegisterRead[pressureSensor,"29"];
	presOutHigh=I2CRegisterRead[pressureSensor,"2A"];

	(* Convert this binary string to millibars, formula taken form sensor manual, page 33 *)
	binaryDigits=Join[IntegerDigits[presOutHigh,2,8],IntegerDigits[presOutLow,2,8],IntegerDigits[presOutXLow,2,8]];
	pres=TwosComplement[binaryDigits]/4096;

	(* Close the device to prevent memory leaks *)
	DeviceClose[pressureSensor];

	(* Return the pressure *)
	Return[N[pres]];
];

SenseHatReadHumidity[]:=Module[{binaryDigits,humiditySensor,hum,humOutHigh,humOutLow,modifiers},
	humiditySensor=DeviceOpen["I2C",95];
	
	(* Initialize the sensor *)
	DeviceWrite[humiditySensor,{FromDigits["20",16],FromDigits["87",16]}];
	DeviceWrite[humiditySensor,{FromDigits["10",16],FromDigits["1B",16]}];

	(* Calibrate humidity sensor *)
	modifiers=CalibrateHumidity[humiditySensor];

	(* Read data from the humidity registers *)
	humOutLow=I2CRegisterRead[humiditySensor,"28"];
	humOutHigh=I2CRegisterRead[humiditySensor,"29"];

	(* Convert this binary string to pressure using two's complement *)
	binaryDigits=Join[IntegerDigits[humOutHigh,2,8],IntegerDigits[humOutLow,2,8]];
	hum=TwosComplement[binaryDigits];

	(* Use calibration modifiers to correct humidity *)
	hum=hum*modifiers[[1]]+modifiers[[2]];

	(* Close the device to prevent memory leaks *)
	DeviceClose[humiditySensor];

	(* Return the humidity *)
	Return[N[hum]];
];

(* Get calibration data for the humidity sensor, modeled after RTHumidityHTS221.cpp *)
CalibrateHumidity[humiditySensor_DeviceObject]:=Module[{h0,h1,h0t0out,h1t0out,humm,humc},
	h0=I2CRegisterRead[humiditySensor,"30"]/2;
	h1=I2CRegisterRead[humiditySensor,"31"]/2;
	h0t0out=TwosComplement[Join[IntegerDigits[I2CRegisterRead[humiditySensor,"36"],2,8],IntegerDigits[I2CRegisterRead[humiditySensor,"37"],2,8]]];
	h1t0out=TwosComplement[Join[IntegerDigits[I2CRegisterRead[humiditySensor,"3A"],2,8],IntegerDigits[I2CRegisterRead[humiditySensor,"3B"],2,8]]];
	humm=(h1-h0)/(h1t0out-h0t0out);
	humc=h0-(humm*h0t0out);
	Return[{humm,humc}];
];

SenseHatReadGyroscope[]:=Module[{conv,ndofSensor,pitch,roll,yaw,xHigh,xLow,yHigh,yLow,zHigh,zLow},
	ndofSensor=DeviceOpen["I2C",106];

	(* Initialize the sensor *)
	DeviceWrite[ndofSensor,{FromDigits["22",16],FromDigits["80",16]}];

	(* Set up control registers *)
	DeviceWrite[ndofSensor,{FromDigits["10",16],FromDigits["89",16]}];
	DeviceWrite[ndofSensor,{FromDigits["12",16],FromDigits["44",16]}];
	DeviceWrite[ndofSensor,{FromDigits["20",16],FromDigits["7B",16]}];
	DeviceWrite[ndofSensor,{FromDigits["21",16],FromDigits["00",16]}];

	(* Read data from the x,y,z angle registers *)
	xLow=I2CRegisterRead[ndofSensor,"18"];
	xHigh=I2CRegisterRead[ndofSensor,"19"];
	yLow=I2CRegisterRead[ndofSensor,"1A"];
	yHigh=I2CRegisterRead[ndofSensor,"1B"];
	zLow=I2CRegisterRead[ndofSensor,"1C"];
	zHigh=I2CRegisterRead[ndofSensor,"1D"];

	(* Convert the raw data to pitch, roll, and yaw in revolutions per second *)
	roll=TwosComplement[Join[IntegerDigits[xHigh,2,8],IntegerDigits[xLow,2,8]]];
	pitch=TwosComplement[Join[IntegerDigits[yHigh,2,8],IntegerDigits[yLow,2,8]]];
	yaw=TwosComplement[Join[IntegerDigits[zHigh,2,8],IntegerDigits[zLow,2,8]]];	

	(* Conversions taken from RTIMULSM9DS1.cpp and RTIMULib.ini *)
	conv=N[0.00875/360]; (* Divided by 360 to convert degrees to revolutions *)
	pitch=pitch*conv; roll=roll*conv*-1; yaw=yaw*conv;

	(* Correct for noise when not moving *)
	If[Abs[pitch]<0.01,pitch=0]; If[Abs[roll]<0.01,roll=0]; If[Abs[yaw]<0.01,yaw=0];

	(* Close the device to prevent memory leaks *)
	DeviceClose[ndofSensor];

	(* Return the gyroscope data *)
	Return[{roll,pitch,yaw}];
];

SenseHatReadAccelerometer[]:=Module[{conv,ndofSensor,pitch,roll,yaw,xHigh,xLow,yHigh,yLow,zHigh,zLow},
	ndofSensor=DeviceOpen["I2C",106];
	
	(* Initialize the sensor *)
	DeviceWrite[ndofSensor,{FromDigits["22",16],FromDigits["80",16]}];

	(* Set up control registers *)
	DeviceWrite[ndofSensor,{FromDigits["10",16],FromDigits["89",16]}];
	DeviceWrite[ndofSensor,{FromDigits["12",16],FromDigits["44",16]}];
	DeviceWrite[ndofSensor,{FromDigits["20",16],FromDigits["7B",16]}];
	DeviceWrite[ndofSensor,{FromDigits["21",16],FromDigits["00",16]}];

	(* Read data from the x,y,z angle registers *)
	xLow=I2CRegisterRead[ndofSensor,"28"];
	xHigh=I2CRegisterRead[ndofSensor,"29"];
	yLow=I2CRegisterRead[ndofSensor,"2A"];
	yHigh=I2CRegisterRead[ndofSensor,"2B"];
	zLow=I2CRegisterRead[ndofSensor,"2C"];
	zHigh=I2CRegisterRead[ndofSensor,"2D"];

	(* Convert the raw data to pitch, roll, and yaw in G's (Earth's gravity) *)
	roll=TwosComplement[Join[IntegerDigits[xHigh,2,8],IntegerDigits[xLow,2,8]]];
	pitch=TwosComplement[Join[IntegerDigits[yHigh,2,8],IntegerDigits[yLow,2,8]]];
	yaw=TwosComplement[Join[IntegerDigits[zHigh,2,8],IntegerDigits[zLow,2,8]]];	

	conv=0.000244;  (* Conversion taken from RTIMULSM9DS1.cpp *)
	pitch=pitch*conv*-1; roll=roll*conv*-1; yaw=yaw*conv;

	(* Close the device to prevent memory leaks *)
	DeviceClose[ndofSensor];

	(* Return the accelerometer data *)
	Return[{roll,pitch,yaw}];
];

SenseHatReadMagnetometer[]:=Module[{conv,magSensor,roll,pitch,yaw,xHigh,xLow,yHigh,yLow,zHigh,zLow},
	magSensor=DeviceOpen["I2C",28];

	(* Set up control registers *)
	DeviceWrite[magSensor,{FromDigits["20",16],FromDigits["14",16]}];
	DeviceWrite[magSensor,{FromDigits["21",16],FromDigits["00",16]}];
	DeviceWrite[magSensor,{FromDigits["22",16],FromDigits["00",16]}];	

	(* Read data from the x,y,z angle registers *)
	xLow=I2CRegisterRead[magSensor,"28"];
	xHigh=I2CRegisterRead[magSensor,"29"];
	yLow=I2CRegisterRead[magSensor,"2A"];
	yHigh=I2CRegisterRead[magSensor,"2B"];
	zLow=I2CRegisterRead[magSensor,"2C"];
	zHigh=I2CRegisterRead[magSensor,"2D"];

	(* Convert the raw data to roll,pitch,yaw in degrees *)
	roll=TwosComplement[Join[IntegerDigits[xHigh,2,8],IntegerDigits[xLow,2,8]]];
	pitch=TwosComplement[Join[IntegerDigits[yHigh,2,8],IntegerDigits[yLow,2,8]]];
	yaw=TwosComplement[Join[IntegerDigits[zHigh,2,8],IntegerDigits[zLow,2,8]]];	

	conv=0.014;  (* Conversion taken from RTIMULSM9DS1.cpp *)
	pitch=pitch*conv; roll=roll*conv*-1; yaw=yaw*conv*-1;

	(* Close the device to prevent memory leaks *)
	DeviceClose[magSensor];

	(* Return the compass data *)
	Return[{roll,pitch,yaw}];
];

SenseHatCalculateOrientation[]:=Module[{vector,mvec,x,y,z,m,q},
	
	(* Turn accelerometer data into a normalized 2D vector (z=0) *)
	vector=AccelToEuler[SenseHatReadAccelerometer[]];
	x=vector[[1]]; y=vector[[2]]; z=vector[[3]];
	
	(* Check for accelerometer reading all 0s - this will cause an error later *)
	If[(x==y==z==0),Return[$Failed]];

	(* Create the quaternion q to rotate m *)
	q=Quaternion[ Cos[x/2]*Cos[y/2], Sin[x/2]*Cos[y/2], Cos[x/2]*Sin[y/2], -Sin[x/2]*Sin[y/2] ];
	
	(* Turn magnetometer readings into a quaternion *)
	mvec=SenseHatReadMagnetometer[];
	m=Quaternion[ 0, mvec[[1]], mvec[[2]], mvec[[3]] ];

	(* Rotation of m to correct for accelerations *)
	m=q**m**Conjugate[q];

	(* Use m to give vector a Z component *)
	vector[[3]]=ArcTan[ m[[3]],m[[2]] ];

	(* Convert radians to degrees and correct offsets *)
	vector[[1]]=Mod[vector[[1]]*(360/(2*Pi))-90,360];
	vector[[2]]=Mod[vector[[2]]*(360/(2*Pi))-90,360];
	vector[[3]]=Mod[vector[[3]]*(360/(2*Pi))-180,360];

	Return[vector];
];

(* Device Read function *)
RaspiSenseHat[{ihandle_,dhandle_},args___]:=( Module[ {datatype},

	(* Call internal functions *)
	datatype=ToLowerCase[args];
	If[StringMatchQ[datatype,"temperature"], Return[ Quantity[SenseHatReadTemperature[],"Celsius"] ] ];
	If[StringMatchQ[datatype,"humidity"], Return[ Quantity[SenseHatReadHumidity[],"%"] ] ];
	If[StringMatchQ[datatype,"pressure"], Return[ Quantity[SenseHatReadPressure[],"mbar"] ] ];
	If[StringMatchQ[datatype,"rotation"], Return[ Map[Quantity[#,"revolution per second"]&, SenseHatReadGyroscope[]] ] ];
	If[StringMatchQ[datatype,"magnetic field"], Return[ Map[Quantity[#,"microtesla"]&, SenseHatReadMagnetometer[]] ] ];
	If[StringMatchQ[datatype,"orientation"], Return[ Map[Quantity[#,"degrees"]&, SenseHatCalculateOrientation[]] ] ];
	If[StringMatchQ[datatype,"acceleration"], Return[ Map[Quantity[#,"standard accelerations due to gravity on the surface of the earth"]&, SenseHatReadAccelerometer[]] ] ];
	
	(* If the arg doesn't match anything else, return an error message *)
	Message[DeviceRead::unknownFunction];
	Return[$Failed];
]);

(* DRIVER FUNCTION *)

deviceOpenDriver[ihandle_]:=Module[{osfile,pres,hmdy,gyro,magn},

	(* Open connections to the 4 I2C addresses used by the SenseHAT *)
	pres=DeviceOpen["I2C",92];
	hmdy=DeviceOpen["I2C",95];
	gyro=DeviceOpen["I2C",106];
	magn=DeviceOpen["I2C",28];

	(* If any failed... *)
	If[AnyTrue[{pres,hmdy,gyro,magn}, #==$Failed &], 
		Message[DeviceOpen::noDevice];Return[$Failed],        (* ...return $Failed *)
		Return[ihandle]                                       (* ...else, return the device handle *)
	];
];


End[]


EndPackage[]
