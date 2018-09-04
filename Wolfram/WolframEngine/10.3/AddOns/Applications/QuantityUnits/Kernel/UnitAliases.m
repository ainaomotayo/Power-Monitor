(*load compressed HashTable for UnitShortName parsing*)
If[ValueQ[QuantityUnits`Private`CUnitInputAliases],	
	QUnitInputAliases=Uncompress[QuantityUnits`Private`CUnitInputAliases];
	Clear[QuantityUnits`Private`CUnitInputAliases];
	Remove[QuantityUnits`Private`CUnitInputAliases];
	,
	$QUnitInputAliasesFailed=True,
	$QUnitInputAliasesFailed=True];

$PacletUnits={
 {"AminoAcids", HoldForm[IndependentUnit["amino acids"]]}, 
 {"AtomicMassUnits", HoldForm["AtomicMassUnit"]}, 
 {"BarrelsPerDay", HoldForm["BarrelsOfOil"/"Days"]}, 
 {"BasePairs", HoldForm[IndependentUnit["base pairs"]]}, 
 {"CentimetersPerHour", HoldForm["Centimeters"/"Hours"]}, 
 {"CubicMeters", HoldForm["Meters"^3]}, 
 {"CubicMetersPerKilogram", HoldForm["Meters"^3/"Kilograms"]}, 
 {"CubicMetersPerMole", HoldForm["Meters"^3/"Moles"]}, 
 {"CubicMetersPerYear", HoldForm["Meters"^3/"Years"]}, 
 {"Degrees", HoldForm["AngularDegrees"]}, 
 {"GramsPerGram", HoldForm["Grams"/"Grams"]},
 {"GramsPerMilliliter", HoldForm["Grams"/"Milliliters"]}, 
 {"GramsPerMole", HoldForm["Grams"/"Moles"]}, 
 {"JoulesPerKilogramKelvin", HoldForm["Joules"/("Kelvins"*"Kilograms")]}, 
 {"KiloelectronVolts", HoldForm["Kiloelectronvolts"]}, 
 {"KilogramsPerCubicMeter", HoldForm["Kilograms"/"Meters"^3]}, 
 {"KilogramsPerKilogram", HoldForm["Kilograms"/"Kilograms"]}, 
 {"KilojoulesPerMole", HoldForm["Kilojoules"/"Moles"]}, 
 {"Kilometer", HoldForm["Kilometers"]}, 
 {"KilometersPerHour", HoldForm["Kilometers"/"Hours"]}, 
 {"KilowattHoursPerYear", HoldForm[("Hours"*"Kilowatts")/"Years"]}, 
 {"LitersPerMole", HoldForm["Liters"/"Moles"]},
 {"LitersSquaredBarsPerMoleSquared", HoldForm[("Liters"^2*"Bars")/"Moles"^2]},
 {"MegaelectronVolts", HoldForm["Megaelectronvolts"]}, 
 {"MegaelectronVoltsPerSpeedOfLightSquared", HoldForm["Megaelectronvolts"/"SpeedOfLight"^2]}, 
 {"Meter", HoldForm["Meters"]}, 
 {"MetersPerSecond", HoldForm["Meters"/"Seconds"]}, 
 {"NewtonsPerMeter", HoldForm["Newtons"/"Meters"]}, 
 {"OhmMeters", HoldForm["Meters"*"Ohms"]}, 
 {"PascalsCubicMetersPerMole", HoldForm[("Meters"^3*"Pascals")/"Moles"]}, 
 {"PascalSeconds", HoldForm["Pascals"*"Seconds"]},  
 {"PeoplePerPerson", HoldForm["People"/"People"]}, 
 {"PeoplePerPersonPerYear", HoldForm["People"/("People"*"Years")]}, 
 {"PeoplePerYear", HoldForm["People"/"Years"]}, 
 {"PerUSDollar", HoldForm["USDollars"^(-1)]}, 
 {"PerYear", HoldForm["Years"^(-1)]}, 
 {"Quarter", HoldForm["QuarterYears"]},
 {"Quarters", HoldForm["QuarterYears"]},
 {"ReciprocalKelvins", HoldForm["Kelvins"^(-1)]}, 
 {"SiemensPerMeter", HoldForm["Siemens"/"Meters"]}, 
 {"SquareAngstroms", HoldForm["Angstroms"^2]}, 
 {"SquareFemtometers", HoldForm["Femtometers"^2]}, 
 {"SquareKilometers", HoldForm["Kilometers"^2]}, 
 {"SquareMeters", HoldForm["Meters"^2]},
 {"SquareMetersPerKilogram", HoldForm["Meters"^2/"Kilograms"]}, 
 {"SquareRootJoulesPerCubicMeter", HoldForm[Sqrt["Joules"]/"Meters"^(3/2)]}, 
 {"SquareRootMegapascals", HoldForm[Sqrt["Megapascals"]]}, 
 {"USDollarsPerYear", HoldForm["USDollars"/"Years"]}, 
 {"USDollarsPerYearPerPerson", HoldForm["USDollars"/("People"*"Years")]}, 
 {"USDollarsPerYearPerYear", HoldForm["USDollars"/"Years"^2]}, 
 {"WattsPerMeterKelvin", HoldForm["Watts"/("KelvinsDifference"*"Meters")]},
 {"GramsPerCubicCentimeter", HoldForm["Grams"/"Meters"^3]},
 {"MetersPerSecondSquared", HoldForm["Meters"/"Seconds"^2]},
 {"MagnitudesPerSquareArcSecond", HoldForm["MagnitudesPerSquareArcSec"]},
 (*singular forms*)
 {"Ampere", HoldForm["Amperes"]},
 {"Coulomb", HoldForm["Coulombs"]},
 {"Farad", HoldForm["Farads"]},
 {"Henry", HoldForm["Henries"]},
 {"Joule", HoldForm["Joules"]},
 {"Kelvin", HoldForm["Kelvins"]},
 {"Kilogram", HoldForm["Kilograms"]},
 {"Mole", HoldForm["Moles"]},
 {"Newton", HoldForm["Newtons"]},
 {"Ohm", HoldForm["Ohms"]},
 {"Pascal", HoldForm["Pascals"]},
 {"Second", HoldForm["Seconds"]},
 {"Tesla", HoldForm["Teslas"]},
 {"Volt", HoldForm["Volts"]},
 {"Watt", HoldForm["Watts"]},
 {"Weber", HoldForm["Webers"]},
 {"Radian", HoldForm["Radians"]}, 
 {"Steradian", HoldForm["Steradians"]}, 
 {"Lumen", HoldForm["Lumens"]}, 
 {"Becquerel", HoldForm["Becquerels"]}, 
 {"Gray", HoldForm["Grays"]}, 
 {"Sievert", HoldForm["Sieverts"]}, 
 {"Katal", HoldForm["Katals"]},
 {"Year", HoldForm["Years"]},
 {"Month", HoldForm["Months"]},
 {"Week", HoldForm["Weeks"]},
 {"Day", HoldForm["Days"]},
 {"Hour", HoldForm["Hours"]},
 {"Minute", HoldForm["Minutes"]}
 };

	
If[UnsameQ[$QUnitInputAliasFailed,True],
	Quiet[System`Utilities`HashTableAdd[QUnitInputAliases,Sequence@@#]&/@$PacletUnits];
	Clear[$PacletUnits];Remove[$PacletUnits];
]