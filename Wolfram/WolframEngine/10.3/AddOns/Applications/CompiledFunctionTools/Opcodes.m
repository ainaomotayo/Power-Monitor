(* Mathematica Package *)

BeginPackage["CompiledFunctionTools`Opcodes`"]


(*
 List all opcodes here
*)

{CCNOP, CCRET, CCBRANCH, CCJUMP, CCLOOPINCR, 
		    CCCOPYBOOL, CCCOPYINT, CCCOPYREAL, CCCOPYCX, CCCVTBOOLTOINT, 
		    CCCVTINTTOREAL, CCCVTREALTOCX, CCPLUSINT, CCPLUSREAL, CCPLUSCX, 
		    CCTIMESINT, CCTIMESREAL, CCTIMESCX, CCMINUSINT, CCMINUSREAL, 
		    CCMINUSCX, CCRE, CCIM, 
		    CCEQBOOL, CCEQINT, CCLTINT, CCLEINT, 
		    CCCOMPAREREAL, CCCOMPARECOMPLEX, 
		    CCAND, CCOR, CCXOR, CCNOT, CCLENGTH, 
		    CCCREATETENSORFROMLIST, 
		    CCCREATETENSORFORTABLE, CCINSERTTOTABLE, CCGETELEMENT, 
		    CCPART, CCSETPART, CCMATH1ARG, 
		    CCMATH2ARG, CCFUNCTIONCALL, CCCOMPILEDFUNCTION, 
		    CCPLUSTENSOR, CCTIMESTENSOR, 
		    CCEVALARG, CCEVALREG, CCEVALDYNAMIC, 
		    CCERR}

OpcodeInitialize::usage = "Initialize[] initializes the system."

Arg1Name::usage = "Arg1Name  "

Arg2Name::usage = "Arg2Name  "

ArgNName::usage = "ArgNName  "

CompareName::usage = "CompareName"

CompareFunctionQ::usage= "CompareFunctionQ"

FunctionCallName::usage = "FunctionCallName"

ArithmeticFlags::usage = "ArithmeticFlags";
UnderflowFlag::usage = "Check machine underflow flag";
OverflowFlag::usage = "Check machine overflow flag";
IntegerOverflowFlag::usage = "Check machine integer overflow flag";

ListableFlag::usage = "ListableFlag"
    
TrySymbolValuesFlag::usage = "TrySymbolValuesFlag"

CleanRegistersFlag::usage = "ClearRegistersFlag"

Begin["`Private`"] (* Begin Private Context *) 

InitializeOpcodes[8 | 10, v_ /; 8. <= v < 20.] :=
	InitializeOpcodes[10, 10.]
	
InitializeOpcodes[10, 10.] :=
	Module[ {setArg2Name},
		{CCNOP, CCRET, CCBRANCH, CCJUMP, CCLOOPINCR, 
		    CCCOPYBOOL, CCCOPYINT, CCCOPYREAL, CCCOPYCX, CCCVTBOOLTOINT, 
		    CCCVTINTTOREAL, CCCVTREALTOCX, CCPLUSINT, CCPLUSREAL, CCPLUSCX, 
		    CCTIMESINT, CCTIMESREAL, CCTIMESCX, CCMINUSINT, CCMINUSREAL, 
		    CCMINUSCX, CCRE, CCIM, 
		    CCEQBOOL, CCEQINT, CCLTINT, CCLEINT, 
		    CCCOMPAREREAL, CCCOMPARECOMPLEX, 
		    CCAND, CCOR, CCXOR, CCNOT, CCLENGTH, 
		    CCCREATETENSORFROMLIST, 
		    CCCREATETENSORFORTABLE, CCINSERTTOTABLE, CCGETELEMENT, 
		    CCPART, CCSETPART, CCMATH1ARG, 
		    CCMATH2ARG, CCFUNCTIONCALL, CCCOMPILEDFUNCTION, 
		    CCPLUSTENSOR, CCTIMESTENSOR, 
		    CCEVALARG, CCEVALREG, CCEVALDYNAMIC, 
		    CCERR} = Range[0, 49];
		    
		Arg1Name[1] = Sin;
		Arg1Name[2] = Cos;
		Arg1Name[3] = Tan;
		Arg1Name[4] = "Error:Not Used";
		Arg1Name[5] = Csc;
		Arg1Name[6] = Sec;
		Arg1Name[7] = Cot;
		Arg1Name[8] = Arg1Name[4];
		
		Arg1Name[9] = Sinh;
		Arg1Name[10] = Cosh;
		Arg1Name[11] = Tanh;
		Arg1Name[12] = Arg1Name[4];
		Arg1Name[13] = Csch;
		Arg1Name[14] = Sech;
		Arg1Name[15] = Coth;
		Arg1Name[16] = Arg1Name[4];

		Arg1Name[17] = ArcSin;
		Arg1Name[18] = ArcCos;
		Arg1Name[19] = ArcTan;
		Arg1Name[20] = Arg1Name[4];
		Arg1Name[21] = ArcCsc;
		Arg1Name[22] = ArcSec;
		Arg1Name[23] = ArcCot;
		Arg1Name[24] = Arg1Name[4];
		
		Arg1Name[25] = ArcSinh;
		Arg1Name[26] = ArcCosh;
		Arg1Name[27] = ArcTanh;
		Arg1Name[28] = Arg1Name[4];
		Arg1Name[29] = ArcCsch;
		Arg1Name[30] = ArcSech;
		Arg1Name[31] = ArcCoth;
		Arg1Name[32] = Arg1Name[4];
		
		Arg1Name[33] = "AbsSquare";
		Arg1Name[34] = Exp;
		Arg1Name[35] = Log;
		Arg1Name[36] = Log2;
		Arg1Name[37] = Log10;
		Arg1Name[38] = Abs;
		Arg1Name[39] = Arg;
		Arg1Name[40] = Conjugate;
		Arg1Name[41] = Im;
		Arg1Name[42] = Re;
		Arg1Name[43] = Minus;
		Arg1Name[44] = Sign;
		Arg1Name[49] = Round;
		Arg1Name[50] = Floor;
		Arg1Name[51] = Ceiling;
		Arg1Name[52] = FractionalPart;
		Arg1Name[53] = IntegerPart;
		Arg1Name[54] = EvenQ;
		Arg1Name[55] = OddQ;
		Arg1Name[56] = Square;
		Arg1Name[57] = Sqrt;
		Arg1Name[58] = CubeRoot;
		Arg1Name[59] = Internal`ReciprocalSqrt;
		Arg1Name[60] = "Reciprocal";
		Arg1Name[61] = "BitOp1Arg";
		Arg1Name[62] = BitNot;
		Arg1Name[63] = BitLength;
		Arg1Name[64] = "IntExp1";
		Arg1Name[65] = "IntExp2";
		Arg1Name[66] = UnitStep;
		Arg1Name[67] = Sinc;
		Arg1Name[68] = Fibonacci;
		Arg1Name[69] = LucasL;
		Arg1Name[70] = Gudermannian;
		Arg1Name[71] = InverseGudermannian;
		Arg1Name[72] = Haversine;
		Arg1Name[73] = InverseHaversine;
		Arg1Name[74] = Erfc;
		Arg1Name[75] = Erf;
		Arg1Name[76] = Gamma;
		Arg1Name[77] = LogGamma;
		Arg1Name[78] = Unitize;
		Arg1Name[79] = "Mod1";

		BinaryOpcodeOffset = 256;

		setArg2Name[i_, name_] := (Arg2Name[i + BinaryOpcodeOffset] = name);

		setArg2Name[1, Plus]; (* M_PLUS *)
		setArg2Name[2, Subtract]; (* M_SUBTRACT *)
		setArg2Name[3, Times]; (* M_TIMES *)
		setArg2Name[4, Divide]; (* M_DIVIDE *)
		setArg2Name[5, Mod]; (* M_MOD *)
		setArg2Name[6, Quotient]; (* M_QUOTIENT *)
		setArg2Name[7, Power]; (* M_POWER *)
		setArg2Name[8, Log]; (* M_LOG2 *)
		setArg2Name[9, ArcTan]; (* M_ATAN2 *)
		setArg2Name[10, BitAnd]; (* M_BITAND *)
		setArg2Name[11, BitOr]; (* M_BITOR *)
		setArg2Name[12, BitXor]; (* M_BITXOR *)
		setArg2Name[13, Chop]; (* M_CHOP *)
		setArg2Name[14, "AbsErr"]; (* M_ABSERR *)
		setArg2Name[15, "RelErr"]; (* M_RELERR *)
		setArg2Name[16, "MaxAbs"]; (* M_MAXABS *)
		setArg2Name[17, "IntExp2"]; (* M_INTEXP2 *)
		setArg2Name[18, "IntLen2"]; (* M_INTLEN2 *)
		setArg2Name[19, "BitShiftLeft"]; (* M_BITSHIFTLEFT *)
		setArg2Name[20, "BitShiftRight"]; (* M_BITSHIFTRIGHT *)
		setArg2Name[21, "Unitize2"]; (* M_UNITIZE2 *)
				
		CompareName[ 1] = SameQ;
		CompareName[ 2] = UnsameQ;
		CompareName[ 3] = Less;
		CompareName[ 4] = LessEqual;
		CompareName[ 5] = Equal;
		CompareName[ 6] = GreaterEqual;
		CompareName[ 7] = Greater;
		CompareName[ 8] = Unequal;
		
		CompareFunctionQ[ name_] := 
				MemberQ[ {SameQ, UnsameQ, Less, LessEqual, Equal, GreaterEqual, Greater, Unequal}, name];
				
		ListableFlag = 128;
		UnderflowFlag = 1;
		OverflowFlag = 2;
		IntegerOverflowFlag = 4;
		ArithmeticFlags = BitOr[UnderflowFlag, OverflowFlag, IntegerOverflowFlag];
		TrySymbolValuesFlag = 2048;
		CleanRegistersFlag = 4096;
		
		True
	]


initialized = False;

OpcodeInitialize[cv_, mv_] := 
Module[{res = True},
	If[UnsameQ[initialized, {cv, mv}],
		res = TrueQ[InitializeOpcodes[cv, mv]];
		If[res, initialized = {cv, mv}]
	];
	res
]

End[] (* End Private Context *)

EndPackage[]