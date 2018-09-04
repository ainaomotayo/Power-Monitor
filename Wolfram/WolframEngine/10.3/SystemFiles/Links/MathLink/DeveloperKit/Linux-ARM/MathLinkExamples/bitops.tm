
int bitand P(( int, int));

:Begin:
:Function: bitand
:Pattern: bitAnd[x_Integer, y_Integer]
:Arguments: {x, y}
:ArgumentTypes: {Integer, Integer}
:ReturnType: Integer
:End:

:Evaluate: bitAnd::usage = "bitAnd[x, y] gives the bitwise conjunction
	of two integers x and y."


#if MLINTERFACE >= 3
void complements P(( int *, long));
#else
void complements P(( intp_nt, long));
#endif /* MLINTERFACE >= 3 */

:Begin:
:Function: complements
:Pattern: bitComplements[x_List]
:Arguments: {x}
:ArgumentTypes: {IntegerList}
:ReturnType: Manual
:End:

:Evaluate: bitComplements::usage = "bitComplements[{x1,x2,...}] generates
	a list of the bitwise complements of the integers xi."
