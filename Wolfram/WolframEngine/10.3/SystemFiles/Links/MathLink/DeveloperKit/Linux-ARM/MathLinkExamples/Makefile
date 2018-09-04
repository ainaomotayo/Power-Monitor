# This makefile can be used to build all or some of the sample
# programs.  To build all of them, use the command
# 'make all'.  To build one, say addtwo, use the command
# 'make addtwo'.

# Portions of this makefile require the use of GNU make.
# see http://www.gnu.org/software/make for more information.

VERSION=10.0
MLINKDIR = $(shell pwd)/..
SYS = Linux-ARM
CADDSDIR = ${MLINKDIR}/CompilerAdditions
EXTRA_CFLAGS=

INCDIR = ${CADDSDIR}
LIBDIR = ${CADDSDIR}

MPREP = ${CADDSDIR}/mprep
RM = rm

CC = /usr/bin/gcc-4.9
CXX = /usr/bin/g++-4.9

BINARIES = addtwo bitops counter factor factor2 factor3 quotient reverse sumalist

all : $(BINARIES)

addtwo : addtwotm.o addtwo.o
	${CXX} ${EXTRA_CFLAGS} -I${INCDIR} addtwotm.o addtwo.o -L${LIBDIR} -lML32i4 -lm -lpthread -lrt -lstdc++ -ldl -luuid -o $@

bitops : bitopstm.o bitops.o
	${CXX} ${EXTRA_CFLAGS} -I${INCDIR} bitopstm.o bitops.o -L${LIBDIR} -lML32i4 -lm -lpthread -lrt -lstdc++ -ldl -luuid -o $@

counter : countertm.o
	${CXX} ${EXTRA_CFLAGS} -I${INCDIR} countertm.o -L${LIBDIR} -lML32i4 -lm -lpthread -lrt -lstdc++ -ldl -luuid -o $@

factor : factor.o
	${CXX} ${EXTRA_CFLAGS} -I${INCDIR} factor.o -L${LIBDIR} -lML32i4 -lm -lpthread -lrt -lstdc++ -ldl -luuid -o $@

factor2 : factor2.o
	${CXX} ${EXTRA_CFLAGS} -I${INCDIR} factor2.o -L${LIBDIR} -lML32i4 -lm -lpthread -lrt -lstdc++ -ldl -luuid -o $@

factor3 : factor3.o
	${CXX} ${EXTRA_CFLAGS} -I${INCDIR} factor3.o -L${LIBDIR} -lML32i4 -lm -lpthread -lrt -lstdc++ -ldl -luuid -o $@

quotient : quotient.o
	${CXX} ${EXTRA_CFLAGS} -I${INCDIR} quotient.o -L${LIBDIR} -lML32i4 -lm -lpthread -lrt -lstdc++ -ldl -luuid -o $@

reverse : reversetm.o
	${CXX} ${EXTRA_CFLAGS} -I${INCDIR} reversetm.o -L${LIBDIR} -lML32i4 -lm -lpthread -lrt -lstdc++ -ldl -luuid -o $@

sumalist : sumalisttm.o
	${CXX} ${EXTRA_CFLAGS} -I${INCDIR} sumalisttm.o -L${LIBDIR} -lML32i4 -lm -lpthread -lrt -lstdc++ -ldl -luuid -o $@

.c.o :
	${CC} -c ${EXTRA_CFLAGS} -I${INCDIR} $<

addtwotm.c : addtwo.tm
	${MPREP} $? -o $@

bitopstm.c : bitops.tm
	${MPREP} $? -o $@

countertm.c : counter.tm
	${MPREP} $? -o $@

reversetm.c : reverse.tm
	${MPREP} $? -o $@

sumalisttm.c : sumalist.tm
	${MPREP} $? -o $@

clean :
	@ ${RM} -rf *.o *tm.c $(BINARIES)
