#!/bin/bash

#
#  Build the json-fortran library and unit tests.
#
#  Requires:
#    FoBiS.py : https://github.com/szaghi/FoBiS      [version 1.2.5 or later required]
#    RoboDoc  : http://rfsber.home.xs4all.nl/Robo/   [version 4.99.38 is the one tested]
#
#  Jacob Williams : 12/27/2014
#

# Set to 1 to use ifort, otherwise use gfortran
set -e
use_ifort=0

PROJECTNAME='jsonfortran'       # project name for robodoc (example: jsonfortran_2.0.0)
DOCDIR='./documentation/'       # build directory for documentation
SRCDIR='./src/'                 # library source directory
TESTDIR='./src/tests/'          # unit test source directory
BINDIR='./bin/'                 # build directory for unit tests
LIBDIR='./lib/'                 # build directory for library
MODCODE='json_module.f90'       # json module file name
LIBOUT='libjsonfortran.a'       # name of json library

if [ $use_ifort -eq 1 ]
then
	# Intel compiler

	FCOMPILER='Intel'
	# The following warning might be triggered by ifort unless explicitly silenced:
	# warning #7601: F2008 standard does not allow an internal procedure to be an actual argument procedure name. (R1214.4).
	# In the context of F2008 this is an erroneous warning.
	# See https://prd1idz.cps.intel.com/en-us/forums/topic/486629
	FCOMPILERFLAGS='-c -O2 -warn -stand f08 -diag-disable 7601 -traceback'
	#FCOMPILERFLAGS='-c -O2 -warn -traceback -stand f08 -assume protect_parens -assume buffered_io -check all'

else
	# GFortran (must be >= 4.9)

	FCOMPILER='gnu'
	FCOMPILERFLAGS='-c -O2 -fbacktrace -Wall -Wextra -Wno-maybe-uninitialized -pedantic -std=f2008'
	if [[ $CODE_COVERAGE == [yY]* ]]; then # Add coverage info with gcov
	    echo "Compiling with gcov code coverage instrumentation."
	    COVERAGE="-coverage"
	fi
	#FCOMPILERFLAGS='-c -O2 -fbacktrace -fall-intrinsics -Wall -Wextra -Wno-maybe-uninitialized -pedantic -std=f2008'

fi

#build the stand-alone library:
echo ""
echo "${LFLAGS}"
echo "Building library..."
FoBiS.py build -compiler ${FCOMPILER} -cflags "${FCOMPILERFLAGS}" ${COVERAGE} -dbld ${LIBDIR} -s ${SRCDIR} -dmod ./ -dobj ./ -t ${MODCODE} -o ${LIBOUT} -mklib static -colors

#build the unit tests (uses the above library):
if [[ $JF_SKIP_TESTS != [yY]* ]]; then
    echo ""
    echo "Building unit tests..."
    for TEST in "${TESTDIR%/}"/jf_test_*.f90; do
	THIS_TEST=${TEST##*/}
	echo "Build ${THIS_TEST%.f90}"
	FoBiS.py build -compiler ${FCOMPILER} -cflags "${FCOMPILERFLAGS}" ${COVERAGE} -dbld ${BINDIR} -s ${TESTDIR} -i ${LIBDIR} -libs ${LIBDIR}/${LIBOUT} -dmod ./ -dobj ./ -t ${THIS_TEST} -o ${THIS_TEST%.f90} -colors
    done
else
    echo "Skip building the unit tests since \$JF_SKIP_TESTS has been set to 'true'."
fi

#build the documentation with RoboDoc:
echo ""
echo "Building documentation..."
robodoc --rc ./robodoc.rc --src ${SRCDIR} --doc ${DOCDIR} --multidoc --html --ignore_case_when_linking --syntaxcolors --source_line_numbers --index --tabsize 4 --documenttitle ${PROJECTNAME} --sections
echo ""

# Run all the tests unless $JF_SKIP_TESTS
if [[ $JF_SKIP_TESTS != [yY]* ]]; then
    cd "$BINDIR"
    rm jf_test*.o jf_test*.mod || true
    OLD_IGNORES="$GLOBIGNORE"
    GLOBIGNORE='*.*'
    #
    for TEST in jf_test_*; do
	# It would be nice to run json output printed to stdout through jsonlint, however,
	# some tests output more than one json structure and these need to be split
	./${TEST}
    done
    GLOBIGNORE="$OLD_IGNORES"
else
    echo "Skip running the unit tests since \$JF_SKIP_TESTS has been set to 'true'."
fi
