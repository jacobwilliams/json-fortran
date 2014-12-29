#!/bin/bash

#
#  Build the json-fortran library and example program.
#
#  Requires: 
#    FoBiS.py : https://github.com/szaghi/FoBiS
#    RoboDoc  : http://rfsber.home.xs4all.nl/Robo/
#
#  Jacob Williams : 12/27/2014
#

# Set to 1 to use ifort, otherwise use gfortran
use_ifort=0

PROJECTNAME='jsonfortran'       # project name for robodoc (example: jsonfortran_2.0.0)
DOCDIR='./documentation/'       # build directory for documentation
SRCDIR='./src/'                 # source directory
BINDIR='./bin/'                 # build directory for example
LIBDIR='./lib/'                 # build directory for library
MODCODE='json_module.f90'       # json module file name
EXAMPLECODE='json_example.f90'  # example program file name
LIBOUT='libjsonfortran.a'       # name of json library

if [ $use_ifort -eq 1 ]
then
	# Intel compiler
	
	FCOMPILER='Intel'
	# The following warning might be triggered by ifort unless explicitly silenced:
	# warning #7601: F2008 standard does not allow an internal procedure to be an actual argument procedure name. (R1214.4).
	# In the context of F2008 this is an erroneous warning.
	# See https://prd1idz.cps.intel.com/en-us/forums/topic/486629
	FCOMPILERFLAGS= '-c -O2 -warn -stand f08 -diag-disable 7601 -traceback'
	#FCOMPILERFLAGS='-c -O2 -warn -traceback -stand f08 -assume protect_parens -assume buffered_io -check all'	

else
	# GFortran (must be >= 4.9)
	
	FCOMPILER='gnu'
	FCOMPILERFLAGS='-c -O2 -fbacktrace -Wall -Wextra -Wno-maybe-uninitialized -pedantic -std=f2008'
fi

#build the stand-alone library:
echo ""
echo "Building library..."
./FoBiS.py build -compiler ${FCOMPILER} -cflags "${FCOMPILERFLAGS}" -dbld ${LIBDIR} -s ${SRCDIR} -dmod ./ -dobj ./ -t ${MODCODE} -o ${LIBOUT} -mklib static

#build the example program:
echo ""
echo "Building example program..."
./FoBiS.py build -compiler ${FCOMPILER} -cflags "${FCOMPILERFLAGS}" -dbld ${BINDIR} -s ${SRCDIR} -dmod ./ -dobj ./ -t ${EXAMPLECODE} -o json

#build the documentation with RoboDoc:
echo ""
echo "Building documentation..."
robodoc --rc ./robodoc.rc --src ${SRCDIR} --doc ${DOCDIR} --multidoc --html --ignore_case_when_linking --syntaxcolors --source_line_numbers --index --tabsize 4 --documenttitle ${PROJECTNAME} --sections
echo ""
