#!/bin/bash

#
#  This is just a simple script to build the json-fortran library 
#    and example program on Linux and Mac.
#
#  It also builds the documentation using RoboDoc
#
#  Jacob Williams : 2/8/2014
#     - modified 6/23/2014
#

# Uncomment to debug
#set -x

# Set to 1 to use ifort, otherwise use gfortran
use_ifort=0

SRCDIR='src/'                #source directory
BUILDDIR='lib/'              #build directory for library
BINDIR='bin/'                #build directory for executable
DOCDIR='documentation/'      #build directory for documentation
ARCHIVER='ar'                #archiver name
ARCHIVERFLAGS='-cq'          #archiver flags
FEXT='.f90'                  #fortran file extension
OBJEXT='.o'                  #object code extension
LIBEXT='.a'                  #static library extension
MODEXT='.mod'                #fortran module file extension
WC='*'                       #wildcard character
LIBOUT='libjsonfortran'      #name of json library
EXEOUT='json'                #name of example program
MODCODE='json_module'        #json module file name (no extension)
EXAMPLECODE='json_example'   #example program file name (no extension)
ROBODOC='robodoc'            #robodoc executable name
ROBOFLAGS="--src ${SRCDIR} --doc ${DOCDIR} --multidoc --html --ignore_case_when_linking --syntaxcolors --source_line_numbers --index --tabsize 4 --documenttitle jsonfortran --sections"  #robodoc flags

#
# Compiler-specifics:
#

if [ $use_ifort -eq 1 ]
then

	# Intel compiler
	
	FCOMPILER='ifort'
	# The following warning might be triggered by ifort unless explicitly silenced:
	# warning #7601: F2008 standard does not allow an internal procedure to be an actual argument procedure name. (R1214.4).
	# In the context of F2008 this is an erroneous warning.
	# See https://prd1idz.cps.intel.com/en-us/forums/topic/486629
	FCOMPILERFLAGS='-O2 -warn -stand f08 -diag-disable 7601 -traceback'
	#FCOMPILERFLAGS='-warn -traceback -stand f08 -assume protect_parens -assume buffered_io -check all'
	# trailing space is significant
	FCMODULEPATHFLAG='-module '

else

	# GFortran (must be >= 4.9)
	
	FCOMPILER='gfortran'
	#FCOMPILER='/opt/local/bin/gfortran-mp-4.9'
	FCOMPILERFLAGS='-O2 -fbacktrace -Wall -Wextra -Wno-maybe-uninitialized -pedantic -std=f2008'
	FCMODULEPATHFLAG='-J'

fi

#
# Always a clean build:
#

mkdir -p $BUILDDIR
mkdir -p $BINDIR
mkdir -p $DOCDIR

rm -f $BUILDDIR$WC$OBJEXT
rm -f $BUILDDIR$WC$MODEXT
rm -f $BUILDDIR$WC$LIBEXT
rm -rf $DOCDIR$WC

#
# build library:
#

$FCOMPILER $FCOMPILERFLAGS -c $SRCDIR$MODCODE$FEXT $FCMODULEPATHFLAG$BUILDDIR
mv $MODCODE$OBJEXT $BUILDDIR
$ARCHIVER $ARCHIVERFLAGS $BUILDDIR$LIBOUT$LIBEXT $BUILDDIR$MODCODE$OBJEXT

#
# build example:
#

$FCOMPILER $FCOMPILERFLAGS -o $BINDIR$EXEOUT $FCMODULEPATHFLAG$BUILDDIR $SRCDIR$EXAMPLECODE$FEXT $BUILDDIR$LIBOUT$LIBEXT

#
# build documentation:
#

$ROBODOC $ROBOFLAGS

