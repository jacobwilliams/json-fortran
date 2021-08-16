#!/bin/bash

#
#  NAME
#    build.sh
#
#  DESCRIPTION
#    Build the JSON-Fortran library and unit tests.
#
#  USAGE
#    build.sh [--compiler {intel|gnu|<other>}] [--cflags '<custom compiler flags here>']
#             [--coverage [{yes|no}]] [--profile [{yes|no}]] [--skip-tests [{yes|no}]]
#             [--skip-documentation [{yes|no}]] [--enable-unicode [{yes|no}]] [--help]
#             [--clean] [--real-kind [{REAL32\REAL64\REAL128}]]
#             [--int-kind [{INT8\INT16\INT32\INT64}]]
#
#    By default, if invoked without any flags, this build script will build the
#    JSON-Fortran library using gfortran,
#        without :
#            unicode support
#            coverage flags
#            profiling flags
#        with :
#            unit tests enabled
#            documentation (if FORD is installed)
#            real(REAL64) kinds
#            integer(INT32) kinds
#
#     More recent (right-most) flags will override preceding flags
#     flags:
#        --compiler : gnu or gfortran for gfortran, intel or ifort for intel compiler
#                     A custom compiler may also be specified here, e.g. ftn
#
#        --cflags : Enter any additional/custom compiler flags here and make sure they are
#                   properly quoted
#
#        --help : Print a usage message and exit.
#
#        --clean : Delete generated files and clean up after builds
#
#
#        The following flags all (optionally) accept an argument, "yes" or "no." If
#        no argument is passed, "yes" will be assumed.
#
#        --enable-unicode [{yes|no}]: Request that the JSON-Fortran be built with (or
#                                     without) unicode/UCS4 support. If your compiler
#                                     does NOT support ISO 10646/UCS4 and it was
#                                     requested, then a warning is printed and the
#                                     library is built without UCS4 support.
#
#        --coverage [{yes|no}]: Compile the library and tests with code coverage enabled
#                               or disabled.
#
#        --profile [{yes|no}]: Compile the library and tests with code profiling enabled
#                              or disabled
#
#        --skip-tests [{yes|no}]: Skip (or don't skip) building and running the json-
#                                 fortran unit tests
#
#        --skip-documentation [{yes|no}]: Skip (or don't skip) building the json-
#                                         fortran documentation using FORD
#
#  REQUIRES
#    FoBiS.py : https://github.com/szaghi/FoBiS                    [version 1.2.5 or later required]
#    FORD     : https://github.com/Fortran-FOSS-Programmers/ford   [version 4.0.0 or later]
#
#  AUTHOR
#    Jacob Williams : 12/27/2014
#

#set -x
#set -v
set -o errexit

FORDMD='json-fortran.md'        # FORD options file for building documentation
DOCDIR='./doc/'                 # build directory for documentation
PAGESDIR='./pages/'             # Directory for FORD "pages"
SRCDIR='./src/'                 # library source directory
TESTDIR='./src/tests/'          # unit test source directory
INTROSPECDIR='./src/tests/introspection/' # pre compile configuration tests directory
UCS4TESTCODE='test_iso_10646_support.f90'
BINDIR='./bin/'                 # build directory for unit tests
LIBDIR='./lib/'                 # build directory for library
MODCODE='json_module.F90'       # json module file name
LIBOUT='libjsonfortran.a'       # name of json library
FPP="gfortran -E"               # default to gfortran -E pre-processing

# The following warning might be triggered by ifort unless explicitly silenced:
# warning #7601: F2008 standard does not allow an internal procedure to be an actual argument procedure name. (R1214.4).
# In the context of F2008 this is an erroneous warning.
# See https://prd1idz.cps.intel.com/en-us/forums/topic/486629
INTELCOMPILERFLAGS='-c -O2 -warn -stand f08 -diag-disable 7601 -diag-disable 4013 -diag-disable 5142 -traceback'
#INTELCOMPILERFLAGS='-c -O2 -warn -traceback -stand f08 -assume protect_parens -assume buffered_io -check all'

GNUCOMPILERFLAGS='-c -O2 -fbacktrace -Wall -Wextra -Wno-maybe-uninitialized -Wno-unused-function -pedantic -std=f2008 -fno-omit-frame-pointer'

FCOMPILER='gnu' #Set default compiler to gfortran


# command line argument parsing
# N.B.: Arguments appearing later in the list take precidence over those appearing earlier.
#       e.g., "./build.sh --compiler intel --coverage no --compiler gnu --coverage" will
#       perform the build with the GFORTRAN compiler, and coverage analysis

script_name="$(basename "$0")"

# usage message
print_usage () {
    echo -e "\n\nUsage:\n"
    echo -e "${script_name} [--compiler {intel|gnu|<other>}] [--cflags '<custom compiler flags here>']\n\
         [--coverage [{yes|no}]] [--profile [{yes|no}]] [--skip-tests [{yes|no}]]\n\
         [--skip-documentation [{yes|no}]] [--enable-unicode [{yes|no}]] [--help]"
    echo ""
    echo -e "Any flags that take an optional yes or no argument will default to 'yes' when no\n\
argument is passed. Additionally, A custom compiler may be passed to the 'compiler'\n\
flag, but appropriate 'cflags' should also be passed to the script.\n\n"
}


while [ "$#" -ge "1" ]; do # Get command line arguments while there are more left to process

    key="$1" # Command line args are key-value pairs or value-less keys

    case $key in #find known keys
    --compiler) #pick the compiler. Defaults to gfortran, but intel or custom compilers can be used
        case "$2" in
        intel|Intel|INTEL|ifort)
            FCOMPILER='Intel'
            FCOMPILERFLAGS="$INTELCOMPILERFLAGS"
	    FPP="fpp"
            shift
            ;;
        gnu|Gnu|GNU|gfortran|Gfortran|GFortran|GFORTRAN)
            FCOMPILER='gnu'
            FCOMPILERFLAGS="$GNUCOMPILERFLAGS"
	    FPP="gfortran -E"
            shift
            ;;
        *)
            FCOMPILER="custom"
            echo "Warning: Trying to build with unsupported compiler, $2." 1>&2
            echo "Please ensure you set appropriate --cflags and (single) quote them" 1>&2
            FC="$2"
	    FPP="gfortran -E" # try gfortran to preprocess as a default
            shift
            ;;
        esac
        ;;
    --cflags)
        FCOMPILERFLAGS="$2"
        # no good way to check that the user didn't do something questionable
        shift
        ;;
    --real-kind)
        REAL_KIND="-D$2"
        # warning: not checking for valid input
        # should be one of: REAL32, REAL64 [default], REAL128
        shift
        ;;
    --int-kind)
        INT_KIND="-D$2"
        # warning: not checking for valid input
        # should be one of: INT8, INT16, INT32 [default], INT64
        shift
        ;;
    --enable-unicode)
        case $2 in
        yes|Yes|YES)
            TRY_UNICODE="yes"
            shift
            ;;
        no|No|NO)
            TRY_UNICODE="no"
            shift
            ;;
        *)
            TRY_UNICODE="yes"
            # don't shift; $2 is next arg
            ;;
        esac
        ;;
    --coverage) # enable coverage
        case $2 in
        yes|Yes|YES)
            CODE_COVERAGE="yes"
            shift
            ;;
        no|No|NO)
            CODE_COVERAGE="no"
            shift
            ;;
        *)
            CODE_COVERAGE="yes"
            # don't shift because $2 is some other flag
            ;;
        esac
        ;;
    --profile) #enable profiling
        case $2 in
        yes|Yes|YES)
            CODE_PROFILE="yes"
            shift
            ;;
        no|No|NO)
            CODE_PROFILE="no"
            shift
            ;;
        *)
            CODE_PROFILE="yes"
            # don't shift because $2 is some other flag
            ;;
        esac
        ;;
    --skip-tests) # skip tests
        case $2 in
        yes|Yes|YES)
            JF_SKIP_TESTS="yes"
            shift
            ;;
        no|No|NO)
            JF_SKIP_TESTS="no"
            shift
            ;;
        *)
            JF_SKIP_TESTS="yes"
            ;;
        esac
        ;;
    --skip-documentation)
        case $2 in
        yes|Yes|YES)
            JF_SKIP_DOCS="yes"
            shift
            ;;
        no|No|NO)
            JF_SKIP_DOCS="no"
            shift
            ;;
        *)
            JF_SKIP_DOCS="yes"
            ;;
        esac
        ;;
    --help)
        print_usage
        exit 0
        ;;
    --clean)
        rm -r -- src{,/tests}/*.o $DOCDIR* $LIBDIR* $BINDIR* *.gcov*
        ;;
    *)
        echo "Unknown flag, \"$1\", passed to ${script_name}!" 2>&1
        print_usage
        exit 1
        ;;
    esac
    shift # look at next argument
done # with argument parsing loop

# if no compiler selected, then we're defaulting to gnu, and need to check that the cflags are set
if [ "$FCOMPILER" = 'gnu' ] && [ -z "$FCOMPILERFLAGS" ]; then
    FCOMPILERFLAGS="$GNUCOMPILERFLAGS"
fi

if [[ $CODE_COVERAGE == [yY]* ]]; then
    echo "Trying to compile with code coverage instrumentation."
    COVERAGE="-coverage"
fi

if [[ $CODE_PROFILE == [yY]* ]]; then
    echo "Trying to compile with code profiling instrumentation."
    PROFILING="-profile"
fi

if [[ $FCOMPILER == custom ]]; then
    echo "Trying to compile with custom compiler, $FC"
    CUSTOM=("-fc" "$FC")
fi

if [[ $TRY_UNICODE == [yY]* ]]; then
    echo "Trying to compile library with Unicode/UCS4 support"
    FoBiS.py build -ch -compiler "${FCOMPILER}" "${CUSTOM[@]}" -cflags "${FCOMPILERFLAGS}" -dbld "${BINDIR}" -s "${INTROSPECDIR}" -dmod ./ -dobj ./ -t "${UCS4TESTCODE}" -o "${UCS4TESTCODE%.f90}" -colors
    if "${BINDIR}/${UCS4TESTCODE%.f90}"; then
    DEFINES="-DUSE_UCS4 -Wunused-function"
    fi
fi

#build the stand-alone library:
echo ""
echo "Building library..."

FoBiS.py build -ch -compiler ${FCOMPILER} "${CUSTOM[@]}" -cflags "${FCOMPILERFLAGS} ${DEFINES} ${REAL_KIND} ${INT_KIND}" ${COVERAGE} ${PROFILING} -dbld ${LIBDIR} -s ${SRCDIR} -dmod ./ -dobj ./ -t ${MODCODE} -o ${LIBOUT} -mklib static -colors

#build the unit tests (uses the above library):
if [[ $JF_SKIP_TESTS != [yY]* ]]; then
    echo ""
    echo "Building unit tests..."

    # FoBiS.py PR #45 work around
    [ -d "$BINDIR" ] || mkdir "$BINDIR"

    for TEST in "${TESTDIR%/}"/jf_test_*.[fF]90; do
    THIS_TEST=${TEST##*/}
    echo "Build ${THIS_TEST%.[fF]90}"
    FoBiS.py build -ch -compiler ${FCOMPILER} "${CUSTOM[@]}" -cflags "${FCOMPILERFLAGS} ${DEFINES}" ${COVERAGE} ${PROFILING} -dbld "${BINDIR}" -s "${TESTDIR}" -i "${LIBDIR}" -libs "${LIBDIR}/${LIBOUT}" -dmod ./ -dobj ./ -t "${THIS_TEST}" -o "${THIS_TEST%.[fF]90}" -colors
    done
else
    echo "Skip building the unit tests since \$JF_SKIP_TESTS has been set to 'true'."
fi

# Run all the tests unless $JF_SKIP_TESTS
echo ""
if [[ $JF_SKIP_TESTS != [yY]* ]] ; then
    echo "Running tests..."
    OLD_IGNORES="$GLOBIGNORE"
    # run next commands in subshell to avoid `cd -`
    (cd "$BINDIR"
    GLOBIGNORE='*.*'
    # from: http://stackoverflow.com/questions/7992689/bash-how-to-loop-all-files-in-sorted-order
    ls jf_test_* | sed 's/^\([^0-9]*\)\([0-9]*\)/\1 \2/' | sort -k2,2n | tr -d ' ' |
    while read TEST; do
        # It would be nice to run json output printed to stdout through jsonlint, however,
        # some tests output more than one json structure and these need to be split
        echo ""
        echo "======================================================"
        echo ""
        echo "Running ${TEST}"
        "./${TEST}"
    done)
    echo ""
    echo "======================================================"
    GLOBIGNORE="$OLD_IGNORES"
    if [[ $CODE_COVERAGE = [yY]* ]] ; then
        for SRCFILE in json_string_utilities.F90 json_value_module.F90 json_file_module.F90 ; do
            [ -f ${SRCDIR}${SRCFILE}.gcov ] && rm ${SRCDIR}${SRCFILE}.gcov
            gcov -o $LIBDIR ${SRCDIR}${SRCFILE}
            if [[ $TRY_UNICODE = [yY]* ]] ; then
                # gcov/gfortran bug work around
                awk -F':' '{line=""; for(i=2;i<=NF;i++){line=line":"$i}; if (NR > 1) print $1 prevline; prevline=line}; END{print "        -"prevline}' ${SRCFILE}.gcov > ${SRCFILE}.gcov.fixed && \
                mv ${SRCFILE}.gcov{.fixed,}
                # rename so we can merge coverage info
                mv ${SRCFILE}.gcov ${SRCFILE}-unicode.gcov
            else
                # rename so we can merge coverage info
                mv ${SRCFILE}.gcov ${SRCFILE}-no-unicode.gcov
            fi
            if [ -f ${SRCFILE}-unicode.gcov ] && [ -f ${SRCFILE}-no-unicode.gcov ]; then

                ############## for debugging
                #echo ""
                #echo "-------------------"
                #echo "no-unicode file"
                #echo "-------------------"
                #cat ${SRCFILE}-no-unicode.gcov
                #echo ""
                #echo "-------------------"
                #echo "unicode file"
                #echo "-------------------"
                #cat ${SRCFILE}-unicode.gcov
                #echo ""
                #./pages/development-resources/gccr.pl -n -c ${SRCFILE}-no-unicode.gcov no-unicode \
                #                  ${SRCFILE}-unicode.gcov unicode
                ##############

                # merge them
                ./pages/development-resources/gccr.pl -n -c ${SRCFILE}-no-unicode.gcov no-unicode \
                                  ${SRCFILE}-unicode.gcov unicode > ${SRCFILE}.gcov
            else
                cp ${SRCFILE}*-unicode.gcov ${SRCFILE}.gcov
            fi
        done

        FoBiS.py rule -gcov_analyzer .
        for SRCFILE in json_string_utilities.F90 json_value_module.F90 json_file_module.F90 ; do
            sed -i"bak" -E 's; \*\*([a-zA-Z]+[a-zA-Z0-9_]*)\*\*; \*\*[[\1]]\*\*;' ${SRCFILE}.gcov.md
            sed -i"bak" -E "s;, line ([0-9]+);, line [\1](https://github.com/jacobwilliams/json-fortran/blob/master/src/${SRCFILE}#L\1);" ${SRCFILE}.gcov.md
        done
        gcov -o $BINDIR ${TESTDIR}*.[Ff]90
    fi
else
    echo "Skip running the unit tests since \$JF_SKIP_TESTS has been set to ${JF_SKIP_TESTS}."
fi

#build the documentation with ford (if present):
echo ""
if [[ $JF_SKIP_DOCS != [yY]* ]]; then
    if hash ford 2>/dev/null; then
    echo "Building documentation..."
    [[ $TRY_UNICODE = [yY]* ]] && MACRO_FLAG=("-m" "USE_UCS4")
    echo "$FPP" > .PREPROCESSOR # Override via include in project file, until FORD gets CLI for this
    ford --debug "${MACRO_FLAG[@]}" -p "$PAGESDIR" "$FORDMD"
    else
    echo "FORD not found! Install using: pip install ford"
    fi
else
    echo "Skip building documentation since \$JF_SKIP_DOCS has been set to ${JF_SKIP_DOCS}."
fi
