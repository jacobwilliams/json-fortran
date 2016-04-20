! JSON-Fortran preprocessor macros.
!
! License
!   JSON-Fortran is released under a BSD-style license.
!   See the [LICENSE](https://github.com/jacobwilliams/json-fortran/blob/master/LICENSE)
!   file for details.

!*********************************************************
! File encoding preprocessor macro.
!
#if defined __GFORTRAN__ && defined USE_UCS4
! gfortran compiler AND UCS4 support requested, & silence redefine warning:
! Make sure we output files with utf-8 encoding too
#define FILE_ENCODING ,encoding='UTF-8'
#else
! don't ask for utf-8 file encoding unless using UCS4
! this may let us use unformatted stream io to read in files more quickly
! even with unicode support turned on `inquire( ... encoding=FL_ENCODING)`
! may be able to detect json files in which each character is exactly one
! byte
#define FILE_ENCODING
#endif
!*********************************************************

!*********************************************************
! This C preprocessor macro will take a procedure name as an
! input, and output either that same procedure name if the
! code is compiled without USE_UCS4 being defined or it will
! expand the procedure name to the original procedure name,
! followed by a comma and then the original procedure name
! with 'wrap_' prepended to it. This is suitable for creating
! overloaded interfaces that will accept UCS4 character actual
! arguments as well as DEFAULT/ASCII character arguments,
! based on whether or not ISO 10646 is supported and requested.
!
# ifdef USE_UCS4
#   ifdef __GFORTRAN__
! gfortran uses cpp in old-school compatibility mode so
! the # stringify and ## concatenate operators don't work
! but we can use C/C++ style comment to ensure PROCEDURE is
! correctly tokenized and prepended with 'wrap_' when the
! macro is expanded
#     define MAYBEWRAP(PROCEDURE) PROCEDURE , wrap_/**/PROCEDURE
#   endif
!   ifdef __INTEL_COMPILER
! Intel's fpp does support the more contemporary ## concatenation
! operator, but doesn't treat the C/C++ comments the same way.
! If you use the gfortran approach and pass the -noB switch to
! fpp, the macro will expand, but with a space between wrap_ and
! whatever PROCEDURE expands to
! Intel doesn't support ISO 10646 yet, but this is here to
! ease the transition once they do.
!     define MAYBEWRAP(PROCEDURE) PROCEDURE , wrap_##PROCEDURE
!   endif
# else
#   define MAYBEWRAP(PROCEDURE) PROCEDURE
# endif
!*********************************************************
