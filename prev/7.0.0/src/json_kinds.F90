!*****************************************************************************************
!> author: Jacob Williams
!  license: BSD
!
!  JSON-Fortran kind definitions.
!
!### License
!  * JSON-Fortran is released under a BSD-style license.
!    See the [LICENSE](https://github.com/jacobwilliams/json-fortran/blob/master/LICENSE)
!    file for details.
!
!@note ```-DUSE_UCS4``` is an optional preprocessor flag.
!      When present, Unicode support is enabled. Note that this
!      is currently only supported with the gfortran compiler.
!      Example: ```gfortran -DUSE_UCS4 ... ```
#ifdef USE_UCS4
#  pragma push_macro("USE_UCS4")
#  undef USE_UCS4
!      The documentation given here assumes ```USE_UCS4``` **is** defined.
#  pragma pop_macro("USE_UCS4")
#else
!      The documentation given here assumes ```USE_UCS4``` **is not** defined.
#endif
!
!@warning ```CK``` and ```CDK``` are the JSON-Fortran character kind and JSON-Fortran default
!         character kind respectively. Client code **MUST** ensure characters of ```kind=CK```
!         are used for all character variables and strings passed to the JSON-Fortran
!         library *EXCEPT* for file names which must be of ```'DEFAULT'``` character kind,
!         provided here as ```CDK```. In particular, any variable that is a: json path, string
!         value or object name passed to the JSON-Fortran library **MUST** be of type ```CK```.
!
!@note Most string literal constants of default kind are fine to pass as arguments to
!      JSON-Fortran procedures since they have been overloaded to accept ```intent(in)```
!      character arguments of the default (```CDK```) kind. If you find a procedure which does
!      not accept an ```intent(in)``` literal string argument of default kind, please
!      [file an issue](https://github.com/jacobwilliams/json-fortran/issues/new) on GitHub.
!
!@note The default real kind (`RK`) and the default integer kind (`IK`) can be
!      changed using optional preprocessor flags. This library was built with kinds:
#ifdef REAL32
!      real(kind=real32) [4 bytes]
#elif REAL64
!      real(kind=real64) [8 bytes]
#elif REAL128
!      real(kind=real128) [16 bytes]
#else
!      real(kind=real64) [8 bytes]
#endif
!      and
#ifdef INT8
!      integer(kind=int8) [1 byte]
#elif INT16
!      integer(kind=int16) [2 bytes]
#elif INT32
!      integer(kind=int32) [4 bytes]
#elif INT64
!      integer(kind=int64) [8 bytes]
#else
!      integer(kind=int32) [4 bytes]
#endif
!      .
!
!@note In addition to the real kind specified by `RK`, interfaces for
!      the real kinds with less precision are also provided in the library,
!      but all are converted to `real(RK)` variables internally.

    module json_kinds

    use,intrinsic :: iso_fortran_env

    implicit none

    private

! used for the reals with less precision
! than the default precision:
#ifndef REAL32
    public :: real32
#endif
#ifdef REAL128
    public :: real64
#endif

#ifdef REAL32
    integer,parameter,public :: RK = real32   !! Default real kind [4 bytes]
#elif REAL64
    integer,parameter,public :: RK = real64   !! Default real kind [8 bytes]
#elif REAL128
    integer,parameter,public :: RK = real128  !! Default real kind [16 bytes]
#else
    integer,parameter,public :: RK = real64   !! Default real kind if not specified [8 bytes]
#endif

#ifdef INT8
    integer,parameter,public :: IK = int8     !! Default integer kind [1 byte]
#elif INT16
    integer,parameter,public :: IK = int16    !! Default integer kind [2 bytes]
#elif INT32
    integer,parameter,public :: IK = int32    !! Default integer kind [4 bytes]
#elif INT64
    integer,parameter,public :: IK = int64    !! Default integer kind [8 bytes]
#else
    integer,parameter,public :: IK = int32    !! Default integer kind if not specified [4 bytes]
#endif

    !*********************************************************
    !>
    !  Processor dependent 'DEFAULT' character kind.
    !  This is 1 byte for the Intel and Gfortran compilers.
    integer,parameter,public :: CDK = selected_char_kind('DEFAULT')
    !*********************************************************

    !*********************************************************
    !>
    !  Default logical kind.
    !  This is 4 bytes for the Intel and Gfortran compilers
    !  (and perhaps others).
    !  The declaration ensures a valid kind
    !  if the compiler doesn't have a logical_kinds(3).
    integer,parameter,public :: LK = logical_kinds(min(3,size(logical_kinds)))
    !*********************************************************

    !*********************************************************
    !>
    !  String kind preprocessor macro.
#if defined __GFORTRAN__ && defined USE_UCS4
    ! gfortran compiler AND UCS4 support requested:
    character(kind=CDK,len=*),parameter :: json_fortran_string_kind = 'ISO_10646'
#else
    ! this is the string kind to use unless compiling with GFortran AND
    ! UCS4/ISO 10646 support is requested
    character(kind=CDK,len=*),parameter :: json_fortran_string_kind = 'DEFAULT'
#endif
    !*********************************************************

    !*********************************************************
    !>
    !  Default character kind used by JSON-Fortran.
    !  If ISO 10646 (UCS4) support is available, use that,
    !  otherwise, gracefully fall back on 'DEFAULT' characters.
    !  Currently only gfortran >= 4.9.2 will correctly support
    !  UCS4 which is stored in 4 bytes.
    !  (and perhaps others).
    integer,parameter,public :: CK = selected_char_kind(json_fortran_string_kind)
    !*********************************************************

    end module json_kinds
!*****************************************************************************************
