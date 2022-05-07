!*****************************************************************************************
!> author: Jacob Williams
!  license: BSD
!
!  A Modern Fortran JSON (JavaScript Object Notation) API.
!
!  This module provides access to [[json_value_module]] and
!  [[json_file_module]]. For normal JSON-Fortran use, using this module
!  is all that is necessary.
!
!  Note that this module renames the kind definition variables from [[json_kinds]]
!  from [`RK`, `IK`, `LK`, `CK`, and `CDK`] to [`json_RK`, `json_IK`, `json_LK`,
!  `json_CK`, and `json_CDK`] so as to avoid namespace pollution with short
!  variable names.
!
#ifdef USE_UCS4
#pragma push_macro("USE_UCS4")
#undef USE_UCS4
!  Since ```USE_UCS4``` **is** defined, this module also exports the
!  operators `==`, `/=`, and `//` from [[json_string_utilities]] for
!  `CK` and `CDK` operations.
#pragma pop_macro("USE_UCS4")
#endif
!
!### License
!  * JSON-Fortran is released under a BSD-style license.
!    See the [LICENSE](https://github.com/jacobwilliams/json-fortran/blob/master/LICENSE)
!    file for details.
!
!### History
!  * Joseph A. Levin : March 2012 : Original [FSON](https://github.com/josephalevin/fson)
!    code [retrieved on 12/2/2013].
!  * Jacob Williams : 2/8/2014 : Extensive modifications to the original FSON code.
!    The original F95 code was split into four files:
!    fson_path_m.f95, fson_string_m.f95, fson_value_m.f95, and fson.f95.
!    The new code has been extensively updated, refactored and combined into this
!    one module (json_module.f90).
!    Various Fortran 2003/2008 features are now used
!    (e.g., allocatable strings, newunit, generic, class, and abstract interface).
!  * Development continues at: [Github](https://github.com/jacobwilliams/json-fortran)
!
!### See also
!  * [json-fortran development site](https://github.com/jacobwilliams/json-fortran)
!  * [json-fortran online documentation](https://jacobwilliams.github.io/json-fortran)
!  * [JSON website](http://www.json.org/)
!  * [JSON validator](http://jsonlint.com/)
!
!@note Originally JSON-Fortran was entirely contained within this module.

    module json_module

    use json_kinds, only: json_RK  => RK, &
                          json_IK  => IK, &
                          json_LK  => LK, &
                          json_CK  => CK, &
                          json_CDK => CDK
#ifdef USE_UCS4
    use json_string_utilities, only: operator(==),&
                                     operator(//),&
                                     operator(/=)
#endif
    use json_parameters, only: json_unknown,&
                               json_null,   &
                               json_object, &
                               json_array,  &
                               json_logical,&
                               json_integer,&
                               json_real,   &
                               json_double, &
                               json_string
    use json_value_module
    use json_file_module

    implicit none

    character(kind=json_CK,len=*),parameter,private :: version = '8.3.0'
        !! JSON-Fortran version.
        !!
        !!@note This string should match the one in the `.VERSION` file (which is used
        !!      for the documentation generation.)

    public

    contains
!*****************************************************************************************

!*****************************************************************************************
!>
!  Returns the JSON-Fortran version string.

    function json_fortran_version() result(ver)

    implicit none

    character(len=:),allocatable :: ver  !! JSON-Fortran version string

    ver = version

    end function json_fortran_version
!*****************************************************************************************

!*****************************************************************************************
    end module json_module
!*****************************************************************************************
