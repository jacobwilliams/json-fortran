!*****************************************************************************************
!> author: Jacob Williams
!  license: BSD
!
!  A Fortran 2008 JSON (JavaScript Object Notation) API.
!
!  This module provides access to [[json_kinds]], [[json_value_module]] and
!  [[json_file_module]].  Either one can be used separately, or all can be
!  used by using this module.
!
!## License
!  * JSON-Fortran is released under a BSD-style license.
!    See the [LICENSE](https://github.com/jacobwilliams/json-fortran/blob/master/LICENSE)
!    file for details.
!
!## History
!  * Joseph A. Levin : March 2012 : Original FSON code [retrieved on 12/2/2013].
!  * Jacob Williams : 2/8/2014 : Extensive modifications to the original FSON code.
!    The original F95 code was split into four files:
!    fson_path_m.f95, fson_string_m.f95, fson_value_m.f95, and fson.f95.
!    The new code has been extensively updated, refactored and combined into this
!    one module (json_module.f90).
!    Various Fortran 2003/2008 features are now used
!    (e.g., allocatable strings, newunit, generic, class, and abstract interface).
!  * Development continues at: [Github](http://github.com/jacobwilliams/json-fortran)
!
!## See also
!  * [json-fortran development site](http://github.com/jacobwilliams/json-fortran)
!  * [json-fortran online documentation](http://jacobwilliams.github.io/json-fortran)
!  * [JSON website](http://www.json.org/)
!  * [JSON validator](http://jsonlint.com/)
!
!@note Originally JSON-Fortran was entirely contained within this module.

    module json_module

    use json_kinds
    use json_value_module
    use json_file_module

    implicit none

    public

    end module json_module
!*****************************************************************************************
