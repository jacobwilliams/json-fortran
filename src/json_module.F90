!*****************************************************************************************
!> author: Jacob Williams
!  license: BSD
!
!# JSON-Fortran:
!  A Fortran 2008 JSON (JavaScript Object Notation) API.
!  This module provides access to the [[json_value_module]] and [[json_file_module]],
!  Either one can be used separately, or both can be used by using this module.
!
!@note Originally JSON-Fortran was entirely contained within this module.

    module json_module

    use json_value_module
    use json_file_module

    implicit none

    public

    end module json_module
!*****************************************************************************************
