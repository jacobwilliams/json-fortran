!*****************************************************************************************
!> author: Jacob Williams
!  license: BSD
!
!  Higher-level [[json_file]] interface for the [[json_value]] type.
!
!### License
!  * JSON-Fortran is released under a BSD-style license.
!    See the [LICENSE](https://github.com/jacobwilliams/json-fortran/blob/master/LICENSE)
!    file for details.

    module json_file_module

    use,intrinsic :: iso_fortran_env
    use json_kinds
    use json_parameters, only: unit2str
    use json_string_utilities
    use json_value_module

    implicit none

    private

#include "json_macros.inc"

    !*********************************************************
    !> author: Jacob Williams
    !  date: 12/9/2013
    !
    !  The `json_file` is the main public class that is
    !  used to open a file and get data from it.
    !
    !  A `json_file` contains only two items: an instance of a [[json_core(type)]],
    !  which is used for all data manipulation, and a [[json_value]] pointer,
    !  which is used to construct the linked-list data structure.
    !  Note that most methods in the `json_file` class are simply wrappers
    !  to the lower-level routines in the [[json_value_module]].
    !
    !### Example
    !
    !```fortran
    !    program test
    !    use json_module
    !    implicit none
    !    type(json_file) :: json
    !    integer :: ival
    !    real(real64) :: rval
    !    character(len=:),allocatable :: cval
    !    logical :: found
    !    call json%initialize(compact_reals=.true.)
    !    call json%load(filename='myfile.json')
    !    call json%print() !print to the console
    !    call json%get('var.i',ival,found)
    !    call json%get('var.r(3)',rval,found)
    !    call json%get('var.c',cval,found)
    !    call json%destroy()
    !    end program test
    !```
    !
    !@note The `destroy()` method may be called to free the memory if necessary.
    !      [[json_file(type)]] includes a finalizer that also calls
    !      `destroy()` when the variable goes out of scope.

    type,public :: json_file

        private

        type(json_core)          :: core         !! The instance of the [[json_core(type)]]
                                                 !! factory used for this file.
        type(json_value),pointer :: p => null()  !! the JSON structure read from the file

    contains

        generic,public :: initialize => initialize_json_core_in_file,&
                                        set_json_core_in_file

        procedure,public :: get_core => get_json_core_in_file

        !>
        !  Load JSON from a file.
        procedure,public :: load => json_file_load

        !>
        !  The same as `load`, but only here for backward compatibility
        procedure,public :: load_file => json_file_load

        !>
        !  Load JSON from a string.
        generic,public :: deserialize => MAYBEWRAP(json_file_load_from_string)

        !>
        !  The same as `deserialize`, but only here for backward compatibility
        generic,public :: load_from_string => MAYBEWRAP(json_file_load_from_string)

        !>
        !  Print the [[json_value]] structure to an allocatable string
        procedure,public :: serialize => json_file_print_to_string

        !>
        !  The same as `serialize`, but only here for backward compatibility
        procedure,public :: print_to_string => json_file_print_to_string

        procedure,public :: destroy => json_file_destroy
        procedure,public :: nullify => json_file_nullify
        procedure,public :: move    => json_file_move_pointer
        generic,public   :: info    => MAYBEWRAP(json_file_variable_info)
        generic,public   :: matrix_info => MAYBEWRAP(json_file_variable_matrix_info)

        !error checking:
        procedure,public :: failed => json_file_failed
        procedure,public :: print_error_message => json_file_print_error_message
        procedure,public :: check_for_errors => json_file_check_for_errors
        procedure,public :: clear_exceptions => json_file_clear_exceptions

        generic,public :: print => json_file_print_to_console, &
                                   json_file_print_to_unit, &
                                   json_file_print_to_filename

        !>
        !  The same as `print`, but only here for backward compatibility
        generic,public :: print_file => json_file_print_to_console, &
                                        json_file_print_to_unit, &
                                        json_file_print_to_filename

        !>
        !  Rename a variable, specifying it by path
        generic,public :: rename => MAYBEWRAP(json_file_rename)
#ifdef USE_UCS4
        generic,public :: rename => json_file_rename_path_ascii, &
                                    json_file_rename_name_ascii
#endif

        !>
        !  Verify that a path is valid
        !  (i.e., a variable with this path exists in the file).
        generic,public :: valid_path => MAYBEWRAP(json_file_valid_path)

        !>
        !  Get a variable from a [[json_file(type)]], by specifying the path.
        generic,public :: get => MAYBEWRAP(json_file_get_object),      &
                                 MAYBEWRAP(json_file_get_integer),     &
#ifndef REAL32
                                 MAYBEWRAP(json_file_get_real32),      &
#endif
                                 MAYBEWRAP(json_file_get_real),        &
#ifdef REAL128
                                 MAYBEWRAP(json_file_get_real64),      &
#endif
                                 MAYBEWRAP(json_file_get_logical),     &
                                 MAYBEWRAP(json_file_get_string),      &
                                 MAYBEWRAP(json_file_get_integer_vec), &
#ifndef REAL32
                                 MAYBEWRAP(json_file_get_real32_vec),  &
#endif
                                 MAYBEWRAP(json_file_get_real_vec),    &
#ifdef REAL128
                                 MAYBEWRAP(json_file_get_real64_vec),  &
#endif
                                 MAYBEWRAP(json_file_get_logical_vec), &
                                 MAYBEWRAP(json_file_get_string_vec),  &
                                 MAYBEWRAP(json_file_get_alloc_string_vec),  &
                                 json_file_get_root

        !>
        !  Add a variable to a [[json_file(type)]], by specifying the path.
        !
        !### Example
        !
        !```fortran
        !  program test
        !  use json_module, rk=>json_rk, ik=>json_ik
        !  implicit none
        !  type(json_file) :: f
        !  call f%initialize()  ! specify whatever init options you want.
        !  call f%add('inputs.t', 0.0_rk)
        !  call f%add('inputs.x', [1.0_rk,2.0_rk,3.0_rk])
        !  call f%add('inputs.flag', .true.)
        !  call f%print()  ! print to the console
        !  end program test
        !```
        generic,public :: add => json_file_add, &
                                 MAYBEWRAP(json_file_add_object),      &
                                 MAYBEWRAP(json_file_add_integer),     &
#ifndef REAL32
                                 MAYBEWRAP(json_file_add_real32),      &
#endif
                                 MAYBEWRAP(json_file_add_real),        &
#ifdef REAL128
                                 MAYBEWRAP(json_file_add_real64),      &
#endif
                                 MAYBEWRAP(json_file_add_logical),     &
                                 MAYBEWRAP(json_file_add_string),      &
                                 MAYBEWRAP(json_file_add_integer_vec), &
#ifndef REAL32
                                 MAYBEWRAP(json_file_add_real32_vec),  &
#endif
                                 MAYBEWRAP(json_file_add_real_vec),    &
#ifdef REAL128
                                 MAYBEWRAP(json_file_add_real64_vec),  &
#endif
                                 MAYBEWRAP(json_file_add_logical_vec), &
                                 MAYBEWRAP(json_file_add_string_vec)
#ifdef USE_UCS4
        generic,public :: add => json_file_add_string_path_ascii, &
                                 json_file_add_string_value_ascii,&
                                 json_file_add_string_vec_path_ascii,&
                                 json_file_add_string_vec_vec_ascii
#endif

        !>
        !  Update a scalar variable in a [[json_file(type)]],
        !  by specifying the path.
        !
        !@note These have been mostly supplanted by the `add`
        !      methods, which do a similar thing (and can be used for
        !      scalars and vectors, etc.)
        generic,public :: update =>  MAYBEWRAP(json_file_update_integer),  &
                                     MAYBEWRAP(json_file_update_logical),  &
#ifndef REAL32
                                     MAYBEWRAP(json_file_update_real32),   &
#endif
                                     MAYBEWRAP(json_file_update_real),     &
#ifdef REAL128
                                     MAYBEWRAP(json_file_update_real64),   &
#endif
                                     MAYBEWRAP(json_file_update_string)
#ifdef USE_UCS4
        generic,public :: update => json_file_update_string_name_ascii, &
                                    json_file_update_string_val_ascii
#endif

        !>
        !  Remove a variable from a [[json_file(type)]]
        !  by specifying the path.
        generic,public :: remove =>  MAYBEWRAP(json_file_remove)

        !traverse
        procedure,public :: traverse => json_file_traverse

        ! ***************************************************
        ! operators
        ! ***************************************************

        generic,public :: operator(.in.) => MAYBEWRAP(json_file_valid_path_op)
        procedure,pass(me) :: MAYBEWRAP(json_file_valid_path_op)

        generic,public :: assignment(=) => assign_json_file,&
                                           assign_json_file_to_string,&
                                           MAYBEWRAP(assign_string_to_json_file)
        procedure :: assign_json_file
        procedure,pass(me) :: assign_json_file_to_string
        procedure :: MAYBEWRAP(assign_string_to_json_file)

        ! ***************************************************
        ! private routines
        ! ***************************************************

        !load from string:
        procedure :: MAYBEWRAP(json_file_load_from_string)

        !initialize
        procedure :: initialize_json_core_in_file
        procedure :: set_json_core_in_file

        !get info:
        procedure :: MAYBEWRAP(json_file_variable_info)
        procedure :: MAYBEWRAP(json_file_variable_matrix_info)

        !rename:
        procedure :: MAYBEWRAP(json_file_rename)
#ifdef USE_UCS4
        procedure :: json_file_rename_path_ascii
        procedure :: json_file_rename_name_ascii
#endif

        !validate path:
        procedure :: MAYBEWRAP(json_file_valid_path)

        !get:
        procedure :: MAYBEWRAP(json_file_get_object)
        procedure :: MAYBEWRAP(json_file_get_integer)
#ifndef REAL32
        procedure :: MAYBEWRAP(json_file_get_real32)
#endif
        procedure :: MAYBEWRAP(json_file_get_real)
#ifdef REAL128
        procedure :: MAYBEWRAP(json_file_get_real64)
#endif
        procedure :: MAYBEWRAP(json_file_get_logical)
        procedure :: MAYBEWRAP(json_file_get_string)
        procedure :: MAYBEWRAP(json_file_get_integer_vec)
#ifndef REAL32
        procedure :: MAYBEWRAP(json_file_get_real32_vec)
#endif
        procedure :: MAYBEWRAP(json_file_get_real_vec)
#ifdef REAL128
        procedure :: MAYBEWRAP(json_file_get_real64_vec)
#endif
        procedure :: MAYBEWRAP(json_file_get_logical_vec)
        procedure :: MAYBEWRAP(json_file_get_string_vec)
        procedure :: MAYBEWRAP(json_file_get_alloc_string_vec)
        procedure :: json_file_get_root

        !add:
        procedure :: json_file_add
        procedure :: MAYBEWRAP(json_file_add_object)
        procedure :: MAYBEWRAP(json_file_add_integer)
#ifndef REAL32
        procedure :: MAYBEWRAP(json_file_add_real32)
#endif
        procedure :: MAYBEWRAP(json_file_add_real)
#ifdef REAL128
        procedure :: MAYBEWRAP(json_file_add_real64)
#endif
        procedure :: MAYBEWRAP(json_file_add_logical)
        procedure :: MAYBEWRAP(json_file_add_string)
        procedure :: MAYBEWRAP(json_file_add_integer_vec)
#ifndef REAL32
        procedure :: MAYBEWRAP(json_file_add_real32_vec)
#endif
        procedure :: MAYBEWRAP(json_file_add_real_vec)
#ifdef REAL128
        procedure :: MAYBEWRAP(json_file_add_real64_vec)
#endif
        procedure :: MAYBEWRAP(json_file_add_logical_vec)
        procedure :: MAYBEWRAP(json_file_add_string_vec)
#ifdef USE_UCS4
        procedure :: json_file_add_string_path_ascii
        procedure :: json_file_add_string_value_ascii
        procedure :: json_file_add_string_vec_path_ascii
        procedure :: json_file_add_string_vec_vec_ascii
#endif

        !update:
        procedure :: MAYBEWRAP(json_file_update_integer)
        procedure :: MAYBEWRAP(json_file_update_logical)
#ifndef REAL32
        procedure :: MAYBEWRAP(json_file_update_real32)
#endif
        procedure :: MAYBEWRAP(json_file_update_real)
#ifdef REAL128
        procedure :: MAYBEWRAP(json_file_update_real64)
#endif
        procedure :: MAYBEWRAP(json_file_update_string)
#ifdef USE_UCS4
        procedure :: json_file_update_string_name_ascii
        procedure :: json_file_update_string_val_ascii
#endif

        !remove:
        procedure :: MAYBEWRAP(json_file_remove)

        !print:
        procedure :: json_file_print_to_console
        procedure :: json_file_print_to_unit
        procedure :: json_file_print_to_filename

        final :: finalize_json_file

    end type json_file
    !*********************************************************

    !*********************************************************
    !> author: Izaak Beekman
    !  date: 07/23/2015
    !
    !  Structure constructor to initialize a [[json_file(type)]]
    !  object with an existing [[json_value]] object or a JSON
    !  string, and either the [[json_core(type)]] settings or a
    !  [[json_core(type)]] instance.
    !
    !### Example
    !
    !```fortran
    ! ...
    ! type(json_file) :: my_file
    ! type(json_value),pointer :: json_object
    ! type(json_core) :: json_core_object
    ! ...
    ! ! Construct a json_object:
    ! !could do this:
    !   my_file = json_file(json_object)
    ! !or:
    !   my_file = json_file(json_object,verbose=.true.)
    ! !or:
    !   my_file = json_file('{"x": [1]}',verbose=.true.)
    ! !or:
    !   my_file = json_file(json_object,json_core_object)
    ! !or:
    !   my_file = json_file('{"x": [1]}',json_core_object)
    !```
    interface json_file
       module procedure initialize_json_file, &
                        initialize_json_file_v2, &
                        MAYBEWRAP(initialize_json_file_from_string), &
                        MAYBEWRAP(initialize_json_file_from_string_v2)
    end interface
    !*************************************************************************************

    contains
!*****************************************************************************************

!*****************************************************************************************
!>
!  Finalizer for [[json_file]] class.
!
!  Just a wrapper for [[json_file_destroy]].

    subroutine finalize_json_file(me)

    implicit none

    type(json_file),intent(inout) :: me

    call me%destroy(destroy_core=.true.)

    end subroutine finalize_json_file
!*****************************************************************************************

!*****************************************************************************************
!>
!  Check error status in the file.

    pure function json_file_failed(me) result(failed)

    implicit none

    class(json_file),intent(in) :: me
    logical(LK)                 :: failed  !! will be true if there has been an error.

    failed = me%core%failed()

    end function json_file_failed
!*****************************************************************************************

!*****************************************************************************************
!>
!  Retrieve error status and message from the class.

    subroutine json_file_check_for_errors(me,status_ok,error_msg)

    implicit none

    class(json_file),intent(inout) :: me
    logical(LK),intent(out),optional :: status_ok !! true if there were no errors
    character(kind=CK,len=:),allocatable,intent(out),optional :: error_msg !! the error message
                                                                           !! (if there were errors)

#if defined __GFORTRAN__
    character(kind=CK,len=:),allocatable :: tmp  !! workaround for gfortran bugs
    call me%core%check_for_errors(status_ok,tmp)
    if (present(error_msg)) error_msg = tmp
#else
    call me%core%check_for_errors(status_ok,error_msg)
#endif

    end subroutine json_file_check_for_errors
!*****************************************************************************************

!*****************************************************************************************
!>
!  Clear exceptions in the class.

    pure subroutine json_file_clear_exceptions(me)

    implicit none

    class(json_file),intent(inout) :: me

    call me%core%clear_exceptions()

    end subroutine json_file_clear_exceptions
!*****************************************************************************************

!*****************************************************************************************
!>
!  This is a wrapper for [[json_print_error_message]].

    subroutine json_file_print_error_message(me,io_unit)

    implicit none

    class(json_file),intent(inout) :: me
    integer, intent(in), optional  :: io_unit

    call me%core%print_error_message(io_unit)

    end subroutine json_file_print_error_message
!*****************************************************************************************

!*****************************************************************************************
!>
!  Initialize the [[json_core(type)]] for this [[json_file]].
!  This is just a wrapper for [[json_initialize]].
!
!@note This does not destroy the data in the file.
!
!@note [[initialize_json_core]], [[json_initialize]],
!      [[initialize_json_core_in_file]], [[initialize_json_file]],
!      [[initialize_json_file_v2]], [[initialize_json_file_from_string]],
!      and [[initialize_json_file_from_string_v2]]
!      all have a similar interface.

    subroutine initialize_json_core_in_file(me,&
#include "json_initialize_dummy_arguments.inc"
                                           )

    implicit none

    class(json_file),intent(inout) :: me
#include "json_initialize_arguments.inc"

    call me%core%initialize(&
#include "json_initialize_dummy_arguments.inc"
                           )
    end subroutine initialize_json_core_in_file
!*****************************************************************************************

!*****************************************************************************************
!>
!  Set the [[json_core(type)]] for this [[json_file]].
!
!@note This does not destroy the data in the file.
!
!@note This one is used if you want to initialize the file with
!       an already-existing [[json_core(type)]] (presumably, this was already
!       initialized by a call to [[initialize_json_core]] or similar).

    subroutine set_json_core_in_file(me,core)

    implicit none

    class(json_file),intent(inout) :: me
    type(json_core),intent(in)     :: core

    me%core = core

    end subroutine set_json_core_in_file
!*****************************************************************************************

!*****************************************************************************************
!>
!  Get a copy of the [[json_core(type)]] in this [[json_file]].

    subroutine get_json_core_in_file(me,core)

    implicit none

    class(json_file),intent(in) :: me
    type(json_core),intent(out) :: core

    core = me%core

    end subroutine get_json_core_in_file
!*****************************************************************************************

!*****************************************************************************************
!> author: Izaak Beekman
!  date: 07/23/2015
!
!  Cast a [[json_value]] object as a [[json_file(type)]] object.
!  It also calls the `initialize()` method.
!
!@note [[initialize_json_core]], [[json_initialize]],
!      [[initialize_json_core_in_file]], [[initialize_json_file]],
!      [[initialize_json_file_v2]], [[initialize_json_file_from_string]],
!      and [[initialize_json_file_from_string_v2]]
!      all have a similar interface.

    function initialize_json_file(p,&
#include "json_initialize_dummy_arguments.inc"
                                 ) result(file_object)

    implicit none

    type(json_file) :: file_object
    type(json_value),pointer,optional :: p  !! `json_value` object to cast
                                            !! as a `json_file` object. This
                                            !! will be nullified.
#include "json_initialize_arguments.inc"

    call file_object%initialize(&
#include "json_initialize_dummy_arguments.inc"
                               )

    if (present(p)) then
        file_object%p => p
        ! we have to nullify it to avoid
        ! a dangling pointer when the file
        ! goes out of scope
        nullify(p)
    end if

    end function initialize_json_file
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 4/26/2016
!
!  Cast a [[json_value]] pointer and a [[json_core(type)]] object
!  as a [[json_file(type)]] object.

    function initialize_json_file_v2(json_value_object,json_core_object) &
                                        result(file_object)

    implicit none

    type(json_file)                     :: file_object
    type(json_value),pointer,intent(in) :: json_value_object
    type(json_core),intent(in)          :: json_core_object

    file_object%p    => json_value_object
    file_object%core = json_core_object

    end function initialize_json_file_v2
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 01/19/2019
!
!  Cast a JSON string as a [[json_file(type)]] object.
!  It also calls the `initialize()` method.
!
!### Example
!
!```fortran
!  type(json_file) :: f
!  f = json_file('{"key ": 1}', trailing_spaces_significant=.true.)
!```
!
!@note [[initialize_json_core]], [[json_initialize]],
!      [[initialize_json_core_in_file]], [[initialize_json_file]],
!      [[initialize_json_file_v2]], [[initialize_json_file_from_string]],
!      and [[initialize_json_file_from_string_v2]]
!      all have a similar interface.

    function initialize_json_file_from_string(str,&
#include "json_initialize_dummy_arguments.inc"
                                             ) result(file_object)

    implicit none

    type(json_file) :: file_object
    character(kind=CK,len=*),intent(in) :: str  !! string to load JSON data from
#include "json_initialize_arguments.inc"

    call file_object%initialize(&
#include "json_initialize_dummy_arguments.inc"
                               )
    call file_object%deserialize(str)

    end function initialize_json_file_from_string
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[initialize_json_file_from_string]], where "str" is kind=CDK.

    function wrap_initialize_json_file_from_string(str,&
#include "json_initialize_dummy_arguments.inc"
                                                  ) result(file_object)

    implicit none

    type(json_file) :: file_object
    character(kind=CDK,len=*),intent(in) :: str  !! string to load JSON data from
#include "json_initialize_arguments.inc"

    file_object = initialize_json_file_from_string(&
                                  to_unicode(str),&
#include "json_initialize_dummy_arguments.inc"
                                                )

    end function wrap_initialize_json_file_from_string
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/19/2019
!
!  Cast a JSON string and a [[json_core(type)]] object
!  as a [[json_file(type)]] object.

    function initialize_json_file_from_string_v2(str, json_core_object) &
                                        result(file_object)

    implicit none

    type(json_file)                     :: file_object
    character(kind=CK,len=*),intent(in) :: str  !! string to load JSON data from
    type(json_core),intent(in)          :: json_core_object

    file_object%core = json_core_object
    call file_object%deserialize(str)

    end function initialize_json_file_from_string_v2
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[initialize_json_file_from_string_v2]], where "str" is kind=CDK.

    function wrap_initialize_json_file_from_string_v2(str,json_core_object) &
                                        result(file_object)

    implicit none

    type(json_file)                      :: file_object
    character(kind=CDK,len=*),intent(in) :: str  !! string to load JSON data from
    type(json_core),intent(in)           :: json_core_object

    file_object = initialize_json_file_from_string_v2(to_unicode(str),json_core_object)

    end function wrap_initialize_json_file_from_string_v2
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Nullify the [[json_value]] pointer in a [[json_file(type)]],
!  but do not destroy it.
!
!  This should normally only be done if the pointer is the target of
!  another pointer outside the class that is still intended to be in
!  scope after the [[json_file(type)]] has gone out of scope.
!  Otherwise, this would result in a memory leak.
!
!### See also
!  * [[json_file_destroy]]
!
!### History
!  * 6/30/2019 : Created

    subroutine json_file_nullify(me)

    implicit none

    class(json_file),intent(inout) :: me

    nullify(me%p)

    end subroutine json_file_nullify
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Destroy the [[json_value]] data in a [[json_file(type)]].
!  This may be done when the variable is no longer needed,
!  or will be reused to open a different file.
!  Otherwise a memory leak will occur.
!
!  Optionally, also destroy the [[json_core(type)]] instance (this
!  is not necessary to prevent memory leaks, since a [[json_core(type)]]
!  does not use pointers).
!
!### See also
!  * [[json_file_nullify]]
!
!### History
!  * 12/9/2013 : Created
!  * 4/26/2016 : Added optional `destroy_core` argument
!
!@note This routine will be called automatically when the variable
!      goes out of scope.

    subroutine json_file_destroy(me,destroy_core)

    implicit none

    class(json_file),intent(inout) :: me
    logical,intent(in),optional :: destroy_core  !! to also destroy the [[json_core(type)]].
                                                 !! default is to leave it as is.

    if (associated(me%p)) call me%core%destroy(me%p)

    if (present(destroy_core)) then
        if (destroy_core) call me%core%destroy()
    end if

    end subroutine json_file_destroy
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 12/5/2014
!
!  Move the [[json_value]] pointer from one [[json_file(type)]] to another.
!  The "from" pointer is then nullified, but not destroyed.
!
!@note If "from%p" is not associated, then an error is thrown.

    subroutine json_file_move_pointer(to,from)

    implicit none

    class(json_file),intent(inout) :: to
    class(json_file),intent(inout) :: from

    if (associated(from%p)) then

        if (from%failed()) then
            !Don't get the data if the FROM file has an
            !active exception, since it may not be valid.
            call to%core%throw_exception('Error in json_file_move_pointer: '//&
                                         'error exception in FROM file.')
        else
            call to%initialize()  !initialize and clear any exceptions that may be present
            to%p => from%p
            nullify(from%p)
        end if

    else
        call to%core%throw_exception('Error in json_file_move_pointer: '//&
                                     'pointer is not associated.')
    end if

    end subroutine json_file_move_pointer
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 12/9/2013
!
!  Load the JSON data from a file.
!
!### Example
!
!```fortran
!     program main
!      use json_module
!      implicit none
!      type(json_file) :: f
!      call f%load('my_file.json')
!      !...
!      call f%destroy()
!     end program main
!```

    subroutine json_file_load(me, filename, unit)

    implicit none

    class(json_file),intent(inout)       :: me
    character(kind=CDK,len=*),intent(in) :: filename  !! the filename to open
    integer(IK),intent(in),optional      :: unit      !! the unit number to use
                                                      !! (if not present, a newunit
                                                      !! is used)

    call me%core%load(file=filename, p=me%p, unit=unit)

    end subroutine json_file_load
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/13/2015
!
!  Load the JSON data from a string.
!
!### Example
!
!  Load JSON from a string:
!```fortran
!     type(json_file) :: f
!     call f%deserialize('{ "name": "Leonidas" }')
!```

    subroutine json_file_load_from_string(me, str)

    implicit none

    class(json_file),intent(inout)      :: me
    character(kind=CK,len=*),intent(in) :: str  !! string to load JSON data from

    call me%core%deserialize(me%p, str)

    end subroutine json_file_load_from_string
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_file_load_from_string]], where "str" is kind=CDK.

    subroutine wrap_json_file_load_from_string(me, str)

    implicit none

    class(json_file),intent(inout)       :: me
    character(kind=CDK,len=*),intent(in) :: str

    call me%deserialize(to_unicode(str))

    end subroutine wrap_json_file_load_from_string
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/11/2015
!
!  Print the JSON file to the console.

    subroutine json_file_print_to_console(me)

    implicit none

    class(json_file),intent(inout)  :: me

    call me%core%print(me%p,iunit=int(output_unit,IK))

    end subroutine json_file_print_to_console
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 12/9/2013
!
!  Prints the JSON file to the specified file unit number.

    subroutine json_file_print_to_unit(me, iunit)

    implicit none

    class(json_file),intent(inout)  :: me
    integer(IK),intent(in)          :: iunit  !! file unit number (must not be -1)

    if (iunit/=unit2str) then
        call me%core%print(me%p,iunit=iunit)
    else
        call me%core%throw_exception('Error in json_file_print_to_unit: iunit must not be -1.')
    end if

    end subroutine json_file_print_to_unit
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/11/2015
!
!  Print the JSON structure to the specified filename.
!  The file is opened, printed, and then closed.
!
!### Example
!  Example loading a JSON file, changing a value, and then printing
!  result to a new file:
!```fortran
!     type(json_file) :: f
!     logical :: found
!     call f%load('my_file.json')       !open the original file
!     call f%update('version',4,found)  !change the value of a variable
!     call f%print('my_file_2.json')    !save file as new name
!```

    subroutine json_file_print_to_filename(me,filename)

    implicit none

    class(json_file),intent(inout)       :: me
    character(kind=CDK,len=*),intent(in) :: filename  !! filename to print to

    call me%core%print(me%p,filename)

    end subroutine json_file_print_to_filename
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/11/2015
!
!  Print the JSON file to a string.
!
!### Example
!
!  Open a JSON file, and then print the contents to a string:
!```fortran
!     type(json_file) :: f
!     character(kind=CK,len=:),allocatable :: str
!     call f%load('my_file.json')
!     call f%serialize(str)
!```

    subroutine json_file_print_to_string(me,str)

    implicit none

    class(json_file),intent(inout)                   :: me
    character(kind=CK,len=:),allocatable,intent(out) :: str  !! string to print JSON data to

    call me%core%serialize(me%p,str)

    end subroutine json_file_print_to_string
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 2/3/2014
!
!  Returns information about a variable in a [[json_file(type)]].
!
!@note If `found` is present, no exceptions will be thrown if an
!      error occurs. Otherwise, an exception will be thrown if the
!      variable is not found.

    subroutine json_file_variable_info(me,path,found,var_type,n_children,name)

    implicit none

    class(json_file),intent(inout)      :: me
    character(kind=CK,len=*),intent(in) :: path       !! path to the variable
    logical(LK),intent(out),optional    :: found      !! the variable exists in the structure
    integer(IK),intent(out),optional    :: var_type   !! variable type
    integer(IK),intent(out),optional    :: n_children !! number of children
    character(kind=CK,len=:),allocatable,intent(out),optional :: name !! variable name

    call me%core%info(me%p,path,found,var_type,n_children,name)

    end subroutine json_file_variable_info
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_file_variable_info]], where "path" is kind=CDK.
!
!@note If `found` is present, no exceptions will be thrown if an
!      error occurs. Otherwise, an exception will be thrown if the
!      variable is not found.

    subroutine wrap_json_file_variable_info(me,path,found,var_type,n_children,name)

    implicit none

    class(json_file),intent(inout)       :: me
    character(kind=CDK,len=*),intent(in) :: path
    logical(LK),intent(out),optional     :: found
    integer(IK),intent(out),optional     :: var_type
    integer(IK),intent(out),optional     :: n_children
    character(kind=CK,len=:),allocatable,intent(out),optional :: name !! variable name

    call me%info(to_unicode(path),found,var_type,n_children,name)

    end subroutine wrap_json_file_variable_info
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 6/26/2016
!
!  Returns matrix information about a variable in a [[json_file(type)]].
!
!@note If `found` is present, no exceptions will be thrown if an
!      error occurs. Otherwise, an exception will be thrown if the
!      variable is not found.

    subroutine json_file_variable_matrix_info(me,path,is_matrix,found,&
                                        var_type,n_sets,set_size,name)

    implicit none

    class(json_file),intent(inout)      :: me
    character(kind=CK,len=*),intent(in) :: path      !! path to the variable
    logical(LK),intent(out)             :: is_matrix !! true if it is a valid matrix
    logical(LK),intent(out),optional    :: found     !! true if it was found
    integer(IK),intent(out),optional    :: var_type  !! variable type of data in
                                                     !! the matrix (if all elements have
                                                     !! the same type)
    integer(IK),intent(out),optional    :: n_sets    !! number of data sets (i.e., matrix
                                                     !! rows if using row-major order)
    integer(IK),intent(out),optional    :: set_size  !! size of each data set (i.e., matrix
                                                     !! cols if using row-major order)
    character(kind=CK,len=:),allocatable,intent(out),optional :: name !! variable name

    call me%core%matrix_info(me%p,path,is_matrix,found,var_type,n_sets,set_size,name)

    end subroutine json_file_variable_matrix_info
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_file_variable_matrix_info]], where "path" is kind=CDK.
!
!@note If `found` is present, no exceptions will be thrown if an
!      error occurs. Otherwise, an exception will be thrown if the
!      variable is not found.

    subroutine wrap_json_file_variable_matrix_info(me,path,is_matrix,found,&
                                                   var_type,n_sets,set_size,name)

    implicit none

    class(json_file),intent(inout)       :: me
    character(kind=CDK,len=*),intent(in) :: path      !! path to the variable
    logical(LK),intent(out)              :: is_matrix !! true if it is a valid matrix
    logical(LK),intent(out),optional     :: found     !! true if it was found
    integer(IK),intent(out),optional     :: var_type  !! variable type of data in
                                                      !! the matrix (if all elements have
                                                      !! the same type)
    integer(IK),intent(out),optional     :: n_sets    !! number of data sets (i.e., matrix
                                                      !! rows if using row-major order)
    integer(IK),intent(out),optional     :: set_size  !! size of each data set (i.e., matrix
                                                      !! cols if using row-major order)
    character(kind=CK,len=:),allocatable,intent(out),optional :: name !! variable name

    call me%matrix_info(to_unicode(path),is_matrix,found,var_type,n_sets,set_size,name)

    end subroutine wrap_json_file_variable_matrix_info
!*****************************************************************************************

!*****************************************************************************************
!> author: Izaak Beekman
!  date: 7/23/2015
!
!  Get a [[json_value]] pointer to the JSON file root.
!
!@note This is equivalent to calling ```[[json_file]]%get('$',p)```

    subroutine json_file_get_root(me,p)

    implicit none

    class(json_file),intent(inout)       :: me
    type(json_value),pointer,intent(out) :: p      !! pointer to the variable

    p => me%p

    end subroutine json_file_get_root
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Assignment operator for [[json_core(type)]] = [[json_core(type)]].
!  This will duplicate the [[json_core(type)]] and also
!  perform a deep copy of the [[json_value(type)]] data structure.

    subroutine assign_json_file(me,f)

    implicit none

    class(json_file),intent(out) :: me
    type(json_file),intent(in)   :: f

    me%core = f%core ! no pointers here so OK to copy
    call me%core%clone(f%p,me%p)

    end subroutine assign_json_file
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Assignment operator for character = [[json_core(type)]].
!  This is just a wrapper for the [[json_value_to_string]] routine.
!
!### Note
!  * If an exception is raised or the file contains no data,
!    this will return an empty string.

    subroutine assign_json_file_to_string(str,me)

    implicit none

    character(kind=CK,len=:),allocatable,intent(out) :: str
    class(json_file),intent(in) :: me

    type(json_core) :: core_copy !! a copy of `core` from `me`

    if (me%core%failed() .or. .not. associated(me%p)) then
        str = CK_''
    else

        ! This is sort of a hack. Since `me` has to have `intent(in)`
        ! for the assignment to work, we need to make a copy of `me%core`
        ! so we can call the low level routine (since it needs it to
        ! be `intent(inout)`) because it's possible for this
        ! function to raise an exception.

        core_copy = me%core ! copy the parser settings

        call core_copy%serialize(me%p,str)
        if (me%core%failed()) str = CK_''

    end if

    end subroutine assign_json_file_to_string
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Assignment operator for [[json_core(type)]] = character.
!  This is just a wrapper for the [[json_file_load_from_string]] routine.

    subroutine assign_string_to_json_file(me,str)

    implicit none

    class(json_file),intent(inout) :: me
    character(kind=CK,len=*),intent(in) :: str

    if (associated(me%p)) call me%destroy()
    if (me%core%failed()) call me%core%clear_exceptions()
    call me%deserialize(str)

    end subroutine assign_string_to_json_file
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Alternate version of [[assign_string_to_json_file]], where "str" is kind=CDK.

    subroutine wrap_assign_string_to_json_file(me,str)

    implicit none

    class(json_file),intent(inout) :: me
    character(kind=CDK,len=*),intent(in) :: str

    call me%assign_string_to_json_file(to_unicode(str))

    end subroutine wrap_assign_string_to_json_file
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  A wrapper for [[json_file_valid_path]] for the `.in.` operator

    function json_file_valid_path_op(path,me) result(found)

    implicit none

    character(kind=CK,len=*),intent(in) :: path   !! the path to the variable
    class(json_file),intent(in)         :: me     !! the JSON file
    logical(LK)                         :: found  !! if the variable was found

    type(json_core) :: core_copy !! a copy of `core` from `me`

    ! This is sort of a hack. Since `me` has to have `intent(in)`
    ! for the operator to work, we need to make a copy of `me%core`
    ! so we can call the low level routine (since it needs it to
    ! be `intent(inout)`) because it's technically possible for this
    ! function to raise an exception. This normally should never
    ! happen here unless the JSON structure is malformed.

    core_copy = me%core ! copy the settings (need them to know
                        ! how to interpret the path)

    found = core_copy%valid_path(me%p, path) ! call the low-level routine

    call core_copy%destroy() ! just in case (but not really necessary)

    end function json_file_valid_path_op
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Alternate version of [[json_file_valid_path_op]], where "path" is kind=CDK.

    function wrap_json_file_valid_path_op(path,me) result(found)

    implicit none

    character(kind=CDK,len=*),intent(in) :: path   !! the path to the variable
    class(json_file),intent(in)          :: me     !! the JSON file
    logical(LK)                          :: found  !! if the variable was found

    found = to_unicode(path) .in. me

    end function wrap_json_file_valid_path_op
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Returns true if the `path` is present in the JSON file.

    function json_file_valid_path(me,path) result(found)

    implicit none

    class(json_file),intent(inout)      :: me
    character(kind=CK,len=*),intent(in) :: path   !! the path to the variable
    logical(LK)                         :: found  !! if the variable was found

    found = me%core%valid_path(me%p, path)

    end function json_file_valid_path
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Alternate version of [[json_file_valid_path]], where "path" is kind=CDK.

    function wrap_json_file_valid_path(me,path) result(found)

    implicit none

    class(json_file),intent(inout)       :: me
    character(kind=CDK,len=*),intent(in) :: path   !! the path to the variable
    logical(LK)                          :: found  !! if the variable was found

    found = me%valid_path(to_unicode(path))

    end function wrap_json_file_valid_path
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Rename a variable in a JSON file.

    subroutine json_file_rename(me,path,name,found)

    implicit none

    class(json_file),intent(inout)      :: me
    character(kind=CK,len=*),intent(in) :: path   !! the path to the variable
    character(kind=CK,len=*),intent(in) :: name   !! the new name
    logical(LK),intent(out),optional    :: found  !! if the variable was found

    call me%core%rename(me%p, path, name, found)

    end subroutine json_file_rename
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Alternate version of [[json_file_rename]], where "path" and "name" are kind=CDK.

    subroutine wrap_json_file_rename(me,path,name,found)

    implicit none

    class(json_file),intent(inout)       :: me
    character(kind=CDK,len=*),intent(in) :: path   !! the path to the variable
    character(kind=CDK,len=*),intent(in) :: name   !! the new name
    logical(LK),intent(out),optional     :: found  !! if the variable was found

    call me%json_file_rename(to_unicode(path),to_unicode(name),found)

    end subroutine wrap_json_file_rename
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Wrapper for [[json_file_rename]] where "path" is kind=CDK).

    subroutine json_file_rename_path_ascii(me,path,name,found)

    implicit none

    class(json_file),intent(inout)       :: me
    character(kind=CDK,len=*),intent(in) :: path   !! the path to the variable
    character(kind=CK,len=*),intent(in)  :: name   !! the new name
    logical(LK),intent(out),optional     :: found  !! if the variable was found

    call me%json_file_rename(to_unicode(path),name,found)

    end subroutine json_file_rename_path_ascii
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Wrapper for [[json_file_rename]] where "name" is kind=CDK).

    subroutine json_file_rename_name_ascii(me,path,name,found)

    implicit none

    class(json_file),intent(inout)       :: me
    character(kind=CK,len=*),intent(in)  :: path   !! the path to the variable
    character(kind=CDK,len=*),intent(in) :: name   !! the new name
    logical(LK),intent(out),optional     :: found  !! if the variable was found

    call me%json_file_rename(path,to_unicode(name),found)

    end subroutine json_file_rename_name_ascii
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 2/3/2014
!
!  Get a [[json_value]] pointer to an object from a JSON file.

    subroutine json_file_get_object(me, path, p, found)

    implicit none

    class(json_file),intent(inout)       :: me
    character(kind=CK,len=*),intent(in)  :: path   !! the path to the variable
    type(json_value),pointer,intent(out) :: p      !! pointer to the variable
    logical(LK),intent(out),optional     :: found  !! if it was really found

    call me%core%get(me%p, path, p, found)

    end subroutine json_file_get_object
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_file_get_object]], where "path" is kind=CDK.

    subroutine wrap_json_file_get_object(me, path, p, found)

    implicit none

    class(json_file),intent(inout)       :: me
    character(kind=CDK,len=*),intent(in) :: path    !! the path to the variable
    type(json_value),pointer,intent(out) :: p       !! pointer to the variable
    logical(LK),intent(out),optional     :: found   !! if it was really found

    call me%get(to_unicode(path), p, found)

    end subroutine wrap_json_file_get_object
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 12/9/2013
!
!  Get an integer value from a JSON file.

    subroutine json_file_get_integer(me, path, val, found, default)

    implicit none

    class(json_file),intent(inout)      :: me
    character(kind=CK,len=*),intent(in) :: path    !! the path to the variable
    integer(IK),intent(out)             :: val     !! value
    logical(LK),intent(out),optional    :: found   !! if it was really found
    integer(IK),intent(in),optional     :: default

    call me%core%get(me%p, path, val, found, default)

    end subroutine json_file_get_integer
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_file_get_integer]], where "path" is kind=CDK.

    subroutine wrap_json_file_get_integer(me, path, val, found, default)

    implicit none

    class(json_file),intent(inout)       :: me
    character(kind=CDK,len=*),intent(in) :: path   !! the path to the variable
    integer(IK),intent(out)              :: val    !! value
    logical(LK),intent(out),optional     :: found  !! if it was really found
    integer(IK),intent(in),optional      :: default

    call me%get(to_unicode(path), val, found, default)

    end subroutine wrap_json_file_get_integer
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/20/2014
!
!  Get an integer vector from a JSON file.

    subroutine json_file_get_integer_vec(me, path, vec, found, default)

    implicit none

    class(json_file),intent(inout)                   :: me
    character(kind=CK,len=*),intent(in)              :: path   !! the path to the variable
    integer(IK),dimension(:),allocatable,intent(out) :: vec    !! the value vector
    logical(LK),intent(out),optional                 :: found  !! if it was really found
    integer(IK),dimension(:),intent(in),optional     :: default

    call me%core%get(me%p, path, vec, found, default)

    end subroutine json_file_get_integer_vec
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_file_get_integer_vec]], where "path" is kind=CDK.

    subroutine wrap_json_file_get_integer_vec(me, path, vec, found, default)

    implicit none

    class(json_file),intent(inout)                   :: me
    character(kind=CDK,len=*),intent(in)             :: path  !! the path to the variable
    integer(IK),dimension(:),allocatable,intent(out) :: vec   !! the value vector
    logical(LK),intent(out),optional                 :: found !! if it was really found
    integer(IK),dimension(:),intent(in),optional     :: default

    call me%get(to_unicode(path), vec, found, default)

    end subroutine wrap_json_file_get_integer_vec
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 12/9/2013
!
!  Get a real(RK) variable value from a JSON file.

    subroutine json_file_get_real (me, path, val, found, default)

    implicit none

    class(json_file),intent(inout)      :: me
    character(kind=CK,len=*),intent(in) :: path  !! the path to the variable
    real(RK),intent(out)                :: val   !! value
    logical(LK),intent(out),optional    :: found !! if it was really found
    real(RK),intent(in),optional        :: default

    call me%core%get(me%p, path, val, found, default)

    end subroutine json_file_get_real
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_file_get_real]], where "path" is kind=CDK.

    subroutine wrap_json_file_get_real (me, path, val, found, default)

    implicit none

    class(json_file),intent(inout)       :: me
    character(kind=CDK,len=*),intent(in) :: path  !! the path to the variable
    real(RK),intent(out)                 :: val   !! value
    logical(LK),intent(out),optional     :: found !! if it was really found
    real(RK),intent(in),optional         :: default

    call me%get(to_unicode(path), val, found, default)

    end subroutine wrap_json_file_get_real
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/19/2014
!
!  Get a real(RK) vector from a JSON file.

    subroutine json_file_get_real_vec(me, path, vec, found, default)

    implicit none

    class(json_file),intent(inout)                :: me
    character(kind=CK,len=*),intent(in)           :: path  !! the path to the variable
    real(RK),dimension(:),allocatable,intent(out) :: vec   !! the value vector
    logical(LK),intent(out),optional              :: found !! if it was really found
    real(RK),dimension(:),intent(in),optional     :: default

    call me%core%get(me%p, path, vec, found, default)

    end subroutine json_file_get_real_vec
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_file_get_real_vec]], where "path" is kind=CDK.

    subroutine wrap_json_file_get_real_vec(me, path, vec, found, default)

    implicit none

    class(json_file),intent(inout)                :: me
    character(kind=CDK,len=*),intent(in)          :: path  !! the path to the variable
    real(RK),dimension(:),allocatable,intent(out) :: vec   !! the value vector
    logical(LK),intent(out),optional              :: found !! if it was really found
    real(RK),dimension(:),intent(in),optional     :: default

    call me%get(to_unicode(path), vec, found, default)

    end subroutine wrap_json_file_get_real_vec
!*****************************************************************************************

#ifndef REAL32
!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/21/2019
!
!  Alternate version of [[json_file_get_real]] where `val` is `real32`.

    subroutine json_file_get_real32 (me, path, val, found, default)

    implicit none

    class(json_file),intent(inout)      :: me
    character(kind=CK,len=*),intent(in) :: path  !! the path to the variable
    real(real32),intent(out)            :: val   !! value
    logical(LK),intent(out),optional    :: found !! if it was really found
    real(real32),intent(in),optional    :: default

    call me%core%get(me%p, path, val, found, default)

    end subroutine json_file_get_real32
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_file_get_real32]], where "path" is kind=CDK.

    subroutine wrap_json_file_get_real32 (me, path, val, found, default)

    implicit none

    class(json_file),intent(inout)       :: me
    character(kind=CDK,len=*),intent(in) :: path  !! the path to the variable
    real(real32),intent(out)             :: val   !! value
    logical(LK),intent(out),optional     :: found !! if it was really found
    real(real32),intent(in),optional     :: default

    call me%get(to_unicode(path), val, found, default)

    end subroutine wrap_json_file_get_real32
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/21/2019
!
!  Alternate version of [[json_file_get_real_vec]] where `vec` is `real32`.

    subroutine json_file_get_real32_vec(me, path, vec, found, default)

    implicit none

    class(json_file),intent(inout)                    :: me
    character(kind=CK,len=*),intent(in)               :: path  !! the path to the variable
    real(real32),dimension(:),allocatable,intent(out) :: vec   !! the value vector
    logical(LK),intent(out),optional                  :: found !! if it was really found
    real(real32),dimension(:),intent(in),optional     :: default

    call me%core%get(me%p, path, vec, found, default)

    end subroutine json_file_get_real32_vec
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_file_get_real32_vec]], where "path" is kind=CDK.

    subroutine wrap_json_file_get_real32_vec(me, path, vec, found, default)

    implicit none

    class(json_file),intent(inout)                    :: me
    character(kind=CDK,len=*),intent(in)              :: path  !! the path to the variable
    real(real32),dimension(:),allocatable,intent(out) :: vec   !! the value vector
    logical(LK),intent(out),optional                  :: found !! if it was really found
    real(real32),dimension(:),intent(in),optional     :: default

    call me%get(to_unicode(path), vec, found, default)

    end subroutine wrap_json_file_get_real32_vec
!*****************************************************************************************
#endif

#ifdef REAL128
!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/21/2019
!
!  Alternate version of [[json_file_get_real]] where `val` is `real64`.

    subroutine json_file_get_real64 (me, path, val, found, default)

    implicit none

    class(json_file),intent(inout)      :: me
    character(kind=CK,len=*),intent(in) :: path  !! the path to the variable
    real(real64),intent(out)            :: val   !! value
    logical(LK),intent(out),optional    :: found !! if it was really found
    real(real64),intent(in),optional    :: default

    call me%core%get(me%p, path, val, found, default)

    end subroutine json_file_get_real64
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_file_get_real64]], where "path" is kind=CDK.

    subroutine wrap_json_file_get_real64 (me, path, val, found, default)

    implicit none

    class(json_file),intent(inout)       :: me
    character(kind=CDK,len=*),intent(in) :: path  !! the path to the variable
    real(real64),intent(out)             :: val   !! value
    logical(LK),intent(out),optional     :: found !! if it was really found
    real(real64),intent(in),optional     :: default

    call me%get(to_unicode(path), val, found, default)

    end subroutine wrap_json_file_get_real64
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/21/2019
!
!  Alternate version of [[json_file_get_real_vec]] where `vec` is `real64`.

    subroutine json_file_get_real64_vec(me, path, vec, found, default)

    implicit none

    class(json_file),intent(inout)                    :: me
    character(kind=CK,len=*),intent(in)               :: path  !! the path to the variable
    real(real64),dimension(:),allocatable,intent(out) :: vec   !! the value vector
    logical(LK),intent(out),optional                  :: found !! if it was really found
    real(real64),dimension(:),intent(in),optional     :: default

    call me%core%get(me%p, path, vec, found, default)

    end subroutine json_file_get_real64_vec
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_file_get_real64_vec]], where "path" is kind=CDK.

    subroutine wrap_json_file_get_real64_vec(me, path, vec, found, default)

    implicit none

    class(json_file),intent(inout)                    :: me
    character(kind=CDK,len=*),intent(in)              :: path  !! the path to the variable
    real(real64),dimension(:),allocatable,intent(out) :: vec   !! the value vector
    logical(LK),intent(out),optional                  :: found !! if it was really found
    real(real64),dimension(:),intent(in),optional     :: default

    call me%get(to_unicode(path), vec, found, default)

    end subroutine wrap_json_file_get_real64_vec
!*****************************************************************************************
#endif

!*****************************************************************************************
!> author: Jacob Williams
!  date: 12/9/2013
!
!  Get a logical(LK) value from a JSON file.

    subroutine json_file_get_logical(me,path,val,found,default)

    implicit none

    class(json_file),intent(inout)       :: me
    character(kind=CK,len=*),intent(in)  :: path   !! the path to the variable
    logical(LK),intent(out)              :: val    !! value
    logical(LK),intent(out),optional     :: found  !! if it was really found
    logical(LK),intent(in),optional      :: default

    call me%core%get(me%p, path, val, found, default)

    end subroutine json_file_get_logical
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_file_get_logical]], where "path" is kind=CDK.

    subroutine wrap_json_file_get_logical(me,path,val,found,default)

    implicit none

    class(json_file),intent(inout)       :: me
    character(kind=CDK,len=*),intent(in) :: path   !! the path to the variable
    logical(LK),intent(out)              :: val    !! value
    logical(LK),intent(out),optional     :: found  !! if it was really found
    logical(LK),intent(in),optional      :: default

    call me%get(to_unicode(path), val, found, default)

    end subroutine wrap_json_file_get_logical
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/20/2014
!
!  Get a logical(LK) vector from a JSON file.

    subroutine json_file_get_logical_vec(me, path, vec, found, default)

    implicit none

    class(json_file),intent(inout)                   :: me
    character(kind=CK,len=*),intent(in)              :: path  !! the path to the variable
    logical(LK),dimension(:),allocatable,intent(out) :: vec   !! the value vector
    logical(LK),intent(out),optional                 :: found !! if it was really found
    logical(LK),dimension(:),intent(in),optional     :: default

    call me%core%get(me%p, path, vec, found, default)

    end subroutine json_file_get_logical_vec
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_file_get_logical_vec]], where "path" is kind=CDK.

    subroutine wrap_json_file_get_logical_vec(me, path, vec, found, default)

    implicit none

    class(json_file),intent(inout)                   :: me
    character(kind=CDK,len=*),intent(in)             :: path  !! the path to the variable
    logical(LK),dimension(:),allocatable,intent(out) :: vec   !! the value vector
    logical(LK),intent(out),optional                 :: found !! if it was really found
    logical(LK),dimension(:),intent(in),optional     :: default

    call me%get(to_unicode(path), vec, found, default)

    end subroutine wrap_json_file_get_logical_vec
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 12/9/2013
!
!  Get a character string from a json file.
!  The output val is an allocatable character string.

    subroutine json_file_get_string(me, path, val, found, default)

    implicit none

    class(json_file),intent(inout)                   :: me
    character(kind=CK,len=*),intent(in)              :: path  !! the path to the variable
    character(kind=CK,len=:),allocatable,intent(out) :: val   !! value
    logical(LK),intent(out),optional                 :: found !! if it was really found
    character(kind=CK,len=*),intent(in),optional     :: default

    call me%core%get(me%p, path, val, found, default)

    end subroutine json_file_get_string
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_file_get_string]], where "path" is kind=CDK.

    subroutine wrap_json_file_get_string(me, path, val, found, default)

    implicit none

    class(json_file),intent(inout)                   :: me
    character(kind=CDK,len=*),intent(in)             :: path  !! the path to the variable
    character(kind=CK,len=:),allocatable,intent(out) :: val   !! value
    logical(LK),intent(out),optional                 :: found !! if it was really found
    character(kind=CK,len=*),intent(in),optional     :: default

    call me%get(to_unicode(path), val, found, default)

    end subroutine wrap_json_file_get_string
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/19/2014
!
!  Get a string vector from a JSON file.

    subroutine json_file_get_string_vec(me, path, vec, found, default)

    implicit none

    class(json_file),intent(inout)                                :: me
    character(kind=CK,len=*),intent(in)                           :: path  !! the path to the variable
    character(kind=CK,len=*),dimension(:),allocatable,intent(out) :: vec   !! value vector
    logical(LK),intent(out),optional                              :: found !! if it was really found
    character(kind=CK,len=*),dimension(:),intent(in),optional     :: default

    call me%core%get(me%p, path, vec, found, default)

    end subroutine json_file_get_string_vec
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_file_get_string_vec]], where "path" is kind=CDK.

    subroutine wrap_json_file_get_string_vec(me, path, vec, found, default)

    implicit none

    class(json_file),intent(inout)                                :: me
    character(kind=CDK,len=*),intent(in)                          :: path  !! the path to the variable
    character(kind=CK,len=*),dimension(:),allocatable,intent(out) :: vec   !! value vector
    logical(LK),intent(out),optional                              :: found !! if it was really found
    character(kind=CK,len=*),dimension(:),intent(in),optional     :: default

    call me%get(to_unicode(path), vec, found, default)

    end subroutine wrap_json_file_get_string_vec
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 12/17/2016
!
!  Get an (allocatable length) string vector from a JSON file.
!  This is just a wrapper for [[json_get_alloc_string_vec_by_path]].

    subroutine json_file_get_alloc_string_vec(me, path, vec, ilen, found, default, default_ilen)

    implicit none

    class(json_file),intent(inout)      :: me
    character(kind=CK,len=*),intent(in) :: path !! the path to the variable
    character(kind=CK,len=:),dimension(:),allocatable,intent(out) :: vec !! value vector
    integer(IK),dimension(:),allocatable,intent(out) :: ilen !! the actual length
                                                             !! of each character
                                                             !! string in the array
    logical(LK),intent(out),optional :: found
    character(kind=CK,len=*),dimension(:),intent(in),optional :: default
    integer(IK),dimension(:),intent(in),optional :: default_ilen !! the actual
                                                                 !! length of `default`

    call me%core%get(me%p, path, vec, ilen, found, default, default_ilen)

    end subroutine json_file_get_alloc_string_vec
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_file_get_alloc_string_vec]], where "path" is kind=CDK.
!  This is just a wrapper for [[wrap_json_get_alloc_string_vec_by_path]].

    subroutine wrap_json_file_get_alloc_string_vec(me, path, vec, ilen, found, default, default_ilen)

    implicit none

    class(json_file),intent(inout)       :: me
    character(kind=CDK,len=*),intent(in) :: path !! the path to the variable
    character(kind=CK,len=:),dimension(:),allocatable,intent(out) :: vec  !! value vector
    integer(IK),dimension(:),allocatable,intent(out) :: ilen !! the actual length
                                                             !! of each character
                                                             !! string in the array
    logical(LK),intent(out),optional :: found
    character(kind=CK,len=*),dimension(:),intent(in),optional :: default
    integer(IK),dimension(:),intent(in),optional :: default_ilen !! the actual
                                                                 !! length of `default`

    call me%get(to_unicode(path), vec, ilen, found, default, default_ilen)

    end subroutine wrap_json_file_get_alloc_string_vec
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Add a [[json_value]] pointer as the root object to a JSON file.
!
!### Note
!
!  This is mostly equivalent to:
!```fortran
!    f = [[json_file]](p)
!```
!  But without the finalization calls.
!
!  And:
!```fortran
!    if (destroy_original) call [[json_file]]%destroy()
!    call [[json_file]]%add('$',p)
!```

    subroutine json_file_add(me,p,destroy_original)

    implicit none

    class(json_file),intent(inout)       :: me
    type(json_value),pointer,intent(in)  :: p    !! pointer to the variable to add
    logical(LK),intent(in),optional      :: destroy_original !! if the file currently contains
                                                             !! an associated pointer, it is
                                                             !! destroyed. [Default is True]

    logical(LK) :: destroy   !! if `me%p` is to be destroyed

    if (present(destroy_original)) then
        destroy = destroy_original
    else
        destroy = .true. ! default
    end if

    if (destroy) call me%core%destroy(me%p)

    me%p => p

    end subroutine json_file_add
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Add a [[json_value]] pointer to an object to a JSON file.

    subroutine json_file_add_object(me,path,p,found,was_created)

    implicit none

    class(json_file),intent(inout)       :: me
    character(kind=CK,len=*),intent(in)  :: path         !! the path to the variable
    type(json_value),pointer,intent(in)  :: p            !! pointer to the variable to add
    logical(LK),intent(out),optional     :: found        !! if the variable was found
    logical(LK),intent(out),optional     :: was_created  !! if the variable had to be created

    if (.not. associated(me%p)) call me%core%create_object(me%p,ck_'') ! create root

    call me%core%add_by_path(me%p,path,p,found,was_created)

    end subroutine json_file_add_object
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Alternate version of [[json_file_add_object]], where "path" is kind=CDK.

    subroutine wrap_json_file_add_object(me,path,p,found,was_created)

    implicit none

    class(json_file),intent(inout)       :: me
    character(kind=CDK,len=*),intent(in) :: path         !! the path to the variable
    type(json_value),pointer,intent(in)  :: p            !! pointer to the variable to add
    logical(LK),intent(out),optional     :: found        !! if the variable was found
    logical(LK),intent(out),optional     :: was_created  !! if the variable had to be created

    call me%json_file_add_object(to_unicode(path),p,found,was_created)

    end subroutine wrap_json_file_add_object
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Add an integer value to a JSON file.

    subroutine json_file_add_integer(me,path,val,found,was_created)

    implicit none

    class(json_file),intent(inout)      :: me
    character(kind=CK,len=*),intent(in) :: path         !! the path to the variable
    integer(IK),intent(in)              :: val          !! value
    logical(LK),intent(out),optional    :: found        !! if the variable was found
    logical(LK),intent(out),optional    :: was_created  !! if the variable had to be created

    if (.not. associated(me%p)) call me%core%create_object(me%p,ck_'') ! create root

    call me%core%add_by_path(me%p,path,val,found,was_created)

    end subroutine json_file_add_integer
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Alternate version of [[json_file_add_integer]], where "path" is kind=CDK.

    subroutine wrap_json_file_add_integer(me,path,val,found,was_created)

    implicit none

    class(json_file),intent(inout)       :: me
    character(kind=CDK,len=*),intent(in) :: path         !! the path to the variable
    integer(IK),intent(in)               :: val          !! value
    logical(LK),intent(out),optional     :: found        !! if the variable was found
    logical(LK),intent(out),optional     :: was_created  !! if the variable had to be created

    call me%json_file_add_integer(to_unicode(path),val,found,was_created)

    end subroutine wrap_json_file_add_integer
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Add an integer vector to a JSON file.

    subroutine json_file_add_integer_vec(me,path,vec,found,was_created)

    implicit none

    class(json_file),intent(inout)       :: me
    character(kind=CK,len=*),intent(in)  :: path         !! the path to the variable
    integer(IK),dimension(:),intent(in)  :: vec          !! the value vector
    logical(LK),intent(out),optional     :: found        !! if the variable was found
    logical(LK),intent(out),optional     :: was_created  !! if the variable had to be created

    if (.not. associated(me%p)) call me%core%create_object(me%p,ck_'') ! create root

    call me%core%add_by_path(me%p,path,vec,found,was_created)

    end subroutine json_file_add_integer_vec
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Alternate version of [[json_file_add_integer_vec]], where "path" is kind=CDK.

    subroutine wrap_json_file_add_integer_vec(me,path,vec,found,was_created)

    implicit none

    class(json_file),intent(inout)       :: me
    character(kind=CDK,len=*),intent(in) :: path         !! the path to the variable
    integer(IK),dimension(:),intent(in)  :: vec          !! the value vector
    logical(LK),intent(out),optional     :: found        !! if the variable was found
    logical(LK),intent(out),optional     :: was_created  !! if the variable had to be created

    call me%json_file_add_integer_vec(to_unicode(path),vec,found,was_created)

    end subroutine wrap_json_file_add_integer_vec
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Add a real(RK) variable value to a JSON file.

    subroutine json_file_add_real(me,path,val,found,was_created)

    implicit none

    class(json_file),intent(inout)      :: me
    character(kind=CK,len=*),intent(in) :: path         !! the path to the variable
    real(RK),intent(in)                 :: val          !! value
    logical(LK),intent(out),optional    :: found        !! if the variable was found
    logical(LK),intent(out),optional    :: was_created  !! if the variable had to be created

    if (.not. associated(me%p)) call me%core%create_object(me%p,ck_'') ! create root

    call me%core%add_by_path(me%p,path,val,found,was_created)

    end subroutine json_file_add_real
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Alternate version of [[json_file_add_real]], where "path" is kind=CDK.

    subroutine wrap_json_file_add_real(me,path,val,found,was_created)

    implicit none

    class(json_file),intent(inout)       :: me
    character(kind=CDK,len=*),intent(in) :: path         !! the path to the variable
    real(RK),intent(in)                  :: val          !! value
    logical(LK),intent(out),optional     :: found        !! if the variable was found
    logical(LK),intent(out),optional     :: was_created  !! if the variable had to be created

    call me%json_file_add_real(to_unicode(path),val,found,was_created)

    end subroutine wrap_json_file_add_real
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Add a real(RK) vector to a JSON file.

    subroutine json_file_add_real_vec(me,path,vec,found,was_created)

    implicit none

    class(json_file),intent(inout)       :: me
    character(kind=CK,len=*),intent(in)  :: path         !! the path to the variable
    real(RK),dimension(:),intent(in)     :: vec          !! the value vector
    logical(LK),intent(out),optional     :: found        !! if the variable was found
    logical(LK),intent(out),optional     :: was_created  !! if the variable had to be created

    if (.not. associated(me%p)) call me%core%create_object(me%p,ck_'') ! create root

    call me%core%add_by_path(me%p,path,vec,found,was_created)

    end subroutine json_file_add_real_vec
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Alternate version of [[json_file_add_real_vec]], where "path" is kind=CDK.

    subroutine wrap_json_file_add_real_vec(me,path,vec,found,was_created)

    implicit none

    class(json_file),intent(inout)       :: me
    character(kind=CDK,len=*),intent(in) :: path         !! the path to the variable
    real(RK),dimension(:),intent(in)     :: vec          !! the value vector
    logical(LK),intent(out),optional     :: found        !! if the variable was found
    logical(LK),intent(out),optional     :: was_created  !! if the variable had to be created

    call me%json_file_add_real_vec(to_unicode(path),vec,found,was_created)

    end subroutine wrap_json_file_add_real_vec
!*****************************************************************************************

#ifndef REAL32
!*****************************************************************************************
!> author: Jacob Williams
!
!  Alternate version of [[json_file_add_real]] where `val` is `real32`.

    subroutine json_file_add_real32(me,path,val,found,was_created)

    implicit none

    class(json_file),intent(inout)      :: me
    character(kind=CK,len=*),intent(in) :: path         !! the path to the variable
    real(real32),intent(in)             :: val          !! value
    logical(LK),intent(out),optional    :: found        !! if the variable was found
    logical(LK),intent(out),optional    :: was_created  !! if the variable had to be created

    call me%core%add_by_path(me%p,path,val,found,was_created)

    end subroutine json_file_add_real32
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Alternate version of [[json_file_add_real32]], where "path" is kind=CDK.

    subroutine wrap_json_file_add_real32(me,path,val,found,was_created)

    implicit none

    class(json_file),intent(inout)       :: me
    character(kind=CDK,len=*),intent(in) :: path         !! the path to the variable
    real(real32),intent(in)              :: val          !! value
    logical(LK),intent(out),optional     :: found        !! if the variable was found
    logical(LK),intent(out),optional     :: was_created  !! if the variable had to be created

    call me%json_file_add_real32(to_unicode(path),val,found,was_created)

    end subroutine wrap_json_file_add_real32
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Alternate version of [[json_file_add_real_vec]] where `vec` is `real32`.

    subroutine json_file_add_real32_vec(me,path,vec,found,was_created)

    implicit none

    class(json_file),intent(inout)       :: me
    character(kind=CK,len=*),intent(in)  :: path         !! the path to the variable
    real(real32),dimension(:),intent(in) :: vec          !! the value vector
    logical(LK),intent(out),optional     :: found        !! if the variable was found
    logical(LK),intent(out),optional     :: was_created  !! if the variable had to be created

    call me%core%add_by_path(me%p,path,vec,found,was_created)

    end subroutine json_file_add_real32_vec
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Alternate version of [[json_file_add_real32_vec]], where "path" is kind=CDK.

    subroutine wrap_json_file_add_real32_vec(me,path,vec,found,was_created)

    implicit none

    class(json_file),intent(inout)       :: me
    character(kind=CDK,len=*),intent(in) :: path         !! the path to the variable
    real(real32),dimension(:),intent(in) :: vec          !! the value vector
    logical(LK),intent(out),optional     :: found        !! if the variable was found
    logical(LK),intent(out),optional     :: was_created  !! if the variable had to be created

    call me%json_file_add_real32_vec(to_unicode(path),vec,found,was_created)

    end subroutine wrap_json_file_add_real32_vec
!*****************************************************************************************
#endif

#ifdef REAL128
!*****************************************************************************************
!> author: Jacob Williams
!
!  Alternate version of [[json_file_add_real]] where `val` is `real64`.

    subroutine json_file_add_real64(me,path,val,found,was_created)

    implicit none

    class(json_file),intent(inout)      :: me
    character(kind=CK,len=*),intent(in) :: path         !! the path to the variable
    real(real64),intent(in)             :: val          !! value
    logical(LK),intent(out),optional    :: found        !! if the variable was found
    logical(LK),intent(out),optional    :: was_created  !! if the variable had to be created

    call me%core%add_by_path(me%p,path,val,found,was_created)

    end subroutine json_file_add_real64
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Alternate version of [[json_file_add_real64]], where "path" is kind=CDK.

    subroutine wrap_json_file_add_real64(me,path,val,found,was_created)

    implicit none

    class(json_file),intent(inout)       :: me
    character(kind=CDK,len=*),intent(in) :: path         !! the path to the variable
    real(real64),intent(in)              :: val          !! value
    logical(LK),intent(out),optional     :: found        !! if the variable was found
    logical(LK),intent(out),optional     :: was_created  !! if the variable had to be created

    call me%json_file_add_real64(to_unicode(path),val,found,was_created)

    end subroutine wrap_json_file_add_real64
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Alternate version of [[json_file_add_real_vec]] where `vec` is `real64`.

    subroutine json_file_add_real64_vec(me,path,vec,found,was_created)

    implicit none

    class(json_file),intent(inout)       :: me
    character(kind=CK,len=*),intent(in)  :: path         !! the path to the variable
    real(real64),dimension(:),intent(in) :: vec          !! the value vector
    logical(LK),intent(out),optional     :: found        !! if the variable was found
    logical(LK),intent(out),optional     :: was_created  !! if the variable had to be created

    call me%core%add_by_path(me%p,path,vec,found,was_created)

    end subroutine json_file_add_real64_vec
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Alternate version of [[json_file_add_real64_vec]], where "path" is kind=CDK.

    subroutine wrap_json_file_add_real64_vec(me,path,vec,found,was_created)

    implicit none

    class(json_file),intent(inout)       :: me
    character(kind=CDK,len=*),intent(in) :: path         !! the path to the variable
    real(real64),dimension(:),intent(in) :: vec          !! the value vector
    logical(LK),intent(out),optional     :: found        !! if the variable was found
    logical(LK),intent(out),optional     :: was_created  !! if the variable had to be created

    call me%json_file_add_real64_vec(to_unicode(path),vec,found,was_created)

    end subroutine wrap_json_file_add_real64_vec
!*****************************************************************************************
#endif

!*****************************************************************************************
!> author: Jacob Williams
!
!  Add a logical(LK) value to a JSON file.

    subroutine json_file_add_logical(me,path,val,found,was_created)

    implicit none

    class(json_file),intent(inout)       :: me
    character(kind=CK,len=*),intent(in)  :: path         !! the path to the variable
    logical(LK),intent(in)               :: val          !! value
    logical(LK),intent(out),optional     :: found        !! if the variable was found
    logical(LK),intent(out),optional     :: was_created  !! if the variable had to be created

    if (.not. associated(me%p)) call me%core%create_object(me%p,ck_'') ! create root

    call me%core%add_by_path(me%p,path,val,found,was_created)

    end subroutine json_file_add_logical
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Alternate version of [[json_file_add_logical]], where "path" is kind=CDK.

    subroutine wrap_json_file_add_logical(me,path,val,found,was_created)

    implicit none

    class(json_file),intent(inout)       :: me
    character(kind=CDK,len=*),intent(in) :: path         !! the path to the variable
    logical(LK),intent(in)               :: val          !! value
    logical(LK),intent(out),optional     :: found        !! if the variable was found
    logical(LK),intent(out),optional     :: was_created  !! if the variable had to be created

    call me%json_file_add_logical(to_unicode(path),val,found,was_created)

    end subroutine wrap_json_file_add_logical
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Add a logical(LK) vector to a JSON file.

    subroutine json_file_add_logical_vec(me,path,vec,found,was_created)

    implicit none

    class(json_file),intent(inout)       :: me
    character(kind=CK,len=*),intent(in)  :: path         !! the path to the variable
    logical(LK),dimension(:),intent(in)  :: vec          !! the value vector
    logical(LK),intent(out),optional     :: found        !! if the variable was found
    logical(LK),intent(out),optional     :: was_created  !! if the variable had to be created

    if (.not. associated(me%p)) call me%core%create_object(me%p,ck_'') ! create root

    call me%core%add_by_path(me%p,path,vec,found,was_created)

    end subroutine json_file_add_logical_vec
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Alternate version of [[json_file_add_logical_vec]], where "path" is kind=CDK.

    subroutine wrap_json_file_add_logical_vec(me,path,vec,found,was_created)

    implicit none

    class(json_file),intent(inout)       :: me
    character(kind=CDK,len=*),intent(in) :: path         !! the path to the variable
    logical(LK),dimension(:),intent(in)  :: vec          !! the value vector
    logical(LK),intent(out),optional     :: found        !! if the variable was found
    logical(LK),intent(out),optional     :: was_created  !! if the variable had to be created

    call me%json_file_add_logical_vec(to_unicode(path),vec,found,was_created)

    end subroutine wrap_json_file_add_logical_vec
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Add a character string to a json file.

    subroutine json_file_add_string(me,path,val,found,was_created,trim_str,adjustl_str)

    implicit none

    class(json_file),intent(inout)      :: me
    character(kind=CK,len=*),intent(in) :: path         !! the path to the variable
    character(kind=CK,len=*),intent(in) :: val          !! value
    logical(LK),intent(out),optional    :: found        !! if the variable was found
    logical(LK),intent(out),optional    :: was_created  !! if the variable had to be created
    logical(LK),intent(in),optional     :: trim_str     !! if TRIM() should be called for the `val`
    logical(LK),intent(in),optional     :: adjustl_str  !! if ADJUSTL() should be called for the `val`
                                                        !! (note that ADJUSTL is done before TRIM)

    if (.not. associated(me%p)) call me%core%create_object(me%p,ck_'') ! create root

    call me%core%add_by_path(me%p,path,val,found,was_created,trim_str,adjustl_str)

    end subroutine json_file_add_string
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Alternate version of [[json_file_add_string]], where "path" and "val" are kind=CDK.

    subroutine wrap_json_file_add_string(me,path,val,found,was_created,trim_str,adjustl_str)

    implicit none

    class(json_file),intent(inout)       :: me
    character(kind=CDK,len=*),intent(in) :: path         !! the path to the variable
    character(kind=CDK,len=*),intent(in) :: val          !! value
    logical(LK),intent(out),optional     :: found        !! if the variable was found
    logical(LK),intent(out),optional     :: was_created  !! if the variable had to be created
    logical(LK),intent(in),optional      :: trim_str     !! if TRIM() should be called for the `val`
    logical(LK),intent(in),optional      :: adjustl_str  !! if ADJUSTL() should be called for the `val`
                                                         !! (note that ADJUSTL is done before TRIM)

    call me%json_file_add_string(to_unicode(path),to_unicode(val),found,&
                                    was_created,trim_str,adjustl_str)

    end subroutine wrap_json_file_add_string
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Wrapper for [[json_file_add_string]] where "path" is kind=CDK).

    subroutine json_file_add_string_path_ascii(me,path,val,found,&
                                                    was_created,trim_str,adjustl_str)

    implicit none

    class(json_file),intent(inout)       :: me
    character(kind=CDK,len=*),intent(in) :: path         !! the path to the variable
    character(kind=CK,len=*),intent(in)  :: val          !! value
    logical(LK),intent(out),optional     :: found        !! if the variable was found
    logical(LK),intent(out),optional     :: was_created  !! if the variable had to be created
    logical(LK),intent(in),optional      :: trim_str     !! if TRIM() should be called for the `val`
    logical(LK),intent(in),optional      :: adjustl_str  !! if ADJUSTL() should be called for the `val`
                                                         !! (note that ADJUSTL is done before TRIM)

    if (.not. associated(me%p)) call me%core%create_object(me%p,ck_'') ! create root

    call me%json_file_add_string(to_unicode(path),val,found,&
                                    was_created,trim_str,adjustl_str)

    end subroutine json_file_add_string_path_ascii
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Wrapper for [[json_file_add_string]] where "val" is kind=CDK).

    subroutine json_file_add_string_value_ascii(me,path,val,found,&
                                                    was_created,trim_str,adjustl_str)

    implicit none

    class(json_file),intent(inout)       :: me
    character(kind=CK,len=*),intent(in)  :: path         !! the path to the variable
    character(kind=CDK,len=*),intent(in) :: val          !! value
    logical(LK),intent(out),optional     :: found        !! if the variable was found
    logical(LK),intent(out),optional     :: was_created  !! if the variable had to be created
    logical(LK),intent(in),optional      :: trim_str     !! if TRIM() should be called for the `val`
    logical(LK),intent(in),optional      :: adjustl_str  !! if ADJUSTL() should be called for the `val`
                                                         !! (note that ADJUSTL is done before TRIM)

    if (.not. associated(me%p)) call me%core%create_object(me%p,ck_'') ! create root

    call me%json_file_add_string(path,to_unicode(val),found,&
                                    was_created,trim_str,adjustl_str)

    end subroutine json_file_add_string_value_ascii
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Add a string vector to a JSON file.

    subroutine json_file_add_string_vec(me,path,vec,found,&
                                            was_created,ilen,trim_str,adjustl_str)

    implicit none

    class(json_file),intent(inout)                   :: me
    character(kind=CK,len=*),intent(in)              :: path         !! the path to the variable
    character(kind=CK,len=*),dimension(:),intent(in) :: vec          !! the value vector
    logical(LK),intent(out),optional                 :: found        !! if the variable was found
    logical(LK),intent(out),optional                 :: was_created  !! if the variable had to be created
    integer(IK),dimension(:),intent(in),optional     :: ilen         !! the string lengths of each
                                                                     !! element in `value`. If not present,
                                                                     !! the full `len(value)` string is added
                                                                     !! for each element.
    logical(LK),intent(in),optional                  :: trim_str     !! if TRIM() should be called for each element
    logical(LK),intent(in),optional                  :: adjustl_str  !! if ADJUSTL() should be called for each element
                                                                     !! (note that ADJUSTL is done before TRIM)

    if (.not. associated(me%p)) call me%core%create_object(me%p,ck_'') ! create root

    call me%core%add_by_path(me%p,path,vec,found,was_created,ilen,trim_str,adjustl_str)

    end subroutine json_file_add_string_vec
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Alternate version of [[json_file_add_string_vec]], where "path" and "vec" are kind=CDK.

    subroutine wrap_json_file_add_string_vec(me,path,vec,found,&
                                                was_created,ilen,trim_str,adjustl_str)

    implicit none

    class(json_file),intent(inout)                   :: me
    character(kind=CDK,len=*),intent(in)             :: path         !! the path to the variable
    character(kind=CDK,len=*),dimension(:),intent(in):: vec          !! the value vector
    logical(LK),intent(out),optional                 :: found        !! if the variable was found
    logical(LK),intent(out),optional                 :: was_created  !! if the variable had to be created
    integer(IK),dimension(:),intent(in),optional     :: ilen         !! the string lengths of each
                                                                     !! element in `value`. If not present,
                                                                     !! the full `len(value)` string is added
                                                                     !! for each element.
    logical(LK),intent(in),optional                  :: trim_str     !! if TRIM() should be called for each element
    logical(LK),intent(in),optional                  :: adjustl_str  !! if ADJUSTL() should be called for each element
                                                                     !! (note that ADJUSTL is done before TRIM)

    call me%json_file_add_string_vec(to_unicode(path),to_unicode(vec),found,&
                                        was_created,ilen,trim_str,adjustl_str)

    end subroutine wrap_json_file_add_string_vec
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Alternate version of [[json_file_add_string_vec]], where "path" is kind=CDK.

    subroutine json_file_add_string_vec_path_ascii(me,path,vec,found,&
                                                    was_created,ilen,trim_str,adjustl_str)

    implicit none

    class(json_file),intent(inout)                   :: me
    character(kind=CDK,len=*),intent(in)             :: path         !! the path to the variable
    character(kind=CK,len=*),dimension(:),intent(in) :: vec          !! the value vector
    logical(LK),intent(out),optional                 :: found        !! if the variable was found
    logical(LK),intent(out),optional                 :: was_created  !! if the variable had to be created
    integer(IK),dimension(:),intent(in),optional     :: ilen         !! the string lengths of each
                                                                     !! element in `value`. If not present,
                                                                     !! the full `len(value)` string is added
                                                                     !! for each element.
    logical(LK),intent(in),optional                  :: trim_str     !! if TRIM() should be called for each element
    logical(LK),intent(in),optional                  :: adjustl_str  !! if ADJUSTL() should be called for each element
                                                                     !! (note that ADJUSTL is done before TRIM)

    call me%json_file_add_string_vec(to_unicode(path),vec,found,&
                                        was_created,ilen,trim_str,adjustl_str)

    end subroutine json_file_add_string_vec_path_ascii
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Alternate version of [[json_file_add_string_vec]], where "vec" is kind=CDK.

    subroutine json_file_add_string_vec_vec_ascii(me,path,vec,found,&
                                                    was_created,ilen,trim_str,adjustl_str)

    implicit none

    class(json_file),intent(inout)                    :: me
    character(kind=CK,len=*),intent(in)               :: path         !! the path to the variable
    character(kind=CDK,len=*),dimension(:),intent(in) :: vec          !! the value vector
    logical(LK),intent(out),optional                  :: found        !! if the variable was found
    logical(LK),intent(out),optional                  :: was_created  !! if the variable had to be created
    integer(IK),dimension(:),intent(in),optional      :: ilen         !! the string lengths of each
                                                                      !! element in `value`. If not present,
                                                                      !! the full `len(value)` string is added
                                                                      !! for each element.
    logical(LK),intent(in),optional                   :: trim_str     !! if TRIM() should be called for each element
    logical(LK),intent(in),optional                   :: adjustl_str  !! if ADJUSTL() should be called for each element
                                                                      !! (note that ADJUSTL is done before TRIM)

    call me%json_file_add_string_vec(path,to_unicode(vec),found,&
                                        was_created,ilen,trim_str,adjustl_str)

    end subroutine json_file_add_string_vec_vec_ascii
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/10/2015
!
!  Given the path string, if the variable is present in the file,
!  and is a scalar, then update its value.
!  If it is not present, then create it and set its value.
!
!### See also
!  * [[json_update_integer]]

    subroutine json_file_update_integer(me,path,val,found)

    implicit none

    class(json_file),intent(inout)      :: me
    character(kind=CK,len=*),intent(in) :: path
    integer(IK),intent(in)              :: val
    logical(LK),intent(out)             :: found

    if (.not. me%core%failed()) call me%core%update(me%p,path,val,found)

    end subroutine json_file_update_integer
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_file_update_integer]], where "path" is kind=CDK.

    subroutine wrap_json_file_update_integer(me,path,val,found)

    implicit none

    class(json_file),intent(inout)       :: me
    character(kind=CDK,len=*),intent(in) :: path
    integer(IK),intent(in)               :: val
    logical(LK),intent(out)              :: found

    call me%update(to_unicode(path),val,found)

    end subroutine wrap_json_file_update_integer
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/10/2015
!
!  Given the path string, if the variable is present in the file,
!  and is a scalar, then update its value.
!  If it is not present, then create it and set its value.
!
!### See also
!  * [[json_update_logical]]

    subroutine json_file_update_logical(me,path,val,found)

    implicit none

    class(json_file),intent(inout)      :: me
    character(kind=CK,len=*),intent(in) :: path
    logical(LK),intent(in)              :: val
    logical(LK),intent(out)             :: found

    if (.not. me%core%failed()) call me%core%update(me%p,path,val,found)

    end subroutine json_file_update_logical
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_file_update_logical]], where "path" is kind=CDK.

    subroutine wrap_json_file_update_logical(me,path,val,found)

    implicit none

    class(json_file),intent(inout)       :: me
    character(kind=CDK,len=*),intent(in) :: path
    logical(LK),intent(in)               :: val
    logical(LK),intent(out)              :: found

    call me%update(to_unicode(path),val,found)

    end subroutine wrap_json_file_update_logical
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/10/2015
!
!  Given the path string, if the variable is present in the file,
!  and is a scalar, then update its value.
!  If it is not present, then create it and set its value.

    subroutine json_file_update_real(me,path,val,found)

    implicit none

    class(json_file),intent(inout)      :: me
    character(kind=CK,len=*),intent(in) :: path
    real(RK),intent(in)                 :: val
    logical(LK),intent(out)             :: found

    if (.not. me%core%failed()) call me%core%update(me%p,path,val,found)

    end subroutine json_file_update_real
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_file_update_real]], where "path" is kind=CDK.

    subroutine wrap_json_file_update_real(me,path,val,found)

    implicit none

    class(json_file),intent(inout)       :: me
    character(kind=CDK,len=*),intent(in) :: path
    real(RK),intent(in)                  :: val
    logical(LK),intent(out)              :: found

    call me%update(to_unicode(path),val,found)

    end subroutine wrap_json_file_update_real
!*****************************************************************************************

#ifndef REAL32
!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/21/2019
!
!  Alternate version of [[json_file_update_real]] where `val` is `real32`.

    subroutine json_file_update_real32(me,path,val,found)

    implicit none

    class(json_file),intent(inout)      :: me
    character(kind=CK,len=*),intent(in) :: path
    real(real32),intent(in)             :: val
    logical(LK),intent(out)             :: found

    call me%update(path,real(val,RK),found)

    end subroutine json_file_update_real32
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_file_update_real32]], where "path" is kind=CDK.

    subroutine wrap_json_file_update_real32(me,path,val,found)

    implicit none

    class(json_file),intent(inout)       :: me
    character(kind=CDK,len=*),intent(in) :: path
    real(real32),intent(in)              :: val
    logical(LK),intent(out)              :: found

    call me%update(to_unicode(path),val,found)

    end subroutine wrap_json_file_update_real32
!*****************************************************************************************
#endif

#ifdef REAL128
!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/21/2019
!
!  Alternate version of [[json_file_update_real]] where `val` is `real64`.

    subroutine json_file_update_real64(me,path,val,found)

    implicit none

    class(json_file),intent(inout)      :: me
    character(kind=CK,len=*),intent(in) :: path
    real(real64),intent(in)             :: val
    logical(LK),intent(out)             :: found

    call me%update(path,real(val,RK),found)

    end subroutine json_file_update_real64
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_file_update_real64]], where "path" is kind=CDK.

    subroutine wrap_json_file_update_real64(me,path,val,found)

    implicit none

    class(json_file),intent(inout)       :: me
    character(kind=CDK,len=*),intent(in) :: path
    real(real64),intent(in)              :: val
    logical(LK),intent(out)              :: found

    call me%update(to_unicode(path),val,found)

    end subroutine wrap_json_file_update_real64
!*****************************************************************************************
#endif

!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/10/2015
!
!  Given the path string, if the variable is present in the file,
!  and is a scalar, then update its value.
!  If it is not present, then create it and set its value.
!
!### See also
!  * [[json_update_string]]

    subroutine json_file_update_string(me,path,val,found,trim_str,adjustl_str)

    implicit none

    class(json_file),intent(inout)      :: me
    character(kind=CK,len=*),intent(in) :: path
    character(kind=CK,len=*),intent(in) :: val
    logical(LK),intent(out)             :: found
    logical(LK),intent(in),optional     :: trim_str     !! if TRIM() should be called for the `val`
    logical(LK),intent(in),optional     :: adjustl_str  !! if ADJUSTL() should be called for the `val`
                                                        !! (note that ADJUSTL is done before TRIM)

    if (.not. me%core%failed()) call me%core%update(me%p,path,val,found,trim_str,adjustl_str)

    end subroutine json_file_update_string
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_file_update_string]], where "path" and "val" are kind=CDK.

    subroutine wrap_json_file_update_string(me,path,val,found,trim_str,adjustl_str)

    implicit none

    class(json_file),intent(inout)       :: me
    character(kind=CDK,len=*),intent(in) :: path
    character(kind=CDK,len=*),intent(in) :: val
    logical(LK),intent(out)              :: found
    logical(LK),intent(in),optional      :: trim_str     !! if TRIM() should be called for the `val`
    logical(LK),intent(in),optional      :: adjustl_str  !! if ADJUSTL() should be called for the `val`
                                                         !! (note that ADJUSTL is done before TRIM)

    call me%update(to_unicode(path),to_unicode(val),found,trim_str,adjustl_str)

    end subroutine wrap_json_file_update_string
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_file_update_string]], where "path" is kind=CDK.

    subroutine json_file_update_string_name_ascii(me,path,val,found,trim_str,adjustl_str)

    implicit none

    class(json_file),intent(inout)       :: me
    character(kind=CDK,len=*),intent(in) :: path
    character(kind=CK, len=*),intent(in) :: val
    logical(LK),intent(out)              :: found
    logical(LK),intent(in),optional      :: trim_str     !! if TRIM() should be called for the `val`
    logical(LK),intent(in),optional      :: adjustl_str  !! if ADJUSTL() should be called for the `val`
                                                         !! (note that ADJUSTL is done before TRIM)

    call me%update(to_unicode(path),val,found,trim_str,adjustl_str)

    end subroutine json_file_update_string_name_ascii
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_file_update_string]], where "val" is kind=CDK.

    subroutine json_file_update_string_val_ascii(me,path,val,found,trim_str,adjustl_str)

    implicit none

    class(json_file),intent(inout)       :: me
    character(kind=CK, len=*),intent(in) :: path
    character(kind=CDK,len=*),intent(in) :: val
    logical(LK),intent(out)              :: found
    logical(LK),intent(in),optional      :: trim_str     !! if TRIM() should be called for the `val`
    logical(LK),intent(in),optional      :: adjustl_str  !! if ADJUSTL() should be called for the `val`
                                                         !! (note that ADJUSTL is done before TRIM)

    call me%update(path,to_unicode(val),found,trim_str,adjustl_str)

    end subroutine json_file_update_string_val_ascii
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 6/11/2016
!
!  Traverse the JSON structure in the file.
!  This routine calls the user-specified [[json_traverse_callback_func]]
!  for each element of the structure.

    subroutine json_file_traverse(me,traverse_callback)

    implicit none

    class(json_file),intent(inout)         :: me
    procedure(json_traverse_callback_func) :: traverse_callback

    call me%core%traverse(me%p,traverse_callback)

    end subroutine json_file_traverse
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 7/7/2018
!
!  Remove a variable from a JSON file.
!
!@note This is just a wrapper to [[remove_if_present]].

    subroutine json_file_remove(me,path)

    implicit none

    class(json_file),intent(inout)      :: me
    character(kind=CK,len=*),intent(in) :: path !! the path to the variable

    call me%core%remove_if_present(me%p,path)

    end subroutine json_file_remove
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_file_remove]], where "path" is kind=CDK.

    subroutine wrap_json_file_remove(me,path)

    implicit none

    class(json_file),intent(inout)       :: me
    character(kind=CDK,len=*),intent(in) :: path !! the path to the variable

    call me%remove(to_unicode(path))

    end subroutine wrap_json_file_remove
!*****************************************************************************************

!*****************************************************************************************
    end module json_file_module
!*****************************************************************************************
