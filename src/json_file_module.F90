!*****************************************************************************************
!> author: Jacob Williams
!  license: BSD
!
!  Higher-level [[json_file]] interface for the [[json_value]] type.
!
!## License
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
    !  A `json_file` contains only two items: an instance of a [[json_core]],
    !  which use used for all data manipulation, and a [[json_value]],
    !  which is used to construct the linked-list data structure.
    !  Note that most methods in the `json_file` class are simply wrappers
    !  to the lower-level routines in the [[json_value_module]].
    !
    !# Example
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
    !    call json%load_file(filename='myfile.json')
    !    call json%print_file() !print to the console
    !    call json%get('var.i',ival,found)
    !    call json%get('var.r(3)',rval,found)
    !    call json%get('var.c',cval,found)
    !    call json%destroy()
    !    end program test
    !```

    type,public :: json_file

        private

        type(json_core) :: json !! the instance of the [[json_core]] factory used for this file

        type(json_value),pointer :: p => null()  !! the JSON structure read from the file

    contains

        procedure,public :: initialize => initialize_json_core_in_file

        procedure,public :: load_file => json_file_load

        generic,public :: load_from_string => MAYBEWRAP(json_file_load_from_string)

        procedure,public :: destroy => json_file_destroy
        procedure,public :: move    => json_file_move_pointer
        generic,public   :: info    => MAYBEWRAP(json_file_variable_info)

        !error checking:
        procedure,public :: failed => json_file_failed
        procedure,public :: print_error_message => json_file_print_error_message
        procedure,public :: check_for_errors => json_file_check_for_errors
        procedure,public :: clear_exceptions => json_file_clear_exceptions

        procedure,public :: print_to_string => json_file_print_to_string

        generic,public :: print_file => json_file_print_to_console, &
                                        json_file_print_1, &
                                        json_file_print_2

        generic,public :: get => MAYBEWRAP(json_file_get_object),      &
                                 MAYBEWRAP(json_file_get_integer),     &
                                 MAYBEWRAP(json_file_get_double),      &
                                 MAYBEWRAP(json_file_get_logical),     &
                                 MAYBEWRAP(json_file_get_string),      &
                                 MAYBEWRAP(json_file_get_integer_vec), &
                                 MAYBEWRAP(json_file_get_double_vec),  &
                                 MAYBEWRAP(json_file_get_logical_vec), &
                                 MAYBEWRAP(json_file_get_string_vec),  &
                                 json_file_get_root

        generic,public :: update =>  MAYBEWRAP(json_file_update_integer),  &
                                     MAYBEWRAP(json_file_update_logical),  &
                                     MAYBEWRAP(json_file_update_real),     &
                                     MAYBEWRAP(json_file_update_string)
#ifdef USE_UCS4
        generic,public :: update => json_file_update_string_name_ascii, &
                                    json_file_update_string_val_ascii
#endif

        !load from string:
        procedure :: MAYBEWRAP(json_file_load_from_string)

        !git info:
        procedure :: MAYBEWRAP(json_file_variable_info)

        !get:
        procedure :: MAYBEWRAP(json_file_get_object)
        procedure :: MAYBEWRAP(json_file_get_integer)
        procedure :: MAYBEWRAP(json_file_get_double)
        procedure :: MAYBEWRAP(json_file_get_logical)
        procedure :: MAYBEWRAP(json_file_get_string)
        procedure :: MAYBEWRAP(json_file_get_integer_vec)
        procedure :: MAYBEWRAP(json_file_get_double_vec)
        procedure :: MAYBEWRAP(json_file_get_logical_vec)
        procedure :: MAYBEWRAP(json_file_get_string_vec)
        procedure :: json_file_get_root

        !update:
        procedure :: MAYBEWRAP(json_file_update_integer)
        procedure :: MAYBEWRAP(json_file_update_logical)
        procedure :: MAYBEWRAP(json_file_update_real)
        procedure :: MAYBEWRAP(json_file_update_string)
#ifdef USE_UCS4
        procedure :: json_file_update_string_name_ascii
        procedure :: json_file_update_string_val_ascii
#endif

        !print_file:
        procedure :: json_file_print_to_console
        procedure :: json_file_print_1
        procedure :: json_file_print_2

    end type json_file
    !*********************************************************

    !*********************************************************
    !> author: Izaak Beekman
    !  date: 07/23/2015
    !
    !  Structure constructor to initialize a [[json_file(type)]] object
    !  with an existing [[json_value]] object
    !
    !# Example
    !
    !```fortran
    ! ...
    ! type(json_file)  :: my_file
    ! type(json_value) :: json_object
    ! ...
    ! ! Construct a json_object
    ! my_file = json_file(json_object)
    !```
    interface json_file
       module procedure initialize_json_file
    end interface
    !*************************************************************************************

    contains
!*****************************************************************************************

!*****************************************************************************************
!>
!  Check error status in the file.

    pure function json_file_failed(me) result(failed)

    implicit none

    class(json_file),intent(in) :: me
    logical(LK)                 :: failed  !! will be true if there has been an error.

    failed = me%json%failed()

    end function json_file_failed
!*****************************************************************************************

!*****************************************************************************************
!>
!  Retrieve error status and message from the class.

    subroutine json_file_check_for_errors(me,status_ok,error_msg)

    implicit none

    class(json_file),intent(inout) :: me
    logical(LK),intent(out) :: status_ok !! true if there were no errors
    character(kind=CK,len=:),allocatable,intent(out) :: error_msg !! the error message (if there were errors)

    call me%json%check_for_errors(status_ok,error_msg)

    end subroutine json_file_check_for_errors
!*****************************************************************************************

!*****************************************************************************************
!>
!  Clear exceptions in the class.

    pure subroutine json_file_clear_exceptions(me)

    implicit none

    class(json_file),intent(inout) :: me

    call me%json%clear_exceptions()

    end subroutine json_file_clear_exceptions
!*****************************************************************************************

!*****************************************************************************************
!>
!  This is a wrapper for [[json_print_error_message]].

    subroutine json_file_print_error_message(me,io_unit)

    implicit none

    class(json_file),intent(inout) :: me
    integer, intent(in), optional  :: io_unit

    call me%json%print_error_message(io_unit)

    end subroutine json_file_print_error_message
!*****************************************************************************************

!*****************************************************************************************
!>
!  Initialize the [[json_core]] for this [[json_file]].
!  This is just a wrapper for [[json_initialize]].
!
!@note: This does not destroy the data in the file.

    subroutine initialize_json_core_in_file(me,verbose,compact_reals,print_signs,real_format,spaces_per_tab)

    implicit none

    class(json_file),intent(inout) :: me
    logical(LK),intent(in),optional :: verbose       !! mainly useful for debugging (default is false)
    logical(LK),intent(in),optional :: compact_reals !! to compact the real number strings for output (default is true)
    logical(LK),intent(in),optional :: print_signs   !! always print numeric sign (default is false)
    character(len=*,kind=CDK),intent(in),optional :: real_format !! Real number format: 'E' [default], '*', 'G', 'EN', or 'ES'
    integer,intent(in),optional :: spaces_per_tab !! number of spaces per tab for indenting (default is 2)

    call me%json%initialize(verbose,compact_reals,print_signs,real_format,spaces_per_tab)

    end subroutine initialize_json_core_in_file
!*****************************************************************************************

!*****************************************************************************************
!> author: Izaak Beekman
!  date: 07/23/2015
!
!  Cast a [[json_value]] object as a [[json_file(type)]] object.
!  It also calls the `initialize()` method.

    function initialize_json_file(p,verbose,compact_reals,print_signs,real_format,spaces_per_tab) result(file_object)

    implicit none

    type(json_file) :: file_object
    type(json_value),pointer,optional,intent(in) :: p  !! `json_value` object to cast
                                                       !! as a `json_file` object
    logical(LK),intent(in),optional :: verbose       !! mainly useful for debugging (default is false)
    logical(LK),intent(in),optional :: compact_reals !! to compact the real number strings for output (default is true)
    logical(LK),intent(in),optional :: print_signs   !! always print numeric sign (default is false)
    character(len=*,kind=CDK),intent(in),optional :: real_format !! Real number format: 'E' [default], '*', 'G', 'EN', or 'ES'
    integer,intent(in),optional :: spaces_per_tab !! number of spaces per tab for indenting (default is 2)

    call file_object%initialize(verbose,compact_reals,print_signs,real_format,spaces_per_tab)

    if (present(p)) file_object%p => p

    end function initialize_json_file
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 12/9/2013
!
!  Destroy the [[json_file(type)]].

    subroutine json_file_destroy(me)

    implicit none

    class(json_file),intent(inout) :: me

    if (associated(me%p)) call me%json%destroy(me%p)

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
            call to%json%throw_exception('Error in json_file_move_pointer: '//&
                                         'error exception in FROM file.')
        else
            call to%initialize()  !initialize and clear any exceptions that may be present
            to%p => from%p
            nullify(from%p)
        end if

    else
        call to%json%throw_exception('Error in json_file_move_pointer: '//&
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
!# Example
!
!```fortran
!     program main
!      use json_module
!      implicit none
!      type(json_file) :: f
!      call f%load_file('my_file.json')
!      !...
!      call f%destroy()
!     end program main
!```

    subroutine json_file_load(me, filename, unit)

    implicit none

    class(json_file),intent(inout)       :: me
    character(kind=CDK,len=*),intent(in) :: filename  !! the filename to open
    integer(IK),intent(in),optional      :: unit      !! the unit number to use (if not present, a newunit is used)

    call me%json%parse(file=filename, p=me%p, unit=unit)

    end subroutine json_file_load
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/13/2015
!
!  Load the JSON data from a string.
!
!# Example
!
!  Load JSON from a string:
!```fortran
!     type(json_file) :: f
!     call f%load_from_string('{ "name": "Leonidas" }')
!```

    subroutine json_file_load_from_string(me, str)

    implicit none

    class(json_file),intent(inout)      :: me
    character(kind=CK,len=*),intent(in) :: str  !! string to load JSON data from

    call me%json%parse(str=str, p=me%p)

    end subroutine json_file_load_from_string
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_file_load_from_string]], where "str" is kind=CDK.

    subroutine wrap_json_file_load_from_string(me, str)

    implicit none

    class(json_file),intent(inout)       :: me
    character(kind=CDK,len=*),intent(in) :: str

    call me%load_from_string(to_unicode(str))

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

    call me%json%print(me%p,iunit=output_unit)

    end subroutine json_file_print_to_console
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 12/9/2013
!
!  Prints the JSON file to the specified file unit number.

    subroutine json_file_print_1(me, iunit)

    implicit none

    class(json_file),intent(inout)  :: me
    integer(IK),intent(in)          :: iunit  !! file unit number (must not be -1)

    if (iunit/=unit2str) then
        call me%json%print(me%p,iunit=iunit)
    else
        call me%json%throw_exception('Error in json_file_print_1: iunit must not be -1.')
    end if

    end subroutine json_file_print_1
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/11/2015
!
!  Print the JSON structure to the specified filename.
!  The file is opened, printed, and then closed.
!
!# Example
!  Example loading a JSON file, changing a value, and then printing
!  result to a new file:
!```fortran
!     type(json_file) :: f
!     logical :: found
!     call f%load_file('my_file.json')    !open the original file
!     call f%update('version',4,found)    !change the value of a variable
!     call f%print_file('my_file_2.json') !save file as new name
!```

    subroutine json_file_print_2(me,filename)

    implicit none

    class(json_file),intent(inout)       :: me
    character(kind=CDK,len=*),intent(in) :: filename  !! filename to print to

    call me%json%print(me%p,filename)

    end subroutine json_file_print_2
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/11/2015
!
!  Print the JSON file to a string.
!
!# Example
!
!  Open a JSON file, and then print the contents to a string:
!```fortran
!     type(json_file) :: f
!     character(kind=CK,len=:),allocatable :: str
!     call f%load_file('my_file.json')
!     call f%print_file(str)
!```

    subroutine json_file_print_to_string(me,str)

    implicit none

    class(json_file),intent(inout)                   :: me
    character(kind=CK,len=:),allocatable,intent(out) :: str  !! string to print JSON data to

    call me%json%print_to_string(me%p,str)

    end subroutine json_file_print_to_string
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 2/3/2014
!
!  Returns information about a variable in a [[json_file(type)]].

    subroutine json_file_variable_info(me,path,found,var_type,n_children)

    implicit none

    class(json_file),intent(inout)      :: me
    character(kind=CK,len=*),intent(in) :: path       !! path to the variable
    logical(LK),intent(out)             :: found      !! the variable exists in the structure
    integer(IK),intent(out)             :: var_type   !! variable type
    integer(IK),intent(out)             :: n_children !! number of children

    type(json_value),pointer :: p

    !initialize:
    nullify(p)

    !get a pointer to the variable (if it is there):
    call me%get(path,p,found)

    if (found) then

        !get info:
        call me%json%info(p,var_type,n_children)

    else

        !set to dummy values:
        var_type = json_unknown
        n_children = 0

    end if

    !cleanup:
    nullify(p)

    end subroutine json_file_variable_info
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_file_variable_info]], where "path" is kind=CDK.

    subroutine wrap_json_file_variable_info(me,path,found,var_type,n_children)

    implicit none

    class(json_file),intent(inout)       :: me
    character(kind=CDK,len=*),intent(in) :: path
    logical(LK),intent(out)              :: found
    integer(IK),intent(out)              :: var_type
    integer(IK),intent(out)              :: n_children

    call me%info(to_unicode(path),found,var_type,n_children)

    end subroutine wrap_json_file_variable_info
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
!  date: 2/3/2014
!
!  Get a [[json_value]] pointer to an object from a JSON file.

    subroutine json_file_get_object(me, path, p, found)

    implicit none

    class(json_file),intent(inout)       :: me
    character(kind=CK,len=*),intent(in)  :: path   !! the path to the variable
    type(json_value),pointer,intent(out) :: p      !! pointer to the variable
    logical(LK),intent(out),optional     :: found  !! if it was really found

    call me%json%get(me%p, path=path, p=p, found=found)

    end subroutine json_file_get_object
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_file_get_object]], where "path" is kind=CDK.

    subroutine wrap_json_file_get_object(me, path, p, found)

    implicit none

    class(json_file),intent(inout)       :: me
    character(kind=CDK,len=*),intent(in) :: path
    type(json_value),pointer,intent(out) :: p
    logical(LK),intent(out),optional     :: found

    call me%get(to_unicode(path), p, found)

    end subroutine wrap_json_file_get_object
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 12/9/2013
!
!  Get an integer value from a JSON file.

    subroutine json_file_get_integer(me, path, val, found)

    implicit none

    class(json_file),intent(inout)      :: me
    character(kind=CK,len=*),intent(in) :: path   !! the path to the variable
    integer(IK),intent(out)             :: val    !! value
    logical(LK),intent(out),optional    :: found  !! if it was really found

    call me%json%get(me%p, path=path, value=val, found=found)

    end subroutine json_file_get_integer
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_file_get_integer]], where "path" is kind=CDK.

    subroutine wrap_json_file_get_integer(me, path, val, found)

    implicit none

    class(json_file),intent(inout)       :: me
    character(kind=CDK,len=*),intent(in) :: path
    integer(IK),intent(out)              :: val
    logical(LK),intent(out),optional     :: found

    call me%get(to_unicode(path), val, found)

    end subroutine wrap_json_file_get_integer
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/20/2014
!
!  Get an integer vector from a JSON file.

    subroutine json_file_get_integer_vec(me, path, vec, found)

    implicit none

    class(json_file),intent(inout)                   :: me
    character(kind=CK,len=*),intent(in)              :: path   !! the path to the variable
    integer(IK),dimension(:),allocatable,intent(out) :: vec    !! the value vector
    logical(LK),intent(out),optional                 :: found  !! if it was really found

    call me%json%get(me%p, path, vec, found)

    end subroutine json_file_get_integer_vec
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_file_get_integer_vec]], where "path" is kind=CDK.

    subroutine wrap_json_file_get_integer_vec(me, path, vec, found)

    implicit none

    class(json_file),intent(inout)                   :: me
    character(kind=CDK,len=*),intent(in)             :: path
    integer(IK),dimension(:),allocatable,intent(out) :: vec
    logical(LK),intent(out),optional                 :: found

    call me%get(to_unicode(path), vec, found)

    end subroutine wrap_json_file_get_integer_vec
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 12/9/2013
!
!  Get a real(RK) variable value from a JSON file.

    subroutine json_file_get_double (me, path, val, found)

    implicit none

    class(json_file),intent(inout)      :: me
    character(kind=CK,len=*),intent(in) :: path
    real(RK),intent(out)                :: val
    logical(LK),intent(out),optional    :: found

    call me%json%get(me%p, path=path, value=val, found=found)

    end subroutine json_file_get_double
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_file_get_double]], where "path" is kind=CDK.

    subroutine wrap_json_file_get_double (me, path, val, found)

    implicit none

    class(json_file),intent(inout)       :: me
    character(kind=CDK,len=*),intent(in) :: path
    real(RK),intent(out)                 :: val
    logical(LK),intent(out),optional     :: found

    call me%get(to_unicode(path), val, found)

    end subroutine wrap_json_file_get_double
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/19/2014
!
!  Get a real(RK) vector from a JSON file.

    subroutine json_file_get_double_vec(me, path, vec, found)

    implicit none

    class(json_file),intent(inout)                :: me
    character(kind=CK,len=*),intent(in)           :: path
    real(RK),dimension(:),allocatable,intent(out) :: vec
    logical(LK),intent(out),optional              :: found

    call me%json%get(me%p, path, vec, found)

    end subroutine json_file_get_double_vec
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_file_get_double_vec]], where "path" is kind=CDK.

    subroutine wrap_json_file_get_double_vec(me, path, vec, found)

    implicit none

    class(json_file),intent(inout)                :: me
    character(kind=CDK,len=*),intent(in)          :: path
    real(RK),dimension(:),allocatable,intent(out) :: vec
    logical(LK),intent(out),optional              :: found

    call me%get(to_unicode(path), vec, found)

    end subroutine wrap_json_file_get_double_vec
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 12/9/2013
!
!  Get a logical(LK) value from a JSON file.

    subroutine json_file_get_logical(me,path,val,found)

    implicit none

    class(json_file),intent(inout)       :: me
    character(kind=CK,len=*),intent(in)  :: path
    logical(LK),intent(out)              :: val
    logical(LK),intent(out),optional     :: found

    call me%json%get(me%p, path=path, value=val, found=found)

    end subroutine json_file_get_logical
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_file_get_logical]], where "path" is kind=CDK.

    subroutine wrap_json_file_get_logical(me,path,val,found)

    implicit none

    class(json_file),intent(inout)       :: me
    character(kind=CDK,len=*),intent(in) :: path
    logical(LK),intent(out)              :: val
    logical(LK),intent(out),optional     :: found

    call me%get(to_unicode(path), val, found)

    end subroutine wrap_json_file_get_logical
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/20/2014
!
!  Get a logical(LK) vector from a JSON file.

    subroutine json_file_get_logical_vec(me, path, vec, found)

    implicit none

    class(json_file),intent(inout)                   :: me
    character(kind=CK,len=*),intent(in)              :: path
    logical(LK),dimension(:),allocatable,intent(out) :: vec
    logical(LK),intent(out),optional                 :: found

    call me%json%get(me%p, path, vec, found)

    end subroutine json_file_get_logical_vec
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_file_get_logical_vec]], where "path" is kind=CDK.

    subroutine wrap_json_file_get_logical_vec(me, path, vec, found)

    implicit none

    class(json_file),intent(inout)                   :: me
    character(kind=CDK,len=*),intent(in)             :: path
    logical(LK),dimension(:),allocatable,intent(out) :: vec
    logical(LK),intent(out),optional                 :: found

    call me%get(to_unicode(path), vec, found)

    end subroutine wrap_json_file_get_logical_vec
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 12/9/2013
!
!  Get a character string from a json file.
!  The output val is an allocatable character string.

    subroutine json_file_get_string(me, path, val, found)

    implicit none

    class(json_file),intent(inout)                   :: me
    character(kind=CK,len=*),intent(in)              :: path
    character(kind=CK,len=:),allocatable,intent(out) :: val
    logical(LK),intent(out),optional                 :: found

    call me%json%get(me%p, path=path, value=val, found=found)

    end subroutine json_file_get_string
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_file_get_string]], where "path" is kind=CDK.

    subroutine wrap_json_file_get_string(me, path, val, found)

    implicit none

    class(json_file),intent(inout)                   :: me
    character(kind=CDK,len=*),intent(in)             :: path
    character(kind=CK,len=:),allocatable,intent(out) :: val
    logical(LK),intent(out),optional                 :: found

    call me%get(to_unicode(path), val, found)

    end subroutine wrap_json_file_get_string
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/19/2014
!
!  Get a string vector from a JSON file.

    subroutine json_file_get_string_vec(me, path, vec, found)

    implicit none

    class(json_file),intent(inout)                                :: me
    character(kind=CK,len=*),intent(in)                           :: path
    character(kind=CK,len=*),dimension(:),allocatable,intent(out) :: vec
    logical(LK),intent(out),optional                              :: found

    call me%json%get(me%p, path, vec, found)

    end subroutine json_file_get_string_vec
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_file_get_string_vec]], where "path" is kind=CDK.

    subroutine wrap_json_file_get_string_vec(me, path, vec, found)

    implicit none

    class(json_file),intent(inout)                                :: me
    character(kind=CDK,len=*),intent(in)                          :: path
    character(kind=CK,len=*),dimension(:),allocatable,intent(out) :: vec
    logical(LK),intent(out),optional                              :: found

    call me%get(to_unicode(path), vec, found)

    end subroutine wrap_json_file_get_string_vec
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date:1/10/2015
!
!  Given the path string, if the variable is present in the file,
!  and is a scalar, then update its value.
!  If it is not present, then create it and set its value.
!
!# See also
!  * [[json_update_integer]]

    subroutine json_file_update_integer(me,name,val,found)

    implicit none

    class(json_file),intent(inout)      :: me
    character(kind=CK,len=*),intent(in) :: name
    integer(IK),intent(in)              :: val
    logical(LK),intent(out)             :: found

    if (.not. me%json%failed()) call me%json%update(me%p,name,val,found)

    end subroutine json_file_update_integer
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_file_update_integer]], where "name" is kind=CDK.

    subroutine wrap_json_file_update_integer(me,name,val,found)

    implicit none

    class(json_file),intent(inout)       :: me
    character(kind=CDK,len=*),intent(in) :: name
    integer(IK),intent(in)               :: val
    logical(LK),intent(out)              :: found

    call me%update(to_unicode(name),val,found)

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
!# See also
!  * [[json_update_logical]]

    subroutine json_file_update_logical(me,name,val,found)

    implicit none

    class(json_file),intent(inout)      :: me
    character(kind=CK,len=*),intent(in) :: name
    logical(LK),intent(in)              :: val
    logical(LK),intent(out)             :: found

    if (.not. me%json%failed()) call me%json%update(me%p,name,val,found)

    end subroutine json_file_update_logical
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_file_update_logical]], where "name" is kind=CDK.

    subroutine wrap_json_file_update_logical(me,name,val,found)

    implicit none

    class(json_file),intent(inout)       :: me
    character(kind=CDK,len=*),intent(in) :: name
    logical(LK),intent(in)               :: val
    logical(LK),intent(out)              :: found

    call me%update(to_unicode(name),val,found)

    end subroutine wrap_json_file_update_logical
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/10/2015
!
!  Given the path string, if the variable is present in the file,
!  and is a scalar, then update its value.
!  If it is not present, then create it and set its value.
!
!# See also
!  * [[json_update_double]]

    subroutine json_file_update_real(me,name,val,found)

    implicit none

    class(json_file),intent(inout)      :: me
    character(kind=CK,len=*),intent(in) :: name
    real(RK),intent(in)                 :: val
    logical(LK),intent(out)             :: found

    if (.not. me%json%failed()) call me%json%update(me%p,name,val,found)

    end subroutine json_file_update_real
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_file_update_real]], where "name" is kind=CDK.

    subroutine wrap_json_file_update_real(me,name,val,found)

    implicit none

    class(json_file),intent(inout)       :: me
    character(kind=CDK,len=*),intent(in) :: name
    real(RK),intent(in)                  :: val
    logical(LK),intent(out)              :: found

    call me%update(to_unicode(name),val,found)

    end subroutine wrap_json_file_update_real
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/10/2015
!
!  Given the path string, if the variable is present in the file,
!  and is a scalar, then update its value.
!  If it is not present, then create it and set its value.
!
!# See also
!  * [[json_update_string]]

    subroutine json_file_update_string(me,name,val,found)

    implicit none

    class(json_file),intent(inout)      :: me
    character(kind=CK,len=*),intent(in) :: name
    character(kind=CK,len=*),intent(in) :: val
    logical(LK),intent(out)             :: found

    if (.not. me%json%failed()) call me%json%update(me%p,name,val,found)

    end subroutine json_file_update_string
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_file_update_string]], where "name" and "val" are kind=CDK.

    subroutine wrap_json_file_update_string(me,name,val,found)

    implicit none

    class(json_file),intent(inout)       :: me
    character(kind=CDK,len=*),intent(in) :: name
    character(kind=CDK,len=*),intent(in) :: val
    logical(LK),intent(out)              :: found

    call me%update(to_unicode(name),to_unicode(val),found)

    end subroutine wrap_json_file_update_string
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_file_update_string]], where "name" is kind=CDK.

    subroutine json_file_update_string_name_ascii(me,name,val,found)

    implicit none

    class(json_file),intent(inout)       :: me
    character(kind=CDK,len=*),intent(in) :: name
    character(kind=CK, len=*),intent(in) :: val
    logical(LK),intent(out)              :: found

    call me%update(to_unicode(name),val,found)

    end subroutine json_file_update_string_name_ascii
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_file_update_string]], where "val" is kind=CDK.

    subroutine json_file_update_string_val_ascii(me,name,val,found)

    implicit none

    class(json_file),intent(inout)       :: me
    character(kind=CK, len=*),intent(in) :: name
    character(kind=CDK,len=*),intent(in) :: val
    logical(LK),intent(out)              :: found

    call me%update(name,to_unicode(val),found)

    end subroutine json_file_update_string_val_ascii
!*****************************************************************************************

!*****************************************************************************************
    end module json_file_module
!*****************************************************************************************
