!*****************************************************************************************
!> author: Jacob Williams
!  license: BSD
!
!  This module provides a low-level interface for manipulation of JSON data.
!  The two public entities are [[json_value]], and [[json_core]].
!  The [[json_file_module]] provides a higher-level interface to some
!  of these routines.
!
!## License
!  * JSON-Fortran is released under a BSD-style license.
!    See the [LICENSE](https://github.com/jacobwilliams/json-fortran/blob/master/LICENSE)
!    file for details.

    module json_value_module

    use,intrinsic :: iso_fortran_env, only: iostat_end,error_unit,output_unit
    use json_kinds
    use json_parameters
    use json_string_utilities

    implicit none

    private

#include "json_macros.inc"

    !*********************************************************
    !>
    !  If Unicode is not enabled, then
    !  JSON files are opened using access='STREAM' and
    !  form='UNFORMATTED'.  This allows the file to
    !  be read faster.
    !
#ifdef USE_UCS4
    logical,parameter :: use_unformatted_stream = .false.
#else
    logical,parameter :: use_unformatted_stream = .true.
#endif
    !*********************************************************

    !*********************************************************
    !>
    !  If Unicode is not enabled, then
    !  JSON files are opened using access='STREAM' and
    !  form='UNFORMATTED'.  This allows the file to
    !  be read faster.
    !
#ifdef USE_UCS4
    character(kind=CDK,len=*),parameter :: access_spec = 'SEQUENTIAL'
#else
    character(kind=CDK,len=*),parameter :: access_spec = 'STREAM'
#endif
    !*********************************************************

    !*********************************************************
    !>
    !  If Unicode is not enabled, then
    !  JSON files are opened using access='STREAM' and
    !  form='UNFORMATTED'.  This allows the file to
    !  be read faster.
    !
#ifdef USE_UCS4
    character(kind=CDK,len=*),parameter :: form_spec   = 'FORMATTED'
#else
    character(kind=CDK,len=*),parameter :: form_spec   = 'UNFORMATTED'
#endif
    !*********************************************************

    !*********************************************************
    !>
    !  Type used to construct the linked-list JSON structure.
    !  Normally, this should always be a pointer variable.
    !  This type should only be used by an instance of [[json_core]].
    !
    !# Example
    !
    !  The following test program:
    !
    !````fortran
    !    program test
    !     use json_module
    !     implicit none
    !     type(json_core) :: json
    !     type(json_value),pointer :: p
    !     call json%create_object(p,'')   !create the root
    !     call json%add(p,'year',1805)    !add some data
    !     call json%add(p,'value',1.0_RK) !add some data
    !     call json%print(p,'test.json')  !write it to a file
    !     call json%destroy(p)            !cleanup
    !    end program test
    !````
    !
    !  Produces the JSON file **test.json**:
    !
    !````json
    !    {
    !      "year": 1805,
    !      "value": 0.1E+1
    !    }
    !````
    type,public :: json_value

        !force the constituents to be stored contiguously
        ![note: on Intel, the order of the variables below
        ! is significant to avoid the misaligned field warnings]
        sequence

        private

        !for the linked list:
        type(json_value),pointer :: previous => null()  !! previous item in the list
        type(json_value),pointer :: next     => null()  !! next item in the list
        type(json_value),pointer :: parent   => null()  !! parent item of this
        type(json_value),pointer :: children => null()  !! first child item of this
        type(json_value),pointer :: tail     => null()  !! last child item of this

        character(kind=CK,len=:),allocatable :: name  !! variable name

        real(RK),allocatable                 :: dbl_value  !! real data for this variable
        logical(LK),allocatable              :: log_value  !! logical data for this variable
        character(kind=CK,len=:),allocatable :: str_value  !! string data for this variable
        integer(IK),allocatable              :: int_value  !! integer data for this variable

        integer(IK) :: var_type = json_unknown  !! variable type

        integer(IK),private :: n_children = 0   !! number of children

    end type json_value
    !*********************************************************

    !*********************************************************
    !>
    !  To access the core routines for manipulation
    !  of [[json_value]] pointer variables. This class allows
    !  for thread safe use of the module.
    !
    !### Usage
    !````fortran
    !    program test
    !     use json_module
    !     implicit none
    !     type(json_core) :: json     !<--have to declare this
    !     type(json_value),pointer :: p
    !     call json%create_object(p,'')   !create the root
    !     call json%add(p,'year',1805)    !add some data
    !     call json%add(p,'value',1.0_RK) !add some data
    !     call json%print(p,'test.json')  !write it to a file
    !     call json%destroy(p)            !cleanup
    !    end program test
    !````
    type,public :: json_core

        private

        integer(IK) :: spaces_per_tab = 2    !! number of spaces for indenting

        logical(LK) :: compact_real = .true.  !! to use the "compact" form of real
                                              !! numbers for output
        character(kind=CDK,len=:),allocatable :: real_fmt   !! the format string to use
                                                            !! for converting real numbers to strings.
                                                            !! It can be set in [[json_initialize]],
                                                            !! and used in [[json_value_print]]
                                                            !! If not set, then `default_real_fmt`
                                                            !! is used instead.

        logical(LK) :: is_verbose = .false.        !! if true, all exceptions are
                                                   !! immediately printed to console.
        logical(LK) :: exception_thrown = .false.  !! The error flag. Will be set to true
                                                   !! when an error is thrown in the class.
                                                   !! Many of the methods will check this
                                                   !! and return immediately if it is true.
        character(kind=CK,len=:),allocatable :: err_message !! the error message

        integer(IK) :: char_count = 0    !! character position in the current line
        integer(IK) :: line_count = 1    !! lines read counter
        integer(IK) :: pushed_index = 0  !! used when parsing lines in file
        character(kind=CK,len=pushed_char_size) :: pushed_char = ''  !! used when parsing
                                                                     !! lines in file

        integer(IK) :: ipos = 1  !! for allocatable strings: next character to read

        logical(LK) :: strict_type_checking = .false. !! if true, then no type conversions are done
                                                      !! in the `get` routines if the actual variable
                                                      !! type is different from the return type (for
                                                      !! example, integer to double).

        logical(LK) :: trailing_spaces_significant = .false.    !! for name and path comparisons, is trailing
                                                                !! space to be considered significant.

        logical(LK) :: case_sensitive_keys = .true.    !! for name and path comparisons, are they
                                                       !! case sensitive.

        logical(LK) :: no_whitespace = .false. !! when printing a JSON string, don't include
                                               !! non-significant spaces or line breaks.
                                               !! If true, the entire structure will be
                                               !! printed on one line.

        logical(LK) :: unescaped_strings = .true.  !! If false, then the raw escaped
                                                   !! string is returned from [[json_get_string]]
                                                   !! and similar routines. If true [default],
                                                   !! then the string is returned unescaped.

        contains

        private

        generic,public :: get_child => json_value_get_by_index, &
                                       json_value_get_child,&
                                       MAYBEWRAP(json_value_get_by_name_chars)
        procedure,private :: json_value_get_by_index
        procedure,private :: MAYBEWRAP(json_value_get_by_name_chars)
        procedure,private :: json_value_get_child

        !>
        !  Add objects to a linked list of [[json_value]]s.
        generic,public :: add => json_value_add_member, &
                                 MAYBEWRAP(json_value_add_null), &
                                 MAYBEWRAP(json_value_add_integer), &
                                 MAYBEWRAP(json_value_add_integer_vec), &
                                 MAYBEWRAP(json_value_add_double), &
                                 MAYBEWRAP(json_value_add_double_vec), &
                                 MAYBEWRAP(json_value_add_logical), &
                                 MAYBEWRAP(json_value_add_logical_vec), &
                                 MAYBEWRAP(json_value_add_string), &
                                 MAYBEWRAP(json_value_add_string_vec)
#ifdef USE_UCS4
        generic,public :: add => json_value_add_string_name_ascii, &
                                 json_value_add_string_val_ascii, &
                                 json_value_add_string_vec_name_ascii, &
                                 json_value_add_string_vec_val_ascii
#endif

    procedure,private :: json_value_add_member
    procedure,private :: MAYBEWRAP(json_value_add_integer)
    procedure,private :: MAYBEWRAP(json_value_add_null)
    procedure,private :: MAYBEWRAP(json_value_add_integer_vec)
    procedure,private :: MAYBEWRAP(json_value_add_double)
    procedure,private :: MAYBEWRAP(json_value_add_double_vec)
    procedure,private :: MAYBEWRAP(json_value_add_logical)
    procedure,private :: MAYBEWRAP(json_value_add_logical_vec)
    procedure,private :: MAYBEWRAP(json_value_add_string)
    procedure,private :: MAYBEWRAP(json_value_add_string_vec)
#ifdef USE_UCS4
    procedure,private :: json_value_add_string_name_ascii
    procedure,private :: json_value_add_string_val_ascii
    procedure,private :: json_value_add_string_vec_name_ascii
    procedure,private :: json_value_add_string_vec_val_ascii
#endif

        !>
        !  These are like the `add` methods, except if a child with the
        !  same name is already present, then its value is simply updated.
        !  Note that currently, these only work for scalar variables.
        !  These routines can also change the variable's type (but an error will be
        !  thrown if the existing variable is not a scalar).
        !
        !@note It should not be used to change the type of a variable in an array,
        !      or it may result in an invalid JSON file.

        generic,public :: update => MAYBEWRAP(json_update_logical),&
                                    MAYBEWRAP(json_update_double),&
                                    MAYBEWRAP(json_update_integer),&
                                    MAYBEWRAP(json_update_string)
#ifdef USE_UCS4
        generic,public :: update => json_update_string_name_ascii,&
                                    json_update_string_val_ascii
#endif
        procedure,private :: MAYBEWRAP(json_update_logical)
        procedure,private :: MAYBEWRAP(json_update_double)
        procedure,private :: MAYBEWRAP(json_update_integer)
        procedure,private :: MAYBEWRAP(json_update_string)
#ifdef USE_UCS4
        procedure,private :: json_update_string_name_ascii
        procedure,private :: json_update_string_val_ascii
#endif

        !>
        !  Get data from a [[json_value]] linked list.
        !
        !@note There are two versions (e.g. [[json_get_integer]] and [[json_get_integer_with_path]]).
        !      The first one gets the value from the [[json_value]] passed into the routine,
        !      while the second one gets the value from the [[json_value]] found by parsing the
        !      path.  The path version is split up into unicode and non-unicode versions.

        generic,public :: get => &
                                  MAYBEWRAP(json_get_by_path),               &
            json_get_integer,     MAYBEWRAP(json_get_integer_with_path),     &
            json_get_integer_vec, MAYBEWRAP(json_get_integer_vec_with_path), &
            json_get_double,      MAYBEWRAP(json_get_double_with_path),      &
            json_get_double_vec,  MAYBEWRAP(json_get_double_vec_with_path),  &
            json_get_logical,     MAYBEWRAP(json_get_logical_with_path),     &
            json_get_logical_vec, MAYBEWRAP(json_get_logical_vec_with_path), &
            json_get_string,      MAYBEWRAP(json_get_string_with_path),      &
            json_get_string_vec,  MAYBEWRAP(json_get_string_vec_with_path),  &
            json_get_array,       MAYBEWRAP(json_get_array_with_path)
        procedure,private :: json_get_integer
        procedure,private :: json_get_integer_vec
        procedure,private :: json_get_double
        procedure,private :: json_get_double_vec
        procedure,private :: json_get_logical
        procedure,private :: json_get_logical_vec
        procedure,private :: json_get_string
        procedure,private :: json_get_string_vec
        procedure,private :: json_get_array
        procedure,private :: MAYBEWRAP(json_get_by_path)
        procedure,private :: MAYBEWRAP(json_get_integer_with_path)
        procedure,private :: MAYBEWRAP(json_get_integer_vec_with_path)
        procedure,private :: MAYBEWRAP(json_get_double_with_path)
        procedure,private :: MAYBEWRAP(json_get_double_vec_with_path)
        procedure,private :: MAYBEWRAP(json_get_logical_with_path)
        procedure,private :: MAYBEWRAP(json_get_logical_vec_with_path)
        procedure,private :: MAYBEWRAP(json_get_string_with_path)
        procedure,private :: MAYBEWRAP(json_get_string_vec_with_path)
        procedure,private :: MAYBEWRAP(json_get_array_with_path)

        procedure,public :: print_to_string => json_value_to_string !! Print the [[json_value]] structure to an allocatable string

        !>
        !  Print the [[json_value]] to a file.
        !
        !# Example
        !
        !````fortran
        !    type(json_core) :: json
        !    type(json_value) :: p
        !    !...
        !    call json%print(p,'test.json')  !this is [[json_print_2]]
        !````
        generic,public :: print => json_print_1,json_print_2
        procedure :: json_print_1
        procedure :: json_print_2

        !>
        !  Destructor routine for a [[json_value]] pointer.
        !  This must be called explicitly if it is no longer needed,
        !  before it goes out of scope.  Otherwise, a memory leak will result.
        !
        !# Example
        !
        !  Destroy the [[json_value]] pointer before the variable goes out of scope:
        !````fortran
        !     subroutine example1()
        !     type(json_core) :: json
        !     type(json_value),pointer :: p
        !     call json%create_object(p,'')
        !     call json%add(p,'year',2015)
        !     call json%print(p)
        !     call json%destroy(p)
        !     end subroutine example1
        !````
        !
        !  Note: it should NOT be called for a [[json_value]] pointer than has already been
        !  added to another [[json_value]] structure, since doing so may render the
        !  other structure invalid.  Consider the following example:
        !````fortran
        !     subroutine example2(p)
        !     type(json_core) :: json
        !     type(json_value),pointer,intent(out) :: p
        !     type(json_value),pointer :: q
        !     call json%create_object(p,'')
        !     call json%add(p,'year',2015)
        !     call json%create_object(q,'q')
        !     call json%add(q,'val',1)
        !     call json%add(p, q)  !add q to p structure
        !     ! do NOT call json%destroy(q) here, because q is
        !     ! now part of the output structure p.  p should be destroyed
        !     ! somewhere upstream by the caller of this routine.
        !     nullify(q) !OK, but not strictly necessary
        !     end subroutine example2
        !````
        generic,public :: destroy => json_value_destroy,destroy_json_core
        procedure :: json_value_destroy
        procedure :: destroy_json_core

        !>
        !  If the child variable is present, then remove it.
        generic,public :: remove_if_present => MAYBEWRAP(json_value_remove_if_present)
        procedure :: MAYBEWRAP(json_value_remove_if_present)

        !>
        !  Allocate a [[json_value]] pointer and make it a double variable.
        !  The pointer should not already be allocated.
        !
        !# Example
        !
        !````fortran
        !    type(json_core) :: json
        !    type(json_value),pointer :: p
        !    call json%create_double(p,'value',1.0_RK)
        !````
        generic,public :: create_double => MAYBEWRAP(json_value_create_double)
        procedure :: MAYBEWRAP(json_value_create_double)

        !>
        !  Allocate a [[json_value]] pointer and make it an array variable.
        !  The pointer should not already be allocated.
        !
        !# Example
        !
        !````fortran
        !    type(json_core) :: json
        !    type(json_value),pointer :: p
        !    call json%create_array(p,'arrayname')
        !````
        generic,public :: create_array => MAYBEWRAP(json_value_create_array)
        procedure :: MAYBEWRAP(json_value_create_array)

        !>
        !  Allocate a [[json_value]] pointer and make it an object variable.
        !  The pointer should not already be allocated.
        !
        !# Example
        !
        !````fortran
        !    type(json_core) :: json
        !    type(json_value),pointer :: p
        !    call json%create_object(p,'objectname')
        !````
        !
        !@note The name is not significant for the root structure or an array element.
        !      In those cases, an empty string can be used.
        generic,public :: create_object => MAYBEWRAP(json_value_create_object)
        procedure :: MAYBEWRAP(json_value_create_object)

        !>
        !  Allocate a json_value pointer and make it a null variable.
        !  The pointer should not already be allocated.
        !
        !# Example
        !
        !````fortran
        !    type(json_core) :: json
        !    type(json_value),pointer :: p
        !    call json%create_null(p,'value')
        !````
        generic,public :: create_null => MAYBEWRAP(json_value_create_null)
        procedure :: MAYBEWRAP(json_value_create_null)

        !>
        !  Allocate a json_value pointer and make it a string variable.
        !  The pointer should not already be allocated.
        !
        !# Example
        !
        !````fortran
        !    type(json_core) :: json
        !    type(json_value),pointer :: p
        !    call json%create_string(p,'value','foobar')
        !````
        generic,public :: create_string => MAYBEWRAP(json_value_create_string)
        procedure :: MAYBEWRAP(json_value_create_string)

        !>
        !  Allocate a json_value pointer and make it an integer variable.
        !  The pointer should not already be allocated.
        !
        !# Example
        !
        !````fortran
        !    type(json_core) :: json
        !    type(json_value),pointer :: p
        !    call json%create_integer(p,'value',42)
        !````
        generic,public :: create_integer => MAYBEWRAP(json_value_create_integer)
        procedure :: MAYBEWRAP(json_value_create_integer)

        !>
        !  Allocate a json_value pointer and make it a logical variable.
        !  The pointer should not already be allocated.
        !
        !# Example
        !
        !````fortran
        !    type(json_core) :: json
        !    type(json_value),pointer :: p
        !    call json%create_logical(p,'value',.true.)
        !````
        generic,public :: create_logical => MAYBEWRAP(json_value_create_logical)
        procedure :: MAYBEWRAP(json_value_create_logical)

        !>
        !  Parse the JSON file and populate the [[json_value]] tree.
        generic,public :: parse => json_parse_file, MAYBEWRAP(json_parse_string)
        procedure :: json_parse_file
        procedure :: MAYBEWRAP(json_parse_string)

        !>
        !  Throw an exception.
        generic,public :: throw_exception => MAYBEWRAP(json_throw_exception)
        procedure :: MAYBEWRAP(json_throw_exception)

        !>
        !  Rename a [[json_value]] variable.
        generic,public :: rename => MAYBEWRAP(json_value_rename)
        procedure :: MAYBEWRAP(json_value_rename)

        !>
        !  get info about a [[json_value]]
        generic,public :: info => json_info, MAYBEWRAP(json_info_by_path)
        procedure :: json_info
        procedure :: MAYBEWRAP(json_info_by_path)

        !>
        !  get matrix info about a [[json_value]]
        generic,public :: matrix_info => json_matrix_info, MAYBEWRAP(json_matrix_info_by_path)
        procedure :: json_matrix_info
        procedure :: MAYBEWRAP(json_matrix_info_by_path)

        !>
        !  insert a new element after an existing one,
        !  updating the JSON structure accordingly
        generic,public :: insert_after => json_value_insert_after, &
                                          json_value_insert_after_child_by_index
        procedure :: json_value_insert_after
        procedure :: json_value_insert_after_child_by_index

        !>
        !  get the path to a JSON variable in a structure:
        generic,public :: get_path => MAYBEWRAP(json_get_path)
        procedure :: MAYBEWRAP(json_get_path)

        procedure,public :: remove              => json_value_remove        !! Remove a [[json_value]] from a linked-list structure.
        procedure,public :: check_for_errors    => json_check_for_errors    !! check for error and get error message
        procedure,public :: clear_exceptions    => json_clear_exceptions    !! clear exceptions
        procedure,public :: count               => json_count               !! count the number of children
        procedure,public :: clone               => json_clone               !! clone a JSON structure (deep copy)
        procedure,public :: failed              => json_failed              !! check for error
        procedure,public :: get_parent          => json_get_parent          !! get pointer to json_value parent
        procedure,public :: get_next            => json_get_next            !! get pointer to json_value next
        procedure,public :: get_previous        => json_get_previous        !! get pointer to json_value previous
        procedure,public :: get_tail            => json_get_tail            !! get pointer to json_value tail
        procedure,public :: initialize          => json_initialize          !! to initialize some parsing parameters
        procedure,public :: traverse            => json_traverse            !! to traverse all elements of a JSON structure
        procedure,public :: print_error_message => json_print_error_message !! simply routine to print error messages
        procedure,public :: swap                => json_value_swap          !! Swap two [[json_value]] pointers
                                                                            !! in a structure (or two different structures).
        procedure,public :: is_child_of         => json_value_is_child_of   !! Check if a [[json_value]] is a descendant of another.
        procedure,public :: validate            => json_value_validate      !! Check that a [[json_value]] linked list is valid
                                                                            !! (i.e., is properly constructed). This may be
                                                                            !! useful if it has been constructed externally.

        !other private routines:
        procedure :: name_equal
        procedure :: json_value_print
        procedure :: string_to_integer
        procedure :: string_to_double
        procedure :: parse_value
        procedure :: parse_number
        procedure :: parse_string
        procedure :: parse_for_chars
        procedure :: parse_object
        procedure :: parse_array
        procedure :: annotate_invalid_json
        procedure :: pop_char
        procedure :: push_char
        procedure :: get_current_line_from_file_stream
        procedure :: get_current_line_from_file_sequential

    end type json_core
    !*********************************************************

    !*********************************************************
    !>
    !  Structure constructor to initialize a
    !  [[json_core]] object
    !
    !# Example
    !
    !```fortran
    ! type(json_file)  :: json_core
    ! json_core = json_core()
    !```
    interface json_core
       module procedure initialize_json_core
    end interface
    !*********************************************************

    !*************************************************************************************
    abstract interface

        subroutine json_array_callback_func(json, element, i, count)
            !! Array element callback function.  Used by [[json_get_array]]
            import :: json_value,json_core,IK
            implicit none
            class(json_core),intent(inout)       :: json
            type(json_value),pointer,intent(in)  :: element
            integer(IK),intent(in)               :: i        !! index
            integer(IK),intent(in)               :: count    !! size of array
        end subroutine json_array_callback_func

        subroutine json_traverse_callback_func(json,p,finished)
            !! Callback function used by [[json_traverse]]
            import :: json_value,json_core,LK
            implicit none
            class(json_core),intent(inout)      :: json
            type(json_value),pointer,intent(in) :: p
            logical(LK),intent(out)             :: finished  !! set true to stop traversing
        end subroutine json_traverse_callback_func

    end interface
    public :: json_array_callback_func
    public :: json_traverse_callback_func
    !*************************************************************************************

    contains
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 4/17/2016
!
!  Destructor for the [[json_core]] type.

    subroutine destroy_json_core(me)

    implicit none

    class(json_core),intent(out) :: me

    end subroutine destroy_json_core
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 4/26/2016
!
!  Function constructor for a [[json_core]].
!  This is just a wrapper for [[json_initialize]].
!
!@note [[initialize_json_core]], [[json_initialize]],
!      [[initialize_json_core_in_file]], and [[initialize_json_file]]
!      all have a similar interface.

    function initialize_json_core(verbose,compact_reals,&
                                  print_signs,real_format,spaces_per_tab,&
                                  strict_type_checking,&
                                  trailing_spaces_significant,&
                                  case_sensitive_keys,&
                                  no_whitespace,&
                                  unescape_strings) result(json_core_object)

    implicit none

    type(json_core) :: json_core_object
    logical(LK),intent(in),optional :: verbose       !! mainly useful for debugging (default is false)
    logical(LK),intent(in),optional :: compact_reals !! to compact the real number strings for output (default is true)
    logical(LK),intent(in),optional :: print_signs   !! always print numeric sign (default is false)
    character(kind=CDK,len=*),intent(in),optional :: real_format !! Real number format: 'E' [default], '*', 'G', 'EN', or 'ES'
    integer(IK),intent(in),optional :: spaces_per_tab !! number of spaces per tab for indenting (default is 2)
    logical(LK),intent(in),optional :: strict_type_checking !! if true, no integer, double, or logical type
                                                            !! conversions are done for the `get` routines
                                                            !! (default is false)
    logical(LK),intent(in),optional :: trailing_spaces_significant  !! for name and path comparisons, is trailing
                                                                    !! space to be considered significant.
                                                                    !! (default is false)
    logical(LK),intent(in),optional :: case_sensitive_keys  !! for name and path comparisons, are they
                                                            !! case sensitive.
                                                            !! (default is true)
    logical(LK),intent(in),optional :: no_whitespace  !! if true, printing the JSON structure is
                                                      !! done without adding any non-significant
                                                      !! spaces or linebreaks (default is false)
    logical(LK),intent(in),optional :: unescape_strings !! If false, then the raw escaped
                                                        !! string is returned from [[json_get_string]]
                                                        !! and similar routines. If true [default],
                                                        !! then the string is returned unescaped.

    call json_core_object%initialize(verbose,compact_reals,&
                                print_signs,real_format,spaces_per_tab,&
                                strict_type_checking,&
                                trailing_spaces_significant,&
                                case_sensitive_keys,&
                                no_whitespace,&
                                unescape_strings)

    end function initialize_json_core
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 12/4/2013
!
!  Initialize the [[json_core]] instance.
!
!  The routine may be called before any of the [[json_core]] methods are used in
!  order to specify certain parameters. If it is not called, then the defaults
!  are used. This routine is also called internally by various routines.
!  It can also be called to clear exceptions, or to reset some
!  of the variables (note that only the arguments present are changed).
!
!# Modified
!  * Izaak Beekman : 02/24/2015
!
!@note [[initialize_json_core]], [[json_initialize]],
!      [[initialize_json_core_in_file]], and [[initialize_json_file]]
!      all have a similar interface.

    subroutine json_initialize(json,verbose,compact_reals,&
                               print_signs,real_format,spaces_per_tab,&
                               strict_type_checking,&
                               trailing_spaces_significant,&
                               case_sensitive_keys,&
                               no_whitespace,&
                               unescape_strings)

    implicit none

    class(json_core),intent(inout)  :: json
    logical(LK),intent(in),optional :: verbose       !! mainly useful for debugging (default is false)
    logical(LK),intent(in),optional :: compact_reals !! to compact the real number strings for output (default is true)
    logical(LK),intent(in),optional :: print_signs   !! always print numeric sign (default is false)
    character(kind=CDK,len=*),intent(in),optional :: real_format !! Real number format: 'E' [default], '*', 'G', 'EN', or 'ES'
    integer(IK),intent(in),optional :: spaces_per_tab !! number of spaces per tab for indenting (default is 2)
    logical(LK),intent(in),optional :: strict_type_checking !! if true, no integer, double, or logical type
                                                            !! conversions are done for the `get` routines
                                                            !! (default is false)
    logical(LK),intent(in),optional :: trailing_spaces_significant  !! for name and path comparisons, is trailing
                                                                    !! space to be considered significant.
                                                                    !! (default is false)
    logical(LK),intent(in),optional :: case_sensitive_keys  !! for name and path comparisons, are they
                                                            !! case sensitive. (default is true)
    logical(LK),intent(in),optional :: no_whitespace  !! if true, printing the JSON structure is
                                                      !! done without adding any non-significant
                                                      !! spaces or linebreaks (default is false)
    logical(LK),intent(in),optional :: unescape_strings !! If false, then the raw escaped
                                                        !! string is returned from [[json_get_string]]
                                                        !! and similar routines. If true [default],
                                                        !! then the string is returned unescaped.

    character(kind=CDK,len=10) :: w,d,e
    character(kind=CDK,len=2)  :: sgn, rl_edit_desc
    integer(IK) :: istat
    logical(LK) :: sgn_prnt

    !reset exception to false:
    call json%clear_exceptions()

    !Just in case, clear these global variables also:
    json%pushed_index = 0
    json%pushed_char  = ''
    json%char_count   = 0
    json%line_count   = 1
    json%ipos         = 1

#ifdef USE_UCS4
    ! reopen stdout and stderr with utf-8 encoding
    open(output_unit,encoding='utf-8')
    open(error_unit, encoding='utf-8')
#endif

    !various optional inputs:
    if (present(spaces_per_tab)) &
        json%spaces_per_tab = spaces_per_tab
    if (present(verbose)) &
        json%is_verbose = verbose
    if (present(strict_type_checking)) &
        json%strict_type_checking = strict_type_checking
    if (present(trailing_spaces_significant)) &
        json%trailing_spaces_significant = trailing_spaces_significant
    if (present(case_sensitive_keys)) &
        json%case_sensitive_keys = case_sensitive_keys
    if (present(no_whitespace)) &
        json%no_whitespace = no_whitespace
    if (present(unescape_strings)) &
        json%unescaped_strings = unescape_strings

    !Set the format for real numbers:
    ! [if not changing it, then it remains the same]

    if ( (.not. allocated(json%real_fmt)) .or. &  ! if this hasn't been done yet
          present(compact_reals) .or. &
          present(print_signs)   .or. &
          present(real_format) ) then

        !allow the special case where real format is '*':
        ! [this overrides the other options]
        if (present(real_format)) then
            if (real_format==star) then
                json%compact_real = .false.
                json%real_fmt = star
                return
            end if
        end if

        if (present(compact_reals)) json%compact_real = compact_reals

        !set defaults
        sgn_prnt = .false.
        if ( present( print_signs) ) sgn_prnt = print_signs
        if ( sgn_prnt ) then
           sgn = 'sp'
        else
           sgn = 'ss'
        end if

        rl_edit_desc = 'E'
        if ( present( real_format ) ) then
           select case ( real_format )
           case ('g','G','e','E','en','EN','es','ES')
              rl_edit_desc = real_format
           case default
              call json%throw_exception('Invalid real format, "' // &
                        trim(real_format) // '", passed to json_initialize.'// &
                        new_line('a') // 'Acceptable formats are: "G", "E", "EN", and "ES".' )
           end select
        end if

        ! set the default output/input format for reals:
                      write(w,'(ss,I0)',iostat=istat) max_numeric_str_len
        if (istat==0) write(d,'(ss,I0)',iostat=istat) real_precision
        if (istat==0) write(e,'(ss,I0)',iostat=istat) real_exponent_digits
        if (istat==0) then
            json%real_fmt = '(' // sgn // ',' // trim(rl_edit_desc) //&
                            trim(w) // '.' // trim(d) // 'E' // trim(e) // ')'
        else
            json%real_fmt = '(' // sgn // ',' // trim(rl_edit_desc) // &
                            '30.16E3)'  !just use this one (should never happen)
        end if

    end if

    end subroutine json_initialize
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 4/30/2016
!
!  Returns true if `name` is equal to `p%name`, using the specified
!  settings for case sensitivity and trailing whitespace.

    function name_equal(json,p,name) result(is_equal)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),intent(in)         :: p        !! the json object
    character(kind=CK,len=*),intent(in) :: name     !! the name to check for
    logical(LK)                         :: is_equal !! true if the string are lexically equal

    if (allocated(p%name)) then

        !must be the same length if we are treating
        !trailing spaces as significant, so do a
        !quick test of this first:
        if (json%trailing_spaces_significant) then
            is_equal = len(p%name) == len(name)
            if (.not. is_equal) return
        end if

        if (json%case_sensitive_keys) then
            is_equal = p%name == name
        else
            is_equal = lowercase_string(p%name) == lowercase_string(name)
        end if

    else
        is_equal = name == '' ! check a blank name
    end if

    end function name_equal
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 10/31/2015
!
!  Create a deep copy of a [[json_value]] linked-list structure.
!
!# Example
!
!````fortran
!    program test
!     use json_module
!     implicit none
!     type(json_core) :: json
!     type(json_value),pointer :: j1, j2
!     call json%parse('../files/inputs/test1.json',j1)
!     call json%clone(j1,j2) !now have two independent copies
!     call json%destroy(j1)  !destroys j1, but j2 remains
!     call json%print(j2,'j2.json')
!     call json%destroy(j2)
!    end program test
!````

    subroutine json_clone(json,from,to)

    implicit none

    class(json_core),intent(inout) :: json
    type(json_value),pointer :: from  !! this is the structure to clone
    type(json_value),pointer :: to    !! the clone is put here
                                      !! (it must not already be associated)

    !call the main function:
    ! [note: this is not part of json_core class]
    call json_value_clone_func(from,to)

    end subroutine json_clone
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 10/31/2015
!
!  Recursive deep copy function called by [[json_clone]].
!
!@note If new data is added to the [[json_value]] type,
!      then this would need to be updated.

    recursive subroutine json_value_clone_func(from,to,parent,previous,next,children,tail)

    implicit none

    type(json_value),pointer          :: from     !! this is the structure to clone
    type(json_value),pointer          :: to       !! the clone is put here
                                                  !! (it must not already be associated)
    type(json_value),pointer,optional :: parent   !! to%parent
    type(json_value),pointer,optional :: previous !! to%previous
    type(json_value),pointer,optional :: next     !! to%next
    type(json_value),pointer,optional :: children !! to%children
    logical,optional                  :: tail     !! if "to" is the tail of its parent's children

    nullify(to)

    if (associated(from)) then

        allocate(to)

        !copy over the data variables:
        ! [note: the allocate() statements don't work here for the
        !  deferred-length characters in gfortran-4.9]
        if (allocated(from%name))      to%name = from%name
        if (allocated(from%dbl_value)) allocate(to%dbl_value,source=from%dbl_value)
        if (allocated(from%log_value)) allocate(to%log_value,source=from%log_value)
        if (allocated(from%str_value)) to%str_value = from%str_value
        if (allocated(from%int_value)) allocate(to%int_value,source=from%int_value)
        to%var_type   = from%var_type
        to%n_children = from%n_children

        !allocate and associate the pointers as necessary:

        if (present(parent))      to%parent      => parent
        if (present(previous))    to%previous    => previous
        if (present(next))        to%next        => next
        if (present(children))    to%children    => children
        if (present(tail)) then
            if (tail) to%parent%tail => to
        end if

        if (associated(from%next)) then
            allocate(to%next)
            call json_value_clone_func(from%next,&
                                       to%next,&
                                       previous=to,&
                                       parent=to%parent,&
                                       tail=(.not. associated(from%next%next)))
        end if

        if (associated(from%children)) then
            allocate(to%children)
            call json_value_clone_func(from%children,&
                                       to%children,&
                                       parent=to,&
                                       tail=(.not. associated(from%children%next)))
        end if

    end if

    end subroutine json_value_clone_func
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Destroy the data within a [[json_value]], and reset type to `json_unknown`.

    subroutine destroy_json_data(d)

    implicit none

    type(json_value),intent(inout) :: d

    d%var_type = json_unknown

    if (allocated(d%log_value)) deallocate(d%log_value)
    if (allocated(d%int_value)) deallocate(d%int_value)
    if (allocated(d%dbl_value)) deallocate(d%dbl_value)
    if (allocated(d%str_value)) deallocate(d%str_value)

    end subroutine destroy_json_data
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 2/13/2014
!
!  Returns information about a [[json_value]].

    subroutine json_info(json,p,var_type,n_children,name)

    implicit none

    class(json_core),intent(inout)   :: json
    type(json_value),pointer         :: p
    integer(IK),intent(out),optional :: var_type   !! variable type
    integer(IK),intent(out),optional :: n_children !! number of children
    character(kind=CK,len=:),allocatable,intent(out),optional :: name !! variable name

    if (present(var_type))    var_type   = p%var_type
    if (present(n_children))  n_children = json%count(p)
    if (present(name)) then
        if (allocated(p%name)) then
            name = p%name
        else
            name = ''
        end if
    end if

    end subroutine json_info
!*****************************************************************************************

!*****************************************************************************************
!
!  Returns information about a [[json_value]], given the path.
!
!### See also
!  * [[json_info]]
!
!@note If `found` is present, no exceptions will be thrown if an
!      error occurs. Otherwise, an exception will be thrown if the
!      variable is not found.

    subroutine json_info_by_path(json,p,path,found,var_type,n_children,name)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer,intent(in)  :: p          !! a JSON linked list
    character(kind=CK,len=*),intent(in)  :: path       !! path to the variable
    logical(LK),intent(out),optional     :: found      !! true if it was found
    integer(IK),intent(out),optional     :: var_type   !! variable type
    integer(IK),intent(out),optional     :: n_children !! number of children
    character(kind=CK,len=:),allocatable,intent(out),optional :: name !! variable name

    type(json_value),pointer :: p_var
    logical(LK) :: ok
#if defined __GFORTRAN__
    character(kind=CK,len=:),allocatable :: p_name
#endif

    call json%get(p,path,p_var,found)

    !check if it was found:
    if (present(found)) then
        ok = found
    else
        ok = .not. json%failed()
    end if

    if (.not. ok) then
        if (present(var_type))   var_type   = json_unknown
        if (present(n_children)) n_children = 0
        if (present(name))       name       = ''
    else
        !get info:

#if defined __GFORTRAN__
        call json%info(p_var,var_type,n_children)
        if (present(name)) then !workaround for gfortran bug
            if (allocated(p_var%name)) then
                p_name = p_var%name
                name = p_name
            else
                name = ''
            end if
        end if
#else
        call json%info(p_var,var_type,n_children,name)
#endif

    end if

    end subroutine json_info_by_path
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_info_by_path]] where "path" is kind=CDK.

    subroutine wrap_json_info_by_path(json,p,path,found,var_type,n_children,name)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer,intent(in)  :: p          !! a JSON linked list
    character(kind=CDK,len=*),intent(in) :: path       !! path to the variable
    logical(LK),intent(out),optional     :: found      !! true if it was found
    integer(IK),intent(out),optional     :: var_type   !! variable type
    integer(IK),intent(out),optional     :: n_children !! number of children
    character(kind=CK,len=:),allocatable,intent(out),optional :: name !! variable name

    call json%info(p,to_unicode(path),found,var_type,n_children,name)

    end subroutine wrap_json_info_by_path
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 10/16/2015
!
!  Alternate version of [[json_info]] that returns matrix
!  information about a [[json_value]].
!
!  A [[json_value]] is a valid rank 2 matrix if all of the following are true:
!
!  * The var_type is *json_array*
!  * Each child is also a *json_array*, each of which has the same number of elements
!  * Each individual element has the same variable type (integer, logical, etc.)
!
!  The idea here is that if it is a valid matrix, it can be interoperable with
!  a Fortran rank 2 array of the same type.
!
!# Example
!
!  The following example is an array with `var_type=json_integer`, `n_sets=3`, and `set_size=4`
!
!```json
!    {
!        "matrix": [
!            [1,2,3,4],
!            [5,6,7,8],
!            [9,10,11,12]
!        ]
!    }
!```

    subroutine json_matrix_info(json,p,is_matrix,var_type,n_sets,set_size,name)

    implicit none

    class(json_core),intent(inout)   :: json
    type(json_value),pointer         :: p          !! a JSON linked list
    logical(LK),intent(out)          :: is_matrix  !! true if it is a valid matrix
    integer(IK),intent(out),optional :: var_type   !! variable type of data in the matrix (if all elements have the same type)
    integer(IK),intent(out),optional :: n_sets     !! number of data sets (i.e., matrix rows if using row-major order)
    integer(IK),intent(out),optional :: set_size   !! size of each data set (i.e., matrix cols if using row-major order)
    character(kind=CK,len=:),allocatable,intent(out),optional :: name !! variable name

    type(json_value),pointer :: p_row       !! for getting a set
    type(json_value),pointer :: p_element   !! for getting an element in a set
    integer(IK) :: vartype         !! json variable type of `p`
    integer(IK) :: row_vartype     !! json variable type of a row
    integer(IK) :: element_vartype !! json variable type of an element in a row
    integer(IK) :: nr              !! number of children of `p`
    integer(IK) :: nc              !! number of elements in first child of `p`
    integer(IK) :: icount          !! number of elements in a set
    integer     :: i               !! counter
    integer     :: j               !! counter
#if defined __GFORTRAN__
    character(kind=CK,len=:),allocatable :: p_name
#endif

    !get info about the variable:
#if defined __GFORTRAN__
    call json%info(p,vartype,nr)
    if (present(name)) then !workaround for gfortran bug
        if (allocated(p%name)) then
            p_name = p%name
            name = p_name
        else
            name = ''
        end if
    end if
#else
    call json%info(p,vartype,nr,name)
#endif

    is_matrix = (vartype==json_array)

    if (is_matrix) then

        main : do i=1,nr

            nullify(p_row)
            call json%get_child(p,i,p_row)
            if (.not. associated(p_row)) then
                is_matrix = .false.
                call json%throw_exception('Error in json_matrix_info: '//&
                                          'Malformed JSON linked list')
                exit main
            end if
            call json%info(p_row,var_type=row_vartype,n_children=icount)

            if (row_vartype==json_array) then
                if (i==1) nc = icount  !number of columns in first row
                if (icount==nc) then   !make sure each row has the same number of columns
                    !see if all the variables in this row are the same type:
                    do j=1,icount
                        nullify(p_element)
                        call json%get_child(p_row,j,p_element)
                        if (.not. associated(p_element)) then
                            is_matrix = .false.
                            call json%throw_exception('Error in json_matrix_info: '//&
                                                      'Malformed JSON linked list')
                            exit main
                        end if
                        call json%info(p_element,var_type=element_vartype)
                        if (i==1 .and. j==1) vartype = element_vartype  !type of first element
                                                                        !in the row
                        if (vartype/=element_vartype) then
                            !not all variables are the same time
                            is_matrix = .false.
                            exit main
                        end if
                    end do
                else
                    is_matrix = .false.
                    exit main
                end if
            else
                is_matrix = .false.
                exit main
            end if

        end do main

    end if

    if (is_matrix) then
        if (present(var_type)) var_type = vartype
        if (present(n_sets))   n_sets   = nr
        if (present(set_size)) set_size = nc
    else
        if (present(var_type)) var_type = json_unknown
        if (present(n_sets))   n_sets   = 0
        if (present(set_size)) set_size = 0
    end if

    end subroutine json_matrix_info
!*****************************************************************************************

!*****************************************************************************************
!>
!  Returns matrix information about a [[json_value]], given the path.
!
!### See also
!  * [[json_matrix_info]]
!
!@note If `found` is present, no exceptions will be thrown if an
!      error occurs. Otherwise, an exception will be thrown if the
!      variable is not found.

    subroutine json_matrix_info_by_path(json,p,path,is_matrix,found,&
                                        var_type,n_sets,set_size,name)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer            :: p         !! a JSON linked list
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

    type(json_value),pointer :: p_var
    logical(LK) :: ok
#if defined __GFORTRAN__
    character(kind=CK,len=:),allocatable :: p_name
#endif

    call json%get(p,path,p_var,found)

    !check if it was found:
    if (present(found)) then
        ok = found
    else
        ok = .not. json%failed()
    end if

    if (.not. ok) then
        if (present(var_type)) var_type = json_unknown
        if (present(n_sets))   n_sets   = 0
        if (present(set_size)) set_size = 0
        if (present(name))     name     = ''
    else

        !get info about the variable:
#if defined __GFORTRAN__
        call json%matrix_info(p_var,is_matrix,var_type,n_sets,set_size)
        if (present(name)) then !workaround for gfortran bug
            if (allocated(p_var%name)) then
                p_name = p_var%name
                name = p_name
            else
                name = ''
            end if
        end if
#else
        call json%matrix_info(p_var,is_matrix,var_type,n_sets,set_size,name)
#endif
        if (json%failed() .and. present(found)) then
            found = .false.
            call json%clear_exceptions()
        end if
    end if

    end subroutine json_matrix_info_by_path
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_matrix_info_by_path]] where "path" is kind=CDK.

    subroutine wrap_json_matrix_info_by_path(json,p,path,is_matrix,found,&
                                             var_type,n_sets,set_size,name)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer             :: p          !! a JSON linked list
    character(kind=CDK,len=*),intent(in) :: path       !! path to the variable
    logical(LK),intent(out)              :: is_matrix  !! true if it is a valid matrix
    logical(LK),intent(out),optional     :: found      !! true if it was found
    integer(IK),intent(out),optional     :: var_type   !! variable type of data in
                                                       !! the matrix (if all elements have
                                                       !! the same type)
    integer(IK),intent(out),optional     :: n_sets     !! number of data sets (i.e., matrix
                                                       !! rows if using row-major order)
    integer(IK),intent(out),optional     :: set_size   !! size of each data set (i.e., matrix
                                                       !! cols if using row-major order)
    character(kind=CK,len=:),allocatable,intent(out),optional :: name !! variable name

    call json%matrix_info(p,to_unicode(path),is_matrix,found,var_type,n_sets,set_size,name)

    end subroutine wrap_json_matrix_info_by_path
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 4/29/2016
!
!  Rename a [[json_value]].

    subroutine json_value_rename(json,p,name)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer,intent(in) :: p
    character(kind=CK,len=*),intent(in) :: name !! new variable name

    p%name = name

    end subroutine json_value_rename
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 4/29/2016
!
!  Alternate version of [[json_value_rename]], where `name` is kind=CDK.

    subroutine wrap_json_value_rename(json,p,name)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer,intent(in)  :: p
    character(kind=CDK,len=*),intent(in) :: name !! new variable name

    call json%rename(p,to_unicode(name))

    end subroutine wrap_json_value_rename
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 12/4/2013
!
!  Clear exceptions in the [[json_core]].

    pure subroutine json_clear_exceptions(json)

    implicit none

    class(json_core),intent(inout)  :: json

    !clear the flag and message:
    json%exception_thrown = .false.
    json%err_message = ''

    end subroutine json_clear_exceptions
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 12/4/2013
!
!  Throw an exception in the [[json_core]].
!  This routine sets the error flag, and prevents any subsequent routine
!  from doing anything, until [[json_clear_exceptions]] is called.
!
!@note If `is_verbose` is true, this will also print a
!      traceback if the Intel compiler is used.

    subroutine json_throw_exception(json,msg)

#ifdef __INTEL_COMPILER
    use ifcore, only: tracebackqq
#endif

    implicit none

    class(json_core),intent(inout)      :: json
    character(kind=CK,len=*),intent(in) :: msg    !! the error message

    json%exception_thrown = .true.
    json%err_message = trim(msg)

    if (json%is_verbose) then
        write(output_unit,'(A)') '***********************'
        write(output_unit,'(A)') 'JSON-Fortran Exception: '//trim(msg)
        !call backtrace()     ! gfortran (use -fbacktrace -fall-intrinsics flags)
#ifdef __INTEL_COMPILER
        call tracebackqq(user_exit_code=-1)  ! print a traceback and return
#endif
        write(output_unit,'(A)') '***********************'
    end if

    end subroutine json_throw_exception
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_throw_exception]], where `msg` is kind=CDK.

    subroutine wrap_json_throw_exception(json,msg)

    implicit none

    class(json_core),intent(inout)  :: json
    character(kind=CDK,len=*),intent(in) :: msg    !! the error message

    call json%throw_exception(to_unicode(msg))

    end subroutine wrap_json_throw_exception
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 12/4/2013
!
!  Retrieve error code from the [[json_core]].
!  This should be called after `parse` to check for errors.
!  If an error is thrown, before using the class again, [[json_initialize]]
!  should be called to clean up before it is used again.
!
!# Example
!
!````fortran
!     type(json_file) :: json
!     logical :: status_ok
!     character(kind=CK,len=:),allocatable :: error_msg
!     call json%load_file(filename='myfile.json')
!     call json%check_for_errors(status_ok, error_msg)
!     if (.not. status_ok) then
!         write(*,*) 'Error: '//error_msg
!         call json%clear_exceptions()
!         call json%destroy()
!     end if
!````
!
!# See also
!  * [[json_failed]]

    subroutine json_check_for_errors(json,status_ok,error_msg)

    implicit none

    class(json_core),intent(inout) :: json
    logical(LK),intent(out) :: status_ok !! true if there were no errors
    character(kind=CK,len=:),allocatable,intent(out) :: error_msg !! the error message (if there were errors)

    status_ok = .not. json%exception_thrown

    if (.not. status_ok) then
        if (allocated(json%err_message)) then
            error_msg = json%err_message
        else
            error_msg = 'Unknown error.'
        end if
    else
        error_msg = ''
    end if

    end subroutine json_check_for_errors
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 12/5/2013
!
!  Logical function to indicate if an exception has been thrown in a [[json_core]].
!
!# Example
!
!````fortran
!    type(json_core) :: json
!    type(json_value),pointer :: p
!    logical :: status_ok
!    character(len=:),allocatable :: error_msg
!    call json%parse(filename='myfile.json',p)
!    if (json%failed()) then
!        call json%check_for_errors(status_ok, error_msg)
!        write(*,*) 'Error: '//error_msg
!        call json%clear_exceptions()
!        call json%destroy(p)
!    end if
!````
!
!  Note that [[json_file]] contains a wrapper for this routine, which is used like:
!````fortran
!    type(json_file) :: f
!    logical :: status_ok
!    character(len=:),allocatable :: error_msg
!    call f%load_file(filename='myfile.json')
!    if (f%failed()) then
!        call f%check_for_errors(status_ok, error_msg)
!        write(*,*) 'Error: '//error_msg
!        call f%clear_exceptions()
!        call f%destroy()
!    end if
!````
!
!# See also
!  * [[json_check_for_errors]]

    pure function json_failed(json) result(failed)

    implicit none

    class(json_core),intent(in) :: json
    logical(LK)                 :: failed  !! will be true if an exception
                                           !! has been thrown.

    failed = json%exception_thrown

    end function json_failed
!*****************************************************************************************

!*****************************************************************************************
!>
!  Allocate a [[json_value]] pointer variable.
!  This should be called before adding data to it.
!
!# Example
!
!````fortran
!    type(json_value),pointer :: var
!    call json_value_create(var)
!    call to_double(var,1.0_RK)
!````
!
!# Notes
!  1. This routine does not check for exceptions.
!  2. The pointer should not already be allocated, or a memory leak will occur.

    subroutine json_value_create(p)

    implicit none

    type(json_value),pointer :: p

    nullify(p)
    allocate(p)

    end subroutine json_value_create
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/22/2014
!
!  Destroy a [[json_value]] linked-list structure.
!
!@note The original FSON version of this
!      routine was not properly freeing the memory.
!      It was rewritten.

    recursive subroutine json_value_destroy(json,p,destroy_next)

    implicit none

    class(json_core),intent(inout)  :: json
    type(json_value),pointer        :: p            !! variable to destroy
    logical(LK),intent(in),optional :: destroy_next !! if true, then `p%next`
                                                    !! is also destroyed (default is true)

    logical(LK) :: des_next
    type(json_value), pointer :: child

    if (associated(p)) then

        if (present(destroy_next)) then
            des_next = destroy_next
        else
            des_next = .true.
        end if

        if (allocated(p%name)) deallocate(p%name)

        call destroy_json_data(p)

        if (associated(p%children)) then
            do while (p%n_children > 0)
                child => p%children
                if (associated(child)) then
                    p%children => p%children%next
                    p%n_children = p%n_children - 1
                    call json_value_destroy(json,child,.false.)
                else
                    call json%throw_exception('Error in json_value_destroy: '//&
                                              'Malformed JSON linked list')
                    exit
                end if
            end do
            nullify(p%children)
            nullify(child)
        end if

        if (associated(p%next) .and. des_next) call json_value_destroy(json,p%next)

        if (associated(p%previous)) nullify(p%previous)
        if (associated(p%parent))   nullify(p%parent)
        if (associated(p%tail))     nullify(p%tail)

        deallocate(p)
        nullify(p)

    end if

    end subroutine json_value_destroy
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 9/9/2014
!
!  Remove a [[json_value]] (and all its children)
!  from a linked-list structure, preserving the rest of the structure.
!
!# Examples
!
!  To extract an object from one JSON structure, and add it to another:
!````fortran
!     type(json_core) :: json
!     type(json_value),pointer :: json1,json2,p
!     logical :: found
!     !create and populate json1 and json2
!     call json%get(json1,'name',p,found)  ! get pointer to name element of json1
!     call json%remove(p,destroy=.false.)  ! remove it from json1 (don't destroy)
!     call json%add(json2,p)               ! add it to json2
!````
!
!  To remove an object from a JSON structure (and destroy it):
!````fortran
!     type(json_core) :: json
!     type(json_value),pointer :: json1,p
!     logical :: found
!     !create and populate json1
!     call json%get(json1,'name',p,found)  ! get pointer to name element of json1
!     call json%remove(p)                  ! remove and destroy it
!````
!
!# History
!  * Jacob Williams : 12/28/2014 : added destroy optional argument.

    subroutine json_value_remove(json,p,destroy)

    implicit none

    class(json_core),intent(inout)  :: json
    type(json_value),pointer        :: p
    logical(LK),intent(in),optional :: destroy  !! If destroy is not present, it is also destroyed.
                                                !! If destroy is present and true, it is destroyed.
                                                !! If destroy is present and false, it is not destroyed.

    type(json_value),pointer :: parent,previous,next
    logical(LK) :: destroy_it

    if (associated(p)) then

        !optional input argument:
        if (present(destroy)) then
            destroy_it = destroy
        else
            destroy_it = .true.
        end if

        if (associated(p%parent)) then

            parent => p%parent

            if (associated(p%next)) then

                !there are later items in the list:

                next => p%next
                nullify(p%next)

                if (associated(p%previous)) then
                    !there are earlier items in the list
                    previous => p%previous
                    previous%next => next
                    next%previous => previous
                else
                    !this is the first item in the list
                    parent%children => next
                    nullify(next%previous)
                end if

            else

                if (associated(p%previous)) then
                    !there are earlier items in the list:
                    previous => p%previous
                    nullify(previous%next)
                    parent%tail => previous
                else
                    !this is the only item in the list:
                    nullify(parent%children)
                    nullify(parent%tail)
                end if

            end if

            parent%n_children = parent%n_children - 1

        end if

        if (destroy_it) call json%destroy(p)

    end if

    end subroutine json_value_remove
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 4/26/2016
!
!  Swap two elements in a JSON structure.
!  All of the children are carried along as well.
!
!@note If both are not associated, then an error is thrown.
!
!@note The assumption here is that both variables are part of a valid
!      [[json_value]] linked list (so the normal `parent`, `previous`,
!      `next`, etc. pointers are properly associated if necessary).
!
!@warning This cannot be used to swap a parent/child pair, since that
!         could lead to a circular linkage. An exception is thrown if
!         this is tried.
!
!@warning There are also other situations where using this routine may
!         produce a malformed JSON structure, such as moving an array
!         element outside of an array. This is not checked for.
!
!@note If `p1` and `p2` have a common parent, it is always safe to swap them.

    subroutine json_value_swap(json,p1,p2)

    implicit none

    class(json_core),intent(inout) :: json
    type(json_value),pointer       :: p1
    type(json_value),pointer       :: p2

    logical :: same_parent,first_last,adjacent
    type(json_value),pointer :: a,b

    if (json%exception_thrown) return

    !both have to be associated:
    if (associated(p1) .and. associated(p2)) then

        !simple check to make sure that they both
        !aren't pointing to the same thing:
        if (.not. associated(p1,p2)) then

            !we will not allow swapping an item with one of its descendants:
            if (json%is_child_of(p1,p2) .or. json%is_child_of(p2,p1)) then
                call json%throw_exception('Error in json_value_swap: '//&
                                          'cannot swap an item with one of its descendants')
            else

                same_parent = ( associated(p1%parent) .and. &
                                associated(p2%parent) .and. &
                                associated(p1%parent,p2%parent) )
                if (same_parent) then
                    !if p1,p2 are the first,last or last,first
                    !children of a common parent
                    first_last = (associated(p1%parent%children,p1) .and. &
                                  associated(p2%parent%tail,p2)) .or. &
                                 (associated(p1%parent%tail,p1) .and. &
                                  associated(p2%parent%children,p2))
                else
                    first_last = .false.
                end if

                !first, we fix children,tail pointers:

                if (same_parent .and. first_last) then

                    !this is all we have to do for the parent in this case:
                    call swap_pointers(p1%parent%children,p2%parent%tail)

                else if (same_parent .and. .not. first_last) then

                    if (associated(p1%parent%children,p1)) then
                        p1%parent%children => p2 ! p1 is the first child of the parent
                    else if (associated(p1%parent%children,p2)) then
                        p1%parent%children => p1 ! p2 is the first child of the parent
                    end if
                    if (associated(p1%parent%tail,p1)) then
                        p1%parent%tail => p2 ! p1 is the last child of the parent
                    else if (associated(p1%parent%tail,p2)) then
                        p1%parent%tail => p1 ! p2 is the last child of the parent
                    end if

                else ! general case: different parents

                    if (associated(p1%parent)) then
                        if (associated(p1%parent%children,p1)) p1%parent%children => p2
                        if (associated(p1%parent%tail,p1))     p1%parent%tail     => p2
                    end if
                    if (associated(p2%parent)) then
                        if (associated(p2%parent%children,p2)) p2%parent%children => p1
                        if (associated(p2%parent%tail,p2))     p2%parent%tail     => p1
                    end if
                    call swap_pointers(p1%parent, p2%parent)

                end if

                !now, have to fix previous,next pointers:

                !first, see if they are adjacent:
                adjacent = associated(p1%next,p2) .or. &
                           associated(p2%next,p1)
                if (associated(p2%next,p1)) then    !p2,p1
                    a => p2
                    b => p1
                else    !p1,p2 (or not adjacent)
                    a => p1
                    b => p2
                end if
                if (associated(a%previous)) a%previous%next => b
                if (associated(b%next))     b%next%previous => a

                if (adjacent) then
                    !a comes before b in the original list
                    b%previous => a%previous
                    a%next     => b%next
                    a%previous => b
                    b%next     => a
                else
                    if (associated(a%next))       a%next%previous => b
                    if (associated(b%previous))   b%previous%next => a
                    call swap_pointers(a%previous,b%previous)
                    call swap_pointers(a%next,    b%next)
                end if

            end if

        else
            call json%throw_exception('Error in json_value_swap: '//&
                                      'both pointers must be associated')
        end if

    end if

    contains

        pure subroutine swap_pointers(s1,s2)

        implicit none

        type(json_value),pointer,intent(inout) :: s1
        type(json_value),pointer,intent(inout) :: s2

        type(json_value),pointer :: tmp  !! temporary pointer

        if (.not. associated(s1,s2)) then
            tmp => s1
            s1  => s2
            s2  => tmp
        end if

        end subroutine swap_pointers

    end subroutine json_value_swap
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 4/28/2016
!
!  Returns True if `p2` is a descendant of `p1`
!  (i.e, a child, or a child of child, etc.)

    function json_value_is_child_of(json,p1,p2) result(is_child_of)

    implicit none

    class(json_core),intent(inout) :: json
    type(json_value),pointer       :: p1
    type(json_value),pointer       :: p2
    logical(LK)                    :: is_child_of

    is_child_of = .false.

    if (json%exception_thrown) return

    if (associated(p1) .and. associated(p2)) then
        if (associated(p1%children)) then
            call json%traverse(p1%children,is_child_of_callback)
        end if
    end if

    contains

    subroutine is_child_of_callback(json,p,finished)
    !! Traverse until `p` is `p2`.
    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer,intent(in) :: p
    logical(LK),intent(out)             :: finished

    is_child_of = associated(p,p2)
    finished = is_child_of  ! stop searching if found

    end subroutine is_child_of_callback

    end function json_value_is_child_of
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 5/2/2016
!
!  Validate a [[json_value]] linked list by checking to make sure
!  all the pointers are properly associated, arrays and objects
!  have the correct number of children, and the correct data is
!  allocated for the variable types.
!
!  It recursively traverses the entire structure and checks every element.
!
!@note This routine does not check or throw any exceptions.

    subroutine json_value_validate(json,p,is_valid,error_msg)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer,intent(in)  :: p
    logical(LK),intent(out)              :: is_valid  !! True if the structure is valid.
    character(kind=CK,len=:),allocatable,intent(out) :: error_msg !! if not valid, this will contain
                                                                  !! a description of the problem

    if (associated(p)) then
        is_valid = .true.
        call check_if_valid(p,require_parent=associated(p%parent))
    else
        error_msg = 'The pointer is not associated'
        is_valid = .false.
    end if

    contains

    recursive subroutine check_if_valid(p,require_parent)

        implicit none

        type(json_value),pointer,intent(in) :: p
        logical,intent(in) :: require_parent !! the first one may be a root (so no parent),
                                             !! but all descendants must have a parent.

        integer :: i !! counter
        type(json_value),pointer :: element
        type(json_value),pointer :: previous

        if (is_valid .and. associated(p)) then

            ! data type:
            select case (p%var_type)
            case(json_null,json_object,json_array)
                if (allocated(p%log_value) .or. allocated(p%int_value) .or. &
                    allocated(p%dbl_value) .or. allocated(p%str_value)) then
                    error_msg = 'incorrect data allocated for '//&
                                'json_null, json_object, or json_array variable type'
                    is_valid = .false.
                    return
                end if
            case(json_logical)
                if (.not. allocated(p%log_value)) then
                    error_msg = 'log_value should be allocated for json_logical variable type'
                    is_valid = .false.
                    return
                else if (allocated(p%int_value) .or. &
                    allocated(p%dbl_value) .or. allocated(p%str_value)) then
                    error_msg = 'incorrect data allocated for json_logical variable type'
                    is_valid = .false.
                    return
                end if
            case(json_integer)
                if (.not. allocated(p%int_value)) then
                    error_msg = 'int_value should be allocated for json_integer variable type'
                    is_valid = .false.
                    return
                else if (allocated(p%log_value) .or. &
                    allocated(p%dbl_value) .or. allocated(p%str_value)) then
                    error_msg = 'incorrect data allocated for json_integer variable type'
                    is_valid = .false.
                    return
                end if
            case(json_double)
                if (.not. allocated(p%dbl_value)) then
                    error_msg = 'dbl_value should be allocated for json_double variable type'
                    is_valid = .false.
                    return
                else if (allocated(p%log_value) .or. allocated(p%int_value) .or. &
                    allocated(p%str_value)) then
                    error_msg = 'incorrect data allocated for json_double variable type'
                    is_valid = .false.
                    return
                end if
            case(json_string)
                if (.not. allocated(p%str_value)) then
                    error_msg = 'str_value should be allocated for json_string variable type'
                    is_valid = .false.
                    return
                else if (allocated(p%log_value) .or. allocated(p%int_value) .or. &
                    allocated(p%dbl_value)) then
                    error_msg = 'incorrect data allocated for json_string variable type'
                    is_valid = .false.
                    return
                end if
            case default
                error_msg = 'invalid JSON variable type'
                is_valid = .false.
                return
            end select

            if (require_parent .and. .not. associated(p%parent)) then
                error_msg = 'parent pointer is not associated'
                is_valid = .false.
                return
            end if

            if (.not. allocated(p%name)) then
                if (associated(p%parent)) then
                    if (p%parent%var_type/=json_array) then
                        error_msg = 'JSON variable must have a name if not an '//&
                                    'array element or the root'
                        is_valid = .false.
                        return
                    end if
                end if
            end if

            if (associated(p%children) .neqv. associated(p%tail)) then
                error_msg = 'both children and tail pointers must be associated'
                is_valid = .false.
                return
            end if

            ! now, check next one:
            if (associated(p%next)) then
                call check_if_valid(p%next,require_parent=require_parent)
            end if

            if (associated(p%children)) then

                if (p%var_type/=json_array .and. p%var_type/=json_object) then
                    error_msg = 'only arrays and objects can have children'
                    is_valid = .false.
                    return
                end if

                ! first validate children pointers:

                previous => null()
                element => p%children
                do i = 1, p%n_children
                    if (.not. associated(element%parent,p)) then
                        error_msg = 'child''s parent pointer not properly associated'
                        is_valid = .false.
                        return
                    end if
                    if (i==1 .and. associated(element%previous)) then
                        error_msg = 'first child shouldn''t have a previous'
                        is_valid = .false.
                        return
                    end if
                    if (i<p%n_children .and. .not. associated(element%next)) then
                        error_msg = 'not enough children'
                        is_valid = .false.
                        return
                    end if
                    if (i==p%n_children .and. associated(element%next)) then
                        error_msg = 'too many children'
                        is_valid = .false.
                        return
                    end if
                    if (i>1) then
                        if (.not. associated(previous,element%previous)) then
                            error_msg = 'previous pointer not properly associated'
                            is_valid = .false.
                            return
                        end if
                    end if
                    if (i==p%n_children .and. &
                        .not. associated(element%parent%tail,element)) then
                        error_msg = 'parent''s tail pointer not properly associated'
                        is_valid = .false.
                        return
                    end if
                    if (i<p%n_children) then
                        !setup next case:
                        previous => element
                        element => element%next
                    end if
                end do

                !now check all the children:
                call check_if_valid(p%children,require_parent=.true.)

            end if

        end if

    end subroutine check_if_valid

    end subroutine json_value_validate
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 12/6/2014
!
!  Given the path string, remove the variable from
!  the [[json_value]] structure, if it exists.

    subroutine json_value_remove_if_present(json,p,name)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer            :: p
    character(kind=CK,len=*),intent(in) :: name

    type(json_value),pointer :: p_var
    logical(LK) :: found

    call json%get(p,name,p_var,found)
    if (found) call json%remove(p_var)

    end subroutine json_value_remove_if_present
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_value_remove_if_present]], where `name` is kind=CDK.

    subroutine wrap_json_value_remove_if_present(json,p,name)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer             :: p
    character(kind=CDK,len=*),intent(in) :: name

    call json%remove_if_present(p,to_unicode(name))

    end subroutine wrap_json_value_remove_if_present
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 12/6/2014
!
!  Given the path string, if the variable is present,
!  and is a scalar, then update its value.
!  If it is not present, then create it and set its value.

    subroutine json_update_logical(json,p,name,val,found)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer            :: p
    character(kind=CK,len=*),intent(in) :: name
    logical(LK),intent(in)              :: val
    logical(LK),intent(out)             :: found

    type(json_value),pointer :: p_var
    integer(IK) :: var_type

    call json%get(p,name,p_var,found)
    if (found) then

        call json%info(p_var,var_type)
        select case (var_type)
        case (json_null,json_logical,json_integer,json_double,json_string)
            call to_logical(p_var,val)    !update the value
        case default
            found = .false.
            call json%throw_exception('Error in json_update_logical: '//&
                                      'the variable is not a scalar value')
        end select

    else
        call json%add(p,name,val)   !add the new element
    end if

    end subroutine json_update_logical
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_update_logical]], where `name` is kind=CDK.

    subroutine wrap_json_update_logical(json,p,name,val,found)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer             :: p
    character(kind=CDK,len=*),intent(in) :: name
    logical(LK),intent(in)               :: val
    logical(LK),intent(out)              :: found

    call json%update(p,to_unicode(name),val,found)

    end subroutine wrap_json_update_logical
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 12/6/2014
!
!  Given the path string, if the variable is present,
!  and is a scalar, then update its value.
!  If it is not present, then create it and set its value.

    subroutine json_update_double(json,p,name,val,found)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer            :: p
    character(kind=CK,len=*),intent(in) :: name
    real(RK),intent(in)                 :: val
    logical(LK),intent(out)             :: found

    type(json_value),pointer :: p_var
    integer(IK) :: var_type

    call json%get(p,name,p_var,found)
    if (found) then

        call json%info(p_var,var_type)
        select case (var_type)
        case (json_null,json_logical,json_integer,json_double,json_string)
            call to_double(p_var,val)    !update the value
        case default
            found = .false.
            call json%throw_exception('Error in json_update_double: '//&
                                      'the variable is not a scalar value')
        end select

    else
        call json%add(p,name,val)   !add the new element
    end if

    end subroutine json_update_double
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_update_double]], where `name` is kind=CDK.

    subroutine wrap_json_update_double(json,p,name,val,found)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer             :: p
    character(kind=CDK,len=*),intent(in) :: name
    real(RK),intent(in)                  :: val
    logical(LK),intent(out)              :: found

    call json%update(p,to_unicode(name),val,found)

    end subroutine wrap_json_update_double
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 12/6/2014
!
!  Given the path string, if the variable is present,
!  and is a scalar, then update its value.
!  If it is not present, then create it and set its value.

    subroutine json_update_integer(json,p,name,val,found)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer            :: p
    character(kind=CK,len=*),intent(in) :: name
    integer(IK),intent(in)              :: val
    logical(LK),intent(out)             :: found

    type(json_value),pointer :: p_var
    integer(IK) :: var_type

    call json%get(p,name,p_var,found)
    if (found) then

        call json%info(p_var,var_type)
        select case (var_type)
        case (json_null,json_logical,json_integer,json_double,json_string)
            call to_integer(p_var,val)    !update the value
        case default
            found = .false.
            call json%throw_exception('Error in json_update_integer: '//&
                                      'the variable is not a scalar value')
        end select

    else
        call json%add(p,name,val)   !add the new element
    end if

    end subroutine json_update_integer
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_update_integer]], where `name` is kind=CDK.

    subroutine wrap_json_update_integer(json,p,name,val,found)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer             :: p
    character(kind=CDK,len=*),intent(in) :: name
    integer(IK),intent(in)               :: val
    logical(LK),intent(out)              :: found

    call json%update(p,to_unicode(name),val,found)

    end subroutine wrap_json_update_integer
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 12/6/2014
!
!  Given the path string, if the variable is present,
!  and is a scalar, then update its value.
!  If it is not present, then create it and set its value.

    subroutine json_update_string(json,p,name,val,found)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer            :: p
    character(kind=CK,len=*),intent(in) :: name
    character(kind=CK,len=*),intent(in) :: val
    logical(LK),intent(out)             :: found

    type(json_value),pointer :: p_var
    integer(IK) :: var_type

    call json%get(p,name,p_var,found)
    if (found) then

        call json%info(p_var,var_type)
        select case (var_type)
        case (json_null,json_logical,json_integer,json_double,json_string)
            call to_string(p_var,val)    !update the value
        case default
            found = .false.
            call json%throw_exception('Error in json_update_string: '//&
                                      'the variable is not a scalar value')
        end select

    else
        call json%add(p,name,val)   !add the new element
    end if

    end subroutine json_update_string
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_update_string]], where `name` and `value` are kind=CDK.

    subroutine wrap_json_update_string(json,p,name,val,found)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer             :: p
    character(kind=CDK,len=*),intent(in) :: name
    character(kind=CDK,len=*),intent(in) :: val
    logical(LK),intent(out)              :: found

    call json%update(p,to_unicode(name),to_unicode(val),found)

    end subroutine wrap_json_update_string
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_update_string]], where `name` is kind=CDK.

    subroutine json_update_string_name_ascii(json,p,name,val,found)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer             :: p
    character(kind=CDK,len=*),intent(in) :: name
    character(kind=CK, len=*),intent(in) :: val
    logical(LK),intent(out)              :: found

    call json%update(p,to_unicode(name),val,found)

    end subroutine json_update_string_name_ascii
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_update_string]], where `val` is kind=CDK.

    subroutine json_update_string_val_ascii(json,p,name,val,found)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer             :: p
    character(kind=CK, len=*),intent(in) :: name
    character(kind=CDK,len=*),intent(in) :: val
    logical(LK),intent(out)              :: found

    call json%update(p,name,to_unicode(val),found)

    end subroutine json_update_string_val_ascii
!*****************************************************************************************

!*****************************************************************************************
!>
!  Adds `member` as a child of `p`.

    subroutine json_value_add_member(json,p,member)

    implicit none

    class(json_core),intent(inout) :: json
    type(json_value),pointer       :: p
    type(json_value),pointer       :: member  !! the child member to add

    if (.not. json%exception_thrown) then

        ! associate the parent
        member%parent => p

        ! add to linked list
        if (associated(p%children)) then

            p%tail%next => member
            member%previous => p%tail

        else

            p%children => member
            member%previous => null()  !first in the list

        end if

        ! new member is now the last one in the list
        p%tail => member
        p%n_children = p%n_children + 1

    end if

    end subroutine json_value_add_member
!*****************************************************************************************

!*****************************************************************************************
!>
!  Inserts `element` after `p`, and updates the JSON structure accordingly.
!
!### Example
!
!````fortran
!  program test
!   use json_module
!   implicit none
!   logical(json_LK) :: found
!   type(json_core) :: json
!   type(json_value),pointer :: p,new,element
!   call json%parse(file='myfile.json', p=p)
!   call json%get(p,'x(3)',element,found) ! get pointer to an array element in the file
!   call json%create_integer(new,1,'')    ! create a new element
!   call json%insert_after(element,new)   ! insert new element after x(3)
!   call json%print(p,'myfile2.json')     ! write it to a file
!   call json%destroy(p)                  ! cleanup
!  end program test
!````
!
!### Details
!
!  * This routine can be used to insert a new element (or set of elements)
!    into an array or object at a specific index.
!    See [[json_value_insert_after_child_by_index]]
!  * Children and subsequent elements of `element` are carried along.
!  * If the inserted elements are part of an existing list, then
!    they are removed from that list.
!
!````
!              p
!       [1] - [2] - [3] - [4]
!                 |
!                [5] - [6] - [7]        n=3 elements inserted
!              element       last
!
!  Result is:
!
!       [1] - [2] - [5] - [6] - [7] - [3] - [4]
!
!````

    subroutine json_value_insert_after(json,p,element)

    implicit none

    class(json_core),intent(inout) :: json
    type(json_value),pointer       :: p       !! a value from a JSON structure
                                              !! (presumably, this is a child of
                                              !! an object or array).
    type(json_value),pointer       :: element !! the element to insert after `p`

    type(json_value),pointer :: parent  !! the parent of `p`
    type(json_value),pointer :: next  !! temp pointer for traversing structure
    type(json_value),pointer :: last  !! the last of the items being inserted
    integer :: n  !! number of items being inserted

    if (.not. json%exception_thrown) then

        parent => p%parent

        ! set first parent of inserted list:
        element%parent => parent

        ! Count the number of inserted elements.
        ! and set their parents.
        n = 1 ! initialize counter
        next => element%next
        last => element
        do
            if (.not. associated(next)) exit
            n = n + 1
            next%parent => parent
            last => next
            next => next%next
        end do

        if (associated(parent)) then
            ! update parent's child counter:
            parent%n_children = parent%n_children + n
            ! if p is last of parents children then
            ! also have to update parent tail pointer:
            if (associated(parent%tail,p)) then
                parent%tail => last
            end if
        end if

        if (associated(element%previous)) then
            ! element is apparently part of an existing list,
            ! so have to update that as well.
            if (associated(element%previous%parent)) then
                element%previous%parent%n_children = &
                    element%previous%parent%n_children - n
                element%previous%parent%tail => &
                    element%previous ! now the last one in the list
            else
                ! this would be a memory leak if the previous entries
                ! are not otherwise being pointed too
                ! [throw an error in this case???]
            end if
            !remove element from the other list:
            element%previous%next => null()
        end if
        element%previous => p

        if (associated(p%next)) then
            ! if there are any in the list after p:
            last%next => p%next
            last%next%previous => element
        else
            last%next => null()
        end if
        p%next => element

    end if

    end subroutine json_value_insert_after
!*****************************************************************************************

!*****************************************************************************************
!>
!  Inserts `element` after the `idx`-th child of `p`,
!  and updates the JSON structure accordingly. This is just
!  a wrapper for [[json_value_insert_after]].

    subroutine json_value_insert_after_child_by_index(json,p,idx,element)

    implicit none

    class(json_core),intent(inout) :: json
    type(json_value),pointer       :: p       !! a JSON object or array.
    integer(IK),intent(in)         :: idx     !! the index of the child of `p` to
                                              !! insert the new element after
    type(json_value),pointer       :: element !! the element to insert

    type(json_value),pointer :: tmp  !! for getting the `idx`-th child of `p`

    if (.not. json%exception_thrown) then

        ! get the idx-th child of p:
        call json%get_child(p,idx,tmp)

        ! call json_value_insert_after:
        if (.not. json%failed()) call json%insert_after(tmp,element)

    end if

    end subroutine json_value_insert_after_child_by_index
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/19/2014
!
!  Add a real value child to the [[json_value]] variable
!
!@note This routine is part of the public API that can be
!      used to build a JSON structure using [[json_value]] pointers.

    subroutine json_value_add_double(json,p,name,val)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer            :: p
    character(kind=CK,len=*),intent(in) :: name  !! variable name
    real(RK),intent(in)                 :: val   !! real value

    type(json_value),pointer :: var

    !create the variable:
    call json%create_double(var,val,name)

    !add it:
    call json%add(p, var)

    end subroutine json_value_add_double
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_value_add_double]] where `name` is kind=CDK.

    subroutine wrap_json_value_add_double(json,p,name,val)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer             :: p
    character(kind=CDK,len=*),intent(in) :: name  !! variable name
    real(RK),intent(in)                  :: val   !! real value

    call json%add(p, to_unicode(name), val)

    end subroutine wrap_json_value_add_double
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/20/2014
!
!  Add a real vector to the structure.
!
!@note This routine is part of the public API that can be
!      used to build a JSON structure using [[json_value]] pointers.

    subroutine json_value_add_double_vec(json, p, name, val)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer            :: p
    character(kind=CK,len=*),intent(in) :: name
    real(RK),dimension(:),intent(in)    :: val

    type(json_value),pointer :: var
    integer(IK) :: i !! counter

    !create the variable as an array:
    call json%create_array(var,name)

    !populate the array:
    do i=1,size(val)
        call json%add(var, '', val(i))
    end do

    !add it:
    call json%add(p, var)

    end subroutine json_value_add_double_vec
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_value_add_double_vec]] where `name` is kind=CDK.

    subroutine wrap_json_value_add_double_vec(json, p, name, val)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer             :: p
    character(kind=CDK,len=*),intent(in) :: name
    real(RK),dimension(:),intent(in)     :: val

    call json%add(p, to_unicode(name), val)

    end subroutine wrap_json_value_add_double_vec
!*****************************************************************************************

!*****************************************************************************************
!>
!  Add a NULL value child to the [[json_value]] variable
!
!@note This routine is part of the public API that can be
!      used to build a JSON structure using [[json_value]] pointers.

    subroutine json_value_add_null(json, p, name)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer            :: p
    character(kind=CK,len=*),intent(in) :: name

    type(json_value),pointer :: var

    !create the variable:
    call json%create_null(var,name)

    !add it:
    call json%add(p, var)

    end subroutine json_value_add_null
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_value_add_null]] where `name` is kind=CDK.

    subroutine wrap_json_value_add_null(json, p, name)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer             :: p
    character(kind=CDK,len=*),intent(in) :: name   !! name of the variable

    call json%add(p, to_unicode(name))

    end subroutine wrap_json_value_add_null
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/20/2014
!
!  Add an integer value child to the [[json_value]] variable
!
!@note This routine is part of the public API that can be
!      used to build a JSON structure using [[json_value]] pointers.

    subroutine json_value_add_integer(json, p, name, val)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer            :: p
    character(kind=CK,len=*),intent(in) :: name
    integer(IK),intent(in)              :: val

    type(json_value),pointer :: var

    !create the variable:
    call json%create_integer(var,val,name)

    !add it:
    call json%add(p, var)

    end subroutine json_value_add_integer
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_value_add_integer]] where `name` is kind=CDK.

    subroutine wrap_json_value_add_integer(json, p, name, val)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer             :: p
    character(kind=CDK,len=*),intent(in) :: name   !! name of the variable
    integer(IK),intent(in)               :: val    !! value

    call json%add(p, to_unicode(name), val)

    end subroutine wrap_json_value_add_integer
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/20/2014
!
!  Add an integer vector to the structure.
!
!@note This routine is part of the public API that can be
!      used to build a JSON structure using [[json_value]] pointers.

    subroutine json_value_add_integer_vec(json, p, name, val)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer            :: p
    character(kind=CK,len=*),intent(in) :: name   !! name of the variable
    integer(IK),dimension(:),intent(in) :: val    !! value

    type(json_value),pointer :: var
    integer(IK) :: i    !! counter

    !create a variable as an array:
    call json%create_array(var,name)

    !populate the array:
    do i=1,size(val)
        call json%add(var, '', val(i))
    end do

    !add it:
    call json%add(p, var)

    end subroutine json_value_add_integer_vec
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_value_add_integer_vec]] where `name` is kind=CDK.

    subroutine wrap_json_value_add_integer_vec(json, p, name, val)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer             :: p
    character(kind=CDK,len=*),intent(in) :: name   !! name of the variable
    integer(IK),dimension(:),intent(in)  :: val    !! value

    call json%add(p, to_unicode(name), val)

    end subroutine wrap_json_value_add_integer_vec
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/20/2014
!
!  Add a logical value child to the [[json_value]] variable
!
!@note This routine is part of the public API that can be
!      used to build a JSON structure using [[json_value]] pointers.

    subroutine json_value_add_logical(json, p, name, val)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer            :: p
    character(kind=CK,len=*),intent(in) :: name   !! name of the variable
    logical(LK),intent(in)              :: val    !! value

    type(json_value),pointer :: var

    !create the variable:
    call json%create_logical(var,val,name)

    !add it:
    call json%add(p, var)

    end subroutine json_value_add_logical
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_value_add_logical]] where `name` is kind=CDK.

    subroutine wrap_json_value_add_logical(json, p, name, val)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer             :: p
    character(kind=CDK,len=*),intent(in) :: name   !! name of the variable
    logical(LK),intent(in)               :: val    !! value

    call json%add(p, to_unicode(name), val)

    end subroutine wrap_json_value_add_logical
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/20/2014
!
!  Add a logical vector to the structure.
!
!@note This routine is part of the public API that can be
!      used to build a JSON structure using [[json_value]] pointers.

    subroutine json_value_add_logical_vec(json, p, name, val)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer            :: p
    character(kind=CK,len=*),intent(in) :: name  !! name of the vector
    logical(LK),dimension(:),intent(in) :: val   !! value

    type(json_value),pointer :: var
    integer(IK) :: i    !! counter

    !create the variable as an array:
    call json%create_array(var,name)

    !populate the array:
    do i=1,size(val)
        call json%add(var, '', val(i))
    end do

    !add it:
    call json%add(p, var)

    end subroutine json_value_add_logical_vec
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_value_add_logical_vec]] where `name` is kind=CDK.

    subroutine wrap_json_value_add_logical_vec(json, p, name, val)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer             :: p
    character(kind=CDK,len=*),intent(in) :: name   !! name of the variable
    logical(LK),dimension(:),intent(in)  :: val    !! value

    call json%add(p, to_unicode(name), val)

    end subroutine wrap_json_value_add_logical_vec
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/19/2014
!
!  Add a character string child to the [[json_value]] variable.
!
!@note This routine is part of the public API that can be
!      used to build a JSON structure using [[json_value]] pointers.

    subroutine json_value_add_string(json, p, name, val)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer            :: p
    character(kind=CK,len=*),intent(in) :: name  !! name of the variable
    character(kind=CK,len=*),intent(in) :: val   !! value

    type(json_value),pointer :: var
    character(kind=CK,len=:),allocatable :: str

    !add escape characters if necessary:
    call escape_string(val, str)

    !create the variable:
    call json%create_string(var,str,name)

    !add it:
    call json%add(p, var)

    end subroutine json_value_add_string
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_value_add_string]] where `name` and `val` are kind=CDK.

    subroutine wrap_json_value_add_string(json, p, name, val)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer             :: p
    character(kind=CDK,len=*),intent(in) :: name   !! name of the variable
    character(kind=CDK,len=*),intent(in) :: val    !! value

    call json%add(p, to_unicode(name), to_unicode(val))

    end subroutine wrap_json_value_add_string
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_value_add_string]] where `name` is kind=CDK.

    subroutine json_value_add_string_name_ascii(json, p, name, val)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer             :: p
    character(kind=CDK,len=*),intent(in) :: name   !! name of the variable
    character(kind=CK, len=*),intent(in) :: val    !! value

    call json%add(p, to_unicode(name), val)

    end subroutine json_value_add_string_name_ascii
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_value_add_string]] where `val` is kind=CDK.

    subroutine json_value_add_string_val_ascii(json, p, name, val)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer             :: p
    character(kind=CK, len=*),intent(in) :: name   !! name of the variable
    character(kind=CDK,len=*),intent(in) :: val    !! value

    call json%add(p, name, to_unicode(val))

    end subroutine json_value_add_string_val_ascii
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/19/2014
!
!  Add an array of character strings to the structure.
!
!@note This routine is part of the public API that can be
!      used to build a JSON structure using [[json_value]] pointers.

    subroutine json_value_add_string_vec(json, p, name, val, trim_str, adjustl_str)

    implicit none

    class(json_core),intent(inout)                   :: json
    type(json_value),pointer                         :: p
    character(kind=CK,len=*),intent(in)              :: name        !! variable name
    character(kind=CK,len=*),dimension(:),intent(in) :: val         !! array of strings
    logical(LK),intent(in),optional                  :: trim_str    !! if TRIM() should be called for each element
    logical(LK),intent(in),optional                  :: adjustl_str !! if ADJUSTL() should be called for each element

    type(json_value),pointer :: var
    integer(IK) :: i
    logical(LK) :: trim_string, adjustl_string
    character(kind=CK,len=:),allocatable :: str

    !if the string is to be trimmed or not:
    if (present(trim_str)) then
        trim_string = trim_str
    else
        trim_string = .false.
    end if
    if (present(adjustl_str)) then
        adjustl_string = adjustl_str
    else
        adjustl_string = .false.
    end if

    !create the variable as an array:
    call json%create_array(var,name)

    !populate the array:
    do i=1,size(val)

        !the string to write:
        str = val(i)
        if (adjustl_string) str = adjustl(str)
        if (trim_string)    str = trim(str)

        !write it:
        call json%add(var, '', str)

        !cleanup
        deallocate(str)

    end do

    !add it:
    call json%add(p, var)

    end subroutine json_value_add_string_vec
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_value_add_string_vec]] where `name` and `val` are kind=CDK.

    subroutine wrap_json_value_add_string_vec(json, p, name, val, trim_str, adjustl_str)

    implicit none

    class(json_core),intent(inout)                    :: json
    type(json_value),pointer                          :: p
    character(kind=CDK,len=*),intent(in)              :: name
    character(kind=CDK,len=*),dimension(:),intent(in) :: val
    logical(LK),intent(in),optional                   :: trim_str
    logical(LK),intent(in),optional                   :: adjustl_str

    call json%add(p, to_unicode(name), to_unicode(val), trim_str, adjustl_str)

    end subroutine wrap_json_value_add_string_vec
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_value_add_string_vec]] where `name` is kind=CDK.

    subroutine json_value_add_string_vec_name_ascii(json, p, name, val, trim_str, adjustl_str)

    implicit none

    class(json_core),intent(inout)                    :: json
    type(json_value),pointer                          :: p
    character(kind=CDK,len=*),intent(in)              :: name
    character(kind=CK, len=*),dimension(:),intent(in) :: val
    logical(LK),intent(in),optional                   :: trim_str
    logical(LK),intent(in),optional                   :: adjustl_str

    call json%add(p, to_unicode(name), val, trim_str, adjustl_str)

    end subroutine json_value_add_string_vec_name_ascii
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_value_add_string_vec]] where `val` is kind=CDK.

    subroutine json_value_add_string_vec_val_ascii(json, p, name, val, trim_str, adjustl_str)

    implicit none

    class(json_core),intent(inout)                    :: json
    type(json_value),pointer                          :: p
    character(kind=CK, len=*),intent(in)              :: name
    character(kind=CDK,len=*),dimension(:),intent(in) :: val
    logical(LK),intent(in),optional                   :: trim_str
    logical(LK),intent(in),optional                   :: adjustl_str

    call json%add(p, name, to_unicode(val), trim_str, adjustl_str)

    end subroutine json_value_add_string_vec_val_ascii
!*****************************************************************************************

!*****************************************************************************************
!>
!  Count the number of children.
!
!# History
!  * JW : 1/4/2014 : Original routine removed.
!    Now using n_children variable.
!    Renamed from json_value_count.

    function json_count(json,p) result(count)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer,intent(in) :: p
    integer(IK)                         :: count  !! number of children

    if (associated(p)) then
        count = p%n_children
    else
        call json%throw_exception('Error in json_count: '//&
                                  'pointer is not associated.')
    end if

    end function json_count
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 10/16/2015
!
!  Returns a pointer to the parent of a [[json_value]].
!  If there is no parent, then a null() pointer is returned.

    subroutine json_get_parent(json,p,parent)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer,intent(in)  :: p        !! JSON object
    type(json_value),pointer,intent(out) :: parent   !! pointer to parent

    if (associated(p)) then
        parent => p%parent
    else
        nullify(parent)
        call json%throw_exception('Error in json_get_parent: '//&
                                  'pointer is not associated.')
    end if

    end subroutine json_get_parent
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 10/31/2015
!
!  Returns a pointer to the next of a [[json_value]].
!  If there is no next, then a null() pointer is returned.

    subroutine json_get_next(json,p,next)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer,intent(in)  :: p       !! JSON object
    type(json_value),pointer,intent(out) :: next    !! pointer to next

    if (associated(p)) then
        next => p%next
    else
        nullify(next)
        call json%throw_exception('Error in json_get_next: '//&
                                  'pointer is not associated.')
    end if

    end subroutine json_get_next
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 10/31/2015
!
!  Returns a pointer to the previous of a [[json_value]].
!  If there is no previous, then a null() pointer is returned.

    subroutine json_get_previous(json,p,previous)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer,intent(in)  :: p        !! JSON object
    type(json_value),pointer,intent(out) :: previous !! pointer to previous

    if (associated(p)) then
        previous => p%previous
    else
        nullify(previous)
        call json%throw_exception('Error in json_get_previous: '//&
                                  'pointer is not associated.')
    end if

    end subroutine json_get_previous
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 10/31/2015
!
!  Returns a pointer to the tail of a [[json_value]]
!  (the last child of an array of object).
!  If there is no tail, then a null() pointer is returned.

    subroutine json_get_tail(json,p,tail)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer,intent(in)  :: p        !! JSON object
    type(json_value),pointer,intent(out) :: tail     !! pointer to tail

    if (associated(p)) then
        tail => p%tail
    else
        nullify(tail)
        call json%throw_exception('Error in json_get_tail: '//&
                                  'pointer is not associated.')
    end if

    end subroutine json_get_tail
!*****************************************************************************************

!*****************************************************************************************
!>
!  Returns a child in the object or array given the index.

    subroutine json_value_get_by_index(json, p, idx, child)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer,intent(in) :: p      !! object or array JSON data
    integer(IK),intent(in)              :: idx    !! index of the child
    type(json_value),pointer            :: child  !! pointer to the child

    integer(IK) :: i

    nullify(child)

    if (.not. json%exception_thrown) then

        if (associated(p%children)) then

            child => p%children

            do i = 1, idx - 1

                if (associated(child%next)) then
                    child => child%next
                else
                    call json%throw_exception('Error in json_value_get_by_index:'//&
                                              ' child%next is not associated.')
                    nullify(child)
                    return
                end if

            end do

        else

            call json%throw_exception('Error in json_value_get_by_index:'//&
                                      ' p%children is not associated.')

        end if

    end if

    end subroutine json_value_get_by_index
!*****************************************************************************************

!*****************************************************************************************
!>
!  Returns pointer to the first child of the object
!  (or null() if it is not associated).

    subroutine json_value_get_child(json, p, child)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer,intent(in) :: p      !! object or array JSON data
    type(json_value),pointer            :: child  !! pointer to the child

    if (associated(p)) then
        child => p%children
    else
        nullify(child)
        call json%throw_exception('Error in json_value_get_child: '//&
                                  'pointer is not associated.')
    end if

    end subroutine json_value_get_child
!*****************************************************************************************

!*****************************************************************************************
!>
!  Returns a child in the object or array given the name string.
!
!  The name search can be case-sensitive or not, and can have significant trailing
!  whitespace or not, depending on the settings in the [[json_core]] class.
!
!@note The `name` input is not a path, and is not parsed like it is in [[json_get_by_path]].

    subroutine json_value_get_by_name_chars(json, p, name, child)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer,intent(in) :: p
    character(kind=CK,len=*),intent(in) :: name      !! the name of a child of `p`
    type(json_value),pointer            :: child     !! pointer to the child

    integer(IK) :: i,n_children

    nullify(child)

    if (.not. json%exception_thrown) then

        if (associated(p)) then

            if (p%var_type==json_object) then
                n_children = json%count(p)
                child => p%children    !start with first one
                do i=1, n_children
                    if (.not. associated(child)) then
                        call json%throw_exception('Error in json_value_get_by_name_chars: '//&
                                                  'Malformed JSON linked list')
                        return
                    end if
                    if (allocated(child%name)) then
                        !name string matching routine:
                        if (json%name_equal(child,name)) return
                    end if
                    child => child%next
                end do
            end if

            !did not find anything:
            call json%throw_exception('Error in json_value_get_by_name_chars: '//&
                                 'child variable '//trim(name)//' was not found.')
            nullify(child)

        else
            call json%throw_exception('Error in json_value_get_by_name_chars: '//&
                                 'pointer is not associated.')
        end if

    end if

    end subroutine json_value_get_by_name_chars
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_value_get_by_name_chars]] where `name` is kind=CDK.

    subroutine wrap_json_value_get_by_name_chars(json, p, name, child)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer,intent(in)  :: p
    character(kind=CDK,len=*),intent(in) :: name
    type(json_value),pointer             :: child

    call json%get(p,to_unicode(name),child)

    end subroutine wrap_json_value_get_by_name_chars
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 2/12/2014
!
!  Print the [[json_value]] structure to an allocatable string.

    subroutine json_value_to_string(json,p,str)

    implicit none

    class(json_core),intent(inout)                   :: json
    type(json_value),pointer,intent(in)              :: p
    character(kind=CK,len=:),intent(out),allocatable :: str  !! prints structure to this string

    str = ''
    call json%json_value_print(p, iunit=unit2str, str=str, indent=1, colon=.true.)

    end subroutine json_value_to_string
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 6/20/2014
!
!  Print the [[json_value]] structure to a file.

    subroutine json_print_1(json,p,iunit)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer,intent(in)  :: p
    integer(IK),intent(in)               :: iunit   !! the file unit (the file must
                                                    !! already have been opened, can't be -1).

    character(kind=CK,len=:),allocatable :: dummy

    if (iunit/=unit2str) then
        call json%json_value_print(p,iunit,str=dummy, indent=1, colon=.true.)
    else
        call json%throw_exception('Error in json_print_1: iunit must not be -1.')
    end if

    end subroutine json_print_1
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 12/23/2014
!
!  Print the [[json_value]] structure to a file.

    subroutine json_print_2(json,p,filename)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer,intent(in)  :: p
    character(kind=CDK,len=*),intent(in) :: filename  !! the filename to print to
                                                      !! (should not already be open)

    integer(IK) :: iunit,istat

    open(newunit=iunit,file=filename,status='REPLACE',iostat=istat FILE_ENCODING )
    if (istat==0) then
        call json%print(p,iunit)
        close(iunit,iostat=istat)
    else
        call json%throw_exception('Error in json_print_2: could not open file: '//&
                              trim(filename))
    end if

    end subroutine json_print_2
!*****************************************************************************************

!*****************************************************************************************
!>
!  Print the JSON structure to a string or a file.
!
!# Notes
!  * This is an internal routine called by the various wrapper routines.
!  * The reason the str argument is non-optional is because of a
!    bug in v4.9 of the gfortran compiler.

    recursive subroutine json_value_print(json,p,iunit,str,indent,&
                                          need_comma,colon,is_array_element)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer,intent(in)  :: p
    integer(IK),intent(in)               :: iunit             !! file unit to write to (6=console)
    integer(IK),intent(in),optional      :: indent            !! indention level
    logical(LK),intent(in),optional      :: is_array_element  !! if this is an array element
    logical(LK),intent(in),optional      :: need_comma        !! if it needs a comma after it
    logical(LK),intent(in),optional      :: colon             !! if the colon was just written
    character(kind=CK,len=:),intent(inout),allocatable :: str
                                                      !! if iunit==unit2str (-1) then the structure is
                                                      !! printed to this string rather than
                                                      !! a file. This mode is used by
                                                      !! [[json_value_to_string]].

    character(kind=CK,len=max_numeric_str_len) :: tmp !for val to string conversions
    character(kind=CK,len=:),allocatable :: s
    type(json_value),pointer :: element
    integer(IK) :: tab, i, count, spaces
    logical(LK) :: print_comma
    logical(LK) :: write_file, write_string
    logical(LK) :: is_array

    if (.not. json%exception_thrown) then

        !whether to write a string or a file (one or the other):
        write_string = (iunit==unit2str)
        write_file = .not. write_string

        !if the comma will be printed after the value
        ! [comma not printed for the last elements]
        if (present(need_comma)) then
            print_comma = need_comma
        else
            print_comma = .false.
        end if

        !number of "tabs" to indent:
        if (present(indent) .and. .not. json%no_whitespace) then
            tab = indent
        else
            tab = 0
        end if
        !convert to number of spaces:
        spaces = tab*json%spaces_per_tab

        !if this is an element in an array:
        if (present(is_array_element)) then
            is_array = is_array_element
        else
            is_array = .false.
        end if

        !if the colon was the last thing written
        if (present(colon)) then
            s = ''
        else
            s = repeat(space, spaces)
        end if

        select case (p%var_type)

        case (json_object)

            count = json%count(p)

            if (count==0) then    !special case for empty object

                call write_it( s//start_object//end_object, comma=print_comma )

            else

                call write_it( s//start_object )

                !if an object is in an array, there is an extra tab:
                if (is_array) then
                    if ( .not. json%no_whitespace) tab = tab+1
                    spaces = tab*json%spaces_per_tab
                end if

                nullify(element)
                element => p%children
                do i = 1, count

                    if (.not. associated(element)) then
                        call json%throw_exception('Error in json_value_print: '//&
                                                  'Malformed JSON linked list')
                        return
                    end if

                    ! print the name
                    if (allocated(element%name)) then
                        if (json%no_whitespace) then
                            !compact printing - no extra space
                            call write_it(repeat(space, spaces)//quotation_mark//&
                                          element%name//quotation_mark//colon_char,&
                                          advance=.false.)
                        else
                            call write_it(repeat(space, spaces)//quotation_mark//&
                                          element%name//quotation_mark//colon_char//space,&
                                          advance=.false.)
                        end if
                    else
                        call json%throw_exception('Error in json_value_print:'//&
                                             ' element%name not allocated')
                        nullify(element)
                        return
                    end if

                    ! recursive print of the element
                    call json%json_value_print(element, iunit=iunit, indent=tab + 1, &
                                    need_comma=i<count, colon=.true., str=str)

                    ! get the next child the list:
                    element => element%next

                end do

                ! [one fewer tab if it isn't an array element]
                if (.not. is_array) s = repeat(space, max(0,spaces-json%spaces_per_tab))
                call write_it( s//end_object, comma=print_comma )
                nullify(element)

            end if

        case (json_array)

            count = json%count(p)

            if (count==0) then    !special case for empty array

                call write_it( s//start_array//end_array, comma=print_comma )

            else

                call write_it( start_array )

                nullify(element)
                element => p%children
                do i = 1, count

                    if (.not. associated(element)) then
                        call json%throw_exception('Error in json_value_print: '//&
                                                  'Malformed JSON linked list')
                        return
                    end if

                    ! recursive print of the element
                    call json%json_value_print(element, iunit=iunit, indent=tab,&
                                    need_comma=i<count, is_array_element=.true., str=str)

                    ! get the next child the list:
                    element => element%next

                end do

                !indent the closing array character:
                call write_it( repeat(space, max(0,spaces-json%spaces_per_tab))//end_array,&
                               comma=print_comma )
                nullify(element)

            end if

        case (json_null)

            call write_it( s//null_str, comma=print_comma )

        case (json_string)

            if (allocated(p%str_value)) then
                call write_it( s//quotation_mark// &
                               trim(p%str_value)//quotation_mark, comma=print_comma )
            else
                call json%throw_exception('Error in json_value_print:'//&
                                          ' p%value_string not allocated')
                return
            end if

        case (json_logical)

            if (p%log_value) then
                call write_it( s//true_str, comma=print_comma )
            else
                call write_it( s//false_str, comma=print_comma )
            end if

        case (json_integer)

            call integer_to_string(p%int_value,int_fmt,tmp)

            call write_it( s//trim(tmp), comma=print_comma )

        case (json_double)

            if (allocated(json%real_fmt)) then
                call real_to_string(p%dbl_value,json%real_fmt,json%compact_real,tmp)
            else
                !use the default format (user has not called initialize() or specified one):
                call real_to_string(p%dbl_value,default_real_fmt,json%compact_real,tmp)
            end if

            call write_it( s//trim(tmp), comma=print_comma )

        case default

            call json%throw_exception('Error in json_value_print: unknown data type')

        end select

        !cleanup:
        if (allocated(s)) deallocate(s)

    end if

    contains

        subroutine write_it(s,advance,comma)

        !! write the string to the file (or the output string)

        implicit none

        character(kind=CK,len=*),intent(in) :: s        !! string to print
        logical(LK),intent(in),optional     :: advance  !! to add line break or not
        logical(LK),intent(in),optional     :: comma    !! print comma after the string

        logical(LK) :: add_comma       !! if a delimiter is to be added after string
        logical(LK) :: add_line_break  !! if a line break is to be added after string
        character(kind=CK,len=:),allocatable :: s2  !! temporary string

        if (present(comma)) then
            add_comma = comma
        else
            add_comma = .false. !default is not to add comma
        end if

        if (present(advance)) then
            add_line_break = advance
        else
            add_line_break = .not. json%no_whitespace ! default is to advance if
                                                      ! we are printing whitespace
        end if

        !string to print:
        s2 = s
        if (add_comma) s2 = s2 // delimiter

        if (write_file) then

            if (add_line_break) then
                write(iunit,fmt='(A)') s2
            else
                write(iunit,fmt='(A)',advance='NO') s2
            end if

        else    !write string

            str = str // s2
            if (add_line_break) str = str // newline

        end if

        !cleanup:
        if (allocated(s2)) deallocate(s2)

        end subroutine write_it

    end subroutine json_value_print
!*****************************************************************************************

!*****************************************************************************************
!>
!  Returns the [[json_value]] pointer given the path string.
!
!# Example
!
!````fortran
!    type(json_value),pointer :: dat,p
!    logical :: found
!    !...
!    call json%get(dat,'data(2).version',p,found)
!````
!
!# Notes
!  The following special characters are used to denote paths:
!
!````
!  $         - root
!  @         - this
!  .         - child object member
!  [] or ()  - child array element
!````
!
!  Thus, if any of these characters are present in the name key,
!  this routine cannot be used to get the value.
!  In that case, the `get_child` methods would need to be used.

    subroutine json_get_by_path(json, me, path, p, found)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer,intent(in)  :: me     !! a JSON linked list
    character(kind=CK,len=*),intent(in)  :: path   !! path to the variable
    type(json_value),pointer,intent(out) :: p      !! pointer to the variable specify by `path`
    logical(LK),intent(out),optional     :: found  !! true if it was found

    integer(IK)              :: i
    integer(IK)              :: length
    integer(IK)              :: child_i
    character(kind=CK,len=1) :: c
    logical(LK)              :: array
    type(json_value),pointer :: tmp

    nullify(p)

    if (.not. json%exception_thrown) then

        ! default to assuming relative to this
        p => me

        child_i = 1

        array = .false.

        !keep trailing space or not:
        if (json%trailing_spaces_significant) then
            length = len(path)
        else
            length = len_trim(path)
        end if

        do i=1, length

            c = path(i:i)

            select case (c)
            case (root)

                ! root
                do while (associated (p%parent))
                    p => p%parent
                end do
                child_i = i + 1

            case (this)

                ! this
                p => me
                child_i = i + 1

            case (child)

                ! get child member from p
                if (child_i < i) then
                    nullify(tmp)
                    call json%get_child(p, path(child_i:i-1), tmp)
                    p => tmp
                    nullify(tmp)
                else
                    child_i = i + 1
                    cycle
                end if

                if (.not. associated(p)) then
                    call json%throw_exception('Error in json_get_by_path:'//&
                                              ' Error getting child member.')
                    exit
                end if

                child_i = i+1

            case (start_array,start_array_alt)

                !....Modified to allow for 'var[3]' style syntax
                !Note: jmozmoz/fson has a slightly different version of this...

                ! start looking for the array element index
                array = .true.

                ! get child member from p
                if (child_i < i) then
                    nullify(tmp)
                    call json%get_child(p, path(child_i:i-1), tmp)
                    p => tmp
                    nullify(tmp)
                else
                    child_i = i + 1
                    cycle
                end if
                if (.not. associated(p)) then
                    call json%throw_exception('Error in json_get_by_path:'//&
                                         ' Error getting array element')
                    exit
                end if
                child_i = i + 1

            case (end_array,end_array_alt)

                if (.not.array) then
                    call json%throw_exception('Error in json_get_by_path: Unexpected ]')
                    exit
                end if
                array = .false.
                child_i = json%string_to_integer(path(child_i:i-1))

                nullify(tmp)
                call json%get_child(p, child_i, tmp)
                p => tmp
                nullify(tmp)

                child_i= i + 1

            end select

        end do

        if (json%exception_thrown) then

            if (present(found)) then
                found = .false.
                call json%clear_exceptions()
            end if

        else

            ! grab the last child if present in the path
            if (child_i <= length) then
                nullify(tmp)
                call json%get_child(p, path(child_i:i-1), tmp)
                p => tmp
                nullify(tmp)
            end if
            if (associated(p)) then
                if (present(found)) found = .true.    !everything seems to be ok
            else
                call json%throw_exception('Error in json_get_by_path:'//&
                                          ' variable not found: '//trim(path))
                if (present(found)) then
                    found = .false.
                    call json%clear_exceptions()
                end if
            end if

        end if

    else
        if (present(found)) found = .false.
    end if

    end subroutine json_get_by_path
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_get_by_path]] where "path" is kind=CDK.

    subroutine wrap_json_get_by_path(json, me, path, p, found)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer,intent(in)  :: me
    character(kind=CDK,len=*),intent(in) :: path
    type(json_value),pointer,intent(out) :: p
    logical(LK),intent(out),optional     :: found

    call json%get(me, to_unicode(path), p, found)

    end subroutine wrap_json_get_by_path
!*****************************************************************************************

!*****************************************************************************************
!>
!  Returns the path to a JSON object that is part
!  of a linked list structure.
!
!  The path returned would be suitable for input to
!  [[json_get_by_path]] and related routines.
!
!@note If an error occurs (which in this case means a malformed
!      JSON structure) then an exception will be thrown, unless
!      `found` is present, which will be set to `false`. `path`
!      will be a blank string.

    subroutine json_get_path(json, p, path, found, use_alt_array_tokens, path_sep)

    implicit none

    class(json_core),intent(inout)                   :: json
    type(json_value),pointer,intent(in)              :: p     !! a JSON linked list object
    character(kind=CK,len=:),allocatable,intent(out) :: path  !! path to the variable
    logical(LK),intent(out),optional                 :: found !! true if there were no problems
    logical(LK),intent(in),optional :: use_alt_array_tokens   !! if true, then '()' are used for array elements
                                                              !! otherwise, '[]' are used [default]
    character(kind=CK,len=1),intent(in),optional :: path_sep  !! character to use for path separator
                                                              !! (default is '.')

    type(json_value),pointer                   :: tmp            !! for traversing the structure
    type(json_value),pointer                   :: element        !! for traversing the structure
    integer(IK)                                :: var_type       !! JSON variable type flag
    character(kind=CK,len=:),allocatable       :: name           !! variable name
    character(kind=CK,len=:),allocatable       :: parent_name    !! variable's parent name
    character(kind=CK,len=max_integer_str_len) :: istr           !! for integer to string conversion (array indices)
    integer(IK)                                :: i              !! counter
    integer(IK)                                :: n_children     !! number of children for parent
    logical(LK)                                :: use_brackets   !! to use '[]' characters for arrays
    logical(LK)                                :: parent_is_root !! if the parent is the root

    !initialize:
    path = ''

    !optional input:
    if (present(use_alt_array_tokens)) then
        use_brackets = .not. use_alt_array_tokens
    else
        use_brackets = .true.
    end if

    if (associated(p)) then

        !traverse the structure via parents up to the root
        tmp => p
        do

            if (.not. associated(tmp)) exit !finished

            !get info about the current variable:
            call json%info(tmp,name=name)

            ! if tmp a child of an object, or an element of an array
            if (associated(tmp%parent)) then

                !get info about the parent:
                call json%info(tmp%parent,var_type=var_type,&
                               n_children=n_children,name=parent_name)

                select case (var_type)
                case (json_array)

                    !get array index of this element:
                    element => tmp%parent%children
                    do i = 1, n_children
                        if (.not. associated(element)) then
                            call json%throw_exception('Error in json_get_path: '//&
                                                      'malformed JSON structure. ')
                            exit
                        end if
                        if (associated(element,tmp)) then
                            exit
                        else
                            element => element%next
                        end if
                        if (i==n_children) then ! it wasn't found (should never happen)
                            call json%throw_exception('Error in json_get_path: '//&
                                                      'malformed JSON structure. ')
                            exit
                        end if
                    end do
                    call integer_to_string(i,int_fmt,istr)
                    if (use_brackets) then
                        call add_to_path(parent_name//start_array//&
                                         trim(adjustl(istr))//end_array,path_sep)
                    else
                        call add_to_path(parent_name//start_array_alt//&
                                         trim(adjustl(istr))//end_array_alt,path_sep)
                    end if
                    tmp => tmp%parent  ! already added parent name

                case (json_object)

                    !process parent on the next pass
                    call add_to_path(name,path_sep)

                case default

                    call json%throw_exception('Error in json_get_path: '//&
                                              'malformed JSON structure. '//&
                                              'A variable that is not an object '//&
                                              'or array should not have a child.')
                    exit

                end select

            else
                !the last one:
                call add_to_path(name,path_sep)
            end if

            if (associated(tmp%parent)) then
                !check if the parent is the root:
                parent_is_root = (.not. associated(tmp%parent%parent))
                if (parent_is_root) exit
            end if

            !go to parent:
            tmp => tmp%parent

        end do

    else
        call json%throw_exception('Error in json_get_path: '//&
                                  'input pointer is not associated')
    end if

    !for errors, return blank string:
    if (json%exception_thrown) path = ''

    !optional output:
    if (present(found)) then
        if (json%exception_thrown) then
            found = .false.
            call json%clear_exceptions()
        else
            found = .true.
        end if
    end if

    contains

        subroutine add_to_path(str,dot)
        !! prepend the string to the path
        implicit none
        character(kind=CK,len=*),intent(in) :: str  !! string to prepend to `path`
        character(kind=CK,len=1),intent(in),optional :: dot  !! path separator (default is '.')
        if (path=='') then
            path = str
        else
            if (present(dot)) then
                path = str//dot//path
            else
                path = str//child//path
            end if
        end if
        end subroutine add_to_path

    end subroutine json_get_path
!*****************************************************************************************

!*****************************************************************************************
!>
!  Wrapper for [[json_get_path]] where "path" and "path_sep" are kind=CDK.

    subroutine wrap_json_get_path(json, p, path, found, use_alt_array_tokens, path_sep)

    implicit none

    class(json_core),intent(inout)                    :: json
    type(json_value),pointer,intent(in)               :: p     !! a JSON linked list object
    character(kind=CDK,len=:),allocatable,intent(out) :: path  !! path to the variable
    logical(LK),intent(out),optional                  :: found !! true if there were no problems
    logical(LK),intent(in),optional :: use_alt_array_tokens    !! if true, then '()' are used for array elements
                                                               !! otherwise, '[]' are used [default]
    character(kind=CDK,len=1),intent(in),optional :: path_sep  !! character to use for path separator
                                                               !! (default is '.')

    character(kind=CK,len=:),allocatable :: ck_path  !! path to the variable

    ! call the main routine:
    call json_get_path(json,p,ck_path,found,use_alt_array_tokens,path_sep)

    ! from unicode:
    path = ck_path

    end subroutine wrap_json_get_path
!*****************************************************************************************

!*****************************************************************************************
!>
!  Convert a string into an integer.
!
!# History
!  * Jacob Williams : 12/10/2013 : Rewrote routine.  Added error checking.
!  * Modified by Izaak Beekman
!
!@note Replacement for the parse_integer function in the original code.

    function string_to_integer(json,str) result(ival)

    implicit none

    class(json_core),intent(inout)      :: json
    character(kind=CK,len=*),intent(in) :: str
    integer(IK)                         :: ival

    character(kind=CDK,len=:),allocatable :: digits
    integer(IK) :: ndigits_digits,ndigits,ierr

    if (.not. json%exception_thrown) then

        ! Compute how many digits we need to read
        ndigits = 2*len_trim(str)
        ndigits_digits = floor(log10(real(ndigits)))+1
        allocate(character(kind=CDK,len=ndigits_digits) :: digits)
        write(digits,'(I0)') ndigits !gfortran will have a runtime error with * edit descriptor here
        ! gfortran bug: '*' edit descriptor for ISO_10646 strings does bad stuff.
        read(str,'(I'//trim(digits)//')',iostat=ierr) ival   !string to integer

        if (ierr/=0) then    !if there was an error
            ival = 0
            call json%throw_exception('Error in string_to_integer: '//&
                                      'string cannot be converted to an integer: '//&
                                      trim(str))
        end if

    else
        ival = 0
    end if

    end function string_to_integer
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/19/2014
!
!  Convert a string into a double.
!
!# History
!  * Jacob Williams, 10/27/2015 : Now using fmt=*, rather than
!    fmt=real_fmt, since it doesn't work for some unusual cases
!    (e.g., when str='1E-5').

    function string_to_double(json,str) result(rval)

    implicit none

    class(json_core),intent(inout)      :: json
    character(kind=CK,len=*),intent(in) :: str
    real(RK)                            :: rval

    integer(IK) :: ierr  !! read iostat error code

    if (.not. json%exception_thrown) then

        !string to double
        read(str,fmt=*,iostat=ierr) rval

        if (ierr/=0) then    !if there was an error
            rval = 0.0_RK
            call json%throw_exception('Error in string_to_double: '//&
                                      'string cannot be converted to a double: '//&
                                      trim(str))
        end if

    else
        rval = 0.0_RK
    end if

    end function string_to_double
!*****************************************************************************************

!*****************************************************************************************
!>
!  Get an integer value from a [[json_value]].

    subroutine json_get_integer(json, me, value)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer,intent(in) :: me
    integer(IK),intent(out)             :: value

    value = 0
    if ( json%exception_thrown ) return

    if (me%var_type == json_integer) then
        value = me%int_value
    else
        if (json%strict_type_checking) then
            call json%throw_exception('Error in get_integer:'//&
                 ' Unable to resolve value to integer: '//me%name)
        else
            !type conversions
            select case(me%var_type)
            case (json_double)
                value = int(me%dbl_value)
            case (json_logical)
                if (me%log_value) then
                    value = 1
                else
                    value = 0
                end if
            case default
                call json%throw_exception('Error in get_integer:'//&
                     ' Unable to resolve value to integer: '//me%name)
            end select
        end if
    end if

    end subroutine json_get_integer
!*****************************************************************************************

!*****************************************************************************************
!>
!  Get an integer value from a [[json_value]], given the path string.

    subroutine json_get_integer_with_path(json, me, path, value, found)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer,intent(in) :: me
    character(kind=CK,len=*),intent(in) :: path
    integer(IK),intent(out)             :: value
    logical(LK),intent(out),optional    :: found

    type(json_value),pointer :: p

    value = 0
    if ( json%exception_thrown ) then
       if ( present(found) ) found = .false.
       return
    end if

    nullify(p)

    call json%get(me=me, path=path, p=p)

    if (.not. associated(p)) then
        call json%throw_exception('Error in json_get_integer:'//&
            ' Unable to resolve path: '// trim(path))
    else
        call json%get(p,value)
        nullify(p)
    end if
    if ( json%exception_thrown ) then
        if ( present(found) ) then
            found = .false.
            call json%clear_exceptions()
        end if
    else
        if ( present(found) ) found = .true.
    end if

    end subroutine json_get_integer_with_path
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_get_integer_with_path]], where "path" is kind=CDK.

    subroutine wrap_json_get_integer_with_path(json, me, path, value, found)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer,intent(in)  :: me
    character(kind=CDK,len=*),intent(in) :: path
    integer(IK),intent(out)              :: value
    logical(LK),intent(out),optional     :: found

    call json%get(me, to_unicode(path), value, found)

    end subroutine wrap_json_get_integer_with_path
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 5/14/2014
!
!  Get an integer vector from a [[json_value]].

    subroutine json_get_integer_vec(json, me, vec)

    implicit none

    class(json_core),intent(inout)                   :: json
    type(json_value),pointer                         :: me
    integer(IK),dimension(:),allocatable,intent(out) :: vec

    logical(LK) :: initialized

    initialized = .false.

    !the callback function is called for each element of the array:
    call json%get(me, array_callback=get_int_from_array)

    contains

        subroutine get_int_from_array(json, element, i, count)

        !! callback function for integer

        implicit none

        class(json_core),intent(inout)      :: json
        type(json_value),pointer,intent(in) :: element
        integer(IK),intent(in)              :: i        !! index
        integer(IK),intent(in)              :: count    !! size of array

        !size the output array:
        if (.not. initialized) then
            allocate(vec(count))
            initialized = .true.
        end if

        !populate the elements:
        call json%get(element, value=vec(i))

        end subroutine get_int_from_array

    end subroutine json_get_integer_vec
!*****************************************************************************************

!*****************************************************************************************
!>
!  Get an integer vector from a [[json_value]], given the path string.

    subroutine json_get_integer_vec_with_path(json, me, path, vec, found)

    implicit none

    class(json_core),intent(inout)                   :: json
    type(json_value),pointer                         :: me
    character(kind=CK,len=*),intent(in)              :: path
    integer(IK),dimension(:),allocatable,intent(out) :: vec
    logical(LK),intent(out),optional                 :: found

    logical(LK) :: initialized

    initialized = .false.

    call json%get(me, path=path, array_callback=get_int_from_array, found=found)

    ! need to duplicate callback function, no other way
    contains

        subroutine get_int_from_array(json, element, i, count)

        !! callback function for integer

        implicit none

        class(json_core),intent(inout)      :: json
        type(json_value),pointer,intent(in) :: element
        integer(IK),intent(in)              :: i        !! index
        integer(IK),intent(in)              :: count    !! size of array

        !size the output array:
        if (.not. initialized) then
            allocate(vec(count))
            initialized = .true.
        end if

        !populate the elements:
        call json%get(element, value=vec(i))

        end subroutine get_int_from_array

    end subroutine json_get_integer_vec_with_path
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_get_integer_vec_with_path]], where "path" is kind=CDK

    subroutine wrap_json_get_integer_vec_with_path(json, me, path, vec, found)

    implicit none

    class(json_core),intent(inout)                   :: json
    type(json_value),pointer                         :: me
    character(kind=CDK,len=*),intent(in)             :: path
    integer(IK),dimension(:),allocatable,intent(out) :: vec
    logical(LK),intent(out),optional                 :: found

    call json%get(me,path=to_unicode(path),vec=vec,found=found)

    end subroutine wrap_json_get_integer_vec_with_path
!*****************************************************************************************

!*****************************************************************************************
!>
!  Get a double value from a [[json_value]].

    subroutine json_get_double(json, me, value)

    implicit none

    class(json_core),intent(inout) :: json
    type(json_value),pointer       :: me
    real(RK),intent(out)           :: value

    value = 0.0_RK
    if ( json%exception_thrown ) return

    if (me%var_type == json_double) then
        value = me%dbl_value
    else
        if (json%strict_type_checking) then
            call json%throw_exception('Error in json_get_double:'//&
                                      ' Unable to resolve value to double: '//me%name)
        else
            !type conversions
            select case (me%var_type)
            case (json_integer)
                value = me%int_value
            case (json_logical)
                if (me%log_value) then
                    value = 1.0_RK
                else
                    value = 0.0_RK
                end if
            case default
                call json%throw_exception('Error in json_get_double:'//&
                                          ' Unable to resolve value to double: '//me%name)
            end select
        end if
    end if

    end subroutine json_get_double
!*****************************************************************************************

!*****************************************************************************************
!>
!  Get a double value from a [[json_value]], given the path.

    subroutine json_get_double_with_path(json, me, path, value, found)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer            :: me
    character(kind=CK,len=*),intent(in) :: path
    real(RK),intent(out)                :: value
    logical(LK),intent(out),optional    :: found

    type(json_value),pointer :: p

    value = 0.0_RK
    if ( json%exception_thrown ) then
        if ( present(found) ) found = .false.
        return
    end if

    nullify(p)

    call json%get(me=me, path=path, p=p)

    if (.not. associated(p)) then

        call json%throw_exception('Error in json_get_double:'//&
                             ' Unable to resolve path: '//trim(path))

    else

        call json%get(p,value)
        nullify(p)

    end if

    if (json%exception_thrown) then
        if (present(found)) then
            found = .false.
            call json%clear_exceptions()
        end if
    else
        if (present(found)) found = .true.
    end if

    end subroutine json_get_double_with_path
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_get_double_with_path]], where "path" is kind=CDK

    subroutine wrap_json_get_double_with_path(json, me, path, value, found)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer             :: me
    character(kind=CDK,len=*),intent(in) :: path
    real(RK),intent(out)                 :: value
    logical(LK),intent(out),optional     :: found

    call json%get(me,to_unicode(path),value,found)

    end subroutine wrap_json_get_double_with_path
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 5/14/2014
!
!  Get a double vector from a [[json_value]].

    subroutine json_get_double_vec(json, me, vec)

    implicit none

    class(json_core),intent(inout)                :: json
    type(json_value),pointer                      :: me
    real(RK),dimension(:),allocatable,intent(out) :: vec

    logical(LK) :: initialized

    initialized = .false.

    !the callback function is called for each element of the array:
    call json%get(me, array_callback=get_double_from_array)

    contains

        subroutine get_double_from_array(json, element, i, count)

        !! callback function for double

        implicit none

        class(json_core),intent(inout)      :: json
        type(json_value),pointer,intent(in) :: element
        integer(IK),intent(in)              :: i        !! index
        integer(IK),intent(in)              :: count    !! size of array

        !size the output array:
        if (.not. initialized) then
            allocate(vec(count))
            initialized = .true.
        end if

        !populate the elements:
        call json%get(element, value=vec(i))

        end subroutine get_double_from_array

    end subroutine json_get_double_vec
!*****************************************************************************************

!*****************************************************************************************
!>
!  Get a double vector from a [[json_value]], given the path.

    subroutine json_get_double_vec_with_path(json, me, path, vec, found)

    implicit none

    class(json_core),intent(inout)                :: json
    type(json_value),pointer                      :: me
    character(kind=CK,len=*),intent(in)           :: path
    real(RK),dimension(:),allocatable,intent(out) :: vec
    logical(LK),intent(out),optional              :: found

    logical(LK) :: initialized

    initialized = .false.

    !the callback function is called for each element of the array:
    call json%get(me, path=path, array_callback=get_double_from_array, found=found)

    contains

        subroutine get_double_from_array(json, element, i, count)
        !! callback function for double

        implicit none

        class(json_core),intent(inout)      :: json
        type(json_value),pointer,intent(in) :: element
        integer(IK),intent(in)              :: i        !! index
        integer(IK),intent(in)              :: count    !! size of array

        !size the output array:
        if (.not. initialized) then
            allocate(vec(count))
            initialized = .true.
        end if

        !populate the elements:
        call json%get(element, value=vec(i))

        end subroutine get_double_from_array

    end subroutine json_get_double_vec_with_path
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_get_double_vec_with_path]], where "path" is kind=CDK

    subroutine wrap_json_get_double_vec_with_path(json, me, path, vec, found)

    implicit none

    class(json_core),intent(inout)                :: json
    type(json_value),pointer                      :: me
    character(kind=CDK,len=*),intent(in)          :: path
    real(RK),dimension(:),allocatable,intent(out) :: vec
    logical(LK),intent(out),optional              :: found

    call json%get(me, to_unicode(path), vec, found)

    end subroutine wrap_json_get_double_vec_with_path
!*****************************************************************************************

!*****************************************************************************************
!>
!  Get a logical value from a [[json_value]].

    subroutine json_get_logical(json, me, value)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer,intent(in) :: me
    logical(LK)                         :: value

    value = .false.
    if ( json%exception_thrown ) return

    if (me%var_type == json_logical) then
        value = me%log_value
    else
        if (json%strict_type_checking) then
            call json%throw_exception('Error in json_get_logical: '//&
                                      'Unable to resolve value to logical: '//&
                                      me%name)
        else
            !type conversions
            select case (me%var_type)
            case (json_integer)
                value = (me%int_value > 0)
            case default
                call json%throw_exception('Error in json_get_logical: '//&
                                          'Unable to resolve value to logical: '//&
                                          me%name)
            end select
        end if
    end if

    end subroutine json_get_logical
!*****************************************************************************************

!*****************************************************************************************
!>
!  Get a logical value from a [[json_value]], given the path.

    subroutine json_get_logical_with_path(json, me, path, value, found)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer,intent(in) :: me
    character(kind=CK,len=*),intent(in) :: path
    logical(LK)                         :: value
    logical(LK),intent(out),optional    :: found

    type(json_value),pointer :: p

    value = .false.
    if ( json%exception_thrown) then
        if ( present(found) ) found = .false.
        return
    end if

    nullify(p)

    call json%get(me=me, path=path, p=p)

    if (.not. associated(p)) then

        call json%throw_exception('Error in json_get_logical:'//&
                             ' Unable to resolve path: '//trim(path))

    else

        call json%get(p,value)
        nullify(p)

    end if

    if (json%exception_thrown) then
        if (present(found)) then
            found = .false.
            call json%clear_exceptions()
        end if
    else
        if (present(found)) found = .true.
    end if

    end subroutine json_get_logical_with_path
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_get_logical_with_path]], where "path" is kind=CDK

    subroutine wrap_json_get_logical_with_path(json, me, path, value, found)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer,intent(in)  :: me
    character(kind=CDK,len=*),intent(in) :: path
    logical(LK)                          :: value
    logical(LK),intent(out),optional     :: found

    call json%get(me,to_unicode(path),value,found)

    end subroutine wrap_json_get_logical_with_path
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 5/14/2014
!
!  Get a logical vector from [[json_value]].

    subroutine json_get_logical_vec(json, me, vec)

    implicit none

    class(json_core),intent(inout)                   :: json
    type(json_value),pointer,intent(in)              :: me
    logical(LK),dimension(:),allocatable,intent(out) :: vec

    logical(LK) :: initialized

    initialized = .false.

    !the callback function is called for each element of the array:
    call json%get(me, array_callback=get_logical_from_array)

    contains

        subroutine get_logical_from_array(json, element, i, count)

        !! callback function for logical

        implicit none

        class(json_core),intent(inout)      :: json
        type(json_value),pointer,intent(in) :: element
        integer(IK),intent(in)              :: i        !! index
        integer(IK),intent(in)              :: count    !! size of array

        !size the output array:
        if (.not. initialized) then
            allocate(vec(count))
            initialized = .true.
        end if

        !populate the elements:
        call json%get(element, value=vec(i))

        end subroutine get_logical_from_array

    end subroutine json_get_logical_vec
!*****************************************************************************************

!*****************************************************************************************
!>
!  Get a logical vector from a [[json_value]], given the path.

    subroutine json_get_logical_vec_with_path(json, me, path, vec, found)

    implicit none

    class(json_core),intent(inout)                   :: json
    type(json_value),pointer,intent(in)              :: me
    character(kind=CK,len=*),intent(in)              :: path
    logical(LK),dimension(:),allocatable,intent(out) :: vec
    logical(LK),intent(out),optional                 :: found

    logical(LK) :: initialized

    initialized = .false.

    !the callback function is called for each element of the array:
    call json%get(me, path=path, array_callback=get_logical_from_array, found=found)

    contains

        subroutine get_logical_from_array(json, element, i, count)

        !! callback function for logical

        implicit none

        class(json_core),intent(inout)      :: json
        type(json_value),pointer,intent(in) :: element
        integer(IK),intent(in)              :: i        !! index
        integer(IK),intent(in)              :: count    !! size of array

        !size the output array:
        if (.not. initialized) then
            allocate(vec(count))
            initialized = .true.
        end if

        !populate the elements:
        call json%get(element, value=vec(i))

        end subroutine get_logical_from_array

    end subroutine json_get_logical_vec_with_path
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_get_logical_vec_with_path]], where "path" is kind=CDK

    subroutine wrap_json_get_logical_vec_with_path(json, me, path, vec, found)

    implicit none

    class(json_core),intent(inout)                   :: json
    type(json_value),pointer,intent(in)              :: me
    character(kind=CDK,len=*),intent(in)             :: path
    logical(LK),dimension(:),allocatable,intent(out) :: vec
    logical(LK),intent(out),optional                 :: found

    call json%get(me,to_unicode(path),vec,found)

    end subroutine wrap_json_get_logical_vec_with_path
!*****************************************************************************************

!*****************************************************************************************
!>
!  Get a character string from a [[json_value]].

    subroutine json_get_string(json, me, value)

    implicit none

    class(json_core),intent(inout)                   :: json
    type(json_value),pointer,intent(in)              :: me
    character(kind=CK,len=:),allocatable,intent(out) :: value

    character(kind=CK,len=:),allocatable :: error_message  !! for [[unescape_string]]

    value = ''
    if (.not. json%exception_thrown) then

        if (me%var_type == json_string) then

            if (allocated(me%str_value)) then
                if (json%unescaped_strings) then
                    call unescape_string(me%str_value, value, error_message)
                    if (allocated(error_message)) then
                        call json%throw_exception(error_message)
                        deallocate(error_message)
                        value = ''
                    end if
                else
                    value = me%str_value
                end if
            else
               call json%throw_exception('Error in json_get_string: '//&
                                         'me%str_value not allocated')
            end if

        else

            if (json%strict_type_checking) then
                call json%throw_exception('Error in json_get_string:'//&
                                          ' Unable to resolve value to string: '//me%name)
            else

                select case (me%var_type)

                case (json_integer)

                    if (allocated(me%int_value)) then
                        value = repeat(' ', max_integer_str_len)
                        call integer_to_string(me%int_value,int_fmt,value)
                        value = trim(value)
                    else
                        call json%throw_exception('Error in json_get_string: '//&
                                                  'me%int_value not allocated')
                    end if

                case (json_double)

                    if (allocated(me%dbl_value)) then
                        value = repeat(' ', max_numeric_str_len)
                        call real_to_string(me%dbl_value,json%real_fmt,&
                                            json%compact_real,value)
                        value = trim(value)
                    else
                        call json%throw_exception('Error in dbl_value: '//&
                                                  'me%int_value not allocated')
                    end if

                case (json_logical)

                    if (allocated(me%log_value)) then
                        if (me%log_value) then
                            value = true_str
                        else
                            value = false_str
                        end if
                    else
                        call json%throw_exception('Error in json_get_string: '//&
                                                  'me%log_value not allocated')
                    end if

                case (json_null)

                    value = null_str

                case default

                    call json%throw_exception('Error in json_get_string: '//&
                                              'Unable to resolve value to characters: '//&
                                              me%name)

                end select

            end if
        end if

    end if

    end subroutine json_get_string
!*****************************************************************************************

!*****************************************************************************************
!>
!  Get a character string from a [[json_value]], given the path.

    subroutine json_get_string_with_path(json, me, path, value, found)

    implicit none

    class(json_core),intent(inout)                   :: json
    type(json_value),pointer,intent(in)              :: me
    character(kind=CK,len=*),intent(in)              :: path
    character(kind=CK,len=:),allocatable,intent(out) :: value
    logical(LK),intent(out),optional                 :: found

    type(json_value),pointer :: p

    value = ''
    if ( json%exception_thrown ) then
        if ( present(found) ) found = .false.
        return
    end if

    nullify(p)

    call json%get(me=me, path=path, p=p)

    if (.not. associated(p)) then
        call json%throw_exception('Error in json_get_string_with_path:'//&
                                  ' Unable to resolve path: '//trim(path))

    else

        call json%get(p,value)
        nullify(p)

    end if

    if (allocated(value) .and. .not. json%exception_thrown) then
        if (present(found)) found = .true.
    else
        if (present(found)) then
            found = .false.
            call json%clear_exceptions()
        end if
    end if

    !cleanup:
    if (associated(p)) nullify(p)

    end subroutine json_get_string_with_path
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_get_string_with_path]], where "path" is kind=CDK

    subroutine wrap_json_get_string_with_path(json, me, path, value, found)

    implicit none

    class(json_core),intent(inout)                   :: json
    type(json_value),pointer,intent(in)              :: me
    character(kind=CDK,len=*),intent(in)             :: path
    character(kind=CK,len=:),allocatable,intent(out) :: value
    logical(LK),intent(out),optional                 :: found

    call json%get(me,to_unicode(path),value,found)

    end subroutine wrap_json_get_string_with_path
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 5/14/2014
!
!  Get a string vector from a [[json_value(type)]].

    subroutine json_get_string_vec(json, me, vec)

    implicit none

    class(json_core),intent(inout)                                :: json
    type(json_value),pointer,intent(in)                           :: me
    character(kind=CK,len=*),dimension(:),allocatable,intent(out) :: vec

    logical(LK) :: initialized

    initialized = .false.

    !the callback function is called for each element of the array:
    call json%get(me, array_callback=get_chars_from_array)

    contains

        subroutine get_chars_from_array(json, element, i, count)

        !! callback function for chars

        implicit none

        class(json_core),intent(inout)      :: json
        type(json_value),pointer,intent(in) :: element
        integer(IK),intent(in)              :: i        !! index
        integer(IK),intent(in)              :: count    !! size of array

        character(kind=CK,len=:),allocatable :: cval

        !size the output array:
        if (.not. initialized) then
            allocate(vec(count))
            initialized = .true.
        end if

        !populate the elements:
        call json%get(element, value=cval)
        if (allocated(cval)) then
            vec(i) = cval
            deallocate(cval)
        else
            vec(i) = ''
        end if

        end subroutine get_chars_from_array

    end subroutine json_get_string_vec
!*****************************************************************************************

!*****************************************************************************************
!>
!  Get a string vector from a [[json_value(type)]], given the path.

    subroutine json_get_string_vec_with_path(json, me, path, vec, found)

    implicit none

    class(json_core),intent(inout)                                :: json
    type(json_value),pointer,intent(in)                           :: me
    character(kind=CK,len=*),intent(in)                           :: path
    character(kind=CK,len=*),dimension(:),allocatable,intent(out) :: vec
    logical(LK),intent(out),optional                              :: found

    logical(LK) :: initialized

    initialized = .false.

    !the callback function is called for each element of the array:
    call json%get(me, path=path, array_callback=get_chars_from_array, found=found)

    contains

        subroutine get_chars_from_array(json, element, i, count)

        !! callback function for chars

        implicit none

        class(json_core),intent(inout)      :: json
        type(json_value),pointer,intent(in) :: element
        integer(IK),intent(in)              :: i        !! index
        integer(IK),intent(in)              :: count    !! size of array

        character(kind=CK,len=:),allocatable :: cval

        !size the output array:
        if (.not. initialized) then
            allocate(vec(count))
            initialized = .true.
        end if

        !populate the elements:
        call json%get(element, value=cval)
        if (allocated(cval)) then
            vec(i) = cval
            deallocate(cval)
        else
            vec(i) = ''
        end if

        end subroutine get_chars_from_array

    end subroutine json_get_string_vec_with_path
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_get_string_vec_with_path]], where "path" is kind=CDK

    subroutine wrap_json_get_string_vec_with_path(json, me, path, vec, found)

    implicit none

    class(json_core),intent(inout)                                :: json
    type(json_value),pointer,intent(in)                           :: me
    character(kind=CDK,len=*),intent(in)                          :: path
    character(kind=CK,len=*),dimension(:),allocatable,intent(out) :: vec
    logical(LK),intent(out),optional                              :: found

    call json%get(me,to_unicode(path),vec,found)

    end subroutine wrap_json_get_string_vec_with_path
!*****************************************************************************************

!*****************************************************************************************
!>
!  This routine calls the user-supplied [[json_array_callback_func]] subroutine
!      for each element in the array.
!
!@note For integer, double, logical, and character arrays,
!      higher-level routines are provided (see `get` methods), so
!      this routine does not have to be used for those cases.

    subroutine json_get_array(json, me, array_callback)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer,intent(in) :: me
    procedure(json_array_callback_func) :: array_callback

    type(json_value),pointer :: element !! temp variable for getting elements
    integer(IK) :: i      !! counter
    integer(IK) :: count  !! number of elements in the array

    if ( json%exception_thrown ) return

    nullify(element)

    select case (me%var_type)
    case (json_array)
        count = json%count(me)
        element => me%children
        do i = 1, count ! callback for each child
            if (.not. associated(element)) then
                call json%throw_exception('Error in json_get_array: '//&
                                          'Malformed JSON linked list')
                return
            end if
            call array_callback(json, element, i, count)
            if (json%exception_thrown) exit
            element => element%next
        end do
    case default

        call json%throw_exception('Error in json_get_array:'//&
                                  ' Resolved value is not an array ')

    end select

    !cleanup:
    if (associated(element)) nullify(element)

    end subroutine json_get_array
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 4/28/2016
!
!  Traverse a JSON structure.
!  This routine calls the user-specified [[json_traverse_callback_func]]
!  for each element of the structure.

    subroutine json_traverse(json,p,traverse_callback)

    implicit none

    class(json_core),intent(inout)         :: json
    type(json_value),pointer,intent(in)    :: p
    procedure(json_traverse_callback_func) :: traverse_callback

    logical(LK) :: finished !! can be used to stop the process

    if (.not. json%exception_thrown) call traverse(p)

    contains

        recursive subroutine traverse(p)

        !! recursive [[json_value]] traversal.

        implicit none

        type(json_value),pointer,intent(in) :: p

        type(json_value),pointer :: element  !! a child element
        integer(IK) :: i        !! counter
        integer(IK) :: icount   !! number of children

        if (json%exception_thrown) return
        call traverse_callback(json,p,finished) ! first call for this object
        if (finished) return

        !for arrays and objects, have to also call for all children:
        if (p%var_type==json_array .or. p%var_type==json_object) then

            icount = json%count(p) ! number of children
            if (icount>0) then
                element => p%children   ! first one
                do i = 1, icount        ! call for each child
                    if (.not. associated(element)) then
                        call json%throw_exception('Error in json_traverse: '//&
                                                  'Malformed JSON linked list')
                        return
                    end if
                    call traverse(element)
                    if (finished .or. json%exception_thrown) exit
                    element => element%next
                end do
            end if
            nullify(element)

        end if

        end subroutine traverse

    end subroutine json_traverse
!*****************************************************************************************

!*****************************************************************************************
!>
!  This routine calls the user-supplied array_callback subroutine
!  for each element in the array (specified by the path).

    subroutine json_get_array_with_path(json, me, path, array_callback, found)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer,intent(in) :: me
    character(kind=CK,len=*),intent(in) :: path
    procedure(json_array_callback_func) :: array_callback
    logical(LK),intent(out),optional    :: found

    type(json_value),pointer :: p

    if ( json%exception_thrown ) then
        if ( present(found) ) found = .false.
        return
    end if

    nullify(p)

    ! resolve the path to the value
    call json%get(me=me, path=path, p=p)

    if (.not. associated(p)) then
        call json%throw_exception('Error in json_get_array:'//&
             ' Unable to resolve path: '//trim(path))
    else
       call json%get(me=p,array_callback=array_callback)
       nullify(p)
    end if
    if ( json%exception_thrown ) then
        if ( present(found) ) then
            found = .false.
            call json%clear_exceptions()
        end if
    else
        if ( present(found) ) found = .true.
    end if

    end subroutine json_get_array_with_path
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_get_array_with_path]], where "path" is kind=CDK

    subroutine wrap_json_get_array_with_path(json, me, path, array_callback, found)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer,intent(in)  :: me
    character(kind=CDK,len=*),intent(in) :: path
    procedure(json_array_callback_func)  :: array_callback
    logical(LK),intent(out),optional     :: found

    call json%get(me, to_unicode(path), array_callback, found)

    end subroutine wrap_json_get_array_with_path
!*****************************************************************************************

!*****************************************************************************************
!>
!  Parse the JSON file and populate the [[json_value]] tree.
!
!# Inputs
!
!  The inputs can be:
!
!  * `file` & `unit` : the specified unit is used to read JSON from file.
!                      [note if unit is already open, then the filename is ignored]
!  * `file`          : JSON is read from file using internal unit number
!
!# Example
!
!````fortran
!    type(json_core) :: json
!    type(json_value),pointer :: p
!    call json%parse(file='myfile.json', p=p)
!````
!
!# History
!  * Jacob Williams : 01/13/2015 : added read from string option.
!  * Izaak Beekman  : 03/08/2015 : moved read from string to separate
!    subroutine, and error annotation to separate subroutine.
!
!@note When calling this routine, any exceptions thrown from previous
!      calls will automatically be cleared.

    subroutine json_parse_file(json, file, p, unit)

    implicit none

    class(json_core),intent(inout)       :: json
    character(kind=CDK,len=*),intent(in) :: file  !! JSON file name
    type(json_value),pointer             :: p     !! output structure
    integer(IK),intent(in),optional      :: unit  !! file unit number (/= 0)

    integer(IK) :: iunit   !! file unit actually used
    integer(IK) :: istat   !! iostat flag
    logical(LK) :: is_open !! if the file is already open

    !clear any exceptions and initialize:
    call json%initialize()

    if ( present(unit) ) then

        if (unit==0) then
            call json%throw_exception('Error in json_parse_file: unit number must not be 0.')
            return
        end if

        iunit = unit

        !check to see if the file is already open
        ! if it is, then use it, otherwise open the file with the name given.
        inquire(unit=iunit, opened=is_open, iostat=istat)
        if (istat==0 .and. .not. is_open) then
           ! open the file
            open (  unit        = iunit, &
                    file        = file, &
                    status      = 'OLD', &
                    action      = 'READ', &
                    form        = form_spec, &
                    access      = access_spec, &
                    iostat      = istat &
                    FILE_ENCODING )
        else
            !if the file is already open, then we need to make sure
            ! that it is open with the correct form/access/etc...
        end if

    else

        ! open the file with a new unit number:
        open (  newunit     = iunit, &
                file        = file, &
                status      = 'OLD', &
                action      = 'READ', &
                form        = form_spec, &
                access      = access_spec, &
                iostat      = istat &
                FILE_ENCODING )

    end if

    if (istat==0) then

        ! create the value and associate the pointer
        call json_value_create(p)

        ! Note: the name of the root json_value doesn't really matter,
        !  but we'll allocate something here just in case.
        p%name = trim(file)  !use the file name

        ! parse as a value
        call json%parse_value(unit=iunit, str=CK_'', value=p)
        if (json%exception_thrown) call json%annotate_invalid_json(iunit,CK_'')

        ! close the file if necessary
        close(unit=iunit, iostat=istat)

    else

        call json%throw_exception('Error in json_parse_file: Error opening file: '//trim(file))
        nullify(p)

    end if

    end subroutine json_parse_file
!*****************************************************************************************

!*****************************************************************************************
!>
!  Parse the JSON string and populate the [[json_value]] tree.
!
!# See also
!  * [[json_parse_file]]

    subroutine json_parse_string(json, p, str)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer            :: p     !! output structure
    character(kind=CK,len=*),intent(in) :: str   !! string with JSON data

    integer(IK),parameter :: iunit = 0 !! indicates that json data will be read from buffer

    !clear any exceptions and initialize:
    call json%initialize()

    ! create the value and associate the pointer
    call json_value_create(p)

    ! Note: the name of the root json_value doesn't really matter,
    !  but we'll allocate something here just in case.
    p%name = ''

    ! parse as a value
    call json%parse_value(unit=iunit, str=str, value=p)

    if (json%exception_thrown) call json%annotate_invalid_json(iunit,str)

    end subroutine json_parse_string
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_parse_string]], where `str` is kind=CDK.

    subroutine wrap_json_parse_string(json, p, str)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer             :: p     !! output structure
    character(kind=CDK,len=*),intent(in) :: str   !! string with JSON data

    call json%parse(p,to_unicode(str))

    end subroutine wrap_json_parse_string
!*****************************************************************************************

!*****************************************************************************************
!>
!  Generate a warning message if there was an error parsing a JSON
!  file or string.

    subroutine annotate_invalid_json(json,iunit,str)

    implicit none

    class(json_core),intent(inout)      :: json
    integer(IK),intent(in)              :: iunit !! file unit number
    character(kind=CK,len=*),intent(in) :: str   !! string with JSON data

    character(kind=CK,len=:),allocatable :: line, arrow_str
    character(kind=CK,len=10) :: line_str, char_str
    integer(IK) :: i, i_nl_prev, i_nl

    !  If there was an error reading the file, then
    !   print the line where the error occurred:
    if (json%exception_thrown) then

        !the counters for the current line and the last character read:
        call integer_to_string(json%line_count, int_fmt, line_str)
        call integer_to_string(json%char_count, int_fmt, char_str)

        !draw the arrow string that points to the current character:
        arrow_str = repeat('-',max( 0, json%char_count - 1) )//'^'

        if (json%line_count>0 .and. json%char_count>0) then

            if (iunit/=0) then

                if (use_unformatted_stream) then
                    call json%get_current_line_from_file_stream(iunit,line)
                else
                    call json%get_current_line_from_file_sequential(iunit,line)
                end if

            else

                !get the current line from the string:
                ! [this is done by counting the newline characters]
                i_nl_prev = 0  !index of previous newline character
                i_nl = 2  !just in case line_count = 0
                do i=1,json%line_count
                    i_nl = index(str(i_nl_prev+1:),newline)
                    if (i_nl==0) then   !last line - no newline character
                        i_nl = len(str)+1
                        exit
                    end if
                    i_nl = i_nl + i_nl_prev   !index of current newline character
                    i_nl_prev = i_nl          !update for next iteration
                end do
                line = str(i_nl_prev+1 : i_nl-1)  !extract current line

            end if

        else
            !in this case, it was an empty line or file
            line = ''
        end if

        !create the error message:
        json%err_message = json%err_message//newline//&
                           'line: '//trim(adjustl(line_str))//', '//&
                           'character: '//trim(adjustl(char_str))//newline//&
                           trim(line)//newline//arrow_str

        if (allocated(line)) deallocate(line)

    end if

    end subroutine annotate_invalid_json
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Rewind the file to the beginning of the current line, and return this line.
!  The file is assumed to be opened.
!  This is the SEQUENTIAL version (see also [[get_current_line_from_file_stream]]).

    subroutine get_current_line_from_file_sequential(json,iunit,line)

    implicit none

    class(json_core),intent(inout)                   :: json
    integer(IK),intent(in)                           :: iunit  !! file unit number
    character(kind=CK,len=:),allocatable,intent(out) :: line   !! current line

    character(kind=CK,len=seq_chunk_size) :: chunk !! for reading line in chunks
    integer(IK) :: istat  !! iostat flag
    integer(IK) :: isize  !! number of characters read in read statement

    !initialize:
    line = ''

    !rewind to beginning of the current record:
    backspace(iunit, iostat=istat)

    !loop to read in all the characters in the current record.
    ![the line is read in chunks until the end of the line is reached]
    if (istat==0) then
        do
            isize = 0
            read(iunit,fmt='(A)',advance='NO',size=isize,iostat=istat) chunk
            if (istat==0) then
                line = line//chunk
            else
                if (isize>0 .and. isize<=seq_chunk_size) line = line//chunk(1:isize)
                exit
            end if
        end do
    end if

    end subroutine get_current_line_from_file_sequential
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Rewind the file to the beginning of the current line, and return this line.
!  The file is assumed to be opened.
!  This is the STREAM version (see also [[get_current_line_from_file_sequential]]).

    subroutine get_current_line_from_file_stream(json,iunit,line)

    implicit none

    class(json_core),intent(inout)                   :: json
    integer(IK),intent(in)                           :: iunit  !! file unit number
    character(kind=CK,len=:),allocatable,intent(out) :: line   !! current line

    integer(IK) :: istart,iend,ios
    character(kind=CK,len=1) :: c

    istart = json%ipos
    do
        if (istart<=1) then
            istart = 1
            exit
        end if
        read(iunit,pos=istart,iostat=ios) c
        if (c==newline .or. ios/=0) then
            if (istart/=1) istart = istart - 1
            exit
        end if
        istart = istart-1  !rewind until the beginning of the line
    end do
    iend = json%ipos
    do
        read(iunit,pos=iend,iostat=ios) c
        if (c==newline .or. ios/=0) exit
        iend=iend+1
    end do
    allocate( character(kind=CK,len=iend-istart+1) :: line )
    read(iunit,pos=istart,iostat=ios) line

    end subroutine get_current_line_from_file_stream
!*****************************************************************************************

!*****************************************************************************************
!>
!  Core parsing routine.

    recursive subroutine parse_value(json, unit, str, value)

    implicit none

    class(json_core),intent(inout)      :: json
    integer(IK),intent(in)              :: unit   !! file unit number
    character(kind=CK,len=*),intent(in) :: str    !! string containing JSON data (only used if unit=0)
    type(json_value),pointer            :: value  !! JSON data that is extracted

    logical(LK)              :: eof !! end-of-file flag
    character(kind=CK,len=1) :: c   !! character read from file (or string)
#if defined __GFORTRAN__
    character(kind=CK,len=:),allocatable :: tmp  !! this is a work-around for a bug
                                                 !! in the gfortran 4.9 compiler.
#endif

    if (.not. json%exception_thrown) then

        !the routine is being called incorrectly.
        if (.not. associated(value)) then
            call json%throw_exception('Error in parse_value: value pointer not associated.')
        end if

        ! pop the next non whitespace character off the file
        c = json%pop_char(unit, str=str, eof = eof, skip_ws = .true.)

        if (eof) then
            return
        else
            select case (c)
            case (start_object)

                ! start object
                call to_object(value)    !allocate class
                call json%parse_object(unit, str, value)

            case (start_array)

                ! start array
                call to_array(value)    !allocate class
                call json%parse_array(unit, str, value)

            case (end_array)

                ! end an empty array
                call json%push_char(c)
                nullify(value)

            case (quotation_mark)

                ! string
                call to_string(value)    !allocate class

                select case (value%var_type)
                case (json_string)
#if defined __GFORTRAN__
                    call json%parse_string(unit,str,tmp)  ! write to a tmp variable because of
                    value%str_value = tmp                 ! a bug in 4.9 gfortran compiler.
                    deallocate(tmp)                       !
#else
                    call json%parse_string(unit, str, value%str_value)
#endif
                end select

            case (CK_'t') !true_str(1:1) gfortran bug work around

                !true
                call json%parse_for_chars(unit, str, true_str(2:))
                !allocate class and set value:
                if (.not. json%exception_thrown) call to_logical(value,.true.)

            case (CK_'f') !false_str(1:1) gfortran bug work around

                !false
                call json%parse_for_chars(unit, str, false_str(2:))
                !allocate class and set value:
                if (.not. json%exception_thrown) call to_logical(value,.false.)

            case (CK_'n') !null_str(1:1) gfortran bug work around

                !null
                call json%parse_for_chars(unit, str, null_str(2:))
                if (.not. json%exception_thrown) call to_null(value)    !allocate class

            case(CK_'-', CK_'0': CK_'9')

                call json%push_char(c)
                call json%parse_number(unit, str, value)

            case default

                call json%throw_exception('Error in parse_value:'//&
                                     ' Unexpected character while parsing value. "'//&
                                     c//'"')

            end select
        end if

    end if

    end subroutine parse_value
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Allocate a [[json_value]] pointer and make it a logical(LK) variable.
!  The pointer should not already be allocated.
!
!# Example
!````fortran
!     type(json_value),pointer :: p
!     type(json_core) :: json
!     call json%create_logical(p,'value',.true.)
!````

    subroutine json_value_create_logical(json,p,val,name)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer            :: p
    character(kind=CK,len=*),intent(in) :: name  !! variable name
    logical(LK),intent(in)              :: val   !! variable value

    call json_value_create(p)
    call to_logical(p,val,name)

    end subroutine json_value_create_logical
!*****************************************************************************************

!*****************************************************************************************
!> author: Izaak Beekman
!
!  Wrapper for [[json_value_create_logical]] so `create_logical` method can
!  be called with name of character kind 'DEFAULT' or 'ISO_10646'

    subroutine wrap_json_value_create_logical(json,p,val,name)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer             :: p
    character(kind=CDK,len=*),intent(in) :: name
    logical(LK),intent(in)               :: val

    call json%create_logical(p,val,to_unicode(name))

    end subroutine wrap_json_value_create_logical
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Allocate a [[json_value]] pointer and make it an integer(IK) variable.
!  The pointer should not already be allocated.
!
!# Example
!````fortran
!     type(json_value),pointer :: p
!     type(json_core) :: json
!     call json%create_integer(p,'value',1)
!````

    subroutine json_value_create_integer(json,p,val,name)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer            :: p
    character(kind=CK,len=*),intent(in) :: name
    integer(IK),intent(in)              :: val

    call json_value_create(p)
    call to_integer(p,val,name)

    end subroutine json_value_create_integer
!*****************************************************************************************

!*****************************************************************************************
!> author: Izaak Beekman
!
!  A wrapper procedure for [[json_value_create_integer]] so that `create_integer`
!  method may be called with either a 'DEFAULT' or 'ISO_10646' character kind
!  `name` actual argument.

    subroutine wrap_json_value_create_integer(json,p,val,name)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer             :: p
    character(kind=CDK,len=*),intent(in) :: name
    integer(IK),intent(in)               :: val

    call json%create_integer(p,val,to_unicode(name))

    end subroutine wrap_json_value_create_integer
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Allocate a [[json_value]] pointer and make it a real(RK) variable.
!  The pointer should not already be allocated.
!
!# Example
!````fortran
!     type(json_value),pointer :: p
!     type(json_core) :: json
!     call json%create_double(p,'value',1.0_RK)
!````

    subroutine json_value_create_double(json,p,val,name)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer            :: p
    character(kind=CK,len=*),intent(in) :: name
    real(RK),intent(in)                 :: val

    call json_value_create(p)
    call to_double(p,val,name)

    end subroutine json_value_create_double
!*****************************************************************************************

!*****************************************************************************************
!> author: Izaak Beekman
!
!  A wrapper for [[json_value_create_double]] so that `create_double` method
!  may be called with an actual argument corresponding to the dummy argument,
!  `name` that may be of 'DEFAULT' or 'ISO_10646' character kind.

    subroutine wrap_json_value_create_double(json,p,val,name)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer             :: p
    character(kind=CDK,len=*),intent(in) :: name
    real(RK),intent(in)                  :: val

    call json%create_double(p,val,to_unicode(name))

    end subroutine wrap_json_value_create_double
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Allocate a json_value pointer and make it a string variable.
!  The pointer should not already be allocated.
!
!# Example
!````fortran
!     type(json_value),pointer :: p
!     type(json_core) :: json
!     call json%create_string(p,'value','hello')
!````

    subroutine json_value_create_string(json,p,val,name)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer            :: p
    character(kind=CK,len=*),intent(in) :: name
    character(kind=CK,len=*),intent(in) :: val

    call json_value_create(p)
    call to_string(p,val,name)

    end subroutine json_value_create_string
!*****************************************************************************************

!*****************************************************************************************
!> author: Izaak Beekman
!
!  Wrap [[json_value_create_string]] so that `create_string` method may be called
!  with actual character string arguments for `name` and `val` that are BOTH of
!  'DEFAULT' or 'ISO_10646' character kind.

    subroutine wrap_json_value_create_string(json,p,val,name)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer             :: p
    character(kind=CDK,len=*),intent(in) :: name
    character(kind=CDK,len=*),intent(in) :: val

    call json%create_string(p,to_unicode(val),to_unicode(name))

    end subroutine wrap_json_value_create_string
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Allocate a json_value pointer and make it a null variable.
!  The pointer should not already be allocated.
!
!# Example
!````fortran
!     type(json_value),pointer :: p
!     type(json_core) :: json
!     call json%create_null(p,'value')
!````

    subroutine json_value_create_null(json,p,name)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer            :: p
    character(kind=CK,len=*),intent(in) :: name

    call json_value_create(p)
    call to_null(p,name)

    end subroutine json_value_create_null
!*****************************************************************************************

!*****************************************************************************************
!> author: Izaak Beekman
!
!  Wrap [[json_value_create_null]] so that `create_null` method may be called with
!  an actual argument corresponding to the dummy argument `name` that is either
!  of 'DEFAULT' or 'ISO_10646' character kind.

    subroutine wrap_json_value_create_null(json,p,name)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer             :: p
    character(kind=CDK,len=*),intent(in) :: name

    call json%create_null(p,to_unicode(name))

    end subroutine wrap_json_value_create_null
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Allocate a [[json_value]] pointer and make it an object variable.
!  The pointer should not already be allocated.
!
!# Example
!````fortran
!     type(json_value),pointer :: p
!     type(json_core) :: json
!     call json%create_object(p,'objectname')
!````
!
!@note The name is not significant for the root structure or an array element.
!      In those cases, an empty string can be used.

    subroutine json_value_create_object(json,p,name)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer            :: p
    character(kind=CK,len=*),intent(in) :: name

    call json_value_create(p)
    call to_object(p,name)

    end subroutine json_value_create_object
!*****************************************************************************************

!*****************************************************************************************
!> author: Izaak Beekman
!
!  Wrap [[json_value_create_object]] so that `create_object` method may be called
!  with an actual argument corresponding to the dummy argument `name` that is of
!  either 'DEFAULT' or 'ISO_10646' character kind.

    subroutine wrap_json_value_create_object(json,p,name)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer             :: p
    character(kind=CDK,len=*),intent(in) :: name

    call json%create_object(p,to_unicode(name))

    end subroutine wrap_json_value_create_object
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Allocate a [[json_value]] pointer and make it an array variable.
!  The pointer should not already be allocated.
!
!# Example
!````fortran
!     type(json_value),pointer :: p
!     type(json_core) :: json
!     call json%create_array(p,'arrayname')
!````

    subroutine json_value_create_array(json,p,name)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer            :: p
    character(kind=CK,len=*),intent(in) :: name

    call json_value_create(p)
    call to_array(p,name)

    end subroutine json_value_create_array
!*****************************************************************************************

!*****************************************************************************************
!> author: Izaak Beekman
!
!  A wrapper for [[json_value_create_array]] so that `create_array` method may be
!  called with an actual argument, corresponding to the dummy argument `name`,
!  that is either of 'DEFAULT' or 'ISO_10646' character kind.

    subroutine wrap_json_value_create_array(json,p,name)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer             :: p
    character(kind=CDK,len=*),intent(in) :: name

    call json%create_array(p,to_unicode(name))

    end subroutine wrap_json_value_create_array
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Change the [[json_value]] variable to a logical.

    subroutine to_logical(p,val,name)

    implicit none

    type(json_value),intent(inout)               :: p
    logical(LK),intent(in),optional              :: val   !! if the value is also to be set (if not present, then .false. is used).
    character(kind=CK,len=*),intent(in),optional :: name  !! if the name is also to be changed.

    !set type and value:
    call destroy_json_data(p)
    p%var_type = json_logical
    allocate(p%log_value)
    if (present(val)) then
        p%log_value = val
    else
        p%log_value = .false.    !default value
    end if

    !name:
    if (present(name)) p%name = trim(name)

    end subroutine to_logical
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Change the [[json_value]] variable to an integer.

    subroutine to_integer(p,val,name)

    implicit none

    type(json_value),intent(inout)               :: p
    integer(IK),intent(in),optional              :: val   !! if the value is also to be set (if not present, then 0 is used).
    character(kind=CK,len=*),intent(in),optional :: name  !! if the name is also to be changed.

    !set type and value:
    call destroy_json_data(p)
    p%var_type = json_integer
    allocate(p%int_value)
    if (present(val)) then
        p%int_value = val
    else
        p%int_value = 0    !default value
    end if

    !name:
    if (present(name)) p%name = trim(name)

    end subroutine to_integer
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Change the [[json_value]] variable to a double.

    subroutine to_double(p,val,name)

    implicit none

    type(json_value),intent(inout)               :: p
    real(RK),intent(in),optional                 :: val   !! if the value is also to be set (if not present, then 0.0_rk is used).
    character(kind=CK,len=*),intent(in),optional :: name  !! if the name is also to be changed.

    !set type and value:
    call destroy_json_data(p)
    p%var_type = json_double
    allocate(p%dbl_value)
    if (present(val)) then
        p%dbl_value = val
    else
        p%dbl_value = 0.0_RK    !default value
    end if

    !name:
    if (present(name)) p%name = trim(name)

    end subroutine to_double
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Change the [[json_value]] variable to a string.
!
!# Modified
!  * Izaak Beekman : 02/24/2015
!

    subroutine to_string(p,val,name)

    implicit none

    type(json_value),intent(inout)               :: p
    character(kind=CK,len=*),intent(in),optional :: val   !! if the value is also to be set (if not present, then '' is used).
    character(kind=CK,len=*),intent(in),optional :: name  !! if the name is also to be changed.

    !set type and value:
    call destroy_json_data(p)
    p%var_type = json_string
    if (present(val)) then
        p%str_value = val
    else
        p%str_value = ''    !default value
    end if

    !name:
    if (present(name)) p%name = trim(name)

    end subroutine to_string
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Change the [[json_value]] variable to a null.

    subroutine to_null(p,name)

    implicit none

    type(json_value),intent(inout)               :: p
    character(kind=CK,len=*),intent(in),optional :: name  !! if the name is also to be changed.

    !set type and value:
    call destroy_json_data(p)
    p%var_type = json_null

    !name:
    if (present(name)) p%name = trim(name)

    end subroutine to_null
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Change the [[json_value]] variable to an object.

    subroutine to_object(p,name)

    implicit none

    type(json_value),intent(inout)               :: p
    character(kind=CK,len=*),intent(in),optional :: name  !! if the name is also to be changed.

    !set type and value:
    call destroy_json_data(p)
    p%var_type = json_object

    !name:
    if (present(name)) p%name = trim(name)

    end subroutine to_object
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Change the [[json_value]] variable to an array.

    subroutine to_array(p,name)

    implicit none

    type(json_value),intent(inout)               :: p
    character(kind=CK,len=*),intent(in),optional :: name  !! if the name is also to be changed.

    !set type and value:
    call destroy_json_data(p)
    p%var_type = json_array

    !name:
    if (present(name)) p%name = trim(name)

    end subroutine to_array
!*****************************************************************************************

!*****************************************************************************************
!>
!  Core parsing routine.

    recursive subroutine parse_object(json, unit, str, parent)

    implicit none

    class(json_core),intent(inout)      :: json
    integer(IK),intent(in)              :: unit    !! file unit number (if parsing from a file)
    character(kind=CK,len=*),intent(in) :: str     !! JSON string (if parsing from a string)
    type(json_value),pointer            :: parent  !! the parsed object will be added as a child of this

    type(json_value),pointer :: pair
    logical(LK) :: eof
    character(kind=CK,len=1) :: c
#if defined __GFORTRAN__
    character(kind=CK,len=:),allocatable :: tmp  !! this is a work-around for a bug
                                                 !! in the gfortran 4.9 compiler.
#endif

    if (.not. json%exception_thrown) then

        !the routine is being called incorrectly.
        if (.not. associated(parent)) then
            call json%throw_exception('Error in parse_object: parent pointer not associated.')
        end if

        nullify(pair)    !probably not necessary

        ! pair name
        c = json%pop_char(unit, str=str, eof = eof, skip_ws = .true.)
        if (eof) then
            call json%throw_exception('Error in parse_object:'//&
                                 ' Unexpected end of file while parsing start of object.')
            return
        else if (end_object == c) then
            ! end of an empty object
            return
        else if (quotation_mark == c) then
            call json_value_create(pair)
#if defined __GFORTRAN__
            call json%parse_string(unit,str,tmp)   ! write to a tmp variable because of
            pair%name = tmp                        ! a bug in 4.9 gfortran compiler.
            deallocate(tmp)
#else
            call json%parse_string(unit,str,pair%name)
#endif
            if (json%exception_thrown) then
                call json%destroy(pair)
                return
            end if
        else
            call json%throw_exception('Error in parse_object: Expecting string: "'//c//'"')
            return
        end if

        ! pair value
        c = json%pop_char(unit, str=str, eof = eof, skip_ws = .true.)
        if (eof) then
            call json%throw_exception('Error in parse_object:'//&
                                 ' Unexpected end of file while parsing object member.')
            return
        else if (colon_char == c) then
            ! parse the value
            call json%parse_value(unit, str, pair)
            if (json%exception_thrown) then
                call json%destroy(pair)
                return
            else
                call json%add(parent, pair)
            end if
        else
            call json%throw_exception('Error in parse_object:'//&
                                      ' Expecting : and then a value: '//c)
            return
        end if

        ! another possible pair
        c = json%pop_char(unit, str=str, eof = eof, skip_ws = .true.)
        if (eof) then
            call json%throw_exception('Error in parse_object: '//&
                                 'End of file encountered when parsing an object')
            return
        else if (delimiter == c) then
            ! read the next member
            call json%parse_object(unit = unit, str=str, parent = parent)
        else if (end_object == c) then
            ! end of object
            return
        else
            call json%throw_exception('Error in parse_object: Expecting end of object: '//c)
            return
        end if

    end if

    end subroutine parse_object
!*****************************************************************************************

!*****************************************************************************************
!>
!  Core parsing routine.

    recursive subroutine parse_array(json, unit, str, array)

    implicit none

    class(json_core),intent(inout)      :: json
    integer(IK),intent(in)              :: unit   !! file unit number (if parsing from a file)
    character(kind=CK,len=*),intent(in) :: str    !! JSON string (if parsing from a string)
    type(json_value),pointer            :: array

    type(json_value),pointer :: element
    logical(LK) :: eof
    character(kind=CK,len=1) :: c

    do

        if (json%exception_thrown) exit

        ! try to parse an element value
        nullify(element)
        call json_value_create(element)
        call json%parse_value(unit, str, element)
        if (json%exception_thrown) then
            if (associated(element)) call json%destroy(element)
            exit
        end if

        ! parse value will disassociate an empty array value
        if (associated(element)) call json%add(array, element)

        ! popped the next character
        c = json%pop_char(unit, str=str, eof = eof, skip_ws = .true.)

        if (eof) then
            ! The file ended before array was finished:
            call json%throw_exception('Error in parse_array: '//&
                                 'End of file encountered when parsing an array.')
            exit
        else if (delimiter == c) then
            ! parse the next element
            cycle
        else if (end_array == c) then
            ! end of array
            exit
        else
            call json%throw_exception('Error in parse_array: '//&
                                      'Unexpected character encountered when parsing array.')
            exit
        end if

    end do

    end subroutine parse_array
!*****************************************************************************************

!*****************************************************************************************
!>
!  Parses a string while reading a JSON file.
!
!# History
!  * Jacob Williams : 6/16/2014 : Added hex validation.
!  * Jacob Williams : 12/3/2015 : Fixed some bugs.

    subroutine parse_string(json, unit, str, string)

    implicit none

    class(json_core),intent(inout)                   :: json
    integer(IK),intent(in)                           :: unit  !! file unit number (if parsing from a file)
    character(kind=CK,len=*),intent(in)              :: str   !! JSON string (if parsing from a string)
    character(kind=CK,len=:),allocatable,intent(out) :: string

    logical(LK) :: eof, is_hex, escape
    character(kind=CK,len=1) :: c
    character(kind=CK,len=4) :: hex
    integer(IK) :: i
    integer(IK) :: ip !! index to put next character,
                      !! to speed up by reducing the number of character string reallocations.

    !at least return a blank string if there is a problem:
    string = repeat(space, chunk_size)

    if (.not. json%exception_thrown) then

        !initialize:
        ip     = 1
        is_hex = .false.
        escape = .false.
        i      = 0

        do

            !get the next character from the file:
            c = json%pop_char(unit, str=str, eof = eof, skip_ws = .false.)

            if (eof) then

                call json%throw_exception('Error in parse_string: Expecting end of string')
                return

            else if (c==quotation_mark .and. .not. escape) then  !end of string

                if (is_hex) call json%throw_exception('Error in parse_string:'//&
                                                 ' incomplete hex string: \u'//trim(hex))
                exit

            else

                !if the string is not big enough, then add another chunk:
                if (ip>len(string)) string = string // repeat(space, chunk_size)

                !append to string:
                string(ip:ip) = c
                ip = ip + 1

                !hex validation:
                if (is_hex) then  !accumulate the four characters after '\u'

                    i=i+1
                    hex(i:i) = c
                    if (i==4) then
                        if (valid_json_hex(hex)) then
                            i = 0
                            hex = ''
                            is_hex = .false.
                        else
                            call json%throw_exception('Error in parse_string:'//&
                                                 ' invalid hex string: \u'//trim(hex))
                            exit
                        end if
                    end if

                else

                    !when the '\u' string is encountered, then
                    !  start accumulating the hex string (should be the next 4 characters)
                    if (escape) then
                        escape = .false.
                        is_hex = (c=='u')    !the next four characters are the hex string
                    else
                        escape = (c==backslash)
                    end if

                end if

            end if

        end do

        !trim the string if necessary:
        if (ip<len(string)+1) then
            if (ip==1) then
                string = ''
            else
                string = string(1:ip-1)
            end if
        end if

    end if

    end subroutine parse_string
!*****************************************************************************************

!*****************************************************************************************
!>
!  Core parsing routine.

    subroutine parse_for_chars(json, unit, str, chars)

    implicit none

    class(json_core),intent(inout)      :: json
    integer(IK),intent(in)              :: unit   !! file unit number (if parsing from a file)
    character(kind=CK,len=*),intent(in) :: str    !! JSON string (if parsing from a string)
    character(kind=CK,len=*),intent(in) :: chars  !! the string to check for.

    integer(IK) :: i, length
    logical(LK) :: eof
    character(kind=CK,len=1) :: c

    if (.not. json%exception_thrown) then

        length = len_trim(chars)

        do i = 1, length
            c = json%pop_char(unit, str=str, eof = eof, skip_ws = .true.)
            if (eof) then
                call json%throw_exception('Error in parse_for_chars:'//&
                                     ' Unexpected end of file while parsing array.')
                return
            else if (c /= chars(i:i)) then
                call json%throw_exception('Error in parse_for_chars:'//&
                                     ' Unexpected character.: "'//c//'" '//chars(i:i))
                return
            end if
        end do

    end if

    end subroutine parse_for_chars
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/20/2014
!
!  Read a numerical value from the file (or string).
!  The routine will determine if it is an integer or a double, and
!  allocate the type accordingly.
!
!@note Complete rewrite of the original FSON routine, which had some problems.

    subroutine parse_number(json, unit, str, value)

    implicit none

    class(json_core),intent(inout)      :: json
    integer(IK),intent(in)              :: unit   !! file unit number (if parsing from a file)
    character(kind=CK,len=*),intent(in) :: str    !! JSON string (if parsing from a string)
    type(json_value),pointer            :: value

    character(kind=CK,len=:),allocatable :: tmp
    character(kind=CK,len=1) :: c
    logical(LK) :: eof
    real(RK) :: rval
    integer(IK) :: ival
    logical(LK) :: first
    logical(LK) :: is_integer
    integer(IK) :: ip !! index to put next character
                      !! [to speed up by reducing the number
                      !! of character string reallocations]

    if (.not. json%exception_thrown) then

        tmp = repeat(space, chunk_size)
        ip = 1
        first = .true.
        is_integer = .true.  !assume it may be an integer, unless otherwise determined

        !read one character at a time and accumulate the string:
        do

            !get the next character:
            c = json%pop_char(unit, str=str, eof = eof, skip_ws = .true.)

            if (eof) then
                call json%throw_exception('Error in parse_number:'//&
                                     ' Unexpected end of file while parsing number.')
                return
            else

                select case (c)
                case(CK_'-',CK_'+')    !note: allowing a '+' as the first character here.

                    if (is_integer .and. (.not. first)) is_integer = .false.

                    !add it to the string:
                    !tmp = tmp // c   !...original
                    if (ip>len(tmp)) tmp = tmp // repeat(space, chunk_size)
                    tmp(ip:ip) = c
                    ip = ip + 1

                case(CK_'.',CK_'E',CK_'e')    !can be present in real numbers

                    if (is_integer) is_integer = .false.

                    !add it to the string:
                    !tmp = tmp // c   !...original
                    if (ip>len(tmp)) tmp = tmp // repeat(space, chunk_size)
                    tmp(ip:ip) = c
                    ip = ip + 1

                case(CK_'0':CK_'9')    !valid characters for numbers

                    !add it to the string:
                    !tmp = tmp // c   !...original
                    if (ip>len(tmp)) tmp = tmp // repeat(space, chunk_size)
                    tmp(ip:ip) = c
                    ip = ip + 1

                case default

                    !push back the last character read:
                    call json%push_char(c)

                    !string to value:
                    if (is_integer) then
                        ival = json%string_to_integer(tmp)
                        call to_integer(value,ival)
                    else
                        rval = json%string_to_double(tmp)
                        call to_double(value,rval)
                    end if

                    exit    !finished

                end select

            end if
            if (first) first = .false.

        end do

        !cleanup:
        if (allocated(tmp)) deallocate(tmp)

    end if

    end subroutine parse_number
!*****************************************************************************************

!*****************************************************************************************
!>
!  Get the next character from the file (or string).
!
!# See also
!  * [[push_char]]
!
!@note This routine ignores non-printing ASCII characters (iachar<=31) that are in strings.

    recursive function pop_char(json, unit, str, eof, skip_ws) result(popped)

    implicit none

    class(json_core),intent(inout)      :: json
    character(kind=CK,len=1)            :: popped  !! the popped character.
    integer(IK),intent(in)              :: unit    !! file unit number (if parsing from a file)
    character(kind=CK,len=*),intent(in) :: str     !! JSON string (if parsing from a string) -- only used if unit=0
    logical(LK),intent(out)             :: eof     !! true if the end of the file has been reached.
    logical(LK),intent(in),optional     :: skip_ws !! to ignore whitespace.

    integer(IK) :: ios,str_len
    character(kind=CK,len=1) :: c
    logical(LK) :: ignore

    if (.not. json%exception_thrown) then

        eof = .false.
        if (.not. present(skip_ws)) then
            ignore = .false.
        else
            ignore = skip_ws
        end if

        do

            if (json%pushed_index > 0) then

                ! there is a character pushed back on, most likely
                ! from the number parsing. Note: this can only occur if
                ! reading from a file when use_unformatted_stream=.false.
                c = json%pushed_char(json%pushed_index:json%pushed_index)
                json%pushed_index = json%pushed_index - 1

            else

                if (unit/=0) then    !read from the file

                    !read the next character:
                    if (use_unformatted_stream) then
                        read(unit=unit,pos=json%ipos,iostat=ios) c
                    else
                        read(unit=unit,fmt='(A1)',advance='NO',iostat=ios) c
                    end if
                    json%ipos = json%ipos + 1

                    !....note: maybe try read the file in chunks...
                    !.... or use asynchronous read with double buffering
                    !     (see Modern Fortran: Style and Usage)

                else    !read from the string

                    str_len = len(str)   !length of the string
                    if (json%ipos<=str_len) then
                        c = str(json%ipos:json%ipos)
                        ios = 0
                    else
                        ios = IOSTAT_END  !end of the string
                    end if
                    json%ipos = json%ipos + 1

                end if

                json%char_count = json%char_count + 1    !character count in the current line

                if (IS_IOSTAT_END(ios)) then  !end of file

                    json%char_count = 0
                    eof = .true.
                    exit

                else if (IS_IOSTAT_EOR(ios) .or. c==newline) then    !end of record

                    json%char_count = 0
                    json%line_count = json%line_count + 1
                    cycle

                end if

            end if

            if (any(c == control_chars)) then

                ! non printing ascii characters
                cycle

            else if (ignore .and. c == space) then

                cycle

            else

                popped = c
                exit

            end if

        end do

    end if

    end function pop_char
!*****************************************************************************************

!*****************************************************************************************
!>
!  Core routine.
!
!# See also
!  * [[pop_char]]
!
!# History
!  * Jacob Williams : 5/3/2015 : replaced original version of this routine.

    subroutine push_char(json,c)

    implicit none

    class(json_core),intent(inout)      :: json
    character(kind=CK,len=1),intent(in) :: c

    character(kind=CK,len=max_numeric_str_len) :: istr

    if (.not. json%exception_thrown) then

        if (use_unformatted_stream) then

            !in this case, c is ignored, and we just
            !decrement the stream position counter:
            json%ipos = json%ipos - 1

        else

            json%pushed_index = json%pushed_index + 1

            if (json%pushed_index>0 .and. json%pushed_index<=len(json%pushed_char)) then
                json%pushed_char(json%pushed_index:json%pushed_index) = c
            else
                call integer_to_string(json%pushed_index,int_fmt,istr)
                call json%throw_exception('Error in push_char: '//&
                                          'invalid valid of pushed_index: '//trim(istr))
            end if

        end if

    end if

    end subroutine push_char
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Print any error message, and then clear the exceptions.
!
!@note This routine is used by the unit tests.
!      It was originally in json_example.f90, and was
!      moved here 2/26/2015 by Izaak Beekman.

    subroutine json_print_error_message(json,io_unit)

    implicit none

    class(json_core),intent(inout) :: json
    integer, intent(in), optional  :: io_unit

    character(kind=CK,len=:),allocatable :: error_msg  !! error message
    logical :: status_ok !! false if there were any errors thrown

    !get error message:
    call json%check_for_errors(status_ok, error_msg)

    !print it if there is one:
    if (.not. status_ok) then
        if (present(io_unit)) then
            write(io_unit,'(A)') error_msg
        else
            write(output_unit,'(A)') error_msg
        end if
        deallocate(error_msg)
        call json%clear_exceptions()
    end if

    end subroutine json_print_error_message
!*****************************************************************************************

!*****************************************************************************************
    end module json_value_module
!*****************************************************************************************
