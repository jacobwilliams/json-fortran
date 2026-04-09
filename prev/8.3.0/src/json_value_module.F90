!*****************************************************************************************
!> author: Jacob Williams
!  license: BSD
!
!  This module provides a low-level interface for manipulation of JSON data.
!  The two public entities are [[json_value]], and [[json_core(type)]].
!  The [[json_file_module]] provides a higher-level interface to some
!  of these routines.
!
!### License
!  * JSON-Fortran is released under a BSD-style license.
!    See the [LICENSE](https://github.com/jacobwilliams/json-fortran/blob/master/LICENSE)
!    file for details.

    module json_value_module

    use,intrinsic :: iso_fortran_env, only: iostat_end,error_unit,output_unit
    use,intrinsic :: ieee_arithmetic
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
    !  This type should only be used by an instance of [[json_core(type)]].
    !
    !### Example
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
    !
    !@warning Pointers of this type should only be allocated
    !         using the methods from [[json_core(type)]].

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

        character(kind=CK,len=:),allocatable :: name  !! variable name (unescaped)

        real(RK),allocatable                 :: dbl_value  !! real data for this variable
        logical(LK),allocatable              :: log_value  !! logical data for this variable
        character(kind=CK,len=:),allocatable :: str_value  !! string data for this variable
                                                           !! (unescaped)
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
    !     use json_module, wp=>json_RK
    !     implicit none
    !     type(json_core) :: json     !<--have to declare this
    !     type(json_value),pointer :: p
    !     call json%create_object(p,'')   !create the root
    !     call json%add(p,'year',1805)    !add some data
    !     call json%add(p,'value',1.0_wp) !add some data
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

        logical(LK) :: stop_on_error = .false.     !! if true, then the program is
                                                   !! stopped immediately when an
                                                   !! exception is raised.

        logical(LK) :: exception_thrown = .false.  !! The error flag. Will be set to true
                                                   !! when an error is thrown in the class.
                                                   !! Many of the methods will check this
                                                   !! and return immediately if it is true.
        character(kind=CK,len=:),allocatable :: err_message
                                                   !! the error message.
                                                   !! if `exception_thrown=False` then
                                                   !! this variable is not allocated.

        integer(IK) :: char_count = 0    !! character position in the current line
        integer(IK) :: line_count = 1    !! lines read counter
        integer(IK) :: pushed_index = 0  !! used when parsing lines in file
        character(kind=CK,len=pushed_char_size) :: pushed_char = CK_''  !! used when parsing
                                                                        !! lines in file

        integer(IK) :: ipos = 1  !! for allocatable strings: next character to read

        logical(LK) :: strict_type_checking = .false. !! if true, then no type conversions are done
                                                      !! in the `get` routines if the actual variable
                                                      !! type is different from the return type (for
                                                      !! example, integer to real).

        logical(LK) :: trailing_spaces_significant = .false.    !! for name and path comparisons, if trailing
                                                                !! space is to be considered significant.

        logical(LK) :: case_sensitive_keys = .true.    !! if name and path comparisons
                                                       !! are case sensitive.

        logical(LK) :: no_whitespace = .false. !! when printing a JSON string, don't include
                                               !! non-significant spaces or line breaks.
                                               !! If true, the entire structure will be
                                               !! printed on one line.

        logical(LK) :: unescaped_strings = .true.  !! If false, then the escaped
                                                   !! string is returned from [[json_get_string]]
                                                   !! and similar routines. If true [default],
                                                   !! then the string is returned unescaped.

        logical(LK) :: allow_comments = .true.  !! if true, any comments will be ignored when
                                                !! parsing a file. The comment tokens are defined
                                                !! by the `comment_char` character variable.
        character(kind=CK,len=:),allocatable :: comment_char !! comment tokens when
                                                             !! `allow_comments` is true.
                                                             !! Examples: '`!`' or '`#`'.
                                                             !! Default is `CK_'/!#'`.

        integer(IK) :: path_mode = 1_IK  !! How the path strings are interpreted in the
                                         !! `get_by_path` routines:
                                         !!
                                         !! * 1 -- Default mode (see [[json_get_by_path_default]])
                                         !! * 2 -- as RFC 6901 "JSON Pointer" paths
                                         !!   (see [[json_get_by_path_rfc6901]])
                                         !! * 3 -- JSONPath "bracket-notation"
                                         !!   see [[json_get_by_path_jsonpath_bracket]])

        character(kind=CK,len=1) :: path_separator = dot !! The `path` separator to use
                                                         !! in the "default" mode for
                                                         !! the paths in the various
                                                         !! `get_by_path` routines.
                                                         !! Note: if `path_mode/=1`
                                                         !! then this is ignored.

        logical(LK) :: compress_vectors = .false. !! If true, then arrays of integers,
                                                  !! nulls, reals, & logicals are
                                                  !! printed all on one line.
                                                  !! [Note: `no_whitespace` will
                                                  !! override this option if necessary]

        logical(LK) :: allow_duplicate_keys = .true. !! If False, then after parsing, if any
                                                     !! duplicate keys are found, an error is
                                                     !! thrown. A call to [[json_value_validate]]
                                                     !! will also check for duplicates. If True
                                                     !! [default] then no special checks are done

        logical(LK) :: escape_solidus = .false.   !! If True then the solidus "`/`" is always escaped
                                                  !! ("`\/`") when serializing JSON.
                                                  !! If False [default], then it is not escaped.
                                                  !! Note that this option does not affect parsing
                                                  !! (both escaped and unescaped versions are still
                                                  !! valid in all cases).

        integer(IK) :: null_to_real_mode = 2_IK   !! if `strict_type_checking=false`:
                                                  !!
                                                  !! * 1 : an exception will be raised if
                                                  !!   try to retrieve a `null` as a real.
                                                  !! * 2 : a `null` retrieved as a real
                                                  !!   will return NaN. [default]
                                                  !! * 3 : a `null` retrieved as a real
                                                  !!   will return 0.0.

        logical(LK) :: non_normals_to_null = .false. !! How to serialize NaN, Infinity,
                                                     !! and -Infinity real values:
                                                     !!
                                                     !! * If true : as JSON `null` values
                                                     !! * If false : as strings (e.g., "NaN",
                                                     !!   "Infinity", "-Infinity") [default]

        logical(LK) :: use_quiet_nan = .true. !! if true [default], `null_to_real_mode=2`
                                              !! and [[string_to_real]] will use
                                              !! `ieee_quiet_nan` for NaN values. If false,
                                              !! `ieee_signaling_nan` will be used.

        logical(LK) :: strict_integer_type_checking = .true.
                            !! * If false, when parsing JSON, if an integer numeric value
                            !!   cannot be converted to an integer (`integer(IK)`),
                            !!   then an attempt is then make to convert it
                            !!   to a real (`real(RK)`).
                            !! * If true [default], an exception will be raised if an integer
                            !!   value cannot be read when parsing JSON.

        integer :: ichunk = 0 !! index in `chunk` for [[pop_char]]
                              !! when `use_unformatted_stream=True`
        integer :: filesize = 0 !! the file size when when `use_unformatted_stream=True`
        character(kind=CK,len=:),allocatable :: chunk   !! a chunk read from a stream file
                                                        !! when `use_unformatted_stream=True`

        contains

        private

        !>
        !  Return a child of a [[json_value]] structure.
        generic,public :: get_child => json_value_get_child_by_index, &
                                       json_value_get_child,&
                                       MAYBEWRAP(json_value_get_child_by_name)
        procedure,private :: json_value_get_child_by_index
        procedure,private :: MAYBEWRAP(json_value_get_child_by_name)
        procedure,private :: json_value_get_child

        !>
        !  Add objects to a linked list of [[json_value]]s.
        !
        !@note It might make more sense to call this `add_child`.
        generic,public :: add => json_value_add_member, &
                                 MAYBEWRAP(json_value_add_null), &
                                 MAYBEWRAP(json_value_add_integer), &
                                 MAYBEWRAP(json_value_add_integer_vec), &
#ifndef REAL32
                                 MAYBEWRAP(json_value_add_real32), &
                                 MAYBEWRAP(json_value_add_real32_vec), &
#endif
                                 MAYBEWRAP(json_value_add_real), &
                                 MAYBEWRAP(json_value_add_real_vec), &
#ifdef REAL128
                                 MAYBEWRAP(json_value_add_real64), &
                                 MAYBEWRAP(json_value_add_real64_vec), &
#endif
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
#ifndef REAL32
        procedure,private :: MAYBEWRAP(json_value_add_real32)
        procedure,private :: MAYBEWRAP(json_value_add_real32_vec)
#endif
        procedure,private :: MAYBEWRAP(json_value_add_real)
        procedure,private :: MAYBEWRAP(json_value_add_real_vec)
#ifdef REAL128
        procedure,private :: MAYBEWRAP(json_value_add_real64)
        procedure,private :: MAYBEWRAP(json_value_add_real64_vec)
#endif
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
        !  These are like the `add` methods, except if a variable with the
        !  same path is already present, then its value is simply updated.
        !  Note that currently, these only work for scalar variables.
        !  These routines can also change the variable's type (but an error will be
        !  thrown if the existing variable is not a scalar).
        !
        !### See also
        !  * [[json_core(type):add_by_path]] - this one can be used to change
        !    arrays and objects to scalars if so desired.
        !
        !@note Unlike some routines, the `found` output is not optional,
        !      so it doesn't present exceptions from being thrown.
        !
        !@note These have been mostly supplanted by the [[json_core(type):add_by_path]]
        !      methods, which do a similar thing (and can be used for
        !      scalars and vectors, etc.)
        generic,public :: update => MAYBEWRAP(json_update_logical),&
#ifndef REAL32
                                    MAYBEWRAP(json_update_real32),&
#endif
                                    MAYBEWRAP(json_update_real),&
#ifdef REAL128
                                    MAYBEWRAP(json_update_real64),&
#endif

                                    MAYBEWRAP(json_update_integer),&
                                    MAYBEWRAP(json_update_string)
#ifdef USE_UCS4
        generic,public :: update => json_update_string_name_ascii,&
                                    json_update_string_val_ascii
#endif
        procedure,private :: MAYBEWRAP(json_update_logical)
#ifndef REAL32
        procedure,private :: MAYBEWRAP(json_update_real32)
#endif
        procedure,private :: MAYBEWRAP(json_update_real)
#ifdef REAL128
        procedure,private :: MAYBEWRAP(json_update_real64)
#endif
        procedure,private :: MAYBEWRAP(json_update_integer)
        procedure,private :: MAYBEWRAP(json_update_string)
#ifdef USE_UCS4
        procedure,private :: json_update_string_name_ascii
        procedure,private :: json_update_string_val_ascii
#endif

        !>
        !  Add variables to a [[json_value]] linked list
        !  by specifying their paths.
        !
        !### Example
        !
        !````fortran
        !    use, intrinsic :: iso_fortran_env, only: output_unit
        !    use json_module, wp=>json_RK
        !    type(json_core) :: json
        !    type(json_value) :: p
        !    call json%create_object(p,'root') ! create the root
        !    ! now add some variables using the paths:
        !    call json%add_by_path(p,'inputs.t',    0.0_wp  )
        !    call json%add_by_path(p,'inputs.x(1)', 100.0_wp)
        !    call json%add_by_path(p,'inputs.x(2)', 200.0_wp)
        !    call json%print(p)  ! now print to console
        !````
        !
        !### Notes
        !  * This uses [[json_create_by_path]]
        !
        !### See also
        !  * The `json_core%update` methods.
        !  * [[json_create_by_path]]

        generic,public :: add_by_path => MAYBEWRAP(json_add_member_by_path),&
                                         MAYBEWRAP(json_add_integer_by_path),&
#ifndef REAL32
                                         MAYBEWRAP(json_add_real32_by_path),&
#endif
                                         MAYBEWRAP(json_add_real_by_path),&
#ifdef REAL128
                                         MAYBEWRAP(json_add_real64_by_path),&
#endif
                                         MAYBEWRAP(json_add_logical_by_path),&
                                         MAYBEWRAP(json_add_string_by_path),&
                                         MAYBEWRAP(json_add_integer_vec_by_path),&
#ifndef REAL32
                                         MAYBEWRAP(json_add_real32_vec_by_path),&
#endif
                                         MAYBEWRAP(json_add_real_vec_by_path),&
#ifdef REAL128
                                         MAYBEWRAP(json_add_real64_vec_by_path),&
#endif
                                         MAYBEWRAP(json_add_logical_vec_by_path),&
                                         MAYBEWRAP(json_add_string_vec_by_path)
#ifdef USE_UCS4
        generic,public :: add_by_path => json_add_string_by_path_value_ascii,&
                                         json_add_string_by_path_path_ascii,&
                                         json_add_string_vec_by_path_value_ascii,&
                                         json_add_string_vec_by_path_path_ascii
#endif
        procedure :: MAYBEWRAP(json_add_member_by_path)
        procedure :: MAYBEWRAP(json_add_integer_by_path)
#ifndef REAL32
        procedure :: MAYBEWRAP(json_add_real32_by_path)
#endif
        procedure :: MAYBEWRAP(json_add_real_by_path)
#ifdef REAL128
        procedure :: MAYBEWRAP(json_add_real64_by_path)
#endif
        procedure :: MAYBEWRAP(json_add_logical_by_path)
        procedure :: MAYBEWRAP(json_add_string_by_path)
        procedure :: MAYBEWRAP(json_add_integer_vec_by_path)
#ifndef REAL32
        procedure :: MAYBEWRAP(json_add_real32_vec_by_path)
#endif
        procedure :: MAYBEWRAP(json_add_real_vec_by_path)
#ifdef REAL128
        procedure :: MAYBEWRAP(json_add_real64_vec_by_path)
#endif
        procedure :: MAYBEWRAP(json_add_logical_vec_by_path)
        procedure :: MAYBEWRAP(json_add_string_vec_by_path)
#ifdef USE_UCS4
        procedure :: json_add_string_by_path_value_ascii
        procedure :: json_add_string_by_path_path_ascii
        procedure :: json_add_string_vec_by_path_value_ascii
        procedure :: json_add_string_vec_by_path_path_ascii
#endif

        !>
        !  Create a [[json_value]] linked list using the
        !  path to the variables. Optionally return a
        !  pointer to the variable.
        !
        !  (This will create a `null` variable)
        !
        !### See also
        !  * [[json_core(type):add_by_path]]

        generic,public :: create => MAYBEWRAP(json_create_by_path)
        procedure :: MAYBEWRAP(json_create_by_path)

        !>
        !  Get data from a [[json_value]] linked list.
        !
        !@note There are two versions (e.g. [[json_get_integer]] and [[json_get_integer_by_path]]).
        !      The first one gets the value from the [[json_value]] passed into the routine,
        !      while the second one gets the value from the [[json_value]] found by parsing the
        !      path.  The path version is split up into unicode and non-unicode versions.

        generic,public :: get => &
                                       MAYBEWRAP(json_get_by_path),             &
            json_get_integer,          MAYBEWRAP(json_get_integer_by_path),     &
            json_get_integer_vec,      MAYBEWRAP(json_get_integer_vec_by_path), &
#ifndef REAL32
            json_get_real32,           MAYBEWRAP(json_get_real32_by_path),      &
            json_get_real32_vec,       MAYBEWRAP(json_get_real32_vec_by_path),  &
#endif
            json_get_real,             MAYBEWRAP(json_get_real_by_path),        &
            json_get_real_vec,         MAYBEWRAP(json_get_real_vec_by_path),    &
#ifdef REAL128
            json_get_real64,           MAYBEWRAP(json_get_real64_by_path),      &
            json_get_real64_vec,       MAYBEWRAP(json_get_real64_vec_by_path),  &
#endif
            json_get_logical,          MAYBEWRAP(json_get_logical_by_path),     &
            json_get_logical_vec,      MAYBEWRAP(json_get_logical_vec_by_path), &
            json_get_string,           MAYBEWRAP(json_get_string_by_path),      &
            json_get_string_vec,       MAYBEWRAP(json_get_string_vec_by_path),  &
            json_get_alloc_string_vec, MAYBEWRAP(json_get_alloc_string_vec_by_path),&
            json_get_array,            MAYBEWRAP(json_get_array_by_path)

        procedure,private :: json_get_integer
        procedure,private :: json_get_integer_vec
#ifndef REAL32
        procedure,private :: json_get_real32
        procedure,private :: json_get_real32_vec
#endif
        procedure,private :: json_get_real
        procedure,private :: json_get_real_vec
#ifdef REAL128
        procedure,private :: json_get_real64
        procedure,private :: json_get_real64_vec
#endif
        procedure,private :: json_get_logical
        procedure,private :: json_get_logical_vec
        procedure,private :: json_get_string
        procedure,private :: json_get_string_vec
        procedure,private :: json_get_alloc_string_vec
        procedure,private :: json_get_array
        procedure,private :: MAYBEWRAP(json_get_by_path)
        procedure,private :: MAYBEWRAP(json_get_integer_by_path)
        procedure,private :: MAYBEWRAP(json_get_integer_vec_by_path)
#ifndef REAL32
        procedure,private :: MAYBEWRAP(json_get_real32_by_path)
        procedure,private :: MAYBEWRAP(json_get_real32_vec_by_path)
#endif
        procedure,private :: MAYBEWRAP(json_get_real_by_path)
        procedure,private :: MAYBEWRAP(json_get_real_vec_by_path)
#ifdef REAL128
        procedure,private :: MAYBEWRAP(json_get_real64_by_path)
        procedure,private :: MAYBEWRAP(json_get_real64_vec_by_path)
#endif
        procedure,private :: MAYBEWRAP(json_get_logical_by_path)
        procedure,private :: MAYBEWRAP(json_get_logical_vec_by_path)
        procedure,private :: MAYBEWRAP(json_get_string_by_path)
        procedure,private :: MAYBEWRAP(json_get_string_vec_by_path)
        procedure,private :: MAYBEWRAP(json_get_array_by_path)
        procedure,private :: MAYBEWRAP(json_get_alloc_string_vec_by_path)
        procedure,private :: json_get_by_path_default
        procedure,private :: json_get_by_path_rfc6901
        procedure,private :: json_get_by_path_jsonpath_bracket

        !>
        !  Print the [[json_value]] to an output unit or file.
        !
        !### Example
        !
        !````fortran
        !    type(json_core) :: json
        !    type(json_value) :: p
        !    !...
        !    call json%print(p,'test.json')  !this is [[json_print_to_filename]]
        !````
        generic,public :: print => json_print_to_console,&
                                   json_print_to_unit,&
                                   json_print_to_filename
        procedure :: json_print_to_console
        procedure :: json_print_to_unit
        procedure :: json_print_to_filename

        !>
        !  Destructor routine for a [[json_value]] pointer.
        !  This must be called explicitly if it is no longer needed,
        !  before it goes out of scope.  Otherwise, a memory leak will result.
        !
        !### Example
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
        !  Allocate a [[json_value]] pointer and make it a real variable.
        !  The pointer should not already be allocated.
        !
        !### Example
        !
        !````fortran
        !    type(json_core) :: json
        !    type(json_value),pointer :: p
        !    call json%create_real(p,'value',1.0_RK)
        !````
        !
        !### Note
        !  * [[json_core(type):create_real]] is just an alias
        !    to this one for backward compatibility.
        generic,public :: create_real => MAYBEWRAP(json_value_create_real)
        procedure :: MAYBEWRAP(json_value_create_real)
#ifndef REAL32
        generic,public :: create_real => MAYBEWRAP(json_value_create_real32)
        procedure :: MAYBEWRAP(json_value_create_real32)
#endif
#ifdef REAL128
        generic,public :: create_real => MAYBEWRAP(json_value_create_real64)
        procedure :: MAYBEWRAP(json_value_create_real64)
#endif

        !>
        !  This is equivalent to [[json_core(type):create_real]],
        !  and is here only for backward compatibility.
        generic,public :: create_double => MAYBEWRAP(json_value_create_real)
#ifndef REAL32
        generic,public :: create_double => MAYBEWRAP(json_value_create_real32)
#endif
#ifdef REAL128
        generic,public :: create_double => MAYBEWRAP(json_value_create_real64)
#endif

        !>
        !  Allocate a [[json_value]] pointer and make it an array variable.
        !  The pointer should not already be allocated.
        !
        !### Example
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
        !### Example
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
        !### Example
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
        !### Example
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
        !### Example
        !
        !````fortran
        !    type(json_core) :: json
        !    type(json_value),pointer :: p
        !    call json%create_integer(p,42,'value')
        !````
        generic,public :: create_integer => MAYBEWRAP(json_value_create_integer)
        procedure :: MAYBEWRAP(json_value_create_integer)

        !>
        !  Allocate a json_value pointer and make it a logical variable.
        !  The pointer should not already be allocated.
        !
        !### Example
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
        generic,public :: load => json_parse_file
        procedure :: json_parse_file

        !>
        !  Print the [[json_value]] structure to an allocatable string
        procedure,public :: serialize => json_value_to_string

        !>
        !  The same as `serialize`, but only here for backward compatibility
        procedure,public :: print_to_string => json_value_to_string

        !>
        !  Parse the JSON string and populate the [[json_value]] tree.
        generic,public :: deserialize => MAYBEWRAP(json_parse_string)
        procedure :: MAYBEWRAP(json_parse_string)

        !>
        !  Same as `load` and `deserialize` but only here for backward compatibility.
        generic,public :: parse => json_parse_file, &
                                   MAYBEWRAP(json_parse_string)

        !>
        !  Throw an exception.
        generic,public :: throw_exception => MAYBEWRAP(json_throw_exception)
        procedure :: MAYBEWRAP(json_throw_exception)

        !>
        !  Rename a [[json_value]] variable.
        generic,public :: rename => MAYBEWRAP(json_value_rename),&
                                    MAYBEWRAP(json_rename_by_path)
        procedure :: MAYBEWRAP(json_value_rename)
        procedure :: MAYBEWRAP(json_rename_by_path)
#ifdef USE_UCS4
        generic,public :: rename => json_rename_by_path_name_ascii,&
                                    json_rename_by_path_path_ascii
        procedure :: json_rename_by_path_name_ascii
        procedure :: json_rename_by_path_path_ascii
#endif

        !>
        !  get info about a [[json_value]]
        generic,public :: info => json_info, MAYBEWRAP(json_info_by_path)
        procedure :: json_info
        procedure :: MAYBEWRAP(json_info_by_path)

        !>
        !  get string info about a [[json_value]]
        generic,public :: string_info => json_string_info
        procedure :: json_string_info

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

        !>
        !  verify if a path is valid
        !  (i.e., a variable with this path exists in the file).
        generic,public :: valid_path => MAYBEWRAP(json_valid_path)
        procedure :: MAYBEWRAP(json_valid_path)

        procedure,public :: remove              => json_value_remove        !! Remove a [[json_value]] from a
                                                                            !! linked-list structure.
        procedure,public :: replace             => json_value_replace       !! Replace a [[json_value]] in a
                                                                            !! linked-list structure.
        procedure,public :: reverse             => json_value_reverse       !! Reverse the order of the children
                                                                            !! of an array of object.
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
        procedure,public :: traverse            => json_traverse            !! to traverse all elements of a JSON
                                                                            !! structure
        procedure,public :: print_error_message => json_print_error_message !! simply routine to print error
                                                                            !! messages
        procedure,public :: swap                => json_value_swap          !! Swap two [[json_value]] pointers
                                                                            !! in a structure (or two different
                                                                            !! structures).
        procedure,public :: is_child_of         => json_value_is_child_of   !! Check if a [[json_value]] is a
                                                                            !! descendant of another.
        procedure,public :: validate            => json_value_validate      !! Check that a [[json_value]] linked
                                                                            !! list is valid (i.e., is properly
                                                                            !! constructed). This may be useful
                                                                            !! if it has been constructed externally.
        procedure,public :: check_for_duplicate_keys &
                                => json_check_all_for_duplicate_keys  !! Check entire JSON structure
                                                                      !! for duplicate keys (recursively)
        procedure,public :: check_children_for_duplicate_keys &
                                => json_check_children_for_duplicate_keys  !! Check a `json_value` object's
                                                                           !! children for duplicate keys

        !other private routines:
        procedure        :: name_equal
        procedure        :: name_strings_equal
        procedure        :: json_value_print
        procedure        :: string_to_int
        procedure        :: string_to_dble
        procedure        :: prepare_parser => json_prepare_parser
        procedure        :: parse_end => json_parse_end
        procedure        :: parse_value
        procedure        :: parse_number
        procedure        :: parse_string
        procedure        :: parse_for_chars
        procedure        :: parse_object
        procedure        :: parse_array
        procedure        :: annotate_invalid_json
        procedure        :: pop_char
        procedure        :: push_char
        procedure        :: get_current_line_from_file_stream
        procedure,nopass :: get_current_line_from_file_sequential
        procedure        :: convert
        procedure        :: to_string
        procedure        :: to_logical
        procedure        :: to_integer
        procedure        :: to_real
        procedure        :: to_null
        procedure        :: to_object
        procedure        :: to_array
        procedure,nopass :: json_value_clone_func
        procedure        :: is_vector => json_is_vector

    end type json_core
    !*********************************************************

    !*********************************************************
    !>
    !  Structure constructor to initialize a
    !  [[json_core(type)]] object
    !
    !### Example
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
!  Destructor for the [[json_core(type)]] type.

    subroutine destroy_json_core(me)

    implicit none

    class(json_core),intent(out) :: me

    end subroutine destroy_json_core
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 4/26/2016
!
!  Function constructor for a [[json_core(type)]].
!  This is just a wrapper for [[json_initialize]].
!
!@note [[initialize_json_core]], [[json_initialize]],
!      [[initialize_json_core_in_file]], and [[initialize_json_file]]
!      all have a similar interface.

    function initialize_json_core(&
#include "json_initialize_dummy_arguments.inc"
                                 ) result(json_core_object)

    implicit none

    type(json_core) :: json_core_object
#include "json_initialize_arguments.inc"

    call json_core_object%initialize(&
#include "json_initialize_dummy_arguments.inc"
                                    )

    end function initialize_json_core
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 12/4/2013
!
!  Initialize the [[json_core(type)]] instance.
!
!  The routine may be called before any of the [[json_core(type)]] methods are used in
!  order to specify certain parameters. If it is not called, then the defaults
!  are used. This routine is also called internally by various routines.
!  It can also be called to clear exceptions, or to reset some
!  of the variables (note that only the arguments present are changed).
!
!### Modified
!  * Izaak Beekman : 02/24/2015
!
!@note [[initialize_json_core]], [[json_initialize]],
!      [[initialize_json_core_in_file]], and [[initialize_json_file]]
!      all have a similar interface.

    subroutine json_initialize(me,&
#include "json_initialize_dummy_arguments.inc"
                              )

    implicit none

    class(json_core),intent(inout)  :: me
#include "json_initialize_arguments.inc"

    character(kind=CDK,len=10) :: w            !! max string length
    character(kind=CDK,len=10) :: d            !! real precision digits
    character(kind=CDK,len=10) :: e            !! real exponent digits
    character(kind=CDK,len=2)  :: sgn          !! sign flag: `ss` or `sp`
    character(kind=CDK,len=2)  :: rl_edit_desc !! `G`, `E`, `EN`, or `ES`
    integer(IK)                :: istat        !! `iostat` flag for
                                               !! write statements
    logical(LK)                :: sgn_prnt     !! print sign flag
    character(kind=CK,len=max_integer_str_len) :: istr !! for integer to
                                                       !! string conversion

    !reset exception to false:
    call me%clear_exceptions()

    !Just in case, clear these global variables also:
    me%pushed_index = 0
    me%pushed_char  = CK_''
    me%char_count   = 0
    me%line_count   = 1
    me%ipos         = 1
    if (use_unformatted_stream) then
        me%filesize = 0
        me%ichunk   = 0
        me%chunk    = repeat(space, stream_chunk_size) ! default chunk size
    end if

#ifdef USE_UCS4
    ! reopen stdout and stderr with utf-8 encoding
    open(output_unit,encoding='utf-8')
    open(error_unit, encoding='utf-8')
#endif

    !various optional inputs:
    if (present(spaces_per_tab)) &
        me%spaces_per_tab = spaces_per_tab
    if (present(stop_on_error)) &
        me%stop_on_error = stop_on_error
    if (present(verbose)) &
        me%is_verbose = verbose
    if (present(strict_type_checking)) &
        me%strict_type_checking = strict_type_checking
    if (present(trailing_spaces_significant)) &
        me%trailing_spaces_significant = trailing_spaces_significant
    if (present(case_sensitive_keys)) &
        me%case_sensitive_keys = case_sensitive_keys
    if (present(no_whitespace)) &
        me%no_whitespace = no_whitespace
    if (present(unescape_strings)) &
        me%unescaped_strings = unescape_strings
    if (present(path_mode)) then
        if (path_mode==1_IK .or. path_mode==2_IK .or. path_mode==3_IK) then
            me%path_mode = path_mode
        else
            me%path_mode = 1_IK  ! just to have a valid value
            call me%throw_exception('Invalid path_mode.')
        end if
    end if

    ! if we are allowing comments in the file:
    ! [an empty string disables comments]
    if (present(comment_char)) then
        me%allow_comments = comment_char/=CK_''
        me%comment_char = trim(adjustl(comment_char))
    end if

    ! path separator:
    if (present(path_separator)) then
        me%path_separator = path_separator
    end if

    ! printing vectors in compressed form:
    if (present(compress_vectors)) then
        me%compress_vectors = compress_vectors
    end if

    ! checking for duplicate keys:
    if (present(allow_duplicate_keys)) then
        me%allow_duplicate_keys = allow_duplicate_keys
    end if

    ! if escaping the forward slash:
    if (present(escape_solidus)) then
        me%escape_solidus = escape_solidus
    end if

    ! how to handle null to read conversions:
    if (present(null_to_real_mode)) then
        select case (null_to_real_mode)
        case(1_IK:3_IK)
            me%null_to_real_mode = null_to_real_mode
        case default
            me%null_to_real_mode = 2_IK  ! just to have a valid value
            call integer_to_string(null_to_real_mode,int_fmt,istr)
            call me%throw_exception('Invalid null_to_real_mode: '//istr)
        end select
    end if

    ! how to handle NaN and Infinities:
    if (present(non_normal_mode)) then
        select case (non_normal_mode)
        case(1_IK) ! use strings
            me%non_normals_to_null = .false.
        case(2_IK) ! use null
            me%non_normals_to_null = .true.
        case default
            call integer_to_string(non_normal_mode,int_fmt,istr)
            call me%throw_exception('Invalid non_normal_mode: '//istr)
        end select
    end if

    if (present(use_quiet_nan)) then
        me%use_quiet_nan = use_quiet_nan
    end if

    if (present(strict_integer_type_checking)) then
        me%strict_integer_type_checking = strict_integer_type_checking
    end if

    !Set the format for real numbers:
    ! [if not changing it, then it remains the same]

    if ( (.not. allocated(me%real_fmt)) .or. &  ! if this hasn't been done yet
          present(compact_reals) .or. &
          present(print_signs)   .or. &
          present(real_format) ) then

        !allow the special case where real format is '*':
        ! [this overrides the other options]
        if (present(real_format)) then
            if (real_format==star) then
                if (present(compact_reals)) then
                    ! we will also allow for compact reals with
                    ! '*' format, if both arguments are present.
                    me%compact_real = compact_reals
                else
                    me%compact_real = .false.
                end if
                me%real_fmt = star
                return
            end if
        end if

        if (present(compact_reals)) me%compact_real = compact_reals

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
              call me%throw_exception('Invalid real format, "' // &
                        trim(real_format) // '", passed to json_initialize.'// &
                        new_line('a') // 'Acceptable formats are: "G", "E", "EN", and "ES".' )
           end select
        end if

        ! set the default output/input format for reals:
                      write(w,'(ss,I0)',iostat=istat) max_numeric_str_len
        if (istat==0) write(d,'(ss,I0)',iostat=istat) real_precision
        if (istat==0) write(e,'(ss,I0)',iostat=istat) real_exponent_digits
        if (istat==0) then
            me%real_fmt = '(' // sgn // ',' // trim(rl_edit_desc) //&
                            trim(w) // '.' // trim(d) // 'E' // trim(e) // ')'
        else
            me%real_fmt = '(' // sgn // ',' // trim(rl_edit_desc) // &
                            '27.17E4)'  !just use this one (should never happen)
        end if

    end if

    end subroutine json_initialize
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Returns true if `name` is equal to `p%name`, using the specified
!  settings for case sensitivity and trailing whitespace.
!
!### History
!  * 4/30/2016 : original version
!  * 8/25/2017 : now just a wrapper for [[name_strings_equal]]

    function name_equal(json,p,name) result(is_equal)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),intent(in)         :: p        !! the json object
    character(kind=CK,len=*),intent(in) :: name     !! the name to check for
    logical(LK)                         :: is_equal !! true if the string are
                                                    !! lexically equal

    if (allocated(p%name)) then
        ! call the low-level routines for the name strings:
        is_equal = json%name_strings_equal(p%name,name)
    else
        is_equal = name == CK_'' ! check a blank name
    end if

    end function name_equal
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 8/25/2017
!
!  Returns true if the name strings `name1` is equal to `name2`, using
!  the specified settings for case sensitivity and trailing whitespace.

    function name_strings_equal(json,name1,name2) result(is_equal)

    implicit none

    class(json_core),intent(inout)      :: json
    character(kind=CK,len=*),intent(in) :: name1     !! the name to check
    character(kind=CK,len=*),intent(in) :: name2     !! the name to check
    logical(LK)                         :: is_equal  !! true if the string are
                                                     !! lexically equal

    !must be the same length if we are treating
    !trailing spaces as significant, so do a
    !quick test of this first:
    if (json%trailing_spaces_significant) then
        is_equal = len(name1) == len(name2)
        if (.not. is_equal) return
    end if

    if (json%case_sensitive_keys) then
        is_equal = name1 == name2
    else
        is_equal = lowercase_string(name1) == lowercase_string(name2)
    end if

    end function name_strings_equal
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 10/31/2015
!
!  Create a deep copy of a [[json_value]] linked-list structure.
!
!### Notes
!
!  * If `from` has children, then they are also cloned.
!  * The parent of `from` is not linked to `to`.
!  * If `from` is an element of an array, then the previous and
!    next entries are not cloned (only that element and it's children, if any).
!
!### Example
!
!````fortran
!    program test
!     use json_module
!     implicit none
!     type(json_core) :: json
!     type(json_value),pointer :: j1, j2
!     call json%load('../files/inputs/test1.json',j1)
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
    call json%json_value_clone_func(from,to)

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

    recursive subroutine json_value_clone_func(from,to,parent,previous,tail)

    implicit none

    type(json_value),pointer          :: from     !! this is the structure to clone
    type(json_value),pointer          :: to       !! the clone is put here (it
                                                  !! must not already be associated)
    type(json_value),pointer,optional :: parent   !! to%parent
    type(json_value),pointer,optional :: previous !! to%previous
    logical,optional                  :: tail     !! if "to" is the tail of
                                                  !! its parent's children

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

        ! allocate and associate the pointers as necessary:
        if (present(parent))   to%parent   => parent
        if (present(previous)) to%previous => previous
        if (present(tail)) then
            if (tail .and. associated(to%parent)) to%parent%tail => to
        end if

        if (associated(from%next) .and. associated(to%parent)) then
            ! we only clone the next entry in an array
            ! if the parent has also been cloned
            call json_value_clone_func(from     = from%next,&
                                       to       = to%next,&
                                       previous = to,&
                                       parent   = to%parent,&
                                       tail     = (.not. associated(from%next%next)))
        end if

        if (associated(from%children)) then
            call json_value_clone_func(from   = from%children,&
                                       to     = to%children,&
                                       parent = to,&
                                       tail   = (.not. associated(from%children%next)))
        end if

    end if

    end subroutine json_value_clone_func
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Destroy the data within a [[json_value]], and reset type to `json_unknown`.

    pure subroutine destroy_json_data(d)

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

    if (.not. json%exception_thrown .and. associated(p)) then

        if (present(var_type))    var_type   = p%var_type
        if (present(n_children))  n_children = json%count(p)
        if (present(name)) then
            if (allocated(p%name)) then
                name = p%name
            else
                name = CK_''
            end if
        end if

    else ! error

        if (.not. json%exception_thrown) then
            call json%throw_exception('Error in json_info: '//&
                                      'pointer is not associated.' )
        end if
        if (present(var_type))   var_type   = json_unknown
        if (present(n_children)) n_children = 0
        if (present(name))       name       = CK_''

    end if

    end subroutine json_info
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 12/18/2016
!
!  Returns information about character strings returned from a [[json_value]].

    subroutine json_string_info(json,p,ilen,max_str_len,found)

    implicit none

    class(json_core),intent(inout)   :: json
    type(json_value),pointer         :: p
    integer(IK),dimension(:),allocatable,intent(out),optional :: ilen !! if `p` is an array, this
                                                                      !! is the actual length
                                                                      !! of each character
                                                                      !! string in the array.
                                                                      !! if not an array, this
                                                                      !! is returned unallocated.
    integer(IK),intent(out),optional :: max_str_len !! The maximum length required to
                                                    !! hold the string representation returned
                                                    !! by a call to a `get` routine. If a scalar,
                                                    !! this is just the length of the scalar. If
                                                    !! a vector, this is the maximum length of
                                                    !! any element.
    logical(LK),intent(out),optional :: found   !! true if there were no errors.
                                                !! if not present, an error will
                                                !! throw an exception

    character(kind=CK,len=:),allocatable :: cval !! for getting values as strings.
    logical(LK) :: initialized !! if the output array has been sized
    logical(LK) :: get_max_len !! if we are returning the `max_str_len`
    logical(LK) :: get_ilen    !! if we are returning the `ilen` array
    integer(IK) :: var_type    !! variable type

    get_max_len = present(max_str_len)
    get_ilen    = present(ilen)

    if (.not. json%exception_thrown) then

        if (present(found)) found = .true.
        initialized = .false.

        if (get_max_len) max_str_len = 0

        select case (p%var_type)

        case (json_array) ! it's an array

            ! call routine for each element
            call json%get(p, array_callback=get_string_lengths)

        case default ! not an array

            if (json%strict_type_checking) then
                ! only allowing strings to be returned
                ! as strings, so we can check size directly
                call json%info(p,var_type=var_type)
                if (var_type==json_string) then
                    if (allocated(p%str_value) .and. get_max_len) &
                        max_str_len = len(p%str_value)
                else
                    ! it isn't a string, so there is no length
                    call json%throw_exception('Error in json_string_info: '//&
                                              'When strict_type_checking is true '//&
                                              'the variable must be a character string.',&
                                              found)
                end if
            else
                ! in this case, we have to get the value
                ! as a string to know what size it is.
                call json%get(p, value=cval)
                if (.not. json%exception_thrown) then
                    if (allocated(cval) .and. get_max_len) &
                        max_str_len = len(cval)
                end if
            end if

        end select

    end if

    if (json%exception_thrown) then
        if (present(found)) then
            call json%clear_exceptions()
            found = .false.
        end if
        if (get_max_len) max_str_len = 0
        if (get_ilen) then
            if (allocated(ilen)) deallocate(ilen)
        end if
    end if

    contains

        subroutine get_string_lengths(json, element, i, count)

        !! callback function to call for each element in the array.

        implicit none

        class(json_core),intent(inout)      :: json
        type(json_value),pointer,intent(in) :: element
        integer(IK),intent(in)              :: i        !! index
        integer(IK),intent(in)              :: count    !! size of array

        character(kind=CK,len=:),allocatable :: cval
        integer(IK) :: var_type

        if (json%exception_thrown) return

        if (.not. initialized) then
            if (get_ilen) allocate(ilen(count))
            initialized = .true.
        end if

        if (json%strict_type_checking) then
            ! only allowing strings to be returned
            ! as strings, so we can check size directly
            call json%info(element,var_type=var_type)
            if (var_type==json_string) then
                if (allocated(element%str_value)) then
                    if (get_max_len) then
                        if (len(element%str_value)>max_str_len) &
                                max_str_len = len(element%str_value)
                    end if
                    if (get_ilen) ilen(i) = len(element%str_value)
                else
                    if (get_ilen) ilen(i) = 0
                end if
            else
                ! it isn't a string, so there is no length
                call json%throw_exception('Error in json_string_info: '//&
                                          'When strict_type_checking is true '//&
                                          'the array must contain only '//&
                                          'character strings.',found)
            end if
        else
            ! in this case, we have to get the value
            ! as a string to know what size it is.
            call json%get(element, value=cval)
            if (json%exception_thrown) return
            if (allocated(cval)) then
                if (get_max_len) then
                    if (len(cval)>max_str_len) max_str_len = len(cval)
                end if
                if (get_ilen) ilen(i) = len(cval)
            else
                if (get_ilen) ilen(i) = 0
            end if
        end if

        end subroutine get_string_lengths

    end subroutine json_string_info
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

    type(json_value),pointer :: p_var  !! temporary pointer
    logical(LK) :: ok  !! if the variable was found
#if defined __GFORTRAN__
    character(kind=CK,len=:),allocatable :: p_name  !! temporary variable for getting name
#endif

    call json%get(p,path,p_var,found)

    !check if it was found:
    if (present(found)) then
        ok = found
    else
        ok = .not. json%exception_thrown
    end if

    if (.not. ok) then
        if (present(var_type))   var_type   = json_unknown
        if (present(n_children)) n_children = 0
        if (present(name))       name       = CK_''
    else
        !get info:

#if defined __GFORTRAN__
        call json%info(p_var,var_type,n_children)
        if (present(name)) then !workaround for gfortran bug
            if (allocated(p_var%name)) then
                p_name = p_var%name
                name = p_name
            else
                name = CK_''
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
!### Example
!
!  The following example is an array with `var_type=json_integer`,
!  `n_sets=3`, and `set_size=4`
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
    integer(IK),intent(out),optional :: var_type   !! variable type of data in the matrix
                                                   !! (if all elements have the same type)
    integer(IK),intent(out),optional :: n_sets     !! number of data sets (i.e., matrix
                                                   !! rows if using row-major order)
    integer(IK),intent(out),optional :: set_size   !! size of each data set (i.e., matrix
                                                   !! cols if using row-major order)
    character(kind=CK,len=:),allocatable,intent(out),optional :: name !! variable name

    type(json_value),pointer :: p_row       !! for getting a set
    type(json_value),pointer :: p_element   !! for getting an element in a set
    integer(IK) :: vartype         !! json variable type of `p`
    integer(IK) :: row_vartype     !! json variable type of a row
    integer(IK) :: element_vartype !! json variable type of an element in a row
    integer(IK) :: nr              !! number of children of `p`
    integer(IK) :: nc              !! number of elements in first child of `p`
    integer(IK) :: icount          !! number of elements in a set
    integer(IK) :: i               !! counter
    integer(IK) :: j               !! counter
#if defined __GFORTRAN__
    character(kind=CK,len=:),allocatable :: p_name  !! temporary variable for getting name
#endif

    !get info about the variable:
#if defined __GFORTRAN__
    call json%info(p,vartype,nr)
    if (present(name)) then !workaround for gfortran bug
        if (allocated(p%name)) then
            p_name = p%name
            name = p_name
        else
            name = CK_''
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
    character(kind=CK,len=:),allocatable :: p_name  !! temporary variable for getting name
#endif

    call json%get(p,path,p_var,found)

    !check if it was found:
    if (present(found)) then
        ok = found
    else
        ok = .not. json%exception_thrown
    end if

    if (.not. ok) then
        if (present(var_type)) var_type = json_unknown
        if (present(n_sets))   n_sets   = 0
        if (present(set_size)) set_size = 0
        if (present(name))     name     = CK_''
    else

        !get info about the variable:
#if defined __GFORTRAN__
        call json%matrix_info(p_var,is_matrix,var_type,n_sets,set_size)
        if (present(name)) then !workaround for gfortran bug
            if (allocated(p_var%name)) then
                p_name = p_var%name
                name = p_name
            else
                name = CK_''
            end if
        end if
#else
        call json%matrix_info(p_var,is_matrix,var_type,n_sets,set_size,name)
#endif
        if (json%exception_thrown .and. present(found)) then
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

    if (json%trailing_spaces_significant) then
        p%name = name
    else
        p%name = trim(name)
    end if

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
!  Clear exceptions in the [[json_core(type)]].

    pure subroutine json_clear_exceptions(json)

    implicit none

    class(json_core),intent(inout)  :: json

    !clear the flag and message:
    json%exception_thrown = .false.
    if (allocated(json%err_message)) deallocate(json%err_message)

    end subroutine json_clear_exceptions
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 12/4/2013
!
!  Throw an exception in the [[json_core(type)]].
!  This routine sets the error flag, and prevents any subsequent routine
!  from doing anything, until [[json_clear_exceptions]] is called.
!
!@note If `is_verbose` is true, this will also print a
!      traceback if the Intel compiler is used.
!
!@note If `stop_on_error` is true, then the program is stopped.

    subroutine json_throw_exception(json,msg,found)

#ifdef __INTEL_COMPILER
    use ifcore, only: tracebackqq
#endif

    implicit none

    class(json_core),intent(inout)      :: json
    character(kind=CK,len=*),intent(in) :: msg    !! the error message
    logical(LK),intent(inout),optional  :: found  !! if the caller is handling the
                                                  !! exception with an optimal return
                                                  !! argument. If so, `json%stop_on_error`
                                                  !! is ignored.

    logical(LK) :: stop_on_error

    json%exception_thrown = .true.
    json%err_message = trim(msg)
    stop_on_error = json%stop_on_error .and. .not. present(found)

    if (stop_on_error) then

#ifdef __INTEL_COMPILER
        ! for Intel, we raise a traceback and quit
        call tracebackqq(string=trim(msg), user_exit_code=0)
#else
        write(error_unit,'(A)') 'JSON-Fortran Exception: '//trim(msg)
        error stop 1
#endif

    elseif (json%is_verbose) then

        write(output_unit,'(A)') '***********************'
        write(output_unit,'(A)') 'JSON-Fortran Exception: '//trim(msg)

!#if defined __GFORTRAN__
!        call backtrace()  ! (have to compile with -fbacktrace -fall-intrinsics flags)
!#endif

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

    subroutine wrap_json_throw_exception(json,msg,found)

    implicit none

    class(json_core),intent(inout)  :: json
    character(kind=CDK,len=*),intent(in) :: msg    !! the error message
    logical(LK),intent(inout),optional  :: found  !! if the caller is handling the
                                                  !! exception with an optimal return
                                                  !! argument. If so, `json%stop_on_error`
                                                  !! is ignored.

    call json%throw_exception(to_unicode(msg),found)

    end subroutine wrap_json_throw_exception
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 12/4/2013
!
!  Retrieve error code from the [[json_core(type)]].
!  This should be called after `parse` to check for errors.
!  If an error is thrown, before using the class again, [[json_initialize]]
!  should be called to clean up before it is used again.
!
!### Example
!
!````fortran
!     type(json_file) :: json
!     logical :: status_ok
!     character(kind=CK,len=:),allocatable :: error_msg
!     call json%load(filename='myfile.json')
!     call json%check_for_errors(status_ok, error_msg)
!     if (.not. status_ok) then
!         write(*,*) 'Error: '//error_msg
!         call json%clear_exceptions()
!         call json%destroy()
!     end if
!````
!
!### See also
!  * [[json_failed]]
!  * [[json_throw_exception]]

    subroutine json_check_for_errors(json,status_ok,error_msg)

    implicit none

    class(json_core),intent(in) :: json
    logical(LK),intent(out),optional :: status_ok !! true if there were no errors
    character(kind=CK,len=:),allocatable,intent(out),optional :: error_msg !! the error message.
                                                                           !! (not allocated if
                                                                           !! there were no errors)

#if defined __GFORTRAN__
    character(kind=CK,len=:),allocatable :: tmp  !! workaround for gfortran bugs
#endif

    if (present(status_ok)) status_ok = .not. json%exception_thrown

    if (present(error_msg)) then
        if (json%exception_thrown) then
            ! if an exception has been thrown,
            ! then this will always be allocated
            ! [see json_throw_exception]
#if defined __GFORTRAN__
            tmp = json%err_message
            error_msg = tmp
#else
            error_msg = json%err_message
#endif
        end if
    end if

    end subroutine json_check_for_errors
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 12/5/2013
!
!  Logical function to indicate if an exception has been thrown in a [[json_core(type)]].
!
!### Example
!
!````fortran
!    type(json_core) :: json
!    type(json_value),pointer :: p
!    logical :: status_ok
!    character(len=:),allocatable :: error_msg
!    call json%load(filename='myfile.json',p)
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
!    call f%load(filename='myfile.json')
!    if (f%failed()) then
!        call f%check_for_errors(status_ok, error_msg)
!        write(*,*) 'Error: '//error_msg
!        call f%clear_exceptions()
!        call f%destroy()
!    end if
!````
!
!### See also
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
!### Example
!
!````fortran
!    type(json_value),pointer :: var
!    call json_value_create(var)
!    call json%to_real(var,1.0_RK)
!````
!
!### Notes
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
!
!@note This routine destroys this variable, it's children, and
!      (if `destroy_next` is true) the subsequent elements in
!      an object or array. It does not destroy the parent or
!      previous elements.
!
!@Note There is some protection here to enable destruction of
!      improperly-created linked lists. However, likely there
!      are cases not handled. Use the [[json_value_validate]]
!      method to validate a JSON structure that was manually
!      created using [[json_value]] pointers.

    pure recursive subroutine json_value_destroy(json,p,destroy_next)

    implicit none

    class(json_core),intent(inout)  :: json
    type(json_value),pointer        :: p            !! variable to destroy
    logical(LK),intent(in),optional :: destroy_next !! if true, then `p%next`
                                                    !! is also destroyed (default is true)

    logical(LK)              :: des_next  !! local copy of `destroy_next`
                                          !! optional argument
    type(json_value),pointer :: child     !! for getting child elements
    logical                  :: circular  !! to check to malformed linked lists

    if (associated(p)) then

        if (present(destroy_next)) then
            des_next = destroy_next
        else
            des_next = .true.
        end if

        if (allocated(p%name)) deallocate(p%name)

        call destroy_json_data(p)

        if (associated(p%next)) then
            ! check for circular references:
            if (associated(p, p%next)) nullify(p%next)
        end if

        if (associated(p%children)) then
            do while (p%n_children > 0)
                child => p%children
                if (associated(child)) then
                    p%children => p%children%next
                    p%n_children = p%n_children - 1
                    ! check children for circular references:
                    circular = (associated(p%children) .and. &
                                associated(p%children,child))
                    call json%destroy(child,destroy_next=.false.)
                    if (circular) exit
                else
                    ! it is a malformed JSON object. But, we will
                    ! press ahead with the destroy process, since
                    ! otherwise, there would be no way to destroy it.
                    exit
                end if
            end do
            nullify(p%children)
            nullify(child)
        end if

        if (associated(p%next) .and. des_next) call json%destroy(p%next)

        nullify(p%previous)
        nullify(p%parent)
        nullify(p%tail)

        if (associated(p)) deallocate(p)
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
!### Examples
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
!### History
!  * Jacob Williams : 12/28/2014 : added destroy optional argument.
!  * Jacob Williams : 12/04/2020 : bug fix.

    subroutine json_value_remove(json,p,destroy)

    implicit none

    class(json_core),intent(inout)  :: json
    type(json_value),pointer        :: p
    logical(LK),intent(in),optional :: destroy  !! Option to destroy `p` after it is removed:
                                                !!
                                                !! * If `destroy` is not present, it is also destroyed.
                                                !! * If `destroy` is present and true, it is destroyed.
                                                !! * If `destroy` is present and false, it is not destroyed.

    type(json_value),pointer :: parent     !! pointer to parent
    type(json_value),pointer :: previous   !! pointer to previous
    type(json_value),pointer :: next       !! pointer to next
    logical(LK)              :: destroy_it !! if `p` should be destroyed

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

            ! nullify all pointers to original structure:
            nullify(p%next)
            nullify(p%previous)
            nullify(p%parent)

            parent%n_children = parent%n_children - 1

        end if

        if (destroy_it) call json%destroy(p)

    end if

    end subroutine json_value_remove
!*****************************************************************************************

!*****************************************************************************************
!>
!  Replace `p1` with `p2` in a JSON structure.
!
!@note The replacement is done using an insert and remove
!      See [[json_value_insert_after]] and [[json_value_remove]]
!      for details.

    subroutine json_value_replace(json,p1,p2,destroy)

    implicit none

    class(json_core),intent(inout)  :: json
    type(json_value),pointer        :: p1       !! the item to replace
    type(json_value),pointer        :: p2       !! item to take the place of `p1`
    logical(LK),intent(in),optional :: destroy  !! Should `p1` also be destroyed
                                                !! (default is True). Normally,
                                                !! this should be true to avoid
                                                !! a memory leak.

    logical(LK) :: destroy_p1 !! if `p1` is to be destroyed

    if (present(destroy)) then
        destroy_p1 = destroy
    else
        destroy_p1 = .true.  ! default
    end if

    call json%insert_after(p1,p2)
    call json%remove(p1,destroy_p1)

    end subroutine json_value_replace
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 4/11/2017
!
!  Reverse the order of the children of an array or object.

    subroutine json_value_reverse(json,p)

    implicit none

    class(json_core),intent(inout) :: json
    type(json_value),pointer       :: p

    type(json_value),pointer :: tmp       !! temp variable for traversing the list
    type(json_value),pointer :: current   !! temp variable for traversing the list
    integer(IK)              :: var_type  !! for getting the variable type

    if (associated(p)) then

        call json%info(p,var_type=var_type)

        ! can only reverse objects or arrays
        if (var_type==json_object .or. var_type==json_array) then

            nullify(tmp)
            current => p%children
            p%tail => current

            ! Swap next and previous for all nodes:
            do
                if (.not. associated(current)) exit
                tmp              => current%previous
                current%previous => current%next
                current%next     => tmp
                current          => current%previous
            end do

            if (associated(tmp)) then
                p%children => tmp%previous
            end if

        end if

    end if

    end subroutine json_value_reverse
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
    type(json_value),pointer       :: p1  !! swap with `p2`
    type(json_value),pointer       :: p2  !! swap with `p1`

    logical                  :: same_parent !! if `p1` and `p2` have the same parent
    logical                  :: first_last  !! if `p1` and `p2` are the first,last or
                                            !! last,first children of a common parent
    logical                  :: adjacent    !! if `p1` and `p2` are adjacent
                                            !! elements in an array
    type(json_value),pointer :: a           !! temporary variable
    type(json_value),pointer :: b           !! temporary variable

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
!### History
!  * Jacob Williams, 8/26/2017 : added duplicate key check.
!
!@note It will return on the first error it encounters.
!
!@note This routine does not check or throw any exceptions.
!      If `json` is currently in a state of exception, it will
!      remain so after calling this routine.

    subroutine json_value_validate(json,p,is_valid,error_msg)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer,intent(in) :: p
    logical(LK),intent(out)             :: is_valid  !! True if the structure is valid.
    character(kind=CK,len=:),allocatable,intent(out) :: error_msg !! if not valid, this will contain
                                                                  !! a description of the problem

    logical(LK) :: has_duplicate !! to check for duplicate keys
    character(kind=CK,len=:),allocatable :: path  !! path to duplicate key
    logical(LK) :: status_ok !! to check for existing exception
    character(kind=CK,len=:),allocatable :: exception_msg  !! error message for an existing exception
    character(kind=CK,len=:),allocatable :: exception_msg2  !! error message for a new exception

    if (associated(p)) then

        is_valid = .true.
        call check_if_valid(p,require_parent=associated(p%parent))

        if (is_valid .and. .not. json%allow_duplicate_keys) then
            ! if no errors so far, also check the
            ! entire structure for duplicate keys:

            ! note: check_for_duplicate_keys does call routines
            ! that check and throw exceptions, so let's clear any
            ! first. (save message for later)
            call json%check_for_errors(status_ok, exception_msg)
            call json%clear_exceptions()

            call json%check_for_duplicate_keys(p,has_duplicate,path=path)
            if (json%failed()) then
                ! if an exception was thrown during this call,
                ! then clear it but make that the error message
                ! returned by this routine. Normally this should
                ! never actually occur since we have already
                ! validated the structure.
                call json%check_for_errors(is_valid, exception_msg2)
                error_msg = exception_msg2
                call json%clear_exceptions()
                is_valid = .false.
            else
                if (has_duplicate) then
                    error_msg = 'duplicate key found: '//path
                    is_valid  = .false.
                end if
            end if

            if (.not. status_ok) then
                ! restore any existing exception if necessary
                call json%throw_exception(exception_msg)
            end if

            ! cleanup:
            if (allocated(path))           deallocate(path)
            if (allocated(exception_msg))  deallocate(exception_msg)
            if (allocated(exception_msg2)) deallocate(exception_msg2)

        end if

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

        integer(IK) :: i !! counter
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
            case(json_real)
                if (.not. allocated(p%dbl_value)) then
                    error_msg = 'dbl_value should be allocated for json_real variable type'
                    is_valid = .false.
                    return
                else if (allocated(p%log_value) .or. allocated(p%int_value) .or. &
                    allocated(p%str_value)) then
                    error_msg = 'incorrect data allocated for json_real variable type'
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
                if (associated(p,p%next)) then
                    error_msg = 'circular linked list'
                    is_valid = .false.
                    return
                else
                    ! if it's an element in an
                    ! array, then require a parent:
                    call check_if_valid(p%next,require_parent=.true.)
                end if
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
                do i = 1_IK, p%n_children
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
!  Given the path string, remove the variable
!  from [[json_value]], if it exists.

    subroutine json_value_remove_if_present(json,p,path)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer            :: p
    character(kind=CK,len=*),intent(in) :: path  !! the path to the variable to remove

    type(json_value),pointer :: p_var
    logical(LK) :: found

    call json%get(p,path,p_var,found)
    if (found) call json%remove(p_var)

    end subroutine json_value_remove_if_present
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_value_remove_if_present]], where `path` is kind=CDK.

    subroutine wrap_json_value_remove_if_present(json,p,path)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer             :: p
    character(kind=CDK,len=*),intent(in) :: path

    call json%remove_if_present(p,to_unicode(path))

    end subroutine wrap_json_value_remove_if_present
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 12/6/2014
!
!  Given the path string, if the variable is present,
!  and is a scalar, then update its value.
!  If it is not present, then create it and set its value.
!
!@note If the variable is not a scalar, an exception will be thrown.

    subroutine json_update_logical(json,p,path,val,found)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer            :: p
    character(kind=CK,len=*),intent(in) :: path  !! path to the variable in the structure
    logical(LK),intent(in)              :: val   !! the new value
    logical(LK),intent(out)             :: found !! if the variable was found and was a scalar.

    type(json_value),pointer :: p_var
    integer(IK) :: var_type

    call json%get(p,path,p_var,found)
    if (found) then

        call json%info(p_var,var_type)
        select case (var_type)
        case (json_null,json_logical,json_integer,json_real,json_string)
            call json%to_logical(p_var,val)    !update the value
        case default
            found = .false.
            call json%throw_exception('Error in json_update_logical: '//&
                                      'the variable is not a scalar value',found)
        end select

    else
        call json%add_by_path(p,path,val)   !add the new element
    end if

    end subroutine json_update_logical
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_update_logical]], where `path` is kind=CDK.

    subroutine wrap_json_update_logical(json,p,path,val,found)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer             :: p
    character(kind=CDK,len=*),intent(in) :: path  !! path to the variable in the structure
    logical(LK),intent(in)               :: val   !! the new value
    logical(LK),intent(out)              :: found !! if the variable was found and was a scalar.

    call json%update(p,to_unicode(path),val,found)

    end subroutine wrap_json_update_logical
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 12/6/2014
!
!  Given the path string, if the variable is present,
!  and is a scalar, then update its value.
!  If it is not present, then create it and set its value.
!
!@note If the variable is not a scalar, an exception will be thrown.

    subroutine json_update_real(json,p,path,val,found)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer            :: p
    character(kind=CK,len=*),intent(in) :: path   !! path to the variable in the structure
    real(RK),intent(in)                 :: val    !! the new value
    logical(LK),intent(out)             :: found  !! if the variable was found and was a scalar.

    type(json_value),pointer :: p_var
    integer(IK) :: var_type

    call json%get(p,path,p_var,found)
    if (found) then

        call json%info(p_var,var_type)
        select case (var_type)
        case (json_null,json_logical,json_integer,json_real,json_string)
            call json%to_real(p_var,val)    !update the value
        case default
            found = .false.
            call json%throw_exception('Error in json_update_real: '//&
                                      'the variable is not a scalar value',found)
        end select

    else
        call json%add_by_path(p,path,val)   !add the new element
    end if

    end subroutine json_update_real
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_update_real]], where `path` is kind=CDK.

    subroutine wrap_json_update_real(json,p,path,val,found)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer             :: p
    character(kind=CDK,len=*),intent(in) :: path  !! path to the variable in the structure
    real(RK),intent(in)                  :: val   !! the new value
    logical(LK),intent(out)              :: found !! if the variable was found and was a scalar.

    call json%update(p,to_unicode(path),val,found)

    end subroutine wrap_json_update_real
!*****************************************************************************************

#ifndef REAL32
!*****************************************************************************************
!>
!  Alternate version of [[json_update_real]], where `val` is `real32`.

    subroutine json_update_real32(json,p,path,val,found)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer            :: p
    character(kind=CK,len=*),intent(in) :: path   !! path to the variable in the structure
    real(real32),intent(in)             :: val    !! the new value
    logical(LK),intent(out)             :: found  !! if the variable was found and was a scalar.

    call json%update(p,path,real(val,RK),found)

    end subroutine json_update_real32
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_update_real32]], where `path` is kind=CDK.

    subroutine wrap_json_update_real32(json,p,path,val,found)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer             :: p
    character(kind=CDK,len=*),intent(in) :: path  !! path to the variable in the structure
    real(real32),intent(in)              :: val   !! the new value
    logical(LK),intent(out)              :: found !! if the variable was found and was a scalar.

    call json%update(p,to_unicode(path),real(val,RK),found)

    end subroutine wrap_json_update_real32
!*****************************************************************************************
#endif

#ifdef REAL128
!*****************************************************************************************
!>
!  Alternate version of [[json_update_real]], where `val` is `real64`.

    subroutine json_update_real64(json,p,path,val,found)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer            :: p
    character(kind=CK,len=*),intent(in) :: path   !! path to the variable in the structure
    real(real64),intent(in)             :: val    !! the new value
    logical(LK),intent(out)             :: found  !! if the variable was found and was a scalar.

    call json%update(p,path,real(val,RK),found)

    end subroutine json_update_real64
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_update_real64]], where `path` is kind=CDK.

    subroutine wrap_json_update_real64(json,p,path,val,found)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer             :: p
    character(kind=CDK,len=*),intent(in) :: path  !! path to the variable in the structure
    real(real64),intent(in)              :: val   !! the new value
    logical(LK),intent(out)              :: found !! if the variable was found and was a scalar.

    call json%update(p,to_unicode(path),real(val,RK),found)

    end subroutine wrap_json_update_real64
!*****************************************************************************************
#endif

!*****************************************************************************************
!> author: Jacob Williams
!  date: 12/6/2014
!
!  Given the path string, if the variable is present,
!  and is a scalar, then update its value.
!  If it is not present, then create it and set its value.
!
!@note If the variable is not a scalar, an exception will be thrown.

    subroutine json_update_integer(json,p,path,val,found)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer            :: p
    character(kind=CK,len=*),intent(in) :: path  !! path to the variable in the structure
    integer(IK),intent(in)              :: val   !! the new value
    logical(LK),intent(out)             :: found !! if the variable was found and was a scalar.

    type(json_value),pointer :: p_var
    integer(IK) :: var_type

    call json%get(p,path,p_var,found)
    if (found) then

        call json%info(p_var,var_type)
        select case (var_type)
        case (json_null,json_logical,json_integer,json_real,json_string)
            call json%to_integer(p_var,val)    !update the value
        case default
            found = .false.
            call json%throw_exception('Error in json_update_integer: '//&
                                      'the variable is not a scalar value',found)
        end select

    else
        call json%add_by_path(p,path,val)   !add the new element
    end if

    end subroutine json_update_integer
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_update_integer]], where `path` is kind=CDK.

    subroutine wrap_json_update_integer(json,p,path,val,found)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer             :: p
    character(kind=CDK,len=*),intent(in) :: path  !! path to the variable in the structure
    integer(IK),intent(in)               :: val   !! the new value
    logical(LK),intent(out)              :: found !! if the variable was found and was a scalar.

    call json%update(p,to_unicode(path),val,found)

    end subroutine wrap_json_update_integer
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 12/6/2014
!
!  Given the path string, if the variable is present,
!  and is a scalar, then update its value.
!  If it is not present, then create it and set its value.
!
!@note If the variable is not a scalar, an exception will be thrown.

    subroutine json_update_string(json,p,path,val,found,trim_str,adjustl_str)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer            :: p
    character(kind=CK,len=*),intent(in) :: path  !! path to the variable in the structure
    character(kind=CK,len=*),intent(in) :: val   !! the new value
    logical(LK),intent(out)             :: found !! if the variable was found and was a scalar.
    logical(LK),intent(in),optional     :: trim_str    !! if TRIM() should be called for the `val`
                                                       !! (only used if `val` is present)
    logical(LK),intent(in),optional     :: adjustl_str !! if ADJUSTL() should be called for the `val`
                                                       !! (only used if `val` is present)
                                                       !! (note that ADJUSTL is done before TRIM)

    type(json_value),pointer :: p_var
    integer(IK) :: var_type

    call json%get(p,path,p_var,found)
    if (found) then

        call json%info(p_var,var_type)
        select case (var_type)
        case (json_null,json_logical,json_integer,json_real,json_string)
            call json%to_string(p_var,val,trim_str=trim_str,adjustl_str=adjustl_str) ! update the value
        case default
            found = .false.
            call json%throw_exception('Error in json_update_string: '//&
                                      'the variable is not a scalar value',found)
        end select

    else
        call json%add_by_path(p,path,val)   !add the new element
    end if

    end subroutine json_update_string
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_update_string]], where `path` and `value` are kind=CDK.

    subroutine wrap_json_update_string(json,p,path,val,found,trim_str,adjustl_str)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer             :: p
    character(kind=CDK,len=*),intent(in) :: path  !! path to the variable in the structure
    character(kind=CDK,len=*),intent(in) :: val   !! the new value
    logical(LK),intent(out)              :: found !! if the variable was found and was a scalar.
    logical(LK),intent(in),optional     :: trim_str       !! if TRIM() should be called for the `val`
                                                          !! (only used if `val` is present)
    logical(LK),intent(in),optional     :: adjustl_str    !! if ADJUSTL() should be called for the `val`
                                                          !! (only used if `val` is present)
                                                          !! (note that ADJUSTL is done before TRIM)

    call json%update(p,to_unicode(path),to_unicode(val),found,trim_str,adjustl_str)

    end subroutine wrap_json_update_string
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_update_string]], where `path` is kind=CDK.

    subroutine json_update_string_name_ascii(json,p,path,val,found,trim_str,adjustl_str)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer             :: p
    character(kind=CDK,len=*),intent(in) :: path  !! path to the variable in the structure
    character(kind=CK, len=*),intent(in) :: val   !! the new value
    logical(LK),intent(out)              :: found !! if the variable was found and was a scalar.
    logical(LK),intent(in),optional     :: trim_str       !! if TRIM() should be called for the `val`
                                                          !! (only used if `val` is present)
    logical(LK),intent(in),optional     :: adjustl_str    !! if ADJUSTL() should be called for the `val`
                                                          !! (only used if `val` is present)
                                                          !! (note that ADJUSTL is done before TRIM)

    call json%update(p,to_unicode(path),val,found,trim_str,adjustl_str)

    end subroutine json_update_string_name_ascii
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_update_string]], where `val` is kind=CDK.

    subroutine json_update_string_val_ascii(json,p,path,val,found,trim_str,adjustl_str)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer             :: p
    character(kind=CK, len=*),intent(in) :: path  !! path to the variable in the structure
    character(kind=CDK,len=*),intent(in) :: val   !! the new value
    logical(LK),intent(out)              :: found !! if the variable was found and was a scalar.
    logical(LK),intent(in),optional     :: trim_str       !! if TRIM() should be called for the `val`
                                                          !! (only used if `val` is present)
    logical(LK),intent(in),optional     :: adjustl_str    !! if ADJUSTL() should be called for the `val`
                                                          !! (only used if `val` is present)
                                                          !! (note that ADJUSTL is done before TRIM)

    call json%update(p,path,to_unicode(val),found,trim_str,adjustl_str)

    end subroutine json_update_string_val_ascii
!*****************************************************************************************

!*****************************************************************************************
!>
!  Adds `member` as a child of `p`.

    subroutine json_value_add_member(json,p,member)

    implicit none

    class(json_core),intent(inout) :: json
    type(json_value),pointer       :: p       !! `p` must be a `json_object`
                                              !! or a `json_array`
    type(json_value),pointer       :: member  !! the child member
                                              !! to add to `p`

    integer(IK) :: var_type  !! variable type of `p`

    if (.not. json%exception_thrown) then

        if (associated(p)) then

            call json%info(p,var_type=var_type)

            select case (var_type)
            case(json_object, json_array)

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

            case default
                call json%throw_exception('Error in json_value_add_member: '//&
                                          'can only add child to object or array')
            end select

        else
            call json%throw_exception('Error in json_value_add_member: '//&
                                      'the pointer is not associated')
        end if

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
!   call json%load(file='myfile.json', p=p)
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
                                              !! (this is a 1-based Fortran
                                              !! style array index)
    type(json_value),pointer       :: element !! the element to insert

    type(json_value),pointer :: tmp  !! for getting the `idx`-th child of `p`

    if (.not. json%exception_thrown) then

        ! get the idx-th child of p:
        call json%get_child(p,idx,tmp)

        ! call json_value_insert_after:
        if (.not. json%exception_thrown) call json%insert_after(tmp,element)

    end if

    end subroutine json_value_insert_after_child_by_index
!*****************************************************************************************

!*****************************************************************************************
!>
!  Add a new member (`json_value` pointer) to a JSON structure, given the path.
!
!@warning If the path points to an existing variable in the structure,
!         then this routine will destroy it and replace it with the
!         new value.

    subroutine json_add_member_by_path(json,me,path,p,found,was_created)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer            :: me           !! the JSON structure
    character(kind=CK,len=*),intent(in) :: path         !! the path to the variable
    type(json_value),pointer,intent(in) :: p            !! the value to add
    logical(LK),intent(out),optional    :: found        !! if the variable was found
    logical(LK),intent(out),optional    :: was_created  !! if the variable had to be created

    type(json_value),pointer :: tmp
    character(kind=CK,len=:),allocatable :: name  !! name of the variable

    if ( .not. json%exception_thrown ) then

        if (.not. associated(p)) then
            call json%throw_exception('Error in json_add_member_by_path:'//&
                                      ' Input pointer p is not associated.',found)
            if (present(found)) then
                found = .false.
                call json%clear_exceptions()
            end if
            if ( present(was_created) ) was_created = .false.
        else

            ! return a pointer to the path (possibly creating it)
            call json%create(me,path,tmp,found,was_created)

            if (.not. associated(tmp)) then

                call json%throw_exception('Error in json_add_member_by_path:'//&
                                          ' Unable to resolve path: '//trim(path),found)
                if (present(found)) then
                    found = .false.
                    call json%clear_exceptions()
                end if

            else

                call json%info(tmp,name=name)

                ! replace it with the new one:
                call json%replace(tmp,p,destroy=.true.)
                call json%rename(p,name)

            end if

        end if

    else
        if ( present(found) ) found = .false.
        if ( present(was_created) ) was_created = .false.
    end if

    end subroutine json_add_member_by_path
!*****************************************************************************************

!*****************************************************************************************
!>
!  Wrapper to [[json_add_member_by_path]] where "path" is kind=CDK.

    subroutine wrap_json_add_member_by_path(json,me,path,p,found,was_created)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer             :: me           !! the JSON structure
    character(kind=CDK,len=*),intent(in) :: path         !! the path to the variable
    type(json_value),pointer,intent(in)  :: p            !! the value to add
    logical(LK),intent(out),optional     :: found        !! if the variable was found
    logical(LK),intent(out),optional     :: was_created  !! if the variable had to be created

    call json%json_add_member_by_path(me,to_unicode(path),p,found,was_created)

    end subroutine wrap_json_add_member_by_path
!*****************************************************************************************

!*****************************************************************************************
!>
!  Add an integer value to a [[json_value]], given the path.
!
!@warning If the path points to an existing variable in the structure,
!         then this routine will destroy it and replace it with the
!         new value.

    subroutine json_add_integer_by_path(json,me,path,value,found,was_created)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer            :: me           !! the JSON structure
    character(kind=CK,len=*),intent(in) :: path         !! the path to the variable
    integer(IK),intent(in)              :: value        !! the value to add
    logical(LK),intent(out),optional    :: found        !! if the variable was found
    logical(LK),intent(out),optional    :: was_created  !! if the variable had to be created

    type(json_value),pointer :: p
    type(json_value),pointer :: tmp
    character(kind=CK,len=:),allocatable :: name  !! variable name

    if ( .not. json%exception_thrown ) then

        nullify(p)

        ! return a pointer to the path (possibly creating it)
        ! If the variable had to be created, then
        ! it will be a json_null variable.
        call json%create(me,path,p,found,was_created)

        if (.not. associated(p)) then

            call json%throw_exception('Error in json_add_integer_by_path:'//&
                                      ' Unable to resolve path: '//trim(path),found)
            if (present(found)) then
                found = .false.
                call json%clear_exceptions()
            end if

        else

            !NOTE: a new object is created, and the old one
            !      is replaced and destroyed. This is to
            !      prevent memory leaks if the type is
            !      being changed (for example, if an array
            !      is being replaced with a scalar).

            if (p%var_type==json_integer) then
                p%int_value = value
            else
                call json%info(p,name=name)
                call json%create_integer(tmp,value,name)
                call json%replace(p,tmp,destroy=.true.)
            end if

        end if

    else
        if ( present(found) )       found = .false.
        if ( present(was_created) ) was_created = .false.
    end if

    end subroutine json_add_integer_by_path
!*****************************************************************************************

!*****************************************************************************************
!>
!  Wrapper to [[json_add_integer_by_path]] where "path" is kind=CDK.

    subroutine wrap_json_add_integer_by_path(json,me,path,value,found,was_created)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer             :: me          !! the JSON structure
    character(kind=CDK,len=*),intent(in) :: path        !! the path to the variable
    integer(IK),intent(in)               :: value       !! the value to add
    logical(LK),intent(out),optional     :: found       !! if the variable was found
    logical(LK),intent(out),optional     :: was_created !! if the variable had to be created

    call json%json_add_integer_by_path(me,to_unicode(path),value,found,was_created)

    end subroutine wrap_json_add_integer_by_path
!*****************************************************************************************

!*****************************************************************************************
!>
!  Add an real value to a [[json_value]], given the path.
!
!@warning If the path points to an existing variable in the structure,
!         then this routine will destroy it and replace it with the
!         new value.

    subroutine json_add_real_by_path(json,me,path,value,found,was_created)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer            :: me           !! the JSON structure
    character(kind=CK,len=*),intent(in) :: path         !! the path to the variable
    real(RK),intent(in)                 :: value        !! the value to add
    logical(LK),intent(out),optional    :: found        !! if the variable was found
    logical(LK),intent(out),optional    :: was_created  !! if the variable had to be created

    type(json_value),pointer :: p
    type(json_value),pointer :: tmp
    character(kind=CK,len=:),allocatable :: name  !! variable name

    if ( .not. json%exception_thrown ) then

        nullify(p)

        ! return a pointer to the path (possibly creating it)
        ! If the variable had to be created, then
        ! it will be a json_null variable.
        call json%create(me,path,p,found,was_created)

        if (.not. associated(p)) then

            call json%throw_exception('Error in json_add_real_by_path:'//&
                                      ' Unable to resolve path: '//trim(path),found)
            if (present(found)) then
                found = .false.
                call json%clear_exceptions()
            end if

        else

            !NOTE: a new object is created, and the old one
            !      is replaced and destroyed. This is to
            !      prevent memory leaks if the type is
            !      being changed (for example, if an array
            !      is being replaced with a scalar).

            if (p%var_type==json_real) then
                p%dbl_value = value
            else
                call json%info(p,name=name)
                call json%create_real(tmp,value,name)
                call json%replace(p,tmp,destroy=.true.)
            end if

        end if

    else
        if ( present(found) )       found = .false.
        if ( present(was_created) ) was_created = .false.
    end if

    end subroutine json_add_real_by_path
!*****************************************************************************************

!*****************************************************************************************
!>
!  Wrapper to [[json_add_real_by_path]] where "path" is kind=CDK.

    subroutine wrap_json_add_real_by_path(json,me,path,value,found,was_created)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer             :: me          !! the JSON structure
    character(kind=CDK,len=*),intent(in) :: path        !! the path to the variable
    real(RK),intent(in)                  :: value       !! the value to add
    logical(LK),intent(out),optional     :: found       !! if the variable was found
    logical(LK),intent(out),optional     :: was_created !! if the variable had to be created

    call json%json_add_real_by_path(me,to_unicode(path),value,found,was_created)

    end subroutine wrap_json_add_real_by_path
!*****************************************************************************************

#ifndef REAL32
!*****************************************************************************************
!>
!  Alternate version of [[json_add_real_by_path]] where value=real32.

    subroutine json_add_real32_by_path(json,me,path,value,found,was_created)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer            :: me           !! the JSON structure
    character(kind=CK,len=*),intent(in) :: path         !! the path to the variable
    real(real32),intent(in)             :: value        !! the value to add
    logical(LK),intent(out),optional    :: found        !! if the variable was found
    logical(LK),intent(out),optional    :: was_created  !! if the variable had to be created

    call json%add_by_path(me,path,real(value,RK),found,was_created)

    end subroutine json_add_real32_by_path
!*****************************************************************************************

!*****************************************************************************************
!>
!  Wrapper to [[json_add_real32_by_path]] where "path" is kind=CDK.

    subroutine wrap_json_add_real32_by_path(json,me,path,value,found,was_created)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer             :: me          !! the JSON structure
    character(kind=CDK,len=*),intent(in) :: path        !! the path to the variable
    real(real32),intent(in)              :: value       !! the value to add
    logical(LK),intent(out),optional     :: found       !! if the variable was found
    logical(LK),intent(out),optional     :: was_created !! if the variable had to be created

    call json%add_by_path(me,to_unicode(path),real(value,RK),found,was_created)

    end subroutine wrap_json_add_real32_by_path
!*****************************************************************************************
#endif

#ifdef REAL128
!*****************************************************************************************
!>
!  Alternate version of [[json_add_real_by_path]] where value=real32.

    subroutine json_add_real64_by_path(json,me,path,value,found,was_created)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer            :: me           !! the JSON structure
    character(kind=CK,len=*),intent(in) :: path         !! the path to the variable
    real(real64),intent(in)             :: value        !! the value to add
    logical(LK),intent(out),optional    :: found        !! if the variable was found
    logical(LK),intent(out),optional    :: was_created  !! if the variable had to be created

    call json%add_by_path(me,path,real(value,RK),found,was_created)

    end subroutine json_add_real64_by_path
!*****************************************************************************************

!*****************************************************************************************
!>
!  Wrapper to [[json_add_real64_by_path]] where "path" is kind=CDK.

    subroutine wrap_json_add_real64_by_path(json,me,path,value,found,was_created)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer             :: me          !! the JSON structure
    character(kind=CDK,len=*),intent(in) :: path        !! the path to the variable
    real(real64),intent(in)              :: value       !! the value to add
    logical(LK),intent(out),optional     :: found       !! if the variable was found
    logical(LK),intent(out),optional     :: was_created !! if the variable had to be created

    call json%add_by_path(me,to_unicode(path),real(value,RK),found,was_created)

    end subroutine wrap_json_add_real64_by_path
!*****************************************************************************************
#endif

!*****************************************************************************************
!>
!  Add a logical value to a [[json_value]], given the path.
!
!@warning If the path points to an existing variable in the structure,
!         then this routine will destroy it and replace it with the
!         new value.

    subroutine json_add_logical_by_path(json,me,path,value,found,was_created)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer            :: me           !! the JSON structure
    character(kind=CK,len=*),intent(in) :: path         !! the path to the variable
    logical(LK),intent(in)              :: value        !! the value to add
    logical(LK),intent(out),optional    :: found        !! if the variable was found
    logical(LK),intent(out),optional    :: was_created  !! if the variable had to be created

    type(json_value),pointer :: p
    type(json_value),pointer :: tmp
    character(kind=CK,len=:),allocatable :: name  !! variable name

    if ( .not. json%exception_thrown ) then

        nullify(p)

        ! return a pointer to the path (possibly creating it)
        ! If the variable had to be created, then
        ! it will be a json_null variable.
        call json%create(me,path,p,found,was_created)

        if (.not. associated(p)) then

            call json%throw_exception('Error in json_add_logical_by_path:'//&
                                      ' Unable to resolve path: '//trim(path),found)
            if (present(found)) then
                found = .false.
                call json%clear_exceptions()
            end if

        else

            !NOTE: a new object is created, and the old one
            !      is replaced and destroyed. This is to
            !      prevent memory leaks if the type is
            !      being changed (for example, if an array
            !      is being replaced with a scalar).

            if (p%var_type==json_logical) then
                p%log_value = value
            else
                call json%info(p,name=name)
                call json%create_logical(tmp,value,name)
                call json%replace(p,tmp,destroy=.true.)
            end if

        end if

    else
        if ( present(found) )       found = .false.
        if ( present(was_created) ) was_created = .false.
    end if

    end subroutine json_add_logical_by_path
!*****************************************************************************************

!*****************************************************************************************
!>
!  Wrapper to [[json_add_logical_by_path]] where "path" is kind=CDK.

    subroutine wrap_json_add_logical_by_path(json,me,path,value,found,was_created)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer             :: me          !! the JSON structure
    character(kind=CDK,len=*),intent(in) :: path        !! the path to the variable
    logical(LK),intent(in)               :: value       !! the value to add
    logical(LK),intent(out),optional     :: found       !! if the variable was found
    logical(LK),intent(out),optional     :: was_created !! if the variable had to be created

    call json%json_add_logical_by_path(me,to_unicode(path),value,found,was_created)

    end subroutine wrap_json_add_logical_by_path
!*****************************************************************************************

!*****************************************************************************************
!>
!  Add a string value to a [[json_value]], given the path.
!
!@warning If the path points to an existing variable in the structure,
!         then this routine will destroy it and replace it with the
!         new value.

    subroutine json_add_string_by_path(json,me,path,value,found,&
                                            was_created,trim_str,adjustl_str)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer            :: me           !! the JSON structure
    character(kind=CK,len=*),intent(in) :: path         !! the path to the variable
    character(kind=CK,len=*),intent(in) :: value        !! the value to add
    logical(LK),intent(out),optional    :: found        !! if the variable was found
    logical(LK),intent(out),optional    :: was_created  !! if the variable had to be created
    logical(LK),intent(in),optional     :: trim_str     !! if TRIM() should be called for each element
    logical(LK),intent(in),optional     :: adjustl_str  !! if ADJUSTL() should be called for each element

    type(json_value),pointer :: p
    type(json_value),pointer :: tmp
    character(kind=CK,len=:),allocatable :: name  !! variable name

    if ( .not. json%exception_thrown ) then

        nullify(p)

        ! return a pointer to the path (possibly creating it)
        ! If the variable had to be created, then
        ! it will be a json_null variable.
        call json%create(me,path,p,found,was_created)

        if (.not. associated(p)) then

            call json%throw_exception('Error in json_add_string_by_path:'//&
                                      ' Unable to resolve path: '//trim(path),found)
            if (present(found)) then
                found = .false.
                call json%clear_exceptions()
            end if

        else

            !NOTE: a new object is created, and the old one
            !      is replaced and destroyed. This is to
            !      prevent memory leaks if the type is
            !      being changed (for example, if an array
            !      is being replaced with a scalar).

            if (p%var_type==json_string) then
                p%str_value = value
            else
                call json%info(p,name=name)
                call json%create_string(tmp,value,name,trim_str,adjustl_str)
                call json%replace(p,tmp,destroy=.true.)
            end if

        end if

    else
        if ( present(found) )       found = .false.
        if ( present(was_created) ) was_created = .false.
    end if

    end subroutine json_add_string_by_path
!*****************************************************************************************

!*****************************************************************************************
!>
!  Wrapper to [[json_add_string_by_path]] where "path" is kind=CDK.

    subroutine wrap_json_add_string_by_path(json,me,path,value,found,&
                                                was_created,trim_str,adjustl_str)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer             :: me          !! the JSON structure
    character(kind=CDK,len=*),intent(in) :: path        !! the path to the variable
    character(kind=CDK,len=*),intent(in) :: value       !! the value to add
    logical(LK),intent(out),optional     :: found       !! if the variable was found
    logical(LK),intent(out),optional     :: was_created !! if the variable had to be created
    logical(LK),intent(in),optional      :: trim_str    !! if TRIM() should be called for each element
    logical(LK),intent(in),optional      :: adjustl_str !! if ADJUSTL() should be called for each element

    call json%json_add_string_by_path(me,to_unicode(path),to_unicode(value),&
                                        found,was_created,trim_str,adjustl_str)

    end subroutine wrap_json_add_string_by_path
!*****************************************************************************************

!*****************************************************************************************
!>
!  Wrapper for [[json_add_string_by_path]] where "path" is kind=CDK.

    subroutine json_add_string_by_path_path_ascii(json,me,path,value,found,&
                                                    was_created,trim_str,adjustl_str)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer             :: me           !! the JSON structure
    character(kind=CDK,len=*),intent(in) :: path         !! the path to the variable
    character(kind=CK,len=*),intent(in)  :: value        !! the value to add
    logical(LK),intent(out),optional     :: found        !! if the variable was found
    logical(LK),intent(out),optional     :: was_created  !! if the variable had to be created
    logical(LK),intent(in),optional      :: trim_str     !! if TRIM() should be called for each element
    logical(LK),intent(in),optional      :: adjustl_str  !! if ADJUSTL() should be called for each element

    call json%json_add_string_by_path(me,to_unicode(path),value,found,was_created,trim_str,adjustl_str)

    end subroutine json_add_string_by_path_path_ascii
!*****************************************************************************************

!*****************************************************************************************
!>
!  Wrapper for [[json_add_string_by_path]] where "value" is kind=CDK.

    subroutine json_add_string_by_path_value_ascii(json,me,path,value,found,&
                                                        was_created,trim_str,adjustl_str)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer             :: me           !! the JSON structure
    character(kind=CK,len=*),intent(in)  :: path         !! the path to the variable
    character(kind=CDK,len=*),intent(in) :: value        !! the value to add
    logical(LK),intent(out),optional     :: found        !! if the variable was found
    logical(LK),intent(out),optional     :: was_created  !! if the variable had to be created
    logical(LK),intent(in),optional      :: trim_str     !! if TRIM() should be called for each element
    logical(LK),intent(in),optional      :: adjustl_str  !! if ADJUSTL() should be called for each element

    call json%json_add_string_by_path(me,path,to_unicode(value),found,was_created,trim_str,adjustl_str)

    end subroutine json_add_string_by_path_value_ascii
!*****************************************************************************************

!*****************************************************************************************
!>
!  Wrapper to [[json_add_integer_by_path]] for adding an integer vector by path.

    subroutine json_add_integer_vec_by_path(json,me,path,value,found,was_created)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer            :: me           !! the JSON structure
    character(kind=CK,len=*),intent(in) :: path         !! the path to the variable
    integer(IK),dimension(:),intent(in) :: value        !! the vector to add
    logical(LK),intent(out),optional    :: found        !! if the variable was found
    logical(LK),intent(out),optional    :: was_created  !! if the variable had to be created

    type(json_value),pointer :: p   !! pointer to path (which may exist)
    type(json_value),pointer :: var !! new variable that is created
    integer(IK) :: i    !! counter
    character(kind=CK,len=:),allocatable :: name !! the variable name
    logical(LK) :: p_found  !! if the path was successfully found (or created)

    if ( .not. json%exception_thrown ) then

        !get a pointer to the variable
        !(creating it if necessary)
        call json%create(me,path,p,found=p_found)
        if (p_found) then
            call json%info(p,name=name)             ! want to keep the existing name
            call json%create_array(var,name)        ! create a new array variable
            call json%replace(p,var,destroy=.true.) ! replace p with this array (destroy p)
            !populate each element of the array:
            do i=1,size(value)
                call json%add(var, CK_'', value(i))
            end do
        end if

    else
        if ( present(found) )       found = .false.
        if ( present(was_created) ) was_created = .false.
    end if

    end subroutine json_add_integer_vec_by_path
!*****************************************************************************************

!*****************************************************************************************
!>
!  Wrapper for [[json_add_integer_vec_by_path]] where "path" is kind=CDK).

    subroutine wrap_json_add_integer_vec_by_path(json,me,path,value,found,was_created)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer             :: me           !! the JSON structure
    character(kind=CDK,len=*),intent(in) :: path         !! the path to the variable
    integer(IK),dimension(:),intent(in)  :: value        !! the vector to add
    logical(LK),intent(out),optional     :: found        !! if the variable was found
    logical(LK),intent(out),optional     :: was_created  !! if the variable had to be created

    call json%json_add_integer_vec_by_path(me,to_unicode(path),value,found,was_created)

    end subroutine wrap_json_add_integer_vec_by_path
!*****************************************************************************************

!*****************************************************************************************
!>
!  Wrapper to [[json_add_logical_by_path]] for adding a logical vector by path.

    subroutine json_add_logical_vec_by_path(json,me,path,value,found,was_created)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer            :: me           !! the JSON structure
    character(kind=CK,len=*),intent(in) :: path         !! the path to the variable
    logical(LK),dimension(:),intent(in) :: value        !! the vector to add
    logical(LK),intent(out),optional    :: found        !! if the variable was found
    logical(LK),intent(out),optional    :: was_created  !! if the variable had to be created

    type(json_value),pointer :: p   !! pointer to path (which may exist)
    type(json_value),pointer :: var !! new variable that is created
    integer(IK) :: i    !! counter
    character(kind=CK,len=:),allocatable :: name !! the variable name
    logical(LK) :: p_found  !! if the path was successfully found (or created)

    if ( .not. json%exception_thrown ) then

        !get a pointer to the variable
        !(creating it if necessary)
        call json%create(me,path,p,found=p_found)
        if (p_found) then
            call json%info(p,name=name)             ! want to keep the existing name
            call json%create_array(var,name)        ! create a new array variable
            call json%replace(p,var,destroy=.true.) ! replace p with this array (destroy p)
            !populate each element of the array:
            do i=1,size(value)
                call json%add(var, CK_'', value(i))
            end do
        end if

    else
        if ( present(found) )       found = .false.
        if ( present(was_created) ) was_created = .false.
    end if

    end subroutine json_add_logical_vec_by_path
!*****************************************************************************************

!*****************************************************************************************
!>
!  Wrapper for [[json_add_logical_vec_by_path]] where "path" is kind=CDK).

    subroutine wrap_json_add_logical_vec_by_path(json,me,path,value,found,was_created)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer             :: me           !! the JSON structure
    character(kind=CDK,len=*),intent(in) :: path         !! the path to the variable
    logical(LK),dimension(:),intent(in)  :: value        !! the vector to add
    logical(LK),intent(out),optional     :: found        !! if the variable was found
    logical(LK),intent(out),optional     :: was_created  !! if the variable had to be created

    call json%json_add_logical_vec_by_path(me,to_unicode(path),value,found,was_created)

    end subroutine wrap_json_add_logical_vec_by_path
!*****************************************************************************************

!*****************************************************************************************
!>
!  Wrapper to [[json_add_real_by_path]] for adding a real vector by path.

    subroutine json_add_real_vec_by_path(json,me,path,value,found,was_created)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer            :: me           !! the JSON structure
    character(kind=CK,len=*),intent(in) :: path         !! the path to the variable
    real(RK),dimension(:),intent(in)    :: value        !! the vector to add
    logical(LK),intent(out),optional    :: found        !! if the variable was found
    logical(LK),intent(out),optional    :: was_created  !! if the variable had to be created

    type(json_value),pointer :: p   !! pointer to path (which may exist)
    type(json_value),pointer :: var !! new variable that is created
    integer(IK) :: i    !! counter
    character(kind=CK,len=:),allocatable :: name !! the variable name
    logical(LK) :: p_found  !! if the path was successfully found (or created)

    if ( .not. json%exception_thrown ) then

        !get a pointer to the variable
        !(creating it if necessary)
        call json%create(me,path,p,found=p_found)
        if (p_found) then
            call json%info(p,name=name)             ! want to keep the existing name
            call json%create_array(var,name)        ! create a new array variable
            call json%replace(p,var,destroy=.true.) ! replace p with this array (destroy p)
            !populate each element of the array:
            do i=1,size(value)
                call json%add(var, CK_'', value(i))
            end do
        end if

    else
        if ( present(found) )       found = .false.
        if ( present(was_created) ) was_created = .false.
    end if

    end subroutine json_add_real_vec_by_path
!*****************************************************************************************

!*****************************************************************************************
!>
!  Wrapper for [[json_add_real_vec_by_path]] where "path" is kind=CDK).

    subroutine wrap_json_add_real_vec_by_path(json,me,path,value,found,was_created)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer             :: me           !! the JSON structure
    character(kind=CDK,len=*),intent(in) :: path         !! the path to the variable
    real(RK),dimension(:),intent(in)     :: value        !! the vector to add
    logical(LK),intent(out),optional     :: found        !! if the variable was found
    logical(LK),intent(out),optional     :: was_created  !! if the variable had to be created

    call json%json_add_real_vec_by_path(me,to_unicode(path),value,found,was_created)

    end subroutine wrap_json_add_real_vec_by_path
!*****************************************************************************************

#ifndef REAL32
!*****************************************************************************************
!>
!  Wrapper to [[json_add_real_by_path]] for adding a real vector by path.

    subroutine json_add_real32_vec_by_path(json,me,path,value,found,was_created)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer             :: me           !! the JSON structure
    character(kind=CK,len=*),intent(in)  :: path         !! the path to the variable
    real(real32),dimension(:),intent(in) :: value        !! the vector to add
    logical(LK),intent(out),optional     :: found        !! if the variable was found
    logical(LK),intent(out),optional     :: was_created  !! if the variable had to be created

    call json%add_by_path(me,path,real(value,RK),found,was_created)

    end subroutine json_add_real32_vec_by_path
!*****************************************************************************************

!*****************************************************************************************
!>
!  Wrapper for [[json_add_real32_vec_by_path]] where "path" is kind=CDK).

    subroutine wrap_json_add_real32_vec_by_path(json,me,path,value,found,was_created)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer             :: me           !! the JSON structure
    character(kind=CDK,len=*),intent(in) :: path         !! the path to the variable
    real(real32),dimension(:),intent(in) :: value        !! the vector to add
    logical(LK),intent(out),optional     :: found        !! if the variable was found
    logical(LK),intent(out),optional     :: was_created  !! if the variable had to be created

    call json%add_by_path(me,to_unicode(path),real(value,RK),found,was_created)

    end subroutine wrap_json_add_real32_vec_by_path
!*****************************************************************************************
#endif

#ifdef REAL128
!*****************************************************************************************
!>
!  Wrapper to [[json_add_real_by_path]] for adding a real vector by path.

    subroutine json_add_real64_vec_by_path(json,me,path,value,found,was_created)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer             :: me           !! the JSON structure
    character(kind=CK,len=*),intent(in)  :: path         !! the path to the variable
    real(real64),dimension(:),intent(in) :: value        !! the vector to add
    logical(LK),intent(out),optional     :: found        !! if the variable was found
    logical(LK),intent(out),optional     :: was_created  !! if the variable had to be created

    call json%add_by_path(me,path,real(value,RK),found,was_created)

    end subroutine json_add_real64_vec_by_path
!*****************************************************************************************

!*****************************************************************************************
!>
!  Wrapper for [[json_add_real64_vec_by_path]] where "path" is kind=CDK).

    subroutine wrap_json_add_real64_vec_by_path(json,me,path,value,found,was_created)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer             :: me           !! the JSON structure
    character(kind=CDK,len=*),intent(in) :: path         !! the path to the variable
    real(real64),dimension(:),intent(in) :: value        !! the vector to add
    logical(LK),intent(out),optional     :: found        !! if the variable was found
    logical(LK),intent(out),optional     :: was_created  !! if the variable had to be created

    call json%add_by_path(me,to_unicode(path),real(value,RK),found,was_created)

    end subroutine wrap_json_add_real64_vec_by_path
!*****************************************************************************************
#endif

!*****************************************************************************************
!>
!  Wrapper to [[json_add_string_by_path]] for adding a string vector by path.
!
!@note The `ilen` input can be used to specify the actual lengths of the
!      the strings in the array. They must all be `<= len(value)`.

    subroutine json_add_string_vec_by_path(json,me,path,value,found,was_created,ilen,trim_str,adjustl_str)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer            :: me           !! the JSON structure
    character(kind=CK,len=*),intent(in) :: path         !! the path to the variable
    character(kind=CK,len=*),dimension(:),intent(in) :: value !! the vector to add
    logical(LK),intent(out),optional    :: found        !! if the variable was found
    logical(LK),intent(out),optional    :: was_created  !! if the variable had to be created
    integer(IK),dimension(:),intent(in),optional :: ilen !! the string lengths of each
                                                         !! element in `value`. If not present,
                                                         !! the full `len(value)` string is added
                                                         !! for each element.
    logical(LK),intent(in),optional     :: trim_str      !! if TRIM() should be called for each element
    logical(LK),intent(in),optional     :: adjustl_str   !! if ADJUSTL() should be called for each element

    type(json_value),pointer :: p   !! pointer to path (which may exist)
    type(json_value),pointer :: var !! new variable that is created
    integer(IK) :: i    !! counter
    character(kind=CK,len=:),allocatable :: name !! the variable name
    logical(LK) :: p_found  !! if the path was successfully found (or created)

    if ( .not. json%exception_thrown ) then

        ! validate ilen array if present:
        if (present(ilen)) then
            if (size(ilen)/=size(value)) then
                call json%throw_exception('Error in json_add_string_vec_by_path: '//&
                                          'Invalid size of ilen input vector.',found)
                if (present(found)) then
                    found = .false.
                    call json%clear_exceptions()
                end if
                if (present(was_created)) was_created = .false.
                return
            else
                ! also have to validate the specified lengths.
                ! (must not be greater than input string length)
                do i = 1, size(value)
                    if (ilen(i)>len(value)) then
                        call json%throw_exception('Error in json_add_string_vec_by_path: '//&
                                                  'Invalid ilen element.',found)
                        if (present(found)) then
                            found = .false.
                            call json%clear_exceptions()
                        end if
                        if (present(was_created)) was_created = .false.
                        return
                    end if
                end do
            end if
        end if

        !get a pointer to the variable
        !(creating it if necessary)
        call json%create(me,path,p,found=p_found)
        if (p_found) then
            call json%info(p,name=name)             ! want to keep the existing name
            call json%create_array(var,name)        ! create a new array variable
            call json%replace(p,var,destroy=.true.) ! replace p with this array (destroy p)
            !populate each element of the array:
            do i=1,size(value)
                if (present(ilen)) then
                    call json%add(var, CK_'', value(i)(1:ilen(i)), &
                                  trim_str=trim_str, adjustl_str=adjustl_str)
                else
                    call json%add(var, CK_'', value(i), &
                                  trim_str=trim_str, adjustl_str=adjustl_str)
                end if
            end do
        end if

    else
        if ( present(found) )       found = .false.
        if ( present(was_created) ) was_created = .false.
    end if

    end subroutine json_add_string_vec_by_path
!*****************************************************************************************

!*****************************************************************************************
!>
!  Wrapper for [[json_add_string_vec_by_path]] where "path" and "value" are kind=CDK).

    subroutine wrap_json_add_string_vec_by_path(json,me,path,value,&
                                                found,was_created,ilen,&
                                                trim_str,adjustl_str)

    implicit none

    class(json_core),intent(inout)                   :: json
    type(json_value),pointer                         :: me           !! the JSON structure
    character(kind=CDK,len=*),intent(in)             :: path         !! the path to the variable
    character(kind=CDK,len=*),dimension(:),intent(in):: value        !! the vector to add
    logical(LK),intent(out),optional                 :: found        !! if the variable was found
    logical(LK),intent(out),optional                 :: was_created  !! if the variable had to be created
    integer(IK),dimension(:),intent(in),optional :: ilen !! the string lengths of each
                                                         !! element in `value`. If not present,
                                                         !! the full `len(value)` string is added
                                                         !! for each element.
    logical(LK),intent(in),optional  :: trim_str         !! if TRIM() should be called for each element
    logical(LK),intent(in),optional  :: adjustl_str      !! if ADJUSTL() should be called for each element

    call json%json_add_string_vec_by_path(me,to_unicode(path),to_unicode(value),&
                                            found,was_created,ilen,trim_str,adjustl_str)

    end subroutine wrap_json_add_string_vec_by_path
!*****************************************************************************************

!*****************************************************************************************
!>
!  Wrapper for [[json_add_string_vec_by_path]] where "value" is kind=CDK).

    subroutine json_add_string_vec_by_path_value_ascii(json,me,path,value,&
                                                        found,was_created,ilen,&
                                                        trim_str,adjustl_str)

    implicit none

    class(json_core),intent(inout)                   :: json
    type(json_value),pointer                         :: me           !! the JSON structure
    character(kind=CK,len=*),intent(in)              :: path         !! the path to the variable
    character(kind=CDK,len=*),dimension(:),intent(in):: value        !! the vector to add
    logical(LK),intent(out),optional                 :: found        !! if the variable was found
    logical(LK),intent(out),optional                 :: was_created  !! if the variable had to be created
    integer(IK),dimension(:),intent(in),optional :: ilen !! the string lengths of each
                                                         !! element in `value`. If not present,
                                                         !! the full `len(value)` string is added
                                                         !! for each element.
    logical(LK),intent(in),optional  :: trim_str         !! if TRIM() should be called for each element
    logical(LK),intent(in),optional  :: adjustl_str      !! if ADJUSTL() should be called for each element

    call json%json_add_string_vec_by_path(me,path,to_unicode(value),&
                                            found,was_created,ilen,trim_str,adjustl_str)

    end subroutine json_add_string_vec_by_path_value_ascii
!*****************************************************************************************

!*****************************************************************************************
!>
!  Wrapper for [[json_add_string_vec_by_path]] where "path" is kind=CDK).

    subroutine json_add_string_vec_by_path_path_ascii(json,me,path,value,&
                                                        found,was_created,ilen,&
                                                        trim_str,adjustl_str)

    implicit none

    class(json_core),intent(inout)                   :: json
    type(json_value),pointer                         :: me           !! the JSON structure
    character(kind=CDK,len=*),intent(in)             :: path         !! the path to the variable
    character(kind=CK,len=*),dimension(:),intent(in) :: value        !! the vector to add
    logical(LK),intent(out),optional                 :: found        !! if the variable was found
    logical(LK),intent(out),optional                 :: was_created  !! if the variable had to be created
    integer(IK),dimension(:),intent(in),optional :: ilen !! the string lengths of each
                                                         !! element in `value`. If not present,
                                                         !! the full `len(value)` string is added
                                                         !! for each element.
    logical(LK),intent(in),optional  :: trim_str         !! if TRIM() should be called for each element
    logical(LK),intent(in),optional  :: adjustl_str      !! if ADJUSTL() should be called for each element

    call json%json_add_string_vec_by_path(me,to_unicode(path),value,&
                                            found,was_created,ilen,trim_str,adjustl_str)

    end subroutine json_add_string_vec_by_path_path_ascii
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/19/2014
!
!  Add a real value child to the [[json_value]] variable.
!
!@note This routine is part of the public API that can be
!      used to build a JSON structure using [[json_value]] pointers.

    subroutine json_value_add_real(json,p,name,val)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer            :: p
    character(kind=CK,len=*),intent(in) :: name  !! variable name
    real(RK),intent(in)                 :: val   !! real value

    type(json_value),pointer :: var

    !create the variable:
    call json%create_real(var,val,name)

    !add it:
    call json%add(p, var)

    end subroutine json_value_add_real
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_value_add_real]] where `name` is kind=CDK.

    subroutine wrap_json_value_add_real(json,p,name,val)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer             :: p
    character(kind=CDK,len=*),intent(in) :: name  !! variable name
    real(RK),intent(in)                  :: val   !! real value

    call json%add(p, to_unicode(name), val)

    end subroutine wrap_json_value_add_real
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/20/2014
!
!  Add a real vector child to the [[json_value]] variable.
!
!@note This routine is part of the public API that can be
!      used to build a JSON structure using [[json_value]] pointers.

    subroutine json_value_add_real_vec(json, p, name, val)

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
        call json%add(var, CK_'', val(i))
    end do

    !add it:
    call json%add(p, var)

    end subroutine json_value_add_real_vec
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_value_add_real_vec]] where `name` is kind=CDK.

    subroutine wrap_json_value_add_real_vec(json, p, name, val)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer             :: p
    character(kind=CDK,len=*),intent(in) :: name
    real(RK),dimension(:),intent(in)     :: val

    call json%add(p, to_unicode(name), val)

    end subroutine wrap_json_value_add_real_vec
!*****************************************************************************************

#ifndef REAL32
!*****************************************************************************************
!>
!  Alternate version of [[json_value_add_real]] where `val` is `real32`.

    subroutine json_value_add_real32(json,p,name,val)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer            :: p
    character(kind=CK,len=*),intent(in) :: name  !! variable name
    real(real32),intent(in)             :: val   !! real value

    call json%add(p,name,real(val,RK))

    end subroutine json_value_add_real32
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_value_add_real32]] where `name` is kind=CDK.

    subroutine wrap_json_value_add_real32(json,p,name,val)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer             :: p
    character(kind=CDK,len=*),intent(in) :: name  !! variable name
    real(real32),intent(in)              :: val   !! real value

    call json%add(p, to_unicode(name), val)

    end subroutine wrap_json_value_add_real32
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_value_add_real_vec]] where `val` is `real32`.

    subroutine json_value_add_real32_vec(json, p, name, val)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer             :: p
    character(kind=CK,len=*),intent(in)  :: name
    real(real32),dimension(:),intent(in) :: val

    call json%add(p,name,real(val,RK))

    end subroutine json_value_add_real32_vec
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_value_add_real32_vec]] where `name` is kind=CDK.

    subroutine wrap_json_value_add_real32_vec(json, p, name, val)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer             :: p
    character(kind=CDK,len=*),intent(in) :: name
    real(real32),dimension(:),intent(in) :: val

    call json%add(p, to_unicode(name), val)

    end subroutine wrap_json_value_add_real32_vec
!*****************************************************************************************
#endif

#ifdef REAL128
!*****************************************************************************************
!>
!  Alternate version of [[json_value_add_real]] where `val` is `real64`.

    subroutine json_value_add_real64(json,p,name,val)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer            :: p
    character(kind=CK,len=*),intent(in) :: name  !! variable name
    real(real64),intent(in)             :: val   !! real value

    call json%add(p,name,real(val,RK))

    end subroutine json_value_add_real64
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_value_add_real64]] where `name` is kind=CDK.

    subroutine wrap_json_value_add_real64(json,p,name,val)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer             :: p
    character(kind=CDK,len=*),intent(in) :: name  !! variable name
    real(real64),intent(in)              :: val   !! real value

    call json%add(p, to_unicode(name), val)

    end subroutine wrap_json_value_add_real64
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_value_add_real_vec]] where `val` is `real64`.

    subroutine json_value_add_real64_vec(json, p, name, val)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer             :: p
    character(kind=CK,len=*),intent(in)  :: name
    real(real64),dimension(:),intent(in) :: val

    call json%add(p, name, real(val,RK))

    end subroutine json_value_add_real64_vec
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_value_add_real64_vec]] where `name` is kind=CDK.

    subroutine wrap_json_value_add_real64_vec(json, p, name, val)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer             :: p
    character(kind=CDK,len=*),intent(in) :: name
    real(real64),dimension(:),intent(in) :: val

    call json%add(p, to_unicode(name), val)

    end subroutine wrap_json_value_add_real64_vec
!*****************************************************************************************
#endif

!*****************************************************************************************
!>
!  Add a NULL value child to the [[json_value]] variable.
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
!  Add an integer value child to the [[json_value]] variable.
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
!  Add a integer vector child to the [[json_value]] variable.
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
        call json%add(var, CK_'', val(i))
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
!  Add a logical value child to the [[json_value]] variable.
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
!  Add a logical vector child to the [[json_value]] variable.
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
        call json%add(var, CK_'', val(i))
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

    subroutine json_value_add_string(json, p, name, val, trim_str, adjustl_str)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer            :: p
    character(kind=CK,len=*),intent(in) :: name        !! name of the variable
    character(kind=CK,len=*),intent(in) :: val         !! value
    logical(LK),intent(in),optional     :: trim_str    !! if TRIM() should be called for the `val`
    logical(LK),intent(in),optional     :: adjustl_str !! if ADJUSTL() should be called for the `val`

    type(json_value),pointer :: var

    !create the variable:
    call json%create_string(var,val,name,trim_str,adjustl_str)

    !add it:
    call json%add(p, var)

    end subroutine json_value_add_string
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_value_add_string]] where `name` and `val` are kind=CDK.

    subroutine wrap_json_value_add_string(json, p, name, val, trim_str, adjustl_str)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer             :: p
    character(kind=CDK,len=*),intent(in) :: name        !! name of the variable
    character(kind=CDK,len=*),intent(in) :: val         !! value
    logical(LK),intent(in),optional      :: trim_str    !! if TRIM() should be called for the `val`
    logical(LK),intent(in),optional      :: adjustl_str !! if ADJUSTL() should be called for the `val`

    call json%add(p, to_unicode(name), to_unicode(val), trim_str, adjustl_str)

    end subroutine wrap_json_value_add_string
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_value_add_string]] where `name` is kind=CDK.

    subroutine json_value_add_string_name_ascii(json, p, name, val, trim_str, adjustl_str)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer             :: p
    character(kind=CDK,len=*),intent(in) :: name        !! name of the variable
    character(kind=CK, len=*),intent(in) :: val         !! value
    logical(LK),intent(in),optional      :: trim_str    !! if TRIM() should be called for the `val`
    logical(LK),intent(in),optional      :: adjustl_str !! if ADJUSTL() should be called for the `val`

    call json%add(p, to_unicode(name), val, trim_str, adjustl_str)

    end subroutine json_value_add_string_name_ascii
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_value_add_string]] where `val` is kind=CDK.

    subroutine json_value_add_string_val_ascii(json, p, name, val, trim_str, adjustl_str)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer             :: p
    character(kind=CK, len=*),intent(in) :: name        !! name of the variable
    character(kind=CDK,len=*),intent(in) :: val         !! value
    logical(LK),intent(in),optional      :: trim_str    !! if TRIM() should be called for the `val`
    logical(LK),intent(in),optional      :: adjustl_str !! if ADJUSTL() should be called for the `val`

    call json%add(p, name, to_unicode(val), trim_str, adjustl_str)

    end subroutine json_value_add_string_val_ascii
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/19/2014
!
!  Add a character string vector child to the [[json_value]] variable.
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
    integer(IK) :: i  !! counter

    !create the variable as an array:
    call json%create_array(var,name)

    !populate the array:
    do i=1,size(val)
        call json%add(var, CK_'', val(i), trim_str, adjustl_str)
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
!  Count the number of children in the object or array.
!
!### History
!  * JW : 1/4/2014 : Original routine removed.
!    Now using `n_children` variable.
!    Renamed from `json_value_count`.

    function json_count(json,p) result(count)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer,intent(in) :: p      !! this should normally be a `json_object`
                                                  !! or a `json_array`. For any other
                                                  !! variable type this will return 0.
    integer(IK)                         :: count  !! number of children in `p`.

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
!  If there is no parent, then a `null()` pointer is returned.

    subroutine json_get_parent(json,p,parent)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer,intent(in)  :: p        !! JSON object
    type(json_value),pointer,intent(out) :: parent   !! pointer to `parent`

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
!  If there is no next, then a `null()` pointer is returned.

    subroutine json_get_next(json,p,next)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer,intent(in)  :: p       !! JSON object
    type(json_value),pointer,intent(out) :: next    !! pointer to `next`

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
!  If there is no previous, then a `null()` pointer is returned.

    subroutine json_get_previous(json,p,previous)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer,intent(in)  :: p        !! JSON object
    type(json_value),pointer,intent(out) :: previous !! pointer to `previous`

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
!  If there is no tail, then a `null()` pointer is returned.

    subroutine json_get_tail(json,p,tail)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer,intent(in)  :: p        !! JSON object
    type(json_value),pointer,intent(out) :: tail     !! pointer to `tail`

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

    subroutine json_value_get_child_by_index(json, p, idx, child, found)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer,intent(in) :: p      !! object or array JSON data
    integer(IK),intent(in)              :: idx    !! index of the child
                                                  !! (this is a 1-based Fortran
                                                  !! style array index).
    type(json_value),pointer            :: child  !! pointer to the child
    logical(LK),intent(out),optional    :: found  !! true if the value was found
                                                  !! (if not present, an exception
                                                  !! will be thrown if it was not
                                                  !! found.  If present and not
                                                  !! found, no exception will be
                                                  !! thrown).

    integer(IK) :: i  !! counter

    nullify(child)

    if (.not. json%exception_thrown) then

        if (associated(p%children)) then

            ! If getting first or last child, we can do this quickly.
            ! Otherwise, traverse the list.
            if (idx==1) then

                child => p%children  ! first one

            elseif (idx==p%n_children) then

                if (associated(p%tail)) then
                    child => p%tail  ! last one
                else
                    call json%throw_exception('Error in json_value_get_child_by_index:'//&
                                              ' child%tail is not associated.',found)
                end if

            elseif (idx<1 .or. idx>p%n_children) then

                call json%throw_exception('Error in json_value_get_child_by_index:'//&
                                          ' idx is out of range.',found)

            else

                ! if idx is closer to the end, we traverse the list backward from tail,
                ! otherwise we traverse it forward from children:

                if (p%n_children-idx < idx) then  ! traverse backward

                    child => p%tail

                    do i = 1, p%n_children - idx

                        if (associated(child%previous)) then
                            child => child%previous
                        else
                            call json%throw_exception('Error in json_value_get_child_by_index:'//&
                                                      ' child%previous is not associated.',found)
                            nullify(child)
                            exit
                        end if

                    end do

                else  ! traverse forward

                    child => p%children

                    do i = 1, idx - 1

                        if (associated(child%next)) then
                            child => child%next
                        else
                            call json%throw_exception('Error in json_value_get_child_by_index:'//&
                                                      ' child%next is not associated.',found)
                            nullify(child)
                            exit
                        end if

                    end do

                end if

            end if

        else

            call json%throw_exception('Error in json_value_get_child_by_index:'//&
                                      ' p%children is not associated.',found)

        end if

        ! found output:
        if (json%exception_thrown) then
            if (present(found)) then
                call json%clear_exceptions()
                found = .false.
            end if
        else
            if (present(found)) found = .true.
        end if

    else
        if (present(found)) found = .false.
    end if

    end subroutine json_value_get_child_by_index
!*****************************************************************************************

!*****************************************************************************************
!>
!  Returns pointer to the first child of the object
!  (or `null()` if it is not associated).

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
!  whitespace or not, depending on the settings in the [[json_core(type)]] class.
!
!@note The `name` input is not a path, and is not parsed like it is in [[json_get_by_path]].

    subroutine json_value_get_child_by_name(json, p, name, child, found)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer,intent(in) :: p
    character(kind=CK,len=*),intent(in) :: name   !! the name of a child of `p`
    type(json_value),pointer            :: child  !! pointer to the child
    logical(LK),intent(out),optional    :: found  !! true if the value was found
                                                  !! (if not present, an exception
                                                  !! will be thrown if it was not
                                                  !! found.  If present and not
                                                  !! found, no exception will be
                                                  !! thrown).

    integer(IK) :: i,n_children
    logical :: error

    nullify(child)

    if (.not. json%exception_thrown) then

        if (associated(p)) then

            error = .true.   ! will be false if it is found
            if (p%var_type==json_object) then
                n_children = json%count(p)
                child => p%children    !start with first one
                do i=1, n_children
                    if (.not. associated(child)) then
                        call json%throw_exception(&
                            'Error in json_value_get_child_by_name: '//&
                            'Malformed JSON linked list',found)
                        exit
                    end if
                    if (allocated(child%name)) then
                        !name string matching routine:
                        if (json%name_equal(child,name)) then
                            error = .false.
                            exit
                        end if
                    end if
                    child => child%next
                end do
            end if

            if (error) then
                !did not find anything:
                call json%throw_exception(&
                    'Error in json_value_get_child_by_name: '//&
                    'child variable '//trim(name)//' was not found.',found)
                nullify(child)
            end if

        else
            call json%throw_exception(&
                'Error in json_value_get_child_by_name: '//&
                'pointer is not associated.',found)
        end if

        ! found output:
        if (json%exception_thrown) then
            if (present(found)) then
                call json%clear_exceptions()
                found = .false.
            end if
        else
            if (present(found)) found = .true.
        end if

    else
        if (present(found)) found = .false.
    end if

    end subroutine json_value_get_child_by_name
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 8/25/2017
!
!  Checks a JSON object for duplicate child names.
!
!  It uses the specified settings for name matching (see [[name_strings_equal]]).
!
!@note This will only check for one duplicate,
!      it will return the first one that it finds.

    subroutine json_check_children_for_duplicate_keys(json,p,has_duplicate,name,path)

    implicit none

    class(json_core),intent(inout) :: json
    type(json_value),pointer,intent(in) :: p  !! the object to search. If `p` is
                                              !! not a `json_object`, then `has_duplicate`
                                              !! will be false.
    logical(LK),intent(out) :: has_duplicate  !! true if there is at least
                                              !! two children have duplicate
                                              !! `name` values.
    character(kind=CK,len=:),allocatable,intent(out),optional :: name !! the duplicate name
                                                                      !! (unallocated if no
                                                                      !! duplicate was found)
    character(kind=CK,len=:),allocatable,intent(out),optional :: path !! the full path to the
                                                                      !! duplicate name
                                                                      !! (unallocated if no
                                                                      !! duplicate was found)

    integer(IK)              :: i           !! counter
    integer(IK)              :: j           !! counter
    type(json_value),pointer :: child       !! pointer to a child of `p`
    integer(IK)              :: n_children  !! number of children of `p`
    logical(LK)              :: found       !! flag for `get_child`

    type :: alloc_str
        !! so we can have an array of allocatable strings
        character(kind=CK,len=:),allocatable :: str  !! name string
    end type alloc_str
    type(alloc_str),dimension(:),allocatable :: names !! array of all the
                                                      !! child name strings

    ! initialize:
    has_duplicate =.false.

    if (.not. json%exception_thrown) then

        if (associated(p)) then

            if (p%var_type==json_object) then

                ! number of items to check:
                n_children = json%count(p)
                allocate(names(n_children))

                ! first get a list of all the name keys:
                do i=1, n_children
                    call json%get_child(p,i,child,found) ! get by index
                    if (.not. found) then
                        call json%throw_exception(&
                            'Error in json_check_children_for_duplicate_keys: '//&
                            'Malformed JSON linked list')
                        exit
                    end if
                    if (allocated(child%name)) then
                        names(i)%str = child%name
                    else
                        call json%throw_exception(&
                            'Error in json_check_children_for_duplicate_keys: '//&
                            'Object child name is not allocated')
                        exit
                    end if
                end do

                if (.not. json%exception_thrown) then
                    ! now check the list for duplicates:
                    main: do i=1,n_children
                        do j=1,i-1
                            if (json%name_strings_equal(names(i)%str,names(j)%str)) then
                                has_duplicate = .true.
                                if (present(name)) then
                                    name = names(i)%str
                                end if
                                if (present(path)) then
                                    call json%get_child(p,names(i)%str,child,found) ! get by name
                                    if (found) then
                                        call json%get_path(child,path,found)
                                        if (.not. found) then
                                            ! should never happen since we know it is there
                                            call json%throw_exception(&
                                                    'Error in json_check_children_for_duplicate_keys: '//&
                                                    'Could not get path')
                                        end if
                                    else
                                        ! should never happen since we know it is there
                                        call json%throw_exception(&
                                            'Error in json_check_children_for_duplicate_keys: '//&
                                            'Could not get child: '//trim(names(i)%str))
                                    end if
                                end if
                                exit main
                            end if
                        end do
                    end do main
                end if

                ! cleanup
                do i=1,n_children
                    if (allocated(names(i)%str)) deallocate(names(i)%str)
                end do
                if (allocated(names)) deallocate(names)

            end if

        end if

    end if

    end subroutine json_check_children_for_duplicate_keys
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 8/25/2017
!
!  Checks a JSON structure for duplicate child names.
!  This one recursively traverses the entire structure
!  (calling [[json_check_children_for_duplicate_keys]]
!  recursively for each element).
!
!@note This will only check for one duplicate,
!      it will return the first one that it finds.

    subroutine json_check_all_for_duplicate_keys(json,p,has_duplicate,name,path)

    implicit none

    class(json_core),intent(inout) :: json
    type(json_value),pointer,intent(in) :: p  !! the object to search. If `p` is
                                              !! not a `json_object`, then `has_duplicate`
                                              !! will be false.
    logical(LK),intent(out) :: has_duplicate  !! true if there is at least
                                              !! one duplicate `name` key anywhere
                                              !! in the structure.
    character(kind=CK,len=:),allocatable,intent(out),optional :: name !! the duplicate name
                                                                      !! (unallocated if no
                                                                      !! duplicates were found)
    character(kind=CK,len=:),allocatable,intent(out),optional :: path !! the full path to the
                                                                      !! duplicate name
                                                                      !! (unallocated if no
                                                                      !! duplicate was found)

    has_duplicate = .false.
    if (.not. json%exception_thrown) then
        call json%traverse(p,duplicate_key_func)
    end if

    contains

        subroutine duplicate_key_func(json,p,finished)

        !! Callback function to check each element
        !! for duplicate child names.

        implicit none

        class(json_core),intent(inout)      :: json
        type(json_value),pointer,intent(in) :: p
        logical(LK),intent(out)             :: finished

#if defined __GFORTRAN__

        ! this is a workaround for a gfortran bug (6 and 7),

        character(kind=CK,len=:),allocatable :: tmp_name !! temp variable for `name` string
        character(kind=CK,len=:),allocatable :: tmp_path !! temp variable for `path` string

        if (present(name) .and. present(path)) then
            call json%check_children_for_duplicate_keys(p,has_duplicate,name=tmp_name,path=tmp_path)
        else if (present(name) .and. .not. present(path)) then
            call json%check_children_for_duplicate_keys(p,has_duplicate,name=tmp_name)
        else if (.not. present(name) .and. present(path)) then
            call json%check_children_for_duplicate_keys(p,has_duplicate,path=tmp_path)
        else
            call json%check_children_for_duplicate_keys(p,has_duplicate)
        end if

        if (has_duplicate) then
            if (present(name)) name = tmp_name
            if (present(path)) path = tmp_path
        end if

#else
        call json%check_children_for_duplicate_keys(p,has_duplicate,name,path)
#endif

        finished = has_duplicate .or. json%exception_thrown

        end subroutine duplicate_key_func

    end subroutine json_check_all_for_duplicate_keys
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_value_get_child_by_name]] where `name` is kind=CDK.

    subroutine wrap_json_value_get_child_by_name(json, p, name, child, found)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer,intent(in)  :: p
    character(kind=CDK,len=*),intent(in) :: name
    type(json_value),pointer             :: child
    logical(LK),intent(out),optional     :: found

    call json%get(p,to_unicode(name),child,found)

    end subroutine wrap_json_value_get_child_by_name
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

    integer(IK) :: iloc  !! used to keep track of size of str
                         !! since it is being allocated in chunks.

    str = repeat(space, print_str_chunk_size)
    iloc = 0_IK
    call json%json_value_print(p, iunit=unit2str, str=str, iloc=iloc, indent=1_IK, colon=.true.)

    ! trim the string if necessary:
    if (len(str)>iloc) str = str(1:iloc)

    end subroutine json_value_to_string
!*****************************************************************************************

!*****************************************************************************************
!>
!  Print the [[json_value]] structure to the console (`output_unit`).
!
!### Note
!  * Just a wrapper for [[json_print_to_unit]].

    subroutine json_print_to_console(json,p)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer,intent(in) :: p

    call json%print(p,int(output_unit,IK))

    end subroutine json_print_to_console
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 6/20/2014
!
!  Print the [[json_value]] structure to a file.

    subroutine json_print_to_unit(json,p,iunit)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer,intent(in)  :: p
    integer(IK),intent(in)               :: iunit   !! the file unit (the file must
                                                    !! already have been opened, can't be -1).

    character(kind=CK,len=:),allocatable :: dummy  !! dummy for `str` argument
                                                   !! to [[json_value_print]]
    integer(IK)                          :: idummy !! dummy for `iloc` argument
                                                   !! to [[json_value_print]]

    if (iunit/=unit2str) then
        idummy = 0_IK
        call json%json_value_print(p,iunit,str=dummy,iloc=idummy,indent=1_IK,colon=.true.)
    else
        call json%throw_exception('Error in json_print_to_unit: iunit must not be -1.')
    end if

    end subroutine json_print_to_unit
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 12/23/2014
!
!  Print the [[json_value]] structure to a file.

    subroutine json_print_to_filename(json,p,filename)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer,intent(in)  :: p
    character(kind=CDK,len=*),intent(in) :: filename  !! the filename to print to
                                                      !! (should not already be open)

    integer(IK) :: iunit  !! file unit for `open` statement
    integer(IK) :: istat  !! `iostat` code for `open` statement

    open(newunit=iunit,file=filename,status='REPLACE',iostat=istat FILE_ENCODING )
    if (istat==0) then
        call json%print(p,iunit)
        close(iunit,iostat=istat)
    else
        call json%throw_exception('Error in json_print_to_filename: could not open file: '//&
                              trim(filename))
    end if

    end subroutine json_print_to_filename
!*****************************************************************************************

!*****************************************************************************************
!>
!  Print the JSON structure to a string or a file.
!
!### Notes
!  * This is an internal routine called by the various wrapper routines.
!  * The reason the `str` argument is non-optional is because of a
!    bug in v4.9 of the gfortran compiler.

    recursive subroutine json_value_print(json,p,iunit,str,indent,&
                                          need_comma,colon,is_array_element,&
                                          is_compressed_vector,iloc)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer,intent(in)  :: p
    integer(IK),intent(in)               :: iunit             !! file unit to write to (the
                                                              !! file is assumed to be open)
    integer(IK),intent(in),optional      :: indent            !! indention level
    logical(LK),intent(in),optional      :: is_array_element  !! if this is an array element
    logical(LK),intent(in),optional      :: need_comma        !! if it needs a comma after it
    logical(LK),intent(in),optional      :: colon             !! if the colon was just written
    character(kind=CK,len=:),intent(inout),allocatable :: str
                                                      !! if `iunit==unit2str` (-1) then
                                                      !! the structure is printed to this
                                                      !! string rather than a file. This mode
                                                      !! is used by [[json_value_to_string]].
    integer(IK),intent(inout) :: iloc  !! current index in `str`. should be set to 0 initially.
                                       !! [only used when `str` is used.]
    logical(LK),intent(in),optional :: is_compressed_vector  !! if True, this is an element
                                                             !! from an array being printed
                                                             !! on one line [default is False]

    character(kind=CK,len=max_numeric_str_len) :: tmp !! for value to string conversions
    character(kind=CK,len=:),allocatable :: s_indent !! the string of spaces for
                                                     !! indenting (see `tab` and `spaces`)
    character(kind=CK,len=:),allocatable :: s !! the string appended to `str`
    type(json_value),pointer :: element !! for getting children
    integer(IK) :: tab           !! number of `tabs` for indenting
    integer(IK) :: spaces        !! number of spaces for indenting
    integer(IK) :: i             !! counter
    integer(IK) :: count         !! number of children
    logical(LK) :: print_comma   !! if the comma will be printed after the value
    logical(LK) :: write_file    !! if we are writing to a file
    logical(LK) :: write_string  !! if we are writing to a string
    logical(LK) :: is_array      !! if this is an element in an array
    logical(LK) :: is_vector     !! if all elements of a vector
                                 !! are scalars of the same type
    character(kind=CK,len=:),allocatable :: str_escaped !! escaped version of
                                                        !! `name` or `str_value`

    if (.not. json%exception_thrown) then

        if (.not. associated(p)) then
            ! note: a null() pointer will trigger this error.
            ! However, if the pointer is undefined, then this will
            ! crash (if this wasn't here it would crash below when
            ! we try to access the contents)
            call json%throw_exception('Error in json_value_print: '//&
                                      'the pointer is not associated')
            return
        end if

        if (present(is_compressed_vector)) then
            is_vector = is_compressed_vector
        else
            is_vector = .false.
        end if

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
            s_indent = CK_''
        else
            s_indent = repeat(space, spaces)
        end if

        select case (p%var_type)

        case (json_object)

            count = json%count(p)

            if (count==0) then    !special case for empty object

                s = s_indent//start_object//end_object
                call write_it( comma=print_comma )

            else

                s = s_indent//start_object
                call write_it()

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
                        call escape_string(element%name,str_escaped,json%escape_solidus)
                        if (json%no_whitespace) then
                            !compact printing - no extra space
                            s = repeat(space, spaces)//quotation_mark//&
                                          str_escaped//quotation_mark//colon_char
                            call write_it(advance=.false.)
                        else
                            s = repeat(space, spaces)//quotation_mark//&
                                          str_escaped//quotation_mark//colon_char//space
                            call write_it(advance=.false.)
                        end if
                    else
                        call json%throw_exception('Error in json_value_print:'//&
                                                  ' element%name not allocated')
                        nullify(element)
                        return
                    end if

                    ! recursive print of the element
                    call json%json_value_print(element, iunit=iunit, indent=tab + 1_IK, &
                                    need_comma=i<count, colon=.true., str=str, iloc=iloc)
                    if (json%exception_thrown) return

                    ! get the next child the list:
                    element => element%next

                end do

                ! [one fewer tab if it isn't an array element]
                if (.not. is_array) then
                    s = repeat(space, max(0_IK,spaces-json%spaces_per_tab))//end_object
                else
                    s = s_indent//end_object
                end if
                call write_it( comma=print_comma )
                nullify(element)

            end if

        case (json_array)

            count = json%count(p)

            if (count==0) then    ! special case for empty array

                s = s_indent//start_array//end_array
                call write_it( comma=print_comma )

            else

                ! if every child is the same type & a scalar:
                is_vector = json%is_vector(p)
                if (json%failed()) return

                s = s_indent//start_array
                call write_it( advance=(.not. is_vector) )

                !if an array is in an array, there is an extra tab:
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

                    ! recursive print of the element
                    if (is_vector) then
                        call json%json_value_print(element, iunit=iunit, indent=0_IK,&
                                        need_comma=i<count, is_array_element=.false., &
                                        str=str, iloc=iloc,&
                                        is_compressed_vector = .true.)
                    else
                        call json%json_value_print(element, iunit=iunit, indent=tab,&
                                        need_comma=i<count, is_array_element=.true., &
                                        str=str, iloc=iloc)
                    end if
                    if (json%exception_thrown) return

                    ! get the next child the list:
                    element => element%next

                end do

                !indent the closing array character:
                if (is_vector) then
                    s = end_array
                    call write_it( comma=print_comma )
                else
                    s = repeat(space, max(0_IK,spaces-json%spaces_per_tab))//end_array
                    call write_it( comma=print_comma )
                end if
                nullify(element)

            end if

        case (json_null)

            s = s_indent//null_str
            call write_it( comma=print_comma, &
                           advance=(.not. is_vector),&
                           space_after_comma=is_vector )

        case (json_string)

            if (allocated(p%str_value)) then
                ! have to escape the string for printing:
                call escape_string(p%str_value,str_escaped,json%escape_solidus)
                s = s_indent//quotation_mark//str_escaped//quotation_mark
                call write_it( comma=print_comma, &
                               advance=(.not. is_vector),&
                               space_after_comma=is_vector )
            else
                call json%throw_exception('Error in json_value_print:'//&
                                          ' p%value_string not allocated')
                return
            end if

        case (json_logical)

            if (p%log_value) then
                s = s_indent//true_str
                call write_it( comma=print_comma, &
                               advance=(.not. is_vector),&
                               space_after_comma=is_vector )
            else
                s = s_indent//false_str
                call write_it( comma=print_comma, &
                               advance=(.not. is_vector),&
                               space_after_comma=is_vector )
            end if

        case (json_integer)

            call integer_to_string(p%int_value,int_fmt,tmp)

            s = s_indent//trim(tmp)
            call write_it( comma=print_comma, &
                           advance=(.not. is_vector),&
                           space_after_comma=is_vector )

        case (json_real)

            if (allocated(json%real_fmt)) then
                call real_to_string(p%dbl_value,json%real_fmt,json%compact_real,json%non_normals_to_null,tmp)
            else
                !use the default format (user has not called initialize() or specified one):
                call real_to_string(p%dbl_value,default_real_fmt,json%compact_real,json%non_normals_to_null,tmp)
            end if

            s = s_indent//trim(tmp)
            call write_it( comma=print_comma, &
                           advance=(.not. is_vector),&
                           space_after_comma=is_vector )

        case default

            call integer_to_string(p%var_type,int_fmt,tmp)
            call json%throw_exception('Error in json_value_print: '//&
                                      'unknown data type: '//trim(tmp))

        end select

    end if

    contains

        subroutine write_it(advance,comma,space_after_comma)

        !! write the string `s` to the file (or the output string)

        implicit none

        logical(LK),intent(in),optional :: advance           !! to add line break or not
        logical(LK),intent(in),optional :: comma             !! print comma after the string
        logical(LK),intent(in),optional :: space_after_comma !! print a space after the comma

        logical(LK) :: add_comma       !! if a delimiter is to be added after string
        logical(LK) :: add_line_break  !! if a line break is to be added after string
        logical(LK) :: add_space       !! if a space is to be added after the comma
        integer(IK) :: n               !! length of actual string `s` appended to `str`
        integer(IK) :: room_left       !! number of characters left in `str`
        integer(IK) :: n_chunks_to_add !! number of chunks to add to `str` for appending `s`

        if (present(comma)) then
            add_comma = comma
        else
            add_comma = .false. !default is not to add comma
        end if
        if (json%no_whitespace) then
            add_space = .false.
        else
            if (present(space_after_comma)) then
                add_space = space_after_comma
            else
                add_space = .false. !default is not to add space
            end if
        end if
        if (present(advance)) then
            if (json%no_whitespace) then
                ! overrides input value:
                add_line_break = .false.
            else
                add_line_break = advance
            end if
        else
            add_line_break = .not. json%no_whitespace ! default is to advance if
                                                      ! we are printing whitespace
        end if

        ! string to print:
        if (add_comma) then
            if (add_space) then
                s = s // delimiter // space
            else
                s = s // delimiter
            end if
        end if

        if (write_file) then

            if (add_line_break) then
                write(iunit,fmt='(A)') s
            else
                write(iunit,fmt='(A)',advance='NO') s
            end if

        else    !write string

            if (add_line_break) s = s // newline

            n = len(s)
            room_left = len(str)-iloc
            if (room_left < n) then
                ! need to add another chunk to fit this string:
                n_chunks_to_add = max(1_IK, ceiling( real(len(s)-room_left,RK) / real(chunk_size,RK), IK ) )
                str = str // repeat(space, print_str_chunk_size*n_chunks_to_add)
            end if
            ! append s to str:
            str(iloc+1:iloc+n) = s
            iloc = iloc + n

        end if

        end subroutine write_it

    end subroutine json_value_print
!*****************************************************************************************

!*****************************************************************************************
!>
!  Returns true if all the children are the same type (and a scalar).
!  Note that integers and reals are considered the same type for this purpose.
!  This routine is used for the `compress_vectors` option.

    function json_is_vector(json, p) result(is_vector)

    implicit none

    class(json_core),intent(inout) :: json
    type(json_value),pointer       :: p
    logical(LK)                    :: is_vector  !! if all elements of a vector
                                                 !! are scalars of the same type

    integer(IK) :: var_type_prev !! for getting the variable type of children
    integer(IK) :: var_type !! for getting the variable type of children
    type(json_value),pointer :: element !! for getting children
    integer(IK) :: i !! counter
    integer(IK) :: count !! number of children

    integer(IK),parameter :: json_invalid = -1_IK  !! to initialize the flag. an invalid value
    integer(IK),parameter :: json_numeric = -2_IK  !! indicates `json_integer` or `json_real`

    if (json%compress_vectors) then
        ! check to see if every child is the same type,
        ! and a scalar:
        is_vector = .true.
        var_type_prev = json_invalid
        count = json%count(p)
        element => p%children
        do i = 1_IK, count
            if (.not. associated(element)) then
                call json%throw_exception('Error in json_is_vector: '//&
                                          'Malformed JSON linked list')
                return
            end if
            ! check variable type of all the children.
            ! They must all be the same, and a scalar.
            call json%info(element,var_type=var_type)
            ! special check for numeric values:
            if (var_type==json_integer .or. var_type==json_real) var_type = json_numeric
            if (var_type==json_object .or. &
                var_type==json_array .or. &
                (i>1_IK .and. var_type/=var_type_prev)) then
                is_vector = .false.
                exit
            end if
            var_type_prev = var_type
            ! get the next child the list:
            element => element%next
        end do
    else
        is_vector = .false.
    end if

    end function json_is_vector
!*****************************************************************************************

!*****************************************************************************************
!>
!  Returns true if the `path` is present in the `p` JSON structure.
!
!@note Just a wrapper for [[json_get_by_path]], so it uses the
!      specified `path_mode` and other settings.

    function json_valid_path(json, p, path) result(found)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer,intent(in)  :: p      !! a JSON linked list
    character(kind=CK,len=*),intent(in)  :: path   !! path to the variable
    logical(LK)                          :: found  !! true if it was found

    type(json_value),pointer :: tmp  !! pointer to the variable specified by `path`

    call json%get(p, path, tmp, found)

    end function json_valid_path
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_valid_path]] where "path" is kind=CDK.

    function wrap_json_valid_path(json, p, path) result(found)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer,intent(in)  :: p      !! a JSON linked list
    character(kind=CDK,len=*),intent(in) :: path   !! path to the variable
    logical(LK)                          :: found  !! true if it was found

    found = json%valid_path(p, to_unicode(path))

    end function wrap_json_valid_path
!*****************************************************************************************

!*****************************************************************************************
!>
!  Returns the [[json_value]] pointer given the path string.
!
!  It uses one of three methods:
!
!  * The original JSON-Fortran defaults
!  * [RFC 6901](https://tools.ietf.org/html/rfc6901)
!  * [JSONPath](http://goessner.net/articles/JsonPath/) "bracket-notation"

    subroutine json_get_by_path(json, me, path, p, found)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer,intent(in)  :: me     !! a JSON linked list
    character(kind=CK,len=*),intent(in)  :: path   !! path to the variable
    type(json_value),pointer,intent(out) :: p      !! pointer to the variable
                                                   !! specified by `path`
    logical(LK),intent(out),optional     :: found  !! true if it was found

    character(kind=CK,len=max_integer_str_len),allocatable :: path_mode_str !! string version
                                                                            !! of `json%path_mode`

    nullify(p)

    if (.not. json%exception_thrown) then

        select case (json%path_mode)
        case(1_IK)
            call json%json_get_by_path_default(me, path, p, found)
        case(2_IK)
            call json%json_get_by_path_rfc6901(me, path, p, found)
        case(3_IK)
            call json%json_get_by_path_jsonpath_bracket(me, path, p, found)
        case default
            call integer_to_string(json%path_mode,int_fmt,path_mode_str)
            call json%throw_exception('Error in json_get_by_path: Unsupported path_mode: '//&
                                        trim(path_mode_str))
            if (present(found)) found = .false.
        end select

        if (present(found)) then
            if (.not. found) call json%clear_exceptions()
        end if

    else
        if (present(found)) found = .false.
    end if

    end subroutine json_get_by_path
!*****************************************************************************************

!*****************************************************************************************
!>
!  Returns the [[json_value]] pointer given the path string,
!  If necessary, by creating the variables as needed.
!
!  By default, the leaf node and any empty array elements
!  are created as `json_null` values.
!
!  It only works for `path_mode=1` or `path_mode=3`.
!  An error will be thrown for `path_mode=2` (RFC 6901).
!
!### See also
!  * [[json_get_by_path]]

    subroutine json_create_by_path(json,me,path,p,found,was_created)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer,intent(in)  :: me           !! a JSON linked list
    character(kind=CK,len=*),intent(in)  :: path         !! path to the variable
    type(json_value),pointer,intent(out),optional :: p   !! pointer to the variable
                                                         !! specify by `path`
    logical(LK),intent(out),optional     :: found        !! true if there were no errors
                                                         !! (variable found or created)
    logical(LK),intent(out),optional     :: was_created  !! true if it was actually created
                                                         !! (as opposed to already being there)

    type(json_value),pointer :: tmp
    character(kind=CK,len=max_integer_str_len) :: path_mode_str !! string version
                                                                !! of `json%path_mode`

    if (present(p)) nullify(p)

    if (.not. json%exception_thrown) then

        select case (json%path_mode)
        case(1_IK)
            call json%json_get_by_path_default(me,path,tmp,found,&
                                                create_it=.true.,&
                                                was_created=was_created)
            if (present(p)) p => tmp
        case(3_IK)
           call json%json_get_by_path_jsonpath_bracket(me,path,tmp,found,&
                                                       create_it=.true.,&
                                                       was_created=was_created)
           if (present(p)) p => tmp

        case default

            if (json%path_mode==2_IK) then
                ! the problem here is there isn't really a way to disambiguate
                ! the array elements, so '/a/0' could be 'a(1)' or 'a.0'.
                call json%throw_exception('Error in json_create_by_path: '//&
                                          'Create by path not supported in RFC 6901 path mode.')
            else
                call integer_to_string(json%path_mode,int_fmt,path_mode_str)
                call json%throw_exception('Error in json_create_by_path: Unsupported path_mode: '//&
                                            trim(path_mode_str))
            end if
            if (present(found)) then
                call json%clear_exceptions()
                found = .false.
            end if
            if (present(was_created)) was_created = .false.
        end select

    else
        if (present(was_created)) was_created = .false.
        if (present(found)) found = .false.
    end if

    end subroutine json_create_by_path
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_create_by_path]] where "path" is kind=CDK.

    subroutine wrap_json_create_by_path(json,me,path,p,found,was_created)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer,intent(in)  :: me           !! a JSON linked list
    character(kind=CDK,len=*),intent(in) :: path         !! path to the variable
    type(json_value),pointer,intent(out),optional :: p   !! pointer to the variable
                                                         !! specify by `path`
    logical(LK),intent(out),optional     :: found        !! true if there were no errors
                                                         !! (variable found or created)
    logical(LK),intent(out),optional     :: was_created  !! true if it was actually created
                                                         !! (as opposed to already being there)

    call json%create(me,to_unicode(path),p,found,was_created)

    end subroutine wrap_json_create_by_path
!*****************************************************************************************

!*****************************************************************************************
!>
!  Rename a [[json_value]], given the path.
!
!@note this is a wrapper for [[json_value_rename]].

    subroutine json_rename_by_path(json, me, path, name, found)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer,intent(in)  :: me
    character(kind=CK,len=*),intent(in)  :: path  !! path to the variable to rename
    character(kind=CK,len=*),intent(in)  :: name  !! the new name
    logical(LK),intent(out),optional     :: found !! if there were no errors

    type(json_value),pointer :: p

    if ( json%exception_thrown ) then
        if ( present(found) ) found = .false.
        return
    end if

    nullify(p)
    call json%get(me=me, path=path, p=p)

    if (.not. associated(p)) then
        call json%throw_exception('Error in json_rename_by_path:'//&
                                  ' Unable to resolve path: '//trim(path),found)
    else
        call json%rename(p,name)
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

    end subroutine json_rename_by_path
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_rename_by_path]], where "path" and "name" are kind=CDK

    subroutine wrap_json_rename_by_path(json, me, path, name, found)

    implicit none

    class(json_core),intent(inout)        :: json
    type(json_value),pointer,intent(in)   :: me
    character(kind=CDK,len=*),intent(in)  :: path
    character(kind=CDK,len=*),intent(in)  :: name
    logical(LK),intent(out),optional      :: found

    call json%rename(me,to_unicode(path),to_unicode(name),found)

    end subroutine wrap_json_rename_by_path
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_rename_by_path]], where "name" is kind=CDK

    subroutine json_rename_by_path_name_ascii(json, me, path, name, found)

    implicit none

    class(json_core),intent(inout)        :: json
    type(json_value),pointer,intent(in)   :: me
    character(kind=CK,len=*),intent(in)   :: path
    character(kind=CDK,len=*),intent(in)  :: name
    logical(LK),intent(out),optional      :: found

    call json%rename(me,path,to_unicode(name),found)

    end subroutine json_rename_by_path_name_ascii
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_rename_by_path]], where "path" is kind=CDK

    subroutine json_rename_by_path_path_ascii(json, me, path, name, found)

    implicit none

    class(json_core),intent(inout)        :: json
    type(json_value),pointer,intent(in)   :: me
    character(kind=CDK,len=*),intent(in)  :: path
    character(kind=CK,len=*),intent(in)   :: name
    logical(LK),intent(out),optional      :: found

    call json%rename(me,to_unicode(path),name,found)

    end subroutine json_rename_by_path_path_ascii
!*****************************************************************************************

!*****************************************************************************************
!>
!  Returns the [[json_value]] pointer given the path string.
!
!### Example
!
!````fortran
!    type(json_core) :: json
!    type(json_value),pointer :: dat,p
!    logical :: found
!    !...
!    call json%initialize(path_mode=1) ! this is the default so not strictly necessary.
!    call json%get(dat,'data(2).version',p,found)
!````
!
!### Notes
!  The syntax used here is a subset of the
!  [http://goessner.net/articles/JsonPath/](JSONPath) "dot–notation".
!  The following special characters are used to denote paths:
!
!  * `$`           - root
!  * `@`           - this
!  * `.`           - child object member (note this can be changed using `json%path_separator`)
!  * `[]` or `()`  - child array element (note that indices are 1-based)
!
!  Thus, if any of these characters are present in the name key,
!  this routine cannot be used to get the value.
!  In that case, the `get_child` methods would need to be used.
!  Or, the alternate [[json_get_by_path_rfc6901]] could be used.
!
!### See also
!  * [[json_get_by_path_rfc6901]]
!  * [[json_get_by_path_jsonpath_bracket]]
!
!@note The syntax is inherited from FSON, and is basically a subset
!      of JSONPath "dot-notation", with the additional allowance of
!      () for array elements.
!
!@note JSON `null` values are used here for unknown variables when `create_it` is True.
!      So, it is possible that an existing null variable can be converted to another
!      type (object or array) if a child is specified in the path. Doing it this way
!      to avoid having to use another type (say `json_unknown`) that would have to be
!      converted to null once all the variables have been created (user would have
!      had to do this).
!
!@warning See (**) in code. I think we need to protect for memory leaks when
!         changing the type of a variable that already exists.

    subroutine json_get_by_path_default(json,me,path,p,found,create_it,was_created)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer,intent(in)  :: me          !! a JSON linked list
    character(kind=CK,len=*),intent(in)  :: path        !! path to the variable
    type(json_value),pointer,intent(out) :: p           !! pointer to the variable
                                                        !! specify by `path`
    logical(LK),intent(out),optional     :: found       !! true if it was found
    logical(LK),intent(in),optional      :: create_it   !! if a variable is not present
                                                        !! in the path, then it is created.
                                                        !! the leaf node is returned as
                                                        !! a `null` json type and can be
                                                        !! changed by the caller.
    logical(LK),intent(out),optional     :: was_created !! if `create_it` is true, this
                                                        !! will be true if the variable
                                                        !! was actually created. Otherwise
                                                        !! it will be false.

    integer(IK)              :: i           !! counter of characters in `path`
    integer(IK)              :: length      !! significant length of `path`
    integer(IK)              :: child_i     !! index for getting children
    character(kind=CK,len=1) :: c           !! a character in the `path`
    logical(LK)              :: array       !! flag when searching for array index in `path`
    type(json_value),pointer :: tmp         !! temp variables for getting child objects
    logical(LK)              :: child_found !! if the child value was found
    logical(LK)              :: create      !! if the object is to be created
    logical(LK)              :: created     !! if `create` is true, then this will be
                                            !! true if the leaf object had to be created
    integer(IK)              :: j           !! counter of children when creating object
    logical(LK)              :: status_ok   !! integer to string conversion flag

    nullify(p)

    if (.not. json%exception_thrown) then

        if (present(create_it)) then
            create = create_it
        else
            create = .false.
        end if

        ! default to assuming relative to me
        p => me

        child_i = 1
        array = .false.
        created = .false.

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
                if (create) created = .false. ! should always exist

            case (this)

                ! this
                p => me
                child_i = i + 1
                if (create) created = .false. ! should always exist

            case (start_array,start_array_alt)

                ! start looking for the array element index
                array = .true.

                ! get child member from p
                if (child_i < i) then
                    nullify(tmp)
                    if (create) then

                        ! Example:
                        !    'aaa.bbb(1)'
                        !     -> and aaa is a null, need to make it an object
                        !
                        !  What about the case: aaa.bbb(1)(3) ?
                        !  Is that already handled?

                        if (p%var_type==json_null) then             ! (**)
                            ! if p was also created, then we need to
                            ! convert it into an object here:
                            p%var_type = json_object
                        end if

                        ! don't want to throw exceptions in this case
                        call json%get_child(p, path(child_i:i-1), tmp, child_found)
                        if (.not. child_found) then
                            ! have to create this child
                            ! [make it an array]
                            call json_value_create(tmp)
                            call json%to_array(tmp,path(child_i:i-1))
                            call json%add(p,tmp)
                            created = .true.
                        else
                            created = .false.
                        end if
                    else
                        ! call the normal way
                        call json%get_child(p, path(child_i:i-1), tmp)
                    end if
                    p => tmp
                else
                    child_i = i + 1     ! say, '@('
                    cycle
                end if
                if (.not. associated(p)) then
                    call json%throw_exception('Error in json_get_by_path_default:'//&
                                              ' Error getting array element',found)
                    exit
                end if
                child_i = i + 1

            case (end_array,end_array_alt)

                if (.not. array) then
                    call json%throw_exception('Error in json_get_by_path_default:'//&
                                              ' Unexpected '//c,found)
                    exit
                end if
                array = .false.
                call string_to_integer(path(child_i:i-1),child_i,status_ok)
                if (.not. status_ok) then
                    call json%throw_exception('Error in json_get_by_path_default:'//&
                                              ' Could not convert array index to integer: '//&
                                              trim(path(child_i:i-1)),found)
                    exit
                end if

                nullify(tmp)
                if (create) then
                    ! don't want to throw exceptions in this case
                    call json%get_child(p, child_i, tmp, child_found)
                    if (.not. child_found) then

                        if (p%var_type==json_null) then            ! (**)
                            ! if p was also created, then we need to
                            ! convert it into an array here:
                            p%var_type = json_array
                        end if

                        ! have to create this element
                        ! [make it a null]
                        ! (and any missing ones before it)
                        do j = 1, child_i
                            nullify(tmp)
                            call json%get_child(p, j, tmp, child_found)
                            if (.not. child_found) then
                                call json_value_create(tmp)
                                call json%to_null(tmp)  ! array element doesn't need a name
                                call json%add(p,tmp)
                                if (j==child_i) created = .true.
                            else
                                if (j==child_i) created = .false.
                            end if
                        end do

                    else
                        created = .false.
                    end if

                else
                    ! call the normal way:
                    call json%get_child(p, child_i, tmp)
                end if

                p => tmp

                child_i = i + 1

            case default

                if (c==json%path_separator) then

                    ! get child member from p
                    if (child_i < i) then
                        nullify(tmp)
                        if (create) then
                            if (p%var_type==json_null) then            ! (**)
                                ! if p was also created, then we need to
                                ! convert it into an object here:
                                p%var_type = json_object
                            end if

                            ! don't want to throw exceptions in this case
                            call json%get_child(p, path(child_i:i-1), tmp, child_found)
                            if (.not. child_found) then
                                ! have to create this child
                                ! [make it an object]
                                call json_value_create(tmp)
                                call json%to_object(tmp,path(child_i:i-1))
                                call json%add(p,tmp)
                                created = .true.
                            else
                                created = .false.
                            end if
                        else
                            ! call the normal way
                            call json%get_child(p, path(child_i:i-1), tmp)
                        end if
                        p => tmp
                    else
                        child_i = i + 1     ! say '$.', '@.', or ').'
                        cycle
                    end if

                    if (.not. associated(p)) then
                        call json%throw_exception('Error in json_get_by_path_default:'//&
                                                  ' Error getting child member.',found)
                        exit
                    end if

                    child_i = i + 1

                end if

            end select

        end do

        if (json%exception_thrown) then

            if (present(found)) then
                nullify(p) ! just in case
                found = .false.
                call json%clear_exceptions()
            end if

        else

            ! grab the last child if present in the path
            if (child_i <= length) then
                nullify(tmp)
                if (create) then
                    if (p%var_type==json_null) then            ! (**)
                        ! if p was also created, then we need to
                        ! convert it into an object here:
                        p%var_type = json_object
                    end if

                    call json%get_child(p, path(child_i:i-1), tmp, child_found)
                    if (.not. child_found) then
                        ! have to create this child
                        ! (make it a null since it is the leaf)
                        call json_value_create(tmp)
                        call json%to_null(tmp,path(child_i:i-1))
                        call json%add(p,tmp)
                        created = .true.
                    else
                        created = .false.
                    end if
                else
                    ! call the normal way
                    call json%get_child(p, path(child_i:i-1), tmp)
                end if
                p => tmp
            else
                ! we already have p
                if (create .and. created) then
                    ! make leaf p a null, but only
                    ! if it wasn't there
                    call json%to_null(p)
                end if
            end if

            ! error checking
            if (associated(p)) then
                if (present(found)) found = .true.    !everything seems to be ok
            else
                call json%throw_exception('Error in json_get_by_path_default:'//&
                                          ' variable not found: '//trim(path),found)
                if (present(found)) then
                    found = .false.
                    call json%clear_exceptions()
                end if
            end if

        end if

        ! if it had to be created:
        if (present(was_created)) was_created = created

    else
        if (present(found)) found = .false.
        if (present(was_created)) was_created = .false.
    end if

    end subroutine json_get_by_path_default
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 2/4/2017
!
!  Returns the [[json_value]] pointer given the path string,
!  using the "JSON Pointer" path specification defined by RFC 6901.
!
!  Note that trailing whitespace significance and case sensitivity
!  are user-specified. To fully conform to the RFC 6901 standard,
!  should probably set (via `initialize`):
!
!  * `case_sensitive_keys = .true.`         [this is the default setting]
!  * `trailing_spaces_significant = .true.` [this is *not* the default setting]
!  * `allow_duplicate_keys = .false.`       [this is *not* the default setting]
!
!### Example
!
!````fortran
!    type(json_core) :: json
!    type(json_value),pointer :: dat,p
!    logical :: found
!    !...
!    call json%initialize(path_mode=2)
!    call json%get(dat,'/data/2/version',p,found)
!````
!
!### See also
!  * [[json_get_by_path_default]]
!  * [[json_get_by_path_jsonpath_bracket]]
!
!### Reference
!  * [JavaScript Object Notation (JSON) Pointer](https://tools.ietf.org/html/rfc6901)
!
!@note Not doing anything special about the `-` character to index an array.
!      This is considered a normal error.
!
!@note Unlike in the default path mode, the array indices here are 0-based
!      (in accordance with the RFC 6901 standard)
!
!@warning Not checking if the member that is referenced is unique.
!         (according to the standard, evaluation of non-unique references
!         should fail). Like [[json_get_by_path_default]], this one will just return
!         the first instance it encounters. This might be changed in the future.
!
!@warning I think the standard indicates that the input paths should use
!         escaped JSON strings (currently we are assuming they are not escaped).

    subroutine json_get_by_path_rfc6901(json, me, path, p, found)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer,intent(in)  :: me     !! a JSON linked list
    character(kind=CK,len=*),intent(in)  :: path   !! path to the variable
                                                   !! (an RFC 6901 "JSON Pointer")
    type(json_value),pointer,intent(out) :: p      !! pointer to the variable
                                                   !! specify by `path`
    logical(LK),intent(out),optional     :: found  !! true if it was found

    character(kind=CK,len=:),allocatable :: token  !! a token in the path (between the `/` characters)
    integer(IK)              :: i                  !! counter
    integer(IK)              :: islash_curr        !! location of current '/' character in the path
    integer(IK)              :: islash_next        !! location of next '/' character in the path
    integer(IK)              :: ilen               !! length of `path` string
    type(json_value),pointer :: tmp                !! temporary variable for traversing the structure
    integer(IK)              :: ival               !! integer array index value (0-based)
    logical(LK)              :: status_ok          !! error flag
    logical(LK)              :: child_found        !! for getting child values

    nullify(p)

    if (.not. json%exception_thrown) then

        p => me ! initialize

        if (path/=CK_'') then

            if (path(1:1)==slash) then  ! the first character must be a slash

                islash_curr = 1   ! initialize current slash index

                !keep trailing space or not:
                if (json%trailing_spaces_significant) then
                    ilen = len(path)
                else
                    ilen = len_trim(path)
                end if

                do

                    ! get the next token by finding the slashes
                    !
                    !  1   2 3
                    !  /abc/d/efg

                    if (islash_curr==ilen) then
                        !the last token is an empty string
                        token = CK_''
                        islash_next = 0  ! will signal to stop
                    else

                        !      .
                        ! '/123/567/'

                        ! index in remaining string:
                        islash_next = index(path(islash_curr+1:ilen),slash)
                        if (islash_next<=0) then
                            !last token:
                            token = path(islash_curr+1:ilen)
                        else
                            ! convert to actual index in path:
                            islash_next = islash_curr + index(path(islash_curr+1:ilen),slash)
                            if (islash_next>islash_curr+1) then
                                token = path(islash_curr+1:islash_next-1)
                            else
                                !empty token:
                                token = CK_''
                            end if
                        end if

                    end if

                    ! remove trailing spaces in the token here if necessary:
                    if (.not. json%trailing_spaces_significant) &
                        token = trim(token)

                    ! decode the token:
                    token = decode_rfc6901(token)

                    ! now, parse the token:

                    ! first see if there is a child with this name
                    call json%get_child(p,token,tmp,child_found)
                    if (child_found) then
                        ! it was found
                        p => tmp
                    else
                        ! No key with this name.
                        ! Is it an integer? If so,
                        ! it might be an array index.
                        status_ok = (len(token)>0)
                        if (status_ok) then
                            do i=1,len(token)
                                ! It must only contain (0..9) characters
                                ! (it must be unsigned)
                                if (scan(token(i:i),CK_'0123456789')<1) then
                                    status_ok = .false.
                                    exit
                                end if
                            end do
                            if (status_ok) then
                                if (len(token)>1 .and. token(1:1)==CK_'0') then
                                    ! leading zeros not allowed for some reason
                                    status_ok = .false.
                                end if
                            end if
                            if (status_ok) then
                                ! if we make it this far, it should be
                                ! convertible to an integer, so do it.
                                call string_to_integer(token,ival,status_ok)
                            end if
                        end if
                        if (status_ok) then
                            ! ival is an array index (0-based)
                            call json%get_child(p,ival+1_IK,tmp,child_found)
                            if (child_found) then
                                p => tmp
                            else
                                ! not found
                                status_ok = .false.
                            end if
                        end if
                        if (.not. status_ok) then
                            call json%throw_exception('Error in json_get_by_path_rfc6901: '//&
                                                      'invalid path specification: '//trim(path),found)
                            exit
                        end if
                    end if

                    if (islash_next<=0) exit ! finished

                    ! set up for next token:
                    islash_curr = islash_next

                end do

            else
                call json%throw_exception('Error in json_get_by_path_rfc6901: '//&
                                            'invalid path specification: '//trim(path),found)
            end if
        end if

        if (json%exception_thrown) then
            nullify(p)
            if (present(found)) then
                found = .false.
                call json%clear_exceptions()
            end if
        else
            if (present(found)) found = .true.
        end if

    else
        if (present(found)) found = .false.
    end if

    end subroutine json_get_by_path_rfc6901
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 9/2/2017
!
!  Returns the [[json_value]] pointer given the path string,
!  using the "JSON Pointer" path specification defined by the
!  JSONPath "bracket-notation".
!
!  The first character `$` is optional, and signifies the root
!  of the structure. If it is not present, then the first key
!  is taken to be in the `me` object.
!
!  Single or real quotes may be used.
!
!### Example
!
!````fortran
!    type(json_core) :: json
!    type(json_value),pointer :: dat,p
!    logical :: found
!    !...
!    call json%initialize(path_mode=3)
!    call json%get(dat,"$['store']['book'][1]['title']",p,found)
!````
!
!### See also
!  * [[json_get_by_path_default]]
!  * [[json_get_by_path_rfc6901]]
!
!### Reference
!  * [JSONPath](http://goessner.net/articles/JsonPath/)
!
!@note Uses 1-based array indices (same as [[json_get_by_path_default]],
!      but unlike [[json_get_by_path_rfc6901]] which uses 0-based indices).
!
!@note When `create_it=True`, if the variable already exists and is a type
!      that is not compatible with the usage in the `path`, then it is
!      destroyed and replaced with what is specified in the `path`. Note that
!      this applies the all variables in the path as it is created. Currently,
!      this behavior is different from [[json_get_by_path_default]].
!
!@note JSON `null` values are used here for unknown variables
!      when `create_it` is True.
!
!@warning Note that if using single quotes, this routine cannot parse
!         a key containing `']`. If using real quotes, this routine
!         cannot parse a key containing `"]`. If the key contains both
!         `']` and `"]`, there is no way to parse it using this routine.

    subroutine json_get_by_path_jsonpath_bracket(json,me,path,p,found,create_it,was_created)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer,intent(in)  :: me          !! a JSON linked list
    character(kind=CK,len=*),intent(in)  :: path        !! path to the variable
                                                        !! (using JSONPath
                                                        !! "bracket-notation")
    type(json_value),pointer,intent(out) :: p           !! pointer to the variable
                                                        !! specify by `path`
    logical(LK),intent(out),optional     :: found       !! true if it was found
    logical(LK),intent(in),optional      :: create_it   !! if a variable is not present
                                                        !! in the path, then it is created.
                                                        !! the leaf node is returned as
                                                        !! a `null` json type and can be
                                                        !! changed by the caller.
    logical(LK),intent(out),optional     :: was_created !! if `create_it` is true, this
                                                        !! will be true if the variable
                                                        !! was actually created. Otherwise
                                                        !! it will be false.

    character(kind=CK,len=:),allocatable :: token  !! a token in the path
                                                   !! (between the `['']` or
                                                   !! `[]` characters)
    integer(IK)              :: istart             !! location of current '['
                                                   !! character in the path
    integer(IK)              :: iend               !! location of current ']'
                                                   !! character in the path
    integer(IK)              :: ival               !! integer array index value
    logical(LK)              :: status_ok          !! error flag
    type(json_value),pointer :: tmp                !! temporary variable for
                                                   !! traversing the structure
    integer(IK)              :: i                  !! counter
    integer(IK)              :: ilen               !! length of `path` string
    logical(LK)              :: real_quotes      !! if the keys are enclosed in `"`,
                                                   !! rather than `'` tokens.
    logical(LK)              :: create             !! if the object is to be created
    logical(LK)              :: created            !! if `create` is true, then this will be
                                                   !! true if the leaf object had to be created
    integer(IK)              :: j                  !! counter of children when creating object

    !TODO instead of reallocating `token` all the time, just
    !     allocate a big size and keep track of the length,
    !     then just reallocate only if necessary.
    !     [would probably be inefficient if there was a very large token,
    !     and then a bunch of small ones... but for similarly-sized ones
    !     it should be way more efficient since it would avoid most
    !     reallocations.]

    nullify(p)

    if (.not. json%exception_thrown) then

        if (present(create_it)) then
            create = create_it
        else
            create = .false.
        end if

        p => me ! initialize
        created = .false.

        if (path==CK_'') then
            call json%throw_exception('Error in json_get_by_path_jsonpath_bracket: '//&
                                      'invalid path specification: '//trim(path),found)
        else

            if (path(1:1)==root .or. path(1:1)==start_array) then ! the first character must be
                                                                  ! a `$` (root) or a `[`
                                                                  ! (element of `me`)

                if (path(1:1)==root) then
                    ! go to the root
                    do while (associated (p%parent))
                        p => p%parent
                    end do
                    if (create) created = .false. ! should always exist
                end if

                !path length (don't need trailing spaces:)
                ilen = len_trim(path)

                if (ilen>1) then

                    istart = 2   ! initialize first '[' location index

                    do

                        if (istart>ilen) exit  ! finished

                        ! must be the next start bracket:
                        if (path(istart:istart) /= start_array) then
                            call json%throw_exception(&
                                    'Error in json_get_by_path_jsonpath_bracket: '//&
                                    'expecting "[", found: "'//trim(path(istart:istart))//&
                                    '" in path: '//trim(path),found)
                            exit
                        end if

                        ! get the next token by checking:
                        !
                        ! * [''] -- is the token after istart a quote?
                        !           if so, then search for the next `']`
                        !
                        ! * [1] -- if not, then maybe it is a number,
                        !          so search for the next `]`

                        ! verify length of remaining string
                        if (istart+2<=ilen) then

                            real_quotes = path(istart+1:istart+1) == quotation_mark   ! ["

                            if (real_quotes .or. path(istart+1:istart+1)==single_quote) then  ! ['

                                ! it might be a key value: ['abc']

                                istart = istart + 1 ! move counter to ' index
                                if (real_quotes) then
                                    iend = istart + index(path(istart+1:ilen),&
                                           quotation_mark//end_array)  ! "]
                                else
                                    iend = istart + index(path(istart+1:ilen),&
                                           single_quote//end_array)  ! ']
                                end if
                                if (iend>istart) then

                                    !     istart  iend
                                    !       |       |
                                    ! ['p']['abcdefg']

                                    if (iend>istart+1) then
                                        token = path(istart+1:iend-1)
                                    else
                                        token = CK_''  ! blank string
                                    end if
                                    ! remove trailing spaces in
                                    ! the token here if necessary:
                                    if (.not. json%trailing_spaces_significant) &
                                        token = trim(token)

                                    if (create) then
                                        ! have a token, create it if necessary

                                        ! we need to convert it into an object here
                                        ! (e.g., if p was also just created)
                                        ! and destroy its data to prevent a memory leak
                                        call json%convert(p,json_object)

                                        ! don't want to throw exceptions in this case
                                        call json%get_child(p,token,tmp,status_ok)
                                        if (.not. status_ok) then
                                            ! have to create this child
                                            ! [make it a null since we don't
                                            ! know what it is yet]
                                            call json_value_create(tmp)
                                            call json%to_null(tmp,token)
                                            call json%add(p,tmp)
                                            status_ok = .true.
                                            created = .true.
                                        else
                                            ! it was already there.
                                            created = .false.
                                        end if
                                    else
                                        ! have a token, see if it is valid:
                                        call json%get_child(p,token,tmp,status_ok)
                                    end if

                                    if (status_ok) then
                                        ! it was found
                                        p => tmp
                                    else
                                        call json%throw_exception(&
                                                'Error in json_get_by_path_jsonpath_bracket: '//&
                                                'invalid token found: "'//token//&
                                                '" in path: '//trim(path),found)
                                        exit
                                    end if
                                    iend = iend + 1 ! move counter to ] index
                                else
                                    call json%throw_exception(&
                                            'Error in json_get_by_path_jsonpath_bracket: '//&
                                            'invalid path: '//trim(path),found)
                                    exit
                                end if

                            else

                                ! it might be an integer value: [123]

                                iend = istart + index(path(istart+1:ilen),end_array)   ! ]
                                if (iend>istart+1) then

                                    ! this should be an integer:
                                    token = path(istart+1:iend-1)

                                    ! verify that there are no spaces or other
                                    ! characters in the string:
                                    status_ok = .true.
                                    do i=1,len(token)
                                        ! It must only contain (0..9) characters
                                        ! (it must be unsigned)
                                        if (scan(token(i:i),CK_'0123456789')<1) then
                                            status_ok = .false.
                                            exit
                                        end if
                                    end do
                                    if (status_ok) then
                                        call string_to_integer(token,ival,status_ok)
                                        if (status_ok) status_ok = ival>0  ! assuming 1-based array indices
                                    end if

                                    if (status_ok) then

                                        ! have a valid integer to use as an index
                                        ! see if this element is really there:
                                        call json%get_child(p,ival,tmp,status_ok)

                                        if (create .and. .not. status_ok) then

                                            ! have to create it:

                                            if (.not.(p%var_type==json_object .or. p%var_type==json_array)) then
                                                ! we need to convert it into an array here
                                                ! (e.g., if p was also just created)
                                                ! and destroy its data to prevent a memory leak
                                                call json%convert(p,json_array)
                                            end if

                                            ! have to create this element
                                            ! [make it a null]
                                            ! (and any missing ones before it)
                                            do j = 1, ival
                                                nullify(tmp)
                                                call json%get_child(p, j, tmp, status_ok)
                                                if (.not. status_ok) then
                                                    call json_value_create(tmp)
                                                    call json%to_null(tmp)  ! array element doesn't need a name
                                                    call json%add(p,tmp)
                                                    if (j==ival) created = .true.
                                                else
                                                    if (j==ival) created = .false.
                                                end if
                                            end do
                                            status_ok = .true.

                                        else
                                            created = .false.
                                        end if

                                        if (status_ok) then
                                            ! found it
                                            p => tmp
                                        else
                                            ! not found
                                            call json%throw_exception(&
                                                    'Error in json_get_by_path_jsonpath_bracket: '//&
                                                    'invalid array index found: "'//token//&
                                                    '" in path: '//trim(path),found)
                                            exit
                                        end if
                                    else
                                        call json%throw_exception(&
                                                'Error in json_get_by_path_jsonpath_bracket: '//&
                                                'invalid token: "'//token//&
                                                '" in path: '//trim(path),found)
                                        exit
                                    end if

                                else
                                    call json%throw_exception(&
                                            'Error in json_get_by_path_jsonpath_bracket: '//&
                                            'invalid path: '//trim(path),found)
                                    exit
                                end if

                            end if

                        else
                            call json%throw_exception(&
                                    'Error in json_get_by_path_jsonpath_bracket: '//&
                                    'invalid path: '//trim(path),found)
                            exit
                        end if

                        ! set up for next token:
                        istart = iend + 1

                    end do

                end if

            else
                call json%throw_exception(&
                        'Error in json_get_by_path_jsonpath_bracket: '//&
                        'expecting "'//root//'", found: "'//path(1:1)//&
                        '" in path: '//trim(path),found)
            end if

        end if

        if (json%exception_thrown) then
            nullify(p)
            if (present(found)) then
                found = .false.
                call json%clear_exceptions()
            end if
        else
            if (present(found)) found = .true.
        end if

        ! if it had to be created:
        if (present(was_created)) was_created = created

    else
        if (present(found)) found = .false.
        if (present(was_created)) was_created = .false.
    end if

    end subroutine json_get_by_path_jsonpath_bracket
!*****************************************************************************************

!*****************************************************************************************
!>
!  Convert an existing JSON variable `p` to a different variable type.
!  The existing variable (and its children) is destroyed. It is replaced
!  in the structure by a new variable of type `var_type`
!  (which can be a `json_null`, `json_object` or `json_array`).
!
!@note This is an internal routine used when creating variables by path.

    subroutine convert(json,p,var_type)

    implicit none

    class(json_core),intent(inout) :: json
    type(json_value),pointer :: p  !! the variable to convert
    integer(IK),intent(in) :: var_type !! the variable type to convert `p` to

    type(json_value),pointer :: tmp  !! temporary variable
    character(kind=CK,len=:),allocatable :: name !! the name of a JSON variable

    logical :: convert_it  !! if `p` needs to be converted

    convert_it = p%var_type /= var_type

    if (convert_it) then

        call json%info(p,name=name) ! get existing name

        select case (var_type)
        case(json_object)
            call json%create_object(tmp,name)
        case(json_array)
            call json%create_array(tmp,name)
        case(json_null)
            call json%create_null(tmp,name)
        case default
            call json%throw_exception('Error in convert: invalid var_type value.')
            return
        end select

        call json%replace(p,tmp,destroy=.true.)
        p => tmp
        nullify(tmp)

    end if

    end subroutine convert
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
!
!@note If `json%path_mode/=1`, then the `use_alt_array_tokens`
!      and `path_sep` inputs are ignored if present.
!
!@note [http://goessner.net/articles/JsonPath/](JSONPath) (`path_mode=3`)
!      does not specify whether or not the keys should be escaped (this routine
!      assumes not, as does http://jsonpath.com).
!      Also, we are using Fortran-style 1-based array indices,
!      not 0-based, to agree with the assumption in `path_mode=1`

    subroutine json_get_path(json, p, path, found, use_alt_array_tokens, path_sep)

    implicit none

    class(json_core),intent(inout)                   :: json
    type(json_value),pointer,intent(in)              :: p     !! a JSON linked list object
    character(kind=CK,len=:),allocatable,intent(out) :: path  !! path to the variable
    logical(LK),intent(out),optional                 :: found !! true if there were no problems
    logical(LK),intent(in),optional :: use_alt_array_tokens   !! if true, then '()' are used for array elements
                                                              !! otherwise, '[]' are used [default]
                                                              !! (only used if `path_mode=1`)
    character(kind=CK,len=1),intent(in),optional :: path_sep  !! character to use for path separator
                                                              !! (otherwise use `json%path_separator`)
                                                              !! (only used if `path_mode=1`)

    character(kind=CK,len=:),allocatable       :: name         !! variable name
    character(kind=CK,len=:),allocatable       :: parent_name  !! variable's parent name
    character(kind=CK,len=max_integer_str_len) :: istr         !! for integer to string conversion
                                                               !! (array indices)
    type(json_value),pointer :: tmp            !! for traversing the structure
    type(json_value),pointer :: element        !! for traversing the structure
    integer(IK)              :: var_type       !! JSON variable type flag
    integer(IK)              :: i              !! counter
    integer(IK)              :: n_children     !! number of children for parent
    logical(LK)              :: use_brackets   !! to use '[]' characters for arrays
    logical(LK)              :: parent_is_root !! if the parent is the root
    character(kind=CK,len=1) :: array_start    !! for `path_mode=1`, the character to start arrays
    character(kind=CK,len=1) :: array_end      !! for `path_mode=1`, the character to end arrays
    logical                  :: consecutive_arrays      !! check for array of array case
    integer(IK)              :: parents_parent_var_type !! `var_type` for parent's parent

    !optional input:
    if (present(use_alt_array_tokens)) then
        use_brackets = .not. use_alt_array_tokens
    else
        use_brackets = .true.
    end if

    if (json%path_mode==1_IK) then
        if (use_brackets) then
            array_start = start_array
            array_end   = end_array
        else
            array_start = start_array_alt
            array_end   = end_array_alt
        end if
    end if

    ! initialize:
    consecutive_arrays = .false.

    if (associated(p)) then

        !traverse the structure via parents up to the root
        tmp => p
        do

            if (.not. associated(tmp)) exit !finished

            !get info about the current variable:
            call json%info(tmp,name=name)
            if (json%path_mode==2_IK) then
                name = encode_rfc6901(name)
            end if

            ! if tmp a child of an object, or an element of an array
            if (associated(tmp%parent)) then

                !get info about the parent:
                call json%info(tmp%parent,var_type=var_type,&
                               n_children=n_children,name=parent_name)
                if (json%path_mode==2_IK) then
                    parent_name = encode_rfc6901(parent_name)
                end if
                if (associated(tmp%parent%parent)) then
                    call json%info(tmp%parent%parent,var_type=parents_parent_var_type)
                    consecutive_arrays = parents_parent_var_type == json_array .and. &
                                         var_type == json_array
                else
                    consecutive_arrays = .false.
                end if

                select case (var_type)
                case (json_array)

                    !get array index of this element:
                    element => tmp%parent%children
                    do i = 1, n_children
                        if (.not. associated(element)) then
                            call json%throw_exception('Error in json_get_path: '//&
                                                      'malformed JSON structure. ',found)
                            exit
                        end if
                        if (associated(element,tmp)) then
                            exit
                        else
                            element => element%next
                        end if
                        if (i==n_children) then ! it wasn't found (should never happen)
                            call json%throw_exception('Error in json_get_path: '//&
                                                      'malformed JSON structure. ',found)
                            exit
                        end if
                    end do
                    select case(json%path_mode)
                    case(3_IK)
                        ! JSONPath "bracket-notation"
                        ! example: `$['key'][1]`
                        ! [note: this uses 1-based indices]
                        call integer_to_string(i,int_fmt,istr)
                        if (consecutive_arrays) then
                            call add_to_path(start_array//trim(adjustl(istr))//end_array,CK_'')
                        else
                            call add_to_path(start_array//single_quote//parent_name//&
                                             single_quote//end_array//&
                                             start_array//trim(adjustl(istr))//end_array,CK_'')
                        end if
                    case(2_IK)
                        ! rfc6901
                        ! Example: '/key/0'
                        call integer_to_string(i-1_IK,int_fmt,istr) ! 0-based index
                        if (consecutive_arrays) then
                            call add_to_path(trim(adjustl(istr)))
                        else
                            call add_to_path(parent_name//slash//trim(adjustl(istr)))
                        end if
                    case(1_IK)
                        ! default
                        ! Example: `key[1]`
                        call integer_to_string(i,int_fmt,istr)
                        if (consecutive_arrays) then
                            call add_to_path(array_start//trim(adjustl(istr))//array_end,path_sep)
                        else
                            call add_to_path(parent_name//array_start//&
                                             trim(adjustl(istr))//array_end,path_sep)
                        end if
                    end select

                    if (.not. consecutive_arrays) tmp => tmp%parent  ! already added parent name

                case (json_object)

                    if (.not. consecutive_arrays) then
                        ! idea is not to print the array name if
                        ! it was already printed with the array

                        !process parent on the next pass
                        select case(json%path_mode)
                        case(3_IK)
                            call add_to_path(start_array//single_quote//name//&
                                            single_quote//end_array,CK_'')
                        case default
                            call add_to_path(name,path_sep)
                        end select

                    end if

                case default

                    call json%throw_exception('Error in json_get_path: '//&
                                              'malformed JSON structure. '//&
                                              'A variable that is not an object '//&
                                              'or array should not have a child.',found)
                    exit

                end select

            else
                !the last one:
                select case(json%path_mode)
                case(3_IK)
                    call add_to_path(start_array//single_quote//name//&
                                     single_quote//end_array,CK_'')
                case default
                    call add_to_path(name,path_sep)
                end select
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
                                  'input pointer is not associated',found)
    end if

    !for errors, return blank string:
    if (json%exception_thrown .or. .not. allocated(path)) then
        path = CK_''
    else
        select case (json%path_mode)
        case(3_IK)
            ! add the outer level object identifier:
            path = root//path
        case(2_IK)
            ! add the root slash:
            path = slash//path
        end select
    end if

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

        subroutine add_to_path(str,path_sep)
        !! prepend the string to the path
        implicit none
        character(kind=CK,len=*),intent(in) :: str  !! string to prepend to `path`
        character(kind=CK,len=*),intent(in),optional :: path_sep
            !! path separator (default is '.').
            !! (ignored if `json%path_mode/=1`)

        select case (json%path_mode)
        case(3_IK)
            ! in this case, the options are ignored
            if (.not. allocated(path)) then
                path = str
            else
                path = str//path
            end if
        case(2_IK)
            ! in this case, the options are ignored
            if (.not. allocated(path)) then
                path = str
            else
                path = str//slash//path
            end if
        case(1_IK)
            ! default path format
            if (.not. allocated(path)) then
                path = str
            else
                ! shouldn't add the path_sep for cases like x[1][2]
                ! [if current is an array element, and the previous was
                ! also an array element] so check for that here:
                if (.not. ( str(len(str):len(str))==array_end .and. &
                            path(1:1)==array_start )) then
                    if (present(path_sep)) then
                        ! use user specified:
                        path = str//path_sep//path
                    else
                        ! use the default:
                        path = str//json%path_separator//path
                    end if
                else
                    path = str//path
                end if
            end if
        end select

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
    logical(LK),intent(in),optional :: use_alt_array_tokens    !! if true, then '()' are used
                                                               !! for array elements otherwise,
                                                               !! '[]' are used [default]
    character(kind=CDK,len=1),intent(in),optional :: path_sep  !! character to use for path
                                                               !! separator (default is '.')

    character(kind=CK,len=:),allocatable :: ck_path  !! path to the variable

    ! call the main routine:
    if (present(path_sep)) then
        call json%get_path(p,ck_path,found,use_alt_array_tokens,to_unicode(path_sep))
    else
        call json%get_path(p,ck_path,found,use_alt_array_tokens)
    end if

    ! from unicode:
    path = ck_path

    end subroutine wrap_json_get_path
!*****************************************************************************************

!*****************************************************************************************
!>
!  Convert a string into an integer.
!
!@note Replacement for the `parse_integer` function in the original code.

    function string_to_int(json,str) result(ival)

    implicit none

    class(json_core),intent(inout)      :: json
    character(kind=CK,len=*),intent(in) :: str   !! a string
    integer(IK)                         :: ival  !! `str` converted to an integer

    logical(LK) :: status_ok !! error flag for [[string_to_integer]]

    ! call the core routine:
    call string_to_integer(str,ival,status_ok)

    if (.not. status_ok) then
        ival = 0
        call json%throw_exception('Error in string_to_int: '//&
                                    'string cannot be converted to an integer: '//&
                                    trim(str))
    end if

    end function string_to_int
!*****************************************************************************************

!*****************************************************************************************
!>
!  Convert a string into a `real(RK)` value.

    function string_to_dble(json,str) result(rval)

    implicit none

    class(json_core),intent(inout)      :: json
    character(kind=CK,len=*),intent(in) :: str   !! a string
    real(RK)                            :: rval  !! `str` converted to a `real(RK)`

    logical(LK) :: status_ok  !! error flag for [[string_to_real]]

    call string_to_real(str,json%use_quiet_nan,rval,status_ok)

    if (.not. status_ok) then    !if there was an error
        rval = 0.0_RK
        call json%throw_exception('Error in string_to_dble: '//&
                                  'string cannot be converted to a real: '//&
                                  trim(str))
    end if

    end function string_to_dble
!*****************************************************************************************

!*****************************************************************************************
!>
!  Get an integer value from a [[json_value]].

    subroutine json_get_integer(json, me, value)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer,intent(in) :: me
    integer(IK),intent(out)             :: value  !! the integer value

    logical(LK) :: status_ok !! for [[string_to_integer]]

    value = 0_IK
    if ( json%exception_thrown ) return

    if (me%var_type == json_integer) then
        value = me%int_value
    else
        if (json%strict_type_checking) then
            if (allocated(me%name)) then
                call json%throw_exception('Error in json_get_integer:'//&
                    ' Unable to resolve value to integer: '//me%name)
            else
                call json%throw_exception('Error in json_get_integer:'//&
                    ' Unable to resolve value to integer')
            end if
        else
            !type conversions
            select case(me%var_type)
            case (json_real)
                value = int(me%dbl_value, IK)
            case (json_logical)
                if (me%log_value) then
                    value = 1_IK
                else
                    value = 0_IK
                end if
            case (json_string)
                call string_to_integer(me%str_value,value,status_ok)
                if (.not. status_ok) then
                    value = 0_IK
                    if (allocated(me%name)) then
                        call json%throw_exception('Error in json_get_integer:'//&
                            ' Unable to convert string value to integer: '//&
                            me%name//' = '//trim(me%str_value))
                    else
                        call json%throw_exception('Error in json_get_integer:'//&
                            ' Unable to convert string value to integer: '//&
                            trim(me%str_value))
                    end if
                end if
            case default
                if (allocated(me%name)) then
                    call json%throw_exception('Error in json_get_integer:'//&
                        ' Unable to resolve value to integer: '//me%name)
                else
                    call json%throw_exception('Error in json_get_integer:'//&
                        ' Unable to resolve value to integer')
                end if
            end select
        end if
    end if

    end subroutine json_get_integer
!*****************************************************************************************

!*****************************************************************************************
!>
!  Get an integer value from a [[json_value]], given the path string.

    subroutine json_get_integer_by_path(json, me, path, value, found, default)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer,intent(in) :: me
    character(kind=CK,len=*),intent(in) :: path
    integer(IK),intent(out)             :: value
    logical(LK),intent(out),optional    :: found
    integer(IK),intent(in),optional     :: default !! default value if not found

    integer(IK),parameter :: default_if_not_specified = 0_IK
    character(kind=CK,len=*),parameter :: routine = CK_'json_get_integer_by_path'

#include "json_get_scalar_by_path.inc"

    end subroutine json_get_integer_by_path
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_get_integer_by_path]], where "path" is kind=CDK.

    subroutine wrap_json_get_integer_by_path(json, me, path, value, found, default)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer,intent(in)  :: me
    character(kind=CDK,len=*),intent(in) :: path
    integer(IK),intent(out)              :: value
    logical(LK),intent(out),optional     :: found
    integer(IK),intent(in),optional      :: default !! default value if not found

    call json%get(me, to_unicode(path), value, found, default)

    end subroutine wrap_json_get_integer_by_path
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

    if ( json%exception_thrown ) return

    ! check for 0-length arrays first:
    select case (me%var_type)
    case (json_array)
        if (json%count(me)==0) then
            allocate(vec(0))
            return
        end if
    end select

    initialized = .false.

    !the callback function is called for each element of the array:
    call json%get(me, array_callback=get_int_from_array)

    if (json%exception_thrown .and. allocated(vec)) deallocate(vec)

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
!  If `found` is present, set it it false.

    subroutine flag_not_found(found)

    implicit none

    logical(LK),intent(out),optional :: found

    if (present(found)) found = .false.

    end subroutine flag_not_found
!*****************************************************************************************

!*****************************************************************************************
!>
!  Get an integer vector from a [[json_value]], given the path string.

    subroutine json_get_integer_vec_by_path(json, me, path, vec, found, default)

    implicit none

    class(json_core),intent(inout)                   :: json
    type(json_value),pointer,intent(in)              :: me
    character(kind=CK,len=*),intent(in)              :: path
    integer(IK),dimension(:),allocatable,intent(out) :: vec
    logical(LK),intent(out),optional                 :: found
    integer(IK),dimension(:),intent(in),optional     :: default !! default value if not found

    character(kind=CK,len=*),parameter :: routine = CK_'json_get_integer_vec_by_path'

#include "json_get_vec_by_path.inc"

    end subroutine json_get_integer_vec_by_path
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_get_integer_vec_by_path]], where "path" is kind=CDK

    subroutine wrap_json_get_integer_vec_by_path(json, me, path, vec, found, default)

    implicit none

    class(json_core),intent(inout)                   :: json
    type(json_value),pointer                         :: me
    character(kind=CDK,len=*),intent(in)             :: path
    integer(IK),dimension(:),allocatable,intent(out) :: vec
    logical(LK),intent(out),optional                 :: found
    integer(IK),dimension(:),intent(in),optional     :: default !! default value if not found

    call json%get(me,path=to_unicode(path),vec=vec,found=found,default=default)

    end subroutine wrap_json_get_integer_vec_by_path
!*****************************************************************************************

!*****************************************************************************************
!>
!  Get a real value from a [[json_value]].

    subroutine json_get_real(json, me, value)

    implicit none

    class(json_core),intent(inout) :: json
    type(json_value),pointer       :: me
    real(RK),intent(out)           :: value

    logical(LK) :: status_ok !! for [[string_to_real]]

    value = 0.0_RK
    if ( json%exception_thrown ) return

    if (me%var_type == json_real) then
        value = me%dbl_value
    else
        if (json%strict_type_checking) then
            if (allocated(me%name)) then
                call json%throw_exception('Error in json_get_real:'//&
                                          ' Unable to resolve value to real: '//me%name)
            else
                call json%throw_exception('Error in json_get_real:'//&
                                          ' Unable to resolve value to real')
            end if
        else
            !type conversions
            select case (me%var_type)
            case (json_integer)
                value = real(me%int_value, RK)
            case (json_logical)
                if (me%log_value) then
                    value = 1.0_RK
                else
                    value = 0.0_RK
                end if
            case (json_string)
                call string_to_real(me%str_value,json%use_quiet_nan,value,status_ok)
                if (.not. status_ok) then
                    value = 0.0_RK
                    if (allocated(me%name)) then
                        call json%throw_exception('Error in json_get_real:'//&
                            ' Unable to convert string value to real: '//&
                            me%name//' = '//trim(me%str_value))
                    else
                        call json%throw_exception('Error in json_get_real:'//&
                            ' Unable to convert string value to real: '//&
                            trim(me%str_value))
                    end if
                end if
            case (json_null)
                if (ieee_support_nan(value) .and. json%null_to_real_mode/=1_IK) then
                    select case (json%null_to_real_mode)
                    case(2_IK)
                        if (json%use_quiet_nan) then
                            value = ieee_value(value,ieee_quiet_nan)
                        else
                            value = ieee_value(value,ieee_signaling_nan)
                        end if
                    case(3_IK)
                        value = 0.0_RK
                    end select
                else
                    if (allocated(me%name)) then
                        call json%throw_exception('Error in json_get_real:'//&
                                                ' Cannot convert null to NaN: '//me%name)
                    else
                        call json%throw_exception('Error in json_get_real:'//&
                                                ' Cannot convert null to NaN')
                    end if
                end if
            case default
                if (allocated(me%name)) then
                    call json%throw_exception('Error in json_get_real:'//&
                                            ' Unable to resolve value to real: '//me%name)
                else
                    call json%throw_exception('Error in json_get_real:'//&
                                            ' Unable to resolve value to real')
                end if
            end select
        end if
    end if

    end subroutine json_get_real
!*****************************************************************************************

!*****************************************************************************************
!>
!  Get a real value from a [[json_value]], given the path.

    subroutine json_get_real_by_path(json, me, path, value, found, default)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer            :: me
    character(kind=CK,len=*),intent(in) :: path
    real(RK),intent(out)                :: value
    logical(LK),intent(out),optional    :: found
    real(RK),intent(in),optional        :: default !! default value if not found

    real(RK),parameter :: default_if_not_specified = 0.0_RK
    character(kind=CK,len=*),parameter :: routine = CK_'json_get_real_by_path'

#include "json_get_scalar_by_path.inc"

    end subroutine json_get_real_by_path
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_get_real_by_path]], where "path" is kind=CDK

    subroutine wrap_json_get_real_by_path(json, me, path, value, found, default)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer             :: me
    character(kind=CDK,len=*),intent(in) :: path
    real(RK),intent(out)                 :: value
    logical(LK),intent(out),optional     :: found
    real(RK),intent(in),optional         :: default !! default value if not found

    call json%get(me,to_unicode(path),value,found,default)

    end subroutine wrap_json_get_real_by_path
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 5/14/2014
!
!  Get a real vector from a [[json_value]].

    subroutine json_get_real_vec(json, me, vec)

    implicit none

    class(json_core),intent(inout)                :: json
    type(json_value),pointer                      :: me
    real(RK),dimension(:),allocatable,intent(out) :: vec

    logical(LK) :: initialized

    if ( json%exception_thrown ) return

    ! check for 0-length arrays first:
    select case (me%var_type)
    case (json_array)
        if (json%count(me)==0) then
            allocate(vec(0))
            return
        end if
    end select

    initialized = .false.

    !the callback function is called for each element of the array:
    call json%get(me, array_callback=get_real_from_array)

    if (json%exception_thrown .and. allocated(vec)) deallocate(vec)

    contains

        subroutine get_real_from_array(json, element, i, count)

        !! callback function for real

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

        end subroutine get_real_from_array

    end subroutine json_get_real_vec
!*****************************************************************************************

!*****************************************************************************************
!>
!  Get a real vector from a [[json_value]], given the path.

    subroutine json_get_real_vec_by_path(json, me, path, vec, found, default)

    implicit none

    class(json_core),intent(inout)                :: json
    type(json_value),pointer,intent(in)           :: me
    character(kind=CK,len=*),intent(in)           :: path
    real(RK),dimension(:),allocatable,intent(out) :: vec
    logical(LK),intent(out),optional              :: found
    real(RK),dimension(:),intent(in),optional     :: default !! default value if not found

    character(kind=CK,len=*),parameter :: routine = CK_'json_get_real_vec_by_path'

#include "json_get_vec_by_path.inc"

    end subroutine json_get_real_vec_by_path
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_get_real_vec_by_path]], where "path" is kind=CDK

    subroutine wrap_json_get_real_vec_by_path(json, me, path, vec, found, default)

    implicit none

    class(json_core),intent(inout)                :: json
    type(json_value),pointer                      :: me
    character(kind=CDK,len=*),intent(in)          :: path
    real(RK),dimension(:),allocatable,intent(out) :: vec
    logical(LK),intent(out),optional              :: found
    real(RK),dimension(:),intent(in),optional     :: default !! default value if not found

    call json%get(me, to_unicode(path), vec, found, default)

    end subroutine wrap_json_get_real_vec_by_path
!*****************************************************************************************

#ifndef REAL32
!*****************************************************************************************
!>
!  Alternate version of [[json_get_real]] where value=real32.

    subroutine json_get_real32(json, me, value)

    implicit none

    class(json_core),intent(inout) :: json
    type(json_value),pointer       :: me
    real(real32),intent(out)       :: value

    real(RK) :: tmp

    call json%get(me, tmp)
    value = real(tmp,real32)

    end subroutine json_get_real32
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_get_real_by_path]] where value=real32.

    subroutine json_get_real32_by_path(json, me, path, value, found, default)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer            :: me
    character(kind=CK,len=*),intent(in) :: path
    real(real32),intent(out)            :: value
    logical(LK),intent(out),optional    :: found
    real(real32),intent(in),optional    :: default !! default value if not found

    real(RK) :: tmp
    real(RK) :: tmp_default

    if (present(default)) then
        tmp_default = real(default,RK)
        call json%get(me, path, tmp, found, tmp_default)
    else
        call json%get(me, path, tmp, found)
    end if

    value = real(tmp,real32)

    end subroutine json_get_real32_by_path
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_get_real32_by_path]], where "path" is kind=CDK

    subroutine wrap_json_get_real32_by_path(json, me, path, value, found, default)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer             :: me
    character(kind=CDK,len=*),intent(in) :: path
    real(real32),intent(out)             :: value
    logical(LK),intent(out),optional     :: found
    real(real32),intent(in),optional     :: default !! default value if not found

    call json%get(me,to_unicode(path),value,found,default)

    end subroutine wrap_json_get_real32_by_path
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_get_real_vec]] where `vec` is `real32`.

    subroutine json_get_real32_vec(json, me, vec)

    implicit none

    class(json_core),intent(inout)                    :: json
    type(json_value),pointer                          :: me
    real(real32),dimension(:),allocatable,intent(out) :: vec

    real(RK),dimension(:),allocatable :: tmp

    call json%get(me, tmp)
    if (allocated(tmp)) vec = real(tmp,real32)

    end subroutine json_get_real32_vec
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_get_real_vec_by_path]] where `vec` is `real32`.

    subroutine json_get_real32_vec_by_path(json, me, path, vec, found, default)

    implicit none

    class(json_core),intent(inout)                    :: json
    type(json_value),pointer,intent(in)               :: me
    character(kind=CK,len=*),intent(in)               :: path
    real(real32),dimension(:),allocatable,intent(out) :: vec
    logical(LK),intent(out),optional                  :: found
    real(real32),dimension(:),intent(in),optional     :: default !! default value if not found

    real(RK),dimension(:),allocatable :: tmp
    real(RK),dimension(:),allocatable :: tmp_default

    if (present(default)) then
        tmp_default = real(default,RK)
        call json%get(me, path, tmp, found, tmp_default)
    else
        call json%get(me, path, tmp, found)
    end if

    if (allocated(tmp)) vec = real(tmp,real32)

    end subroutine json_get_real32_vec_by_path
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_get_real32_vec_by_path]], where "path" is kind=CDK

    subroutine wrap_json_get_real32_vec_by_path(json, me, path, vec, found, default)

    implicit none

    class(json_core),intent(inout)                    :: json
    type(json_value),pointer                          :: me
    character(kind=CDK,len=*),intent(in)              :: path
    real(real32),dimension(:),allocatable,intent(out) :: vec
    logical(LK),intent(out),optional                  :: found
    real(real32),dimension(:),intent(in),optional     :: default !! default value if not found

    call json%get(me, to_unicode(path), vec, found, default)

    end subroutine wrap_json_get_real32_vec_by_path
!*****************************************************************************************
#endif

#ifdef REAL128
!*****************************************************************************************
!>
!  Alternate version of [[json_get_real]] where `value` is `real64`.

    subroutine json_get_real64(json, me, value)

    implicit none

    class(json_core),intent(inout) :: json
    type(json_value),pointer       :: me
    real(real64),intent(out)       :: value

    real(RK) :: tmp

    call json%get(me, tmp)
    value = real(tmp,real64)

    end subroutine json_get_real64
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_get_real_by_path]] where `value` is `real64`.

    subroutine json_get_real64_by_path(json, me, path, value, found, default)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer            :: me
    character(kind=CK,len=*),intent(in) :: path
    real(real64),intent(out)            :: value
    logical(LK),intent(out),optional    :: found
    real(real64),intent(in),optional    :: default !! default value if not found

    real(RK) :: tmp

    call json%get(me, path, tmp, found, default)
    value = real(tmp,real64)

    end subroutine json_get_real64_by_path
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_get_real64_by_path]], where "path" is kind=CDK

    subroutine wrap_json_get_real64_by_path(json, me, path, value, found, default)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer             :: me
    character(kind=CDK,len=*),intent(in) :: path
    real(real64),intent(out)             :: value
    logical(LK),intent(out),optional     :: found
    real(real64),intent(in),optional     :: default !! default value if not found

    call json%get(me,to_unicode(path),value,found, default)

    end subroutine wrap_json_get_real64_by_path
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_get_real_vec]] where `vec` is `real64`.

    subroutine json_get_real64_vec(json, me, vec)

    implicit none

    class(json_core),intent(inout)                    :: json
    type(json_value),pointer                          :: me
    real(real64),dimension(:),allocatable,intent(out) :: vec

    real(RK),dimension(:),allocatable :: tmp

    call json%get(me, tmp)
    if (allocated(tmp)) vec = real(tmp,real64)

    end subroutine json_get_real64_vec
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_get_real_vec_by_path]] where `vec` is `real64`.

    subroutine json_get_real64_vec_by_path(json, me, path, vec, found, default)

    implicit none

    class(json_core),intent(inout)                    :: json
    type(json_value),pointer,intent(in)               :: me
    character(kind=CK,len=*),intent(in)               :: path
    real(real64),dimension(:),allocatable,intent(out) :: vec
    logical(LK),intent(out),optional                  :: found
    real(real64),dimension(:),intent(in),optional     :: default !! default value if not found

    real(RK),dimension(:),allocatable :: tmp

    call json%get(me, path, tmp, found, default)
    if (allocated(tmp)) vec = real(tmp,real64)

    end subroutine json_get_real64_vec_by_path
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_get_real64_vec_by_path]], where "path" is kind=CDK

    subroutine wrap_json_get_real64_vec_by_path(json, me, path, vec, found, default)

    implicit none

    class(json_core),intent(inout)                    :: json
    type(json_value),pointer                          :: me
    character(kind=CDK,len=*),intent(in)              :: path
    real(real64),dimension(:),allocatable,intent(out) :: vec
    logical(LK),intent(out),optional                  :: found
    real(real64),dimension(:),intent(in),optional     :: default !! default value if not found

    call json%get(me, to_unicode(path), vec, found, default)

    end subroutine wrap_json_get_real64_vec_by_path
!*****************************************************************************************
#endif

!*****************************************************************************************
!>
!  Get a logical value from a [[json_value]].
!
!### Note
!  If `strict_type_checking` is False, then the following assumptions are made:
!
!  * For integers: a value > 0 is True
!  * For reals: a value > 0 is True
!  * For strings: 'true' is True, and everything else is false. [case sensitive match]

    subroutine json_get_logical(json, me, value)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer,intent(in) :: me
    logical(LK),intent(out)             :: value

    value = .false.
    if ( json%exception_thrown ) return

    if (me%var_type == json_logical) then
        value = me%log_value
    else
        if (json%strict_type_checking) then
            if (allocated(me%name)) then
                call json%throw_exception('Error in json_get_logical: '//&
                                          'Unable to resolve value to logical: '//&
                                          me%name)
            else
                call json%throw_exception('Error in json_get_logical: '//&
                                          'Unable to resolve value to logical')
            end if
        else
            !type conversions
            select case (me%var_type)
            case (json_integer)
                value = (me%int_value > 0_IK)
            case (json_real)
                value = (me%dbl_value > 0.0_RK)
            case (json_string)
                value = (me%str_value == true_str)
            case default
                if (allocated(me%name)) then
                    call json%throw_exception('Error in json_get_logical: '//&
                                              'Unable to resolve value to logical: '//&
                                              me%name)
                else
                    call json%throw_exception('Error in json_get_logical: '//&
                                              'Unable to resolve value to logical')
                end if
            end select
        end if
    end if

    end subroutine json_get_logical
!*****************************************************************************************

!*****************************************************************************************
!>
!  Get a logical value from a [[json_value]], given the path.

    subroutine json_get_logical_by_path(json, me, path, value, found, default)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer,intent(in) :: me
    character(kind=CK,len=*),intent(in) :: path
    logical(LK),intent(out)             :: value
    logical(LK),intent(out),optional    :: found
    logical(LK),intent(in),optional     :: default !! default value if not found

    logical(LK),parameter :: default_if_not_specified = .false.
    character(kind=CK,len=*),parameter :: routine = CK_'json_get_logical_by_path'

#include "json_get_scalar_by_path.inc"

    end subroutine json_get_logical_by_path
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_get_logical_by_path]], where "path" is kind=CDK

    subroutine wrap_json_get_logical_by_path(json, me, path, value, found, default)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer,intent(in)  :: me
    character(kind=CDK,len=*),intent(in) :: path
    logical(LK),intent(out)              :: value
    logical(LK),intent(out),optional     :: found
    logical(LK),intent(in),optional      :: default !! default value if not found

    call json%get(me,to_unicode(path),value,found,default)

    end subroutine wrap_json_get_logical_by_path
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

    if ( json%exception_thrown ) return

    ! check for 0-length arrays first:
    select case (me%var_type)
    case (json_array)
        if (json%count(me)==0) then
            allocate(vec(0))
            return
        end if
    end select

    initialized = .false.

    !the callback function is called for each element of the array:
    call json%get(me, array_callback=get_logical_from_array)

    if (json%exception_thrown .and. allocated(vec)) deallocate(vec)

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

    subroutine json_get_logical_vec_by_path(json, me, path, vec, found, default)

    implicit none

    class(json_core),intent(inout)                   :: json
    type(json_value),pointer,intent(in)              :: me
    character(kind=CK,len=*),intent(in)              :: path
    logical(LK),dimension(:),allocatable,intent(out) :: vec
    logical(LK),intent(out),optional                 :: found
    logical(LK),dimension(:),intent(in),optional     :: default

    character(kind=CK,len=*),parameter :: routine = CK_'json_get_logical_vec_by_path'

#include "json_get_vec_by_path.inc"

    end subroutine json_get_logical_vec_by_path
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_get_logical_vec_by_path]], where "path" is kind=CDK

    subroutine wrap_json_get_logical_vec_by_path(json, me, path, vec, found, default)

    implicit none

    class(json_core),intent(inout)                   :: json
    type(json_value),pointer,intent(in)              :: me
    character(kind=CDK,len=*),intent(in)             :: path
    logical(LK),dimension(:),allocatable,intent(out) :: vec
    logical(LK),intent(out),optional                 :: found
    logical(LK),dimension(:),intent(in),optional     :: default

    call json%get(me,to_unicode(path),vec,found,default)

    end subroutine wrap_json_get_logical_vec_by_path
!*****************************************************************************************

!*****************************************************************************************
!>
!  Get a character string from a [[json_value]].

    subroutine json_get_string(json, me, value)

    implicit none

    class(json_core),intent(inout)                   :: json
    type(json_value),pointer,intent(in)              :: me
    character(kind=CK,len=:),allocatable,intent(out) :: value

    value = CK_''
    if (.not. json%exception_thrown) then

        if (me%var_type == json_string) then

            if (allocated(me%str_value)) then
                if (json%unescaped_strings) then
                    ! default: it is stored already unescaped:
                    value = me%str_value
                else
                    ! return the escaped version:
                    call escape_string(me%str_value, value, json%escape_solidus)
                end if
            else
               call json%throw_exception('Error in json_get_string: '//&
                                         'me%str_value not allocated')
            end if

        else

            if (json%strict_type_checking) then
                if (allocated(me%name)) then
                    call json%throw_exception('Error in json_get_string:'//&
                                              ' Unable to resolve value to string: '//me%name)
                else
                    call json%throw_exception('Error in json_get_string:'//&
                                              ' Unable to resolve value to string')
                end if
            else

                select case (me%var_type)

                case (json_integer)

                    if (allocated(me%int_value)) then
                        value = repeat(space, max_integer_str_len)
                        call integer_to_string(me%int_value,int_fmt,value)
                        value = trim(value)
                    else
                        call json%throw_exception('Error in json_get_string: '//&
                                                  'me%int_value not allocated')
                    end if

                case (json_real)

                    if (allocated(me%dbl_value)) then
                        value = repeat(space, max_numeric_str_len)
                        call real_to_string(me%dbl_value,json%real_fmt,&
                                            json%non_normals_to_null,&
                                            json%compact_real,value)
                        value = trim(value)
                    else
                        call json%throw_exception('Error in json_get_string: '//&
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
                    if (allocated(me%name)) then
                        call json%throw_exception('Error in json_get_string: '//&
                                                  'Unable to resolve value to characters: '//&
                                                  me%name)
                    else
                        call json%throw_exception('Error in json_get_string: '//&
                                                  'Unable to resolve value to characters')
                    end if
                end select

            end if
        end if

    end if

    end subroutine json_get_string
!*****************************************************************************************

!*****************************************************************************************
!>
!  Get a character string from a [[json_value]], given the path.

    subroutine json_get_string_by_path(json, me, path, value, found, default)

    implicit none

    class(json_core),intent(inout)                   :: json
    type(json_value),pointer,intent(in)              :: me
    character(kind=CK,len=*),intent(in)              :: path
    character(kind=CK,len=:),allocatable,intent(out) :: value
    logical(LK),intent(out),optional                 :: found
    character(kind=CK,len=*),intent(in),optional     :: default

    character(kind=CK,len=*),parameter :: default_if_not_specified = CK_''
    character(kind=CK,len=*),parameter :: routine = CK_'json_get_string_by_path'

#include "json_get_scalar_by_path.inc"

    end subroutine json_get_string_by_path
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_get_string_by_path]], where "path" is kind=CDK

    subroutine wrap_json_get_string_by_path(json, me, path, value, found, default)

    implicit none

    class(json_core),intent(inout)                   :: json
    type(json_value),pointer,intent(in)              :: me
    character(kind=CDK,len=*),intent(in)             :: path
    character(kind=CK,len=:),allocatable,intent(out) :: value
    logical(LK),intent(out),optional                 :: found
    character(kind=CK,len=*),intent(in),optional     :: default

    call json%get(me,to_unicode(path),value,found,default)

    end subroutine wrap_json_get_string_by_path
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

    if ( json%exception_thrown ) return

    ! check for 0-length arrays first:
    select case (me%var_type)
    case (json_array)
        if (json%count(me)==0) then
            allocate(vec(0))
            return
        end if
    end select

    initialized = .false.

    !the callback function is called for each element of the array:
    call json%get(me, array_callback=get_chars_from_array)

    if (json%exception_thrown .and. allocated(vec)) deallocate(vec)

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
            vec(i) = CK_''
        end if

        end subroutine get_chars_from_array

    end subroutine json_get_string_vec
!*****************************************************************************************

!*****************************************************************************************
!>
!  Get a string vector from a [[json_value(type)]], given the path.

    subroutine json_get_string_vec_by_path(json, me, path, vec, found, default)

    implicit none

    class(json_core),intent(inout)                                :: json
    type(json_value),pointer,intent(in)                           :: me
    character(kind=CK,len=*),intent(in)                           :: path
    character(kind=CK,len=*),dimension(:),allocatable,intent(out) :: vec
    logical(LK),intent(out),optional                              :: found
    character(kind=CK,len=*),dimension(:),intent(in),optional     :: default

    character(kind=CK,len=*),parameter :: routine = CK_'json_get_string_vec_by_path'

#include "json_get_vec_by_path.inc"

    end subroutine json_get_string_vec_by_path
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_get_string_vec_by_path]], where "path" is kind=CDK

    subroutine wrap_json_get_string_vec_by_path(json, me, path, vec, found, default)

    implicit none

    class(json_core),intent(inout)                                :: json
    type(json_value),pointer,intent(in)                           :: me
    character(kind=CDK,len=*),intent(in)                          :: path
    character(kind=CK,len=*),dimension(:),allocatable,intent(out) :: vec
    logical(LK),intent(out),optional                              :: found
    character(kind=CK,len=*),dimension(:),intent(in),optional     :: default

    call json%get(me,to_unicode(path),vec,found,default)

    end subroutine wrap_json_get_string_vec_by_path
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 12/16/2016
!
!  Get a string vector from a [[json_value(type)]]. This is an alternate
!  version of [[json_get_string_vec]]. This one returns an allocatable
!  length character (where the string length is the maximum length of
!  any element in the array). It also returns an integer array of the
!  actual sizes of the strings in the JSON structure.
!
!@note This is somewhat inefficient since it does
!      cycle through the array twice.
!
!@warning The allocation of `vec` doesn't work with
!         gfortran 4.9 or 5 due to compiler bugs

    subroutine json_get_alloc_string_vec(json, me, vec, ilen)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer,intent(in)  :: me
    character(kind=CK,len=:),dimension(:),allocatable,intent(out) :: vec
    integer(IK),dimension(:),allocatable,intent(out) :: ilen !! the actual length
                                                             !! of each character
                                                             !! string in the array

    logical(LK) :: initialized !! if the output array has been sized
    integer(IK) :: max_len     !! the length of the longest string in the array

    if ( json%exception_thrown ) return

    ! check for 0-length arrays first:
    select case (me%var_type)
    case (json_array)
        if (json%count(me)==0) then
            allocate(character(kind=CK,len=0) :: vec(0))
            allocate(ilen(0))
            return
        end if
    end select

    initialized = .false.

    call json%string_info(me,ilen=ilen,max_str_len=max_len)
    if (.not. json%exception_thrown) then
        ! now get each string using the callback function:
        call json%get(me, array_callback=get_chars_from_array)
    end if

    if (json%exception_thrown) then
        if (allocated(vec))  deallocate(vec)
        if (allocated(ilen)) deallocate(ilen)
    end if

    contains

        subroutine get_chars_from_array(json, element, i, count)

        !! callback function for chars

        implicit none

        class(json_core),intent(inout)      :: json
        type(json_value),pointer,intent(in) :: element
        integer(IK),intent(in)              :: i        !! index
        integer(IK),intent(in)              :: count    !! size of array

        character(kind=CK,len=:),allocatable :: cval  !! for getting string

        !size the output array:
        if (.not. initialized) then
            ! string length long enough to hold the longest one
            ! Note that this doesn't work with gfortran 4.9 or 5.
            allocate( character(kind=CK,len=max_len) :: vec(count) )
            initialized = .true.
        end if

        !populate the elements:
        call json%get(element, value=cval)
        if (allocated(cval)) then
            vec(i)  = cval
            ilen(i) = len(cval)  ! return the actual length
            deallocate(cval)
        else
            vec(i)  = CK_''
            ilen(i) = 0
        end if

        end subroutine get_chars_from_array

    end subroutine json_get_alloc_string_vec
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_get_alloc_string_vec]] where input is the path.
!
!  This is an alternate version of [[json_get_string_vec_by_path]].
!  This one returns an allocatable length character (where the string
!  length is the maximum length of any element in the array). It also
!  returns an integer array of the actual sizes of the strings in the
!  JSON structure.
!
!@note An alternative to using this routine is to use [[json_get_array]] with
!      a callback function that gets the string from each element and populates
!      a user-defined string type.
!
!@note If the `default` argument is used, and `default_ilen` is not present,
!      then `ilen` will just be returned as the length of the `default` dummy
!      argument (all elements with the same length).

    subroutine json_get_alloc_string_vec_by_path(json,me,path,vec,ilen,found,default,default_ilen)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer,intent(in) :: me
    character(kind=CK,len=*),intent(in) :: path
    character(kind=CK,len=:),dimension(:),allocatable,intent(out) :: vec
    integer(IK),dimension(:),allocatable,intent(out) :: ilen !! the actual length
                                                             !! of each character
                                                             !! string in the array
    logical(LK),intent(out),optional :: found
    character(kind=CK,len=*),dimension(:),intent(in),optional :: default
    integer(IK),dimension(:),intent(in),optional :: default_ilen !! the actual
                                                                 !! length of `default`

    character(kind=CK,len=*),parameter :: routine = CK_'json_get_alloc_string_vec_by_path'

#include "json_get_vec_by_path_alloc.inc"

    end subroutine json_get_alloc_string_vec_by_path
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_get_alloc_string_vec_by_path]], where "path" is kind=CDK

    subroutine wrap_json_get_alloc_string_vec_by_path(json,me,path,vec,ilen,found,default,default_ilen)

    implicit none

    class(json_core),intent(inout)        :: json
    type(json_value),pointer,intent(in)   :: me
    character(kind=CDK,len=*),intent(in)  :: path
    character(kind=CK,len=:),dimension(:),allocatable,intent(out) :: vec
    integer(IK),dimension(:),allocatable,intent(out) :: ilen !! the actual length
                                                             !! of each character
                                                             !! string in the array
    logical(LK),intent(out),optional :: found
    character(kind=CK,len=*),dimension(:),intent(in),optional :: default
    integer(IK),dimension(:),intent(in),optional :: default_ilen !! the actual
                                                                 !! length of `default`

    call json%get(me,to_unicode(path),vec,ilen,found,default,default_ilen)

    end subroutine wrap_json_get_alloc_string_vec_by_path
!*****************************************************************************************

!*****************************************************************************************
!>
!  This routine calls the user-supplied [[json_array_callback_func]]
!  subroutine for each element in the array.
!
!@note For integer, real, logical, and character arrays,
!      higher-level routines are provided (see `get` methods), so
!      this routine does not have to be used for those cases.

    recursive subroutine json_get_array(json, me, array_callback)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer,intent(in) :: me
    procedure(json_array_callback_func) :: array_callback

    type(json_value),pointer :: element !! temp variable for getting elements
    integer(IK) :: i      !! counter
    integer(IK) :: count  !! number of elements in the array

    if ( json%exception_thrown ) return

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

    recursive subroutine json_get_array_by_path(json, me, path, array_callback, found)

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
                                  ' Unable to resolve path: '//trim(path),found)
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

    end subroutine json_get_array_by_path
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_get_array_by_path]], where "path" is kind=CDK

    recursive subroutine wrap_json_get_array_by_path(json, me, path, array_callback, found)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer,intent(in)  :: me
    character(kind=CDK,len=*),intent(in) :: path
    procedure(json_array_callback_func)  :: array_callback
    logical(LK),intent(out),optional     :: found

    call json%get(me, to_unicode(path), array_callback, found)

    end subroutine wrap_json_get_array_by_path
!*****************************************************************************************

!*****************************************************************************************
!>
!  Internal routine to be called before parsing JSON.
!  Currently, all this does it allocate the `comment_char` if none was specified.

    subroutine json_prepare_parser(json)

    implicit none

    class(json_core),intent(inout) :: json

    if (json%allow_comments .and. .not. allocated(json%comment_char)) then
        ! comments are enabled, but user hasn't set the comment char,
        ! so in this case use the default:
        json%comment_char = CK_'/!#'
    end if

    end subroutine json_prepare_parser
!*****************************************************************************************

!*****************************************************************************************
!>
!  Parse the JSON file and populate the [[json_value]] tree.
!
!### Inputs
!
!  The inputs can be:
!
!  * `file` & `unit` : the specified unit is used to read JSON from file.
!                      [note if unit is already open, then the filename is ignored]
!  * `file`          : JSON is read from file using internal unit number
!
!### Example
!
!````fortran
!    type(json_core) :: json
!    type(json_value),pointer :: p
!    call json%load(file='myfile.json', p=p)
!````
!
!### History
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
    logical(LK) :: has_duplicate  !! if checking for duplicate keys
    character(kind=CK,len=:),allocatable :: path !! path to any duplicate key

    ! clear any exceptions and initialize:
    call json%initialize()
    call json%prepare_parser()

    if ( present(unit) ) then

        if (unit==0) then
            call json%throw_exception('Error in json_parse_file: unit number must not be 0.')
            return
        end if

        iunit = unit

        ! check to see if the file is already open
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
            ! if the file is already open, then we need to make sure
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

        if (use_unformatted_stream) then
            ! save the file size to be read:
            inquire(unit=iunit, size=json%filesize, iostat=istat)
        end if

        ! create the value and associate the pointer
        call json_value_create(p)

        ! Note: the name of the root json_value doesn't really matter,
        !  but we'll allocate something here just in case.
        p%name = trim(file)  !use the file name

        ! parse as a value
        call json%parse_value(unit=iunit, str=CK_'', value=p)
        call json%parse_end(unit=iunit, str=CK_'')

        ! check for errors:
        if (json%exception_thrown) then
            call json%annotate_invalid_json(iunit,CK_'')
        else
            if (.not. json%allow_duplicate_keys) then
                call json%check_for_duplicate_keys(p,has_duplicate,path=path)
                if (.not. json%exception_thrown) then
                    if (has_duplicate) then
                        call json%throw_exception('Error in json_parse_file: '//&
                                                  'Duplicate key found: '//path)
                    end if
                end if
            end if
        end if

        ! close the file:
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
!### See also
!  * [[json_parse_file]]

    subroutine json_parse_string(json, p, str)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer            :: p     !! output structure
    character(kind=CK,len=*),intent(in) :: str   !! string with JSON data

    integer(IK),parameter :: iunit = 0 !! indicates that json data will be read from buffer

    logical(LK) :: has_duplicate  !! if checking for duplicate keys
    character(kind=CK,len=:),allocatable :: path !! path to any duplicate key

    ! clear any exceptions and initialize:
    call json%initialize()
    call json%prepare_parser()

    ! create the value and associate the pointer
    call json_value_create(p)

    ! Note: the name of the root json_value doesn't really matter,
    !  but we'll allocate something here just in case.
    p%name = CK_''

    ! parse as a value
    call json%parse_value(unit=iunit, str=str, value=p)
    call json%parse_end(unit=iunit, str=str)

    if (json%exception_thrown) then
        call json%annotate_invalid_json(iunit,str)
    else
        if (.not. json%allow_duplicate_keys) then
            call json%check_for_duplicate_keys(p,has_duplicate,path=path)
            if (.not. json%exception_thrown) then
                if (has_duplicate) then
                    call json%throw_exception('Error in json_parse_string: '//&
                                              'Duplicate key found: '//path)
                end if
            end if
        end if
    end if

    end subroutine json_parse_string
!*****************************************************************************************

!*****************************************************************************************
!>
!  An error checking routine to call after a file (or string) has been parsed.
!  It will throw an exception if there are any other non-whitespace characters
!  in the file.

    subroutine json_parse_end(json, unit, str)

    implicit none

    class(json_core),intent(inout)      :: json
    integer(IK),intent(in)              :: unit   !! file unit number
    character(kind=CK,len=*),intent(in) :: str    !! string containing JSON
                                                  !! data (only used if `unit=0`)

    logical(LK)              :: eof !! end-of-file flag
    character(kind=CK,len=1) :: c   !! character read from file
                                    !! (or string) by [[pop_char]]

    ! first check for exceptions:
    if (json%exception_thrown) return

    ! pop the next non whitespace character off the file
    call json%pop_char(unit, str=str, eof=eof, skip_ws=.true., &
                        skip_comments=json%allow_comments, popped=c)

    if (.not. eof) then
        call json%throw_exception('Error in json_parse_end:'//&
                                  ' Unexpected character found after parsing value. "'//&
                                  c//'"')
    end if

    end subroutine json_parse_end
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_parse_string]], where `str` is kind=CDK.

    subroutine wrap_json_parse_string(json, p, str)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer             :: p     !! output structure
    character(kind=CDK,len=*),intent(in) :: str   !! string with JSON data

    call json%deserialize(p,to_unicode(str))

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

    character(kind=CK,len=:),allocatable :: line      !! line containing the error
    character(kind=CK,len=:),allocatable :: arrow_str !! arrow string that points
                                                      !! to the current character
    character(kind=CK,len=max_integer_str_len) :: line_str !! current line number string
    character(kind=CK,len=max_integer_str_len) :: char_str !! current character count string
    integer(IK) :: i          !! line number counter
    integer(IK) :: i_nl_prev  !! index of previous newline character
    integer(IK) :: i_nl       !! index of current newline character

    ! If there was an error reading the file, then
    ! print the line where the error occurred:
    if (json%exception_thrown) then

        !the counters for the current line and the last character read:
        call integer_to_string(json%line_count, int_fmt, line_str)
        call integer_to_string(json%char_count, int_fmt, char_str)

        !draw the arrow string that points to the current character:
        arrow_str = repeat('-',max( 0_IK, json%char_count - 1_IK) )//'^'

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
            line = CK_''
        end if

        ! add a newline for the error display if necessary:
        line = trim(line)
        if (len(line)>0) then
            i = len(line)
            if (line(i:i)/=newline) line = line//newline
        else
            line = line//newline
        end if

        !create the error message:
        if (allocated(json%err_message)) then
            json%err_message = json%err_message//newline
        else
            json%err_message = ''
        end if
        json%err_message = json%err_message//&
                           'line: '//trim(adjustl(line_str))//', '//&
                           'character: '//trim(adjustl(char_str))//newline//&
                           line//arrow_str

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

    subroutine get_current_line_from_file_sequential(iunit,line)

    implicit none

    integer(IK),intent(in)                           :: iunit  !! file unit number
    character(kind=CK,len=:),allocatable,intent(out) :: line   !! current line

    character(kind=CK,len=seq_chunk_size) :: chunk !! for reading line in chunks
    integer(IK) :: istat  !! iostat flag
    integer(IK) :: isize  !! number of characters read in read statement

    !initialize:
    line = CK_''

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

    integer(IK)              :: istart  !! start position of current line
    integer(IK)              :: iend    !! end position of current line
    integer(IK)              :: ios     !! file read `iostat` code
    character(kind=CK,len=1) :: c       !! a character read from the file
    logical                  :: done    !! flag to exit the loop

    istart = json%ipos
    do
        if (istart<=1) then
            istart = 1
            exit
        end if
        read(iunit,pos=istart,iostat=ios) c
        done = ios /= 0_IK
        if (.not. done) done = c==newline
        if (done) then
            if (istart/=1) istart = istart - 1
            exit
        end if
        istart = istart-1  !rewind until the beginning of the line
    end do
    iend = json%ipos
    do
        read(iunit,pos=iend,iostat=ios) c
        if (IS_IOSTAT_END(ios)) then
            ! account for end of file without linebreak
            iend=iend-1
            exit
        end if
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
    character(kind=CK,len=*),intent(in) :: str    !! string containing JSON
                                                  !! data (only used if `unit=0`)
    type(json_value),pointer            :: value  !! JSON data that is extracted

    logical(LK)              :: eof !! end-of-file flag
    character(kind=CK,len=1) :: c   !! character read from file
                                    !! (or string) by [[pop_char]]
#if defined __GFORTRAN__
    character(kind=CK,len=:),allocatable :: tmp  !! this is a work-around for a bug
                                                 !! in the gfortran 4.9 compiler.
#endif

    if (.not. json%exception_thrown) then

        !the routine is being called incorrectly.
        if (.not. associated(value)) then
            call json%throw_exception('Error in parse_value: value pointer not associated.')
            return
        end if

        ! pop the next non whitespace character off the file
        call json%pop_char(unit, str=str, eof=eof, skip_ws=.true., &
                           skip_comments=json%allow_comments, popped=c)

        if (eof) then
            return
        else

            select case (c)

            case (start_object)

                ! start object
                call json%to_object(value)    !allocate class
                call json%parse_object(unit, str, value)

            case (start_array)

                ! start array
                call json%to_array(value)    !allocate class
                call json%parse_array(unit, str, value)

            case (end_array)

                ! end an empty array
                call json%push_char(c)
                if (associated(value)) then
                    deallocate(value)
                    nullify(value)
                end if

            case (quotation_mark)

                ! string
                call json%to_string(value)    !allocate class

                select case (value%var_type)
                case (json_string)
#if defined __GFORTRAN__
                    ! write to a tmp variable because of
                    ! a bug in 4.9 gfortran compiler.
                    call json%parse_string(unit,str,tmp)
                    value%str_value = tmp
                    if (allocated(tmp))  deallocate(tmp)
#else
                    call json%parse_string(unit,str,value%str_value)
#endif
                end select

            case (CK_'t') !true_str(1:1) gfortran bug work around

                !true
                call json%parse_for_chars(unit, str, true_str(2:))
                !allocate class and set value:
                if (.not. json%exception_thrown) call json%to_logical(value,.true.)

            case (CK_'f') !false_str(1:1) gfortran bug work around

                !false
                call json%parse_for_chars(unit, str, false_str(2:))
                !allocate class and set value:
                if (.not. json%exception_thrown) call json%to_logical(value,.false.)

            case (CK_'n') !null_str(1:1) gfortran bug work around

                !null
                call json%parse_for_chars(unit, str, null_str(2:))
                if (.not. json%exception_thrown) call json%to_null(value) ! allocate class

            case(CK_'-', CK_'0': CK_'9', CK_'.', CK_'+')

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
!### Example
!````fortran
!     type(json_value),pointer :: p
!     type(json_core) :: json
!     call json%create_logical(p,'value',.true.)
!````

    subroutine json_value_create_logical(json,p,val,name)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer            :: p
    logical(LK),intent(in)              :: val   !! variable value
    character(kind=CK,len=*),intent(in) :: name  !! variable name

    call json_value_create(p)
    call json%to_logical(p,val,name)

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
    logical(LK),intent(in)               :: val
    character(kind=CDK,len=*),intent(in) :: name

    call json%create_logical(p,val,to_unicode(name))

    end subroutine wrap_json_value_create_logical
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Allocate a [[json_value]] pointer and make it an integer(IK) variable.
!  The pointer should not already be allocated.
!
!### Example
!````fortran
!     type(json_value),pointer :: p
!     type(json_core) :: json
!     call json%create_integer(p,'value',1)
!````

    subroutine json_value_create_integer(json,p,val,name)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer            :: p
    integer(IK),intent(in)              :: val
    character(kind=CK,len=*),intent(in) :: name

    call json_value_create(p)
    call json%to_integer(p,val,name)

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
    integer(IK),intent(in)               :: val
    character(kind=CDK,len=*),intent(in) :: name

    call json%create_integer(p,val,to_unicode(name))

    end subroutine wrap_json_value_create_integer
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Allocate a [[json_value]] pointer and make it a real(RK) variable.
!  The pointer should not already be allocated.
!
!### Example
!````fortran
!     type(json_value),pointer :: p
!     type(json_core) :: json
!     call json%create_real(p,'value',1.0_RK)
!````

    subroutine json_value_create_real(json,p,val,name)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer            :: p
    real(RK),intent(in)                 :: val
    character(kind=CK,len=*),intent(in) :: name

    call json_value_create(p)
    call json%to_real(p,val,name)

    end subroutine json_value_create_real
!*****************************************************************************************

!*****************************************************************************************
!> author: Izaak Beekman
!
!  A wrapper for [[json_value_create_real]] so that `create_real` method
!  may be called with an actual argument corresponding to the dummy argument,
!  `name` that may be of 'DEFAULT' or 'ISO_10646' character kind.

    subroutine wrap_json_value_create_real(json,p,val,name)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer             :: p
    real(RK),intent(in)                  :: val
    character(kind=CDK,len=*),intent(in) :: name

    call json%create_real(p,val,to_unicode(name))

    end subroutine wrap_json_value_create_real
!*****************************************************************************************

#ifndef REAL32
!*****************************************************************************************
!>
!  Alternate version of [[json_value_create_real]] where val=real32.
!
!@note The value is converted into a `real(RK)` variable internally.

    subroutine json_value_create_real32(json,p,val,name)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer            :: p
    real(real32),intent(in)             :: val
    character(kind=CK,len=*),intent(in) :: name

    call json%create_real(p,real(val,RK),name)

    end subroutine json_value_create_real32
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_value_create_real32]] where "name" is kind(CDK).

    subroutine wrap_json_value_create_real32(json,p,val,name)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer             :: p
    real(real32),intent(in)              :: val
    character(kind=CDK,len=*),intent(in) :: name

    call json%create_real(p,val,to_unicode(name))

    end subroutine wrap_json_value_create_real32
!*****************************************************************************************
#endif

#ifdef REAL128
!*****************************************************************************************
!>
!  Alternate version of [[json_value_create_real]] where val=real64.
!
!@note The value is converted into a `real(RK)` variable internally.

    subroutine json_value_create_real64(json,p,val,name)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer            :: p
    real(real64),intent(in)             :: val
    character(kind=CK,len=*),intent(in) :: name

    call json%create_real(p,real(val,RK),name)

    end subroutine json_value_create_real64
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_value_create_real64]] where "name" is kind(CDK).

    subroutine wrap_json_value_create_real64(json,p,val,name)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer             :: p
    real(real64),intent(in)              :: val
    character(kind=CDK,len=*),intent(in) :: name

    call json%create_real(p,val,to_unicode(name))

    end subroutine wrap_json_value_create_real64
!*****************************************************************************************
#endif

!*****************************************************************************************
!> author: Jacob Williams
!
!  Allocate a json_value pointer and make it a string variable.
!  The pointer should not already be allocated.
!
!### Example
!````fortran
!     type(json_value),pointer :: p
!     type(json_core) :: json
!     call json%create_string(p,'value','hello')
!````

    subroutine json_value_create_string(json,p,val,name,trim_str,adjustl_str)

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer            :: p
    character(kind=CK,len=*),intent(in) :: val
    character(kind=CK,len=*),intent(in) :: name
    logical(LK),intent(in),optional     :: trim_str      !! if TRIM() should be called for the `val`
    logical(LK),intent(in),optional     :: adjustl_str   !! if ADJUSTL() should be called for the `val`

    call json_value_create(p)
    call json%to_string(p,val,name,trim_str,adjustl_str)

    end subroutine json_value_create_string
!*****************************************************************************************

!*****************************************************************************************
!> author: Izaak Beekman
!
!  Wrap [[json_value_create_string]] so that `create_string` method may be called
!  with actual character string arguments for `name` and `val` that are BOTH of
!  'DEFAULT' or 'ISO_10646' character kind.

    subroutine wrap_json_value_create_string(json,p,val,name,trim_str,adjustl_str)

    implicit none

    class(json_core),intent(inout)       :: json
    type(json_value),pointer             :: p
    character(kind=CDK,len=*),intent(in) :: val
    character(kind=CDK,len=*),intent(in) :: name
    logical(LK),intent(in),optional      :: trim_str      !! if TRIM() should be called for the `val`
    logical(LK),intent(in),optional      :: adjustl_str   !! if ADJUSTL() should be called for the `val`

    call json%create_string(p,to_unicode(val),to_unicode(name),trim_str,adjustl_str)

    end subroutine wrap_json_value_create_string
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Allocate a json_value pointer and make it a null variable.
!  The pointer should not already be allocated.
!
!### Example
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
    call json%to_null(p,name)

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
!### Example
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
    call json%to_object(p,name)

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
!### Example
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
    call json%to_array(p,name)

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

    subroutine to_logical(json,p,val,name)

    implicit none

    class(json_core),intent(inout)               :: json
    type(json_value),pointer                     :: p
    logical(LK),intent(in),optional              :: val   !! if the value is also to be set
                                                          !! (if not present, then .false. is used).
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
    if (present(name)) call json%rename(p,name)

    end subroutine to_logical
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Change the [[json_value]] variable to an integer.

    subroutine to_integer(json,p,val,name)

    implicit none

    class(json_core),intent(inout)               :: json
    type(json_value),pointer                     :: p
    integer(IK),intent(in),optional              :: val   !! if the value is also to be set
                                                          !! (if not present, then 0 is used).
    character(kind=CK,len=*),intent(in),optional :: name  !! if the name is also to be changed.

    !set type and value:
    call destroy_json_data(p)
    p%var_type = json_integer
    allocate(p%int_value)
    if (present(val)) then
        p%int_value = val
    else
        p%int_value = 0_IK    !default value
    end if

    !name:
    if (present(name)) call json%rename(p,name)

    end subroutine to_integer
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Change the [[json_value]] variable to a real.

    subroutine to_real(json,p,val,name)

    implicit none

    class(json_core),intent(inout)               :: json
    type(json_value),pointer                     :: p
    real(RK),intent(in),optional                 :: val   !! if the value is also to be set
                                                          !! (if not present, then 0.0_rk is used).
    character(kind=CK,len=*),intent(in),optional :: name  !! if the name is also to be changed.

    !set type and value:
    call destroy_json_data(p)
    p%var_type = json_real
    allocate(p%dbl_value)
    if (present(val)) then
        p%dbl_value = val
    else
        p%dbl_value = 0.0_RK    !default value
    end if

    !name:
    if (present(name)) call json%rename(p,name)

    end subroutine to_real
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Change the [[json_value]] variable to a string.
!
!### Modified
!  * Izaak Beekman : 02/24/2015

    subroutine to_string(json,p,val,name,trim_str,adjustl_str)

    implicit none

    class(json_core),intent(inout)  :: json
    type(json_value),pointer        :: p
    character(kind=CK,len=*),intent(in),optional :: val   !! if the value is also to be set
                                                          !! (if not present, then '' is used).
    character(kind=CK,len=*),intent(in),optional :: name  !! if the name is also to be changed.
    logical(LK),intent(in),optional     :: trim_str       !! if TRIM() should be called for the `val`
                                                          !! (only used if `val` is present)
    logical(LK),intent(in),optional     :: adjustl_str    !! if ADJUSTL() should be called for the `val`
                                                          !! (only used if `val` is present)
                                                          !! (note that ADJUSTL is done before TRIM)

    character(kind=CK,len=:),allocatable :: str !! temp string for `trim()` and/or `adjustl()`
    logical :: trim_string    !! if the string is to be trimmed
    logical :: adjustl_string !! if the string is to be adjusted left

    !set type and value:
    call destroy_json_data(p)
    p%var_type = json_string
    if (present(val)) then

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

        if (trim_string .or. adjustl_string) then
            str = val
            if (adjustl_string) str = adjustl(str)
            if (trim_string)    str = trim(str)
            p%str_value = str
        else
            p%str_value = val
        end if

    else
        p%str_value = CK_''  ! default value
    end if

    !name:
    if (present(name)) call json%rename(p,name)

    end subroutine to_string
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Change the [[json_value]] variable to a null.

    subroutine to_null(json,p,name)

    implicit none

    class(json_core),intent(inout)               :: json
    type(json_value),pointer                     :: p
    character(kind=CK,len=*),intent(in),optional :: name  !! if the name is also to be changed.

    !set type and value:
    call destroy_json_data(p)
    p%var_type = json_null

    !name:
    if (present(name)) call json%rename(p,name)

    end subroutine to_null
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Change the [[json_value]] variable to an object.

    subroutine to_object(json,p,name)

    implicit none

    class(json_core),intent(inout)               :: json
    type(json_value),pointer                     :: p
    character(kind=CK,len=*),intent(in),optional :: name  !! if the name is also to be changed.

    !set type and value:
    call destroy_json_data(p)
    p%var_type = json_object

    !name:
    if (present(name)) call json%rename(p,name)

    end subroutine to_object
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Change the [[json_value]] variable to an array.

    subroutine to_array(json,p,name)

    implicit none

    class(json_core),intent(inout)               :: json
    type(json_value),pointer                     :: p
    character(kind=CK,len=*),intent(in),optional :: name  !! if the name is also to be changed.

    !set type and value:
    call destroy_json_data(p)
    p%var_type = json_array

    !name:
    if (present(name)) call json%rename(p,name)

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

    type(json_value),pointer :: pair  !! temp variable
    logical(LK)              :: eof   !! end of file flag
    character(kind=CK,len=1) :: c     !! character returned by [[pop_char]]
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
        call json%pop_char(unit, str=str, eof=eof, skip_ws=.true., &
                            skip_comments=json%allow_comments, popped=c)
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
        call json%pop_char(unit, str=str, eof=eof, skip_ws=.true., &
                            skip_comments=json%allow_comments, popped=c)
        if (eof) then
            call json%destroy(pair)
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
            call json%destroy(pair)
            call json%throw_exception('Error in parse_object:'//&
                                      ' Expecting : and then a value: '//c)
            return
        end if

        ! another possible pair
        call json%pop_char(unit, str=str, eof=eof, skip_ws=.true., &
                            skip_comments=json%allow_comments, popped=c)
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

    type(json_value),pointer :: element !! temp variable for array element
    logical(LK)              :: eof     !! end of file flag
    character(kind=CK,len=1) :: c       !! character returned by [[pop_char]]

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

        ! parse value will deallocate an empty array value
        if (associated(element)) call json%add(array, element)

        ! popped the next character
        call json%pop_char(unit, str=str, eof=eof, skip_ws=.true., &
                           skip_comments=json%allow_comments, popped=c)

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
!### History
!  * Jacob Williams : 6/16/2014 : Added hex validation.
!  * Jacob Williams : 12/3/2015 : Fixed some bugs.
!  * Jacob Williams : 8/23/2015 : `string` is now returned unescaped.
!  * Jacob Williams : 7/21/2018 : moved hex validate to [[unescape_string]].

    subroutine parse_string(json, unit, str, string)

    implicit none

    class(json_core),intent(inout)                   :: json
    integer(IK),intent(in)                           :: unit   !! file unit number (if
                                                               !! parsing from a file)
    character(kind=CK,len=*),intent(in)              :: str    !! JSON string (if parsing
                                                               !! from a string)
    character(kind=CK,len=:),allocatable,intent(out) :: string !! the string (unescaped
                                                               !! if necessary)

    logical(LK)              :: eof      !! end of file flag
    logical(LK)              :: escape   !! for escape string parsing
    character(kind=CK,len=1) :: c        !! character returned by [[pop_char]]
    integer(IK)              :: ip       !! index to put next character,
                                         !! to speed up by reducing the number
                                         !! of character string reallocations.
    character(kind=CK,len=:),allocatable :: error_message !! for string unescaping

    !at least return a blank string if there is a problem:
    string = blank_chunk

    if (.not. json%exception_thrown) then

        !initialize:
        escape = .false.
        ip     = 1

        do

            !get the next character from the file:
            call json%pop_char(unit, str=str, eof=eof, skip_ws=.false., popped=c)

            if (eof) then

                call json%throw_exception('Error in parse_string: Expecting end of string')
                return

            else if (c==quotation_mark .and. .not. escape) then  !end of string

                exit

            else

                !if the string is not big enough, then add another chunk:
                if (ip>len(string)) string = string // blank_chunk

                !append to string:
                string(ip:ip) = c
                ip = ip + 1

                ! check for escape character, so we don't
                ! exit prematurely if escaping a quotation
                ! character:
                if (escape) then
                    escape = .false.
                else
                    escape = (c==backslash)
                end if

            end if

        end do

        !trim the string if necessary:
        if (ip<len(string)+1) then
            if (ip==1) then
                string = CK_''
            else
                string = string(1:ip-1)
            end if
        end if

        ! string is returned unescaped:
        ! (this will also validate any hex strings present)
        call unescape_string(string,error_message)
        if (allocated(error_message)) then
            call json%throw_exception(error_message)
            deallocate(error_message)  !cleanup
        end if

    end if

    end subroutine parse_string
!*****************************************************************************************

!*****************************************************************************************
!>
!  Core parsing routine.
!
!  This is used to verify the strings `true`, `false`, and `null` during parsing.

    subroutine parse_for_chars(json, unit, str, chars)

    implicit none

    class(json_core),intent(inout)      :: json
    integer(IK),intent(in)              :: unit   !! file unit number (if parsing from a file)
    character(kind=CK,len=*),intent(in) :: str    !! JSON string (if parsing from a string)
    character(kind=CK,len=*),intent(in) :: chars  !! the string to check for.

    integer(IK) :: i               !! counter
    integer(IK) :: length          !! trimmed length of `chars`
    logical(LK) :: eof             !! end of file flag
    character(kind=CK,len=1) :: c  !! character returned by [[pop_char]]

    if (.not. json%exception_thrown) then

        length = len_trim(chars)

        do i = 1, length
            call json%pop_char(unit, str=str, eof=eof, skip_ws=.false., popped=c)
            if (eof) then
                call json%throw_exception('Error in parse_for_chars:'//&
                                     ' Unexpected end of file while parsing.')
                return
            else if (c /= chars(i:i)) then
                call json%throw_exception('Error in parse_for_chars:'//&
                                     ' Unexpected character: "'//c//'" (expecting "'//&
                                     chars(i:i)//'")')
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
!  The routine will determine if it is an integer or a real, and
!  allocate the type accordingly.
!
!@note Complete rewrite of the original FSON routine, which had some problems.

    subroutine parse_number(json, unit, str, value)

    implicit none

    class(json_core),intent(inout)      :: json
    integer(IK),intent(in)              :: unit   !! file unit number (if parsing from a file)
    character(kind=CK,len=*),intent(in) :: str    !! JSON string (if parsing from a string)
    type(json_value),pointer            :: value

    character(kind=CK,len=:),allocatable :: tmp !! temp string
    character(kind=CK,len=:),allocatable :: saved_err_message !! temp error message for
                                                              !! string to int conversion
    character(kind=CK,len=1) :: c           !! character returned by [[pop_char]]
    logical(LK)              :: eof         !! end of file flag
    real(RK)                 :: rval        !! real value
    integer(IK)              :: ival        !! integer value
    logical(LK)              :: first       !! first character
    logical(LK)              :: is_integer  !! it is an integer
    integer(IK)              :: ip          !! index to put next character
                                            !! [to speed up by reducing the number
                                            !! of character string reallocations]

    if (.not. json%exception_thrown) then

        tmp = blank_chunk
        ip = 1
        first = .true.
        is_integer = .true.  !assume it may be an integer, unless otherwise determined

        !read one character at a time and accumulate the string:
        do

            !get the next character:
            call json%pop_char(unit, str=str, eof=eof, skip_ws=.true., popped=c)

            select case (c)
            case(CK_'-',CK_'+')    !note: allowing a '+' as the first character here.

                if (is_integer .and. (.not. first)) is_integer = .false.

                !add it to the string:
                !tmp = tmp // c   !...original
                if (ip>len(tmp)) tmp = tmp // blank_chunk
                tmp(ip:ip) = c
                ip = ip + 1

            case(CK_'.',CK_'E',CK_'e',CK_'D',CK_'d')    !can be present in real numbers

                if (is_integer) is_integer = .false.

                !add it to the string:
                !tmp = tmp // c   !...original
                if (ip>len(tmp)) tmp = tmp // blank_chunk
                tmp(ip:ip) = c
                ip = ip + 1

            case(CK_'0':CK_'9')    !valid characters for numbers

                !add it to the string:
                !tmp = tmp // c   !...original
                if (ip>len(tmp)) tmp = tmp // blank_chunk
                tmp(ip:ip) = c
                ip = ip + 1

            case default

                !push back the last character read:
                call json%push_char(c)

                !string to value:
                if (is_integer) then
                    ! it is an integer:
                    ival = json%string_to_int(tmp)

                    if (json%exception_thrown .and. .not. json%strict_integer_type_checking) then
                        ! if it couldn't be converted to an integer,
                        ! then try to convert it to a real value and see if that works

                        saved_err_message = json%err_message  ! keep the original error message
                        call json%clear_exceptions()          ! clear exceptions
                        rval = json%string_to_dble(tmp)
                        if (json%exception_thrown) then
                            ! restore original error message and continue
                            json%err_message = saved_err_message
                            call json%to_integer(value,ival) ! just so we have something
                        else
                            ! in this case, we return a real
                            call json%to_real(value,rval)
                        end if

                    else
                        call json%to_integer(value,ival)
                    end if

                else
                    ! it is a real:
                    rval = json%string_to_dble(tmp)
                    call json%to_real(value,rval)
                end if

                exit    !finished

            end select

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
!### See also
!  * [[push_char]]
!
!@note This routine ignores non-printing ASCII characters
!      (`iachar<=31`) that are in strings.

    subroutine pop_char(json,unit,str,skip_ws,skip_comments,eof,popped)

    implicit none

    class(json_core),intent(inout)       :: json
    integer(IK),intent(in)               :: unit          !! file unit number (if parsing
                                                          !! from a file)
    character(kind=CK,len=*),intent(in)  :: str           !! JSON string (if parsing from a
                                                          !! string) -- only used if `unit=0`
    logical(LK),intent(in),optional      :: skip_ws       !! to ignore whitespace [default False]
    logical(LK),intent(in),optional      :: skip_comments !! to ignore comment lines [default False]
    logical(LK),intent(out)              :: eof           !! true if the end of the file has
                                                          !! been reached.
    character(kind=CK,len=1),intent(out) :: popped        !! the popped character returned

    integer(IK)              :: ios             !! `iostat` flag
    integer(IK)              :: str_len         !! length of `str`
    character(kind=CK,len=1) :: c               !! a character read from the file (or string)
    logical(LK)              :: ignore          !! if whitespace is to be ignored
    logical(LK)              :: ignore_comments !! if comment lines are to be ignored
    logical(LK)              :: parsing_comment !! if we are in the process
                                                !! of parsing a comment line

    if (.not. json%exception_thrown) then

        eof = .false.
        if (.not. present(skip_ws)) then
            ignore = .false.
        else
            ignore = skip_ws
        end if
        parsing_comment = .false.
        if (.not. present(skip_comments)) then
            ignore_comments = .false.
        else
            ignore_comments = skip_comments
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

                        ! in this case, we read the file in chunks.
                        ! if we already have the character we need,
                        ! then get it from the chunk. Otherwise,
                        ! read in another chunk.
                        if (json%ichunk<1) then
                            ! read in a chunk:
                            json%ichunk = 0
                            if (json%filesize<json%ipos+len(json%chunk)-1) then
                                ! for the last chunk, we resize
                                ! it to the correct size:
                                json%chunk = repeat(space, json%filesize-json%ipos+1)
                            end if
                            read(unit=unit,pos=json%ipos,iostat=ios) json%chunk
                        else
                            ios = 0
                        end if
                        json%ichunk = json%ichunk + 1
                        if (json%ichunk>len(json%chunk)) then
                            ! check this just in case
                            ios = IOSTAT_END
                        else
                            ! get the next character from the chunk:
                            c = json%chunk(json%ichunk:json%ichunk)
                            if (json%ichunk==len(json%chunk)) then
                                json%ichunk = 0 ! reset for next chunk
                            end if
                        end if

                    else
                        ! a formatted read:
                        read(unit=unit,fmt='(A1)',advance='NO',iostat=ios) c
                    end if
                    json%ipos = json%ipos + 1

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

                    ! no character to return
                    json%char_count = 0
                    eof = .true.
                    popped = space ! just to set a value
                    exit

                else if (IS_IOSTAT_EOR(ios) .or. c==newline) then    !end of record

                    json%char_count = 0
                    json%line_count = json%line_count + 1
                    if (ignore_comments) parsing_comment = .false. ! done parsing this comment line
                    cycle

                end if

            end if

            if (ignore_comments .and. (parsing_comment .or. scan(c,json%comment_char,kind=IK)>0_IK) ) then

                ! skipping the comment
                parsing_comment = .true.
                cycle

            else if (any(c == control_chars)) then

                ! non printing ascii characters
                cycle

            else if (ignore .and. c == space) then

                ! ignoring whitespace
                cycle

            else

                ! return the character
                popped = c
                exit

            end if

        end do

    end if

    end subroutine pop_char
!*****************************************************************************************

!*****************************************************************************************
!>
!  Core routine.
!
!### See also
!  * [[pop_char]]
!
!### History
!  * Jacob Williams : 5/3/2015 : replaced original version of this routine.

    subroutine push_char(json,c)

    implicit none

    class(json_core),intent(inout)      :: json
    character(kind=CK,len=1),intent(in) :: c     !! to character to push

    character(kind=CK,len=max_numeric_str_len) :: istr  !! for error printing

    if (.not. json%exception_thrown) then

        if (use_unformatted_stream) then

            !in this case, c is ignored, and we just
            !decrement the stream position counter:
            json%ipos = json%ipos - 1
            json%ichunk = json%ichunk - 1

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

        !character count in the current line
        json%char_count = json%char_count - 1

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
    integer, intent(in), optional  :: io_unit  !! unit number for
                                               !! printing error message

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
