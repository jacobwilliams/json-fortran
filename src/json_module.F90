!*****************************************************************************************
!>author: Jacob Williams
!
!  JSON-FORTRAN: A Fortran 2008 JSON (JavaScript Object Notation) API.
!
!# See also
!  * [json-fortran development site](http://github.com/jacobwilliams/json-fortran)
!  * [json-fortran online documentation](http://jacobwilliams.github.io/json-fortran)
!  * [JSON website](http://www.json.org/)
!  * [JSON validator](http://jsonlint.com/)
!
!# License
!
!    json-fortran License:
!
!    JSON-FORTRAN: A Fortran 2008 JSON API
!
!    http://github.com/jacobwilliams/json-fortran
!
!    Copyright (c) 2014, Jacob Williams
!
!    All rights reserved.
!
!    Redistribution and use in source and binary forms, with or without modification,
!    are permitted provided that the following conditions are met:
!    * Redistributions of source code must retain the above copyright notice, this
!      list of conditions and the following disclaimer.
!    * Redistributions in binary form must reproduce the above copyright notice, this
!      list of conditions and the following disclaimer in the documentation and/or
!      other materials provided with the distribution.
!    * The names of its contributors may not be used to endorse or promote products
!      derived from this software without specific prior written permission.
!    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
!    ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
!    WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
!    DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
!    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
!    (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
!    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
!    ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
!    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
!    SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
!
!    Original FSON License:
!
!    http://github.com/josephalevin/fson
!
!    Copyright (c) 2012 Joseph A. Levin
!
!    Permission is hereby granted, free of charge, to any person obtaining a copy of this
!    software and associated documentation files (the "Software"), to deal in the Software
!    without restriction, including without limitation the rights to use, copy, modify, merge,
!    publish, distribute, sublicense, and/or sell copies of the Software, and to permit
!    persons to whom the Software is furnished to do so, subject to the following conditions:
!
!    The above copyright notice and this permission notice shall be included in all copies or
!    substantial portions of the Software.
!
!    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
!    INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
!    PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
!    LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT
!    OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
!    DEALINGS IN THE SOFTWARE.
!
!# History
!  * Joseph A. Levin : March 2012 : Original FSON code [retrieved on 12/2/2013].
!  * Jacob Williams : 2/8/2014 : Extensive modifications to the original FSON code.
!    The original F95 code was split into four files:
!    fson_path_m.f95, fson_string_m.f95, fson_value_m.f95, and fson.f95.
!    The new code has been extensively updated, refactored and combined into this
!    one module (json_module.f90).
!    Various Fortran 2003/2008 features are now used
!    (e.g., allocatable strings, newunit, generic, class, and abstract interface).
!  * Development continues at: [Github](http://github.com/jacobwilliams/json-fortran)

    module json_module

    use,intrinsic :: iso_fortran_env

    implicit none

    private

    !*********************************************************
    !
    !  USE_UCS4 is an optional preprocessor flag.
    !  When present, Unicode support is enabled.
    !
    !  USAGE
    !    When compiling:
    !    * gfortran -DUSE_UCS4 ...
    !
    !*********************************************************

    integer,parameter :: RK = real64  !! Default real kind [8 bytes]

    integer,parameter :: IK = int32   !! Default integer kind [4 bytes].

    !*********************************************************
    !> 
    !  Processor dependendant 'DEFAULT' character kind.
    !  This is 1 byte for the Intel and Gfortran compilers.
    !
    !# Notes
    !  CK and CDK are the json-fortran character kind and json-fortran default
    !  character kind respectively. Client code must ensure characters of kind=CK
    !  are used for all character variables and strings passed to the json-fortran
    !  library *EXCEPT* for file names which must be of 'DEFAULT' character kind,
    !  provided here as CDK. In particular, any:
    !   * file name
    !   * format statement
    !   * file path
    !  passed to the json-fortran library *MUST* be of type CDK. This
    !  will be the case for all string literals nor prepended with CK_ and only
    !  if ISO 10646 is supported and enabled, will strings of kind CK be different
    !  than CDK
    !
    integer,parameter,public :: CDK = selected_char_kind('DEFAULT')
    !*********************************************************

    !*********************************************************
    !> 
    !  Default logical kind.
    !  This is 4 bytes for the Intel and Gfortran compilers
    !  (and perhaps others).
    !  The declaration ensures a valid kind
    !  if the compiler doesn't have a logical_kinds(3).
    !
    integer,parameter :: LK = logical_kinds(min(3,size(logical_kinds)))
    !*********************************************************
 
    !*********************************************************
    !> 
    !  String kind preprocessor macro.
    !
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
    !  Default character kind used by json-fortran.
    !  If ISO 10646 (UCS4) support is available, use that,
    !  otherwise, gracefully fall back on 'DEFAULT' characters.
    !  Currently only gfortran >= 4.9.2 will correctly support
    !  UCS4 which is stored in 4 bytes.
    !  (and perhaps others).
    !
    !# Notes
    !  CK and CDK are the json-fortran character kind and json-fortran default
    !  character kind respectively. Client code must ensure characters of kind=CK
    !  are used for all character variables and strings passed to the json-fortran
    !  library *EXCEPT* for file names which must be of 'DEFAULT' character kind,
    !  provided here as JDCK. In particular, any:
    !  * json path
    !  * character or string
    !  * object name
    !  passed to the json-fortran library *MUST* be of type CK.

    integer,parameter,public :: CK = selected_char_kind(json_fortran_string_kind)
    !*********************************************************

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
    !
    !  The types of JSON data.
    !
    integer(IK),parameter,public :: json_unknown   = 0  !! Unknown JSON data type (see [[json_file_variable_info]] and [[json_info]])
    integer(IK),parameter,public :: json_null      = 1  !! Null JSON data type (see [[json_file_variable_info]] and [[json_info]])
    integer(IK),parameter,public :: json_object    = 2  !! Object JSON data type (see [[json_file_variable_info]] and [[json_info]])
    integer(IK),parameter,public :: json_array     = 3  !! Array JSON data type (see [[json_file_variable_info]] and [[json_info]])
    integer(IK),parameter,public :: json_logical   = 4  !! Logical JSON data type (see [[json_file_variable_info]] and [[json_info]])
    integer(IK),parameter,public :: json_integer   = 5  !! Integer JSON data type (see [[json_file_variable_info]] and [[json_info]])
    integer(IK),parameter,public :: json_double    = 6  !! Double JSON data type (see [[json_file_variable_info]] and [[json_info]])
    integer(IK),parameter,public :: json_string    = 7  !! String JSON data type (see [[json_file_variable_info]] and [[json_info]])
    !*********************************************************

    !*********************************************************
    !> 
    !  Type used to construct the linked-list JSON structure.
    !  Normally, this should always be a pointer variable.
    !
    !# Example
    !
    !```fortran
    !    type(json_value),pointer :: p
    !    call json_create_object(p,'')  !root
    !    call json_add(p,'year',1805)
    !    call json_add(p,'value',1.0d0)
    !    call json_print(p,'test.json')
    !    call json_destroy(p)
    !```
    !
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
    !> author: Jacob Williams
    !  date: 12/9/2013
    !
    !  The json_file is the main public class that is
    !  used to open a file and get data from it.
    !
    !# Example
    !
    !```fortran
    !    type(json_file) :: json
    !    integer :: ival
    !    real(real64) :: rval
    !    character(len=:),allocatable :: cval
    !    logical :: found
    !    call json%load_file(filename='myfile.json')
    !    call json%print_file() !print to the console
    !    call json%get('var.i',ival,found)
    !    call json%get('var.r(3)',rval,found)
    !    call json%get('var.c',cval,found)
    !    call json%destroy()
    !```

    type,public :: json_file

        private

        !the JSON structure read from the file:
        type(json_value),pointer :: p => null()

    contains

        procedure,public :: load_file => json_file_load

        generic,  public :: load_from_string => MAYBEWRAP(json_file_load_from_string)

        procedure,public :: destroy => json_file_destroy
        procedure,public :: move    => json_file_move_pointer
        generic,public   :: info    => MAYBEWRAP(json_file_variable_info)

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
                                 MAYBEWRAP(json_file_get_string_vec)

        generic,public :: update =>  MAYBEWRAP(json_file_update_integer),  &
                                     MAYBEWRAP(json_file_update_logical),  &
                                     MAYBEWRAP(json_file_update_real),     &
                                     MAYBEWRAP(json_file_update_string)
#     ifdef USE_UCS4
        generic,public :: update => json_file_update_string_name_ascii, &
                                    json_file_update_string_val_ascii
#     endif

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

        !update:
        procedure :: MAYBEWRAP(json_file_update_integer)
        procedure :: MAYBEWRAP(json_file_update_logical)
        procedure :: MAYBEWRAP(json_file_update_real)
        procedure :: MAYBEWRAP(json_file_update_string)
#     ifdef USE_UCS4
        procedure :: json_file_update_string_name_ascii
        procedure :: json_file_update_string_val_ascii
#     endif

        !print_file:
        procedure :: json_file_print_to_console
        procedure :: json_file_print_1
        procedure :: json_file_print_2

    end type json_file
    !*********************************************************

    !*************************************************************************************
    !> 
    !  Array element callback function.  Used by [[json_get_array]].

    abstract interface
        subroutine array_callback_func(element, i, count)
            import :: json_value,IK
            implicit none
            type(json_value), pointer,intent(in) :: element
            integer(IK),intent(in) :: i        !index
            integer(IK),intent(in) :: count    !size of array
        end subroutine array_callback_func
    end interface
    !*************************************************************************************

# ifdef USE_UCS4
    ! Provide a means to convert to UCS4 while concatenating UCS4 and default strings
    interface operator(//)
       module procedure ucs4_join_default, default_join_ucs4
    end interface

    ! Provide a string comparison operator that works with mixed kinds
    interface operator(==)
       module procedure ucs4_comp_default, default_comp_ucs4
    end interface
# endif

    !*************************************************************************************
    !> 
    !  Get a child, either by index or name string.
    !  Both of these return a [[json_value]] pointer.
    !
    !@note Formerly, this was called json_value_get_child
    
    interface json_get_child
        module procedure json_value_get_by_index
        module procedure MAYBEWRAP(json_value_get_by_name_chars)
    end interface json_get_child
    !*************************************************************************************

    !*************************************************************************************
    !> 
    !  Add objects to a linked list of [[json_value]]s.
    !
    !@note Formerly, this was called json_value_add
    
    interface json_add
        module procedure json_value_add_member
        module procedure MAYBEWRAP(json_value_add_integer)
        module procedure MAYBEWRAP(json_value_add_integer_vec)
        module procedure MAYBEWRAP(json_value_add_double)
        module procedure MAYBEWRAP(json_value_add_double_vec)
        module procedure MAYBEWRAP(json_value_add_logical)
        module procedure MAYBEWRAP(json_value_add_logical_vec)
        module procedure MAYBEWRAP(json_value_add_string)
        module procedure MAYBEWRAP(json_value_add_string_vec)
#     ifdef USE_UCS4
        module procedure json_value_add_string_name_ascii
        module procedure json_value_add_string_val_ascii
        module procedure json_value_add_string_vec_name_ascii
        module procedure json_value_add_string_vec_val_ascii
#     endif
    end interface json_add
    !*************************************************************************************

    !*************************************************************************************
    !> 
    !  These are like [[json_add]], except if a child with the same name is
    !  already present, then its value is simply updated.
    !  Note that currently, these only work for scalar variables.
    !  These routines can also change the variable's type (but an error will be
    !  thrown if the existing variable is not a scalar).
    !
    !@note It should not be used to change the type of a variable in an array,
    !      or it may result in an invalid JSON file.
    
    interface json_update
        module procedure MAYBEWRAP(json_update_logical),&
                         MAYBEWRAP(json_update_double),&
                         MAYBEWRAP(json_update_integer),&
                         MAYBEWRAP(json_update_string)
#     ifdef USE_UCS4
        module procedure json_update_string_name_ascii
        module procedure json_update_string_val_ascii
#     endif
    end interface json_update
    !*************************************************************************************

    !*************************************************************************************
    !> 
    !  Get data from a [[json_value]] linked list.
    !
    !@note There are two versions (e.g. [[json_get_integer]] and [[json_get_integer_with_path]]).
    !      The first one gets the value from the [[json_value]] passed into the routine,
    !      while the second one gets the value from the [[json_value]] found by parsing the
    !      path.  The path version is split up into unicode and non-unicode versions.
    
    interface json_get
        module procedure                       MAYBEWRAP(json_get_by_path)
        module procedure json_get_integer,     MAYBEWRAP(json_get_integer_with_path)
        module procedure json_get_integer_vec, MAYBEWRAP(json_get_integer_vec_with_path)
        module procedure json_get_double,      MAYBEWRAP(json_get_double_with_path)
        module procedure json_get_double_vec,  MAYBEWRAP(json_get_double_vec_with_path)
        module procedure json_get_logical,     MAYBEWRAP(json_get_logical_with_path)
        module procedure json_get_logical_vec, MAYBEWRAP(json_get_logical_vec_with_path)
        module procedure json_get_string,      MAYBEWRAP(json_get_string_with_path)
        module procedure json_get_string_vec,  MAYBEWRAP(json_get_string_vec_with_path)
        module procedure json_get_array,       MAYBEWRAP(json_get_array_with_path)
    end interface json_get
    !*************************************************************************************

    !*************************************************************************************
    !> 
    !  Print the json_value structure to an allocatable string.

    interface json_print_to_string
        module procedure json_value_to_string
    end interface
    !*************************************************************************************

    !*************************************************************************************
    !> 
    !  Print the [[json_value]] to a file.
    !
    !# Example
    !
    !```fortran
    !    type(json_value) :: p
    !    !...
    !    call json_print(p,'test.json')  !this is [[json_print_2]]
    !```
    
    interface json_print
        module procedure json_print_1    !input is unit number
        module procedure json_print_2    !input is file name
    end interface
    !*************************************************************************************

    !*************************************************************************************
    !> 
    !  Destructor routine for a [[json_value]] pointer.
    !  This must be called explicitly if it is no longer needed,
    !  before it goes out of scope.  Otherwise, a memory leak will result.
    !
    !# Example
    !
    !  Destroy the [[json_value]] pointer before the variable goes out of scope:
    !```fortran
    !     subroutine example1()
    !     type(json_value),pointer :: p
    !     call json_create_object(p,'')
    !     call json_add(p,'year',2015)
    !     call json_print(p)
    !     call json_destroy(p)
    !     end subroutine example1
    !```
    !
    !  Note: it should NOT be called for a [[json_value]] pointer than has already been
    !  added to another [[json_value]] structure, since doing so may render the
    !  other structure invalid.  Consider the following example:
    !```fortran
    !     subroutine example2(p)
    !     type(json_value),pointer,intent(out) :: p
    !     type(json_value),pointer :: q
    !     call json_create_object(p,'')
    !     call json_add(p,'year',2015)
    !     call json_create_object(q,'q')
    !     call json_add(q,'val',1)
    !     call json_add(p, q)  !add q to p structure
    !     ! do NOT call json_destroy(q) here, because q is
    !     ! now part of the output structure p.  p should be destroyed
    !     ! somewhere upstream by the caller of this routine.
    !     nullify(q) !OK, but not strictly necessary
    !     end subroutine example2
    !```

    interface json_destroy
        module procedure json_value_destroy
    end interface
    !*************************************************************************************

    !*************************************************************************************
    !> 
    !  Remove a [[json_value]] from a linked-list structure.

    interface json_remove
        module procedure json_value_remove
    end interface
    !*************************************************************************************

    !*************************************************************************************
    !> 
    !  If the child variable is present, then remove it.

    interface json_remove_if_present
        module procedure MAYBEWRAP(json_value_remove_if_present)
    end interface
    !*************************************************************************************

    !*************************************************************************************
    !>
    !  Allocate a [[json_value]] pointer and make it a double variable.
    !  The pointer should not already be allocated.
    !
    !# Example
    !
    !```fortran
    !    type(json_value),pointer :: p
    !    call json_create_double(p,'value',1.0d0)
    !```

    interface json_create_double
        module procedure  MAYBEWRAP(json_value_create_double)
    end interface
    !*************************************************************************************

    !*************************************************************************************
    !> 
    !  Allocate a [[json_value]] pointer and make it an array variable.
    !  The pointer should not already be allocated.
    !
    !# Example
    !
    !```fortran
    !     type(json_value),pointer :: p
    !     call json_create(p,'arrayname')
    !```

    interface json_create_array
        module procedure  MAYBEWRAP(json_value_create_array)
    end interface
    !*************************************************************************************

    !*************************************************************************************
    !> 
    !  Allocate a [[json_value]] pointer and make it an object variable.
    !  The pointer should not already be allocated.
    !
    !# Example
    !
    !```fortran
    !     type(json_value),pointer :: p
    !     call json_create(p,'objectname')
    !```
    !
    !@note The name is not significant for the root structure or an array element.
    !      In those cases, an empty string can be used.

    interface json_create_object
        module procedure  MAYBEWRAP(json_value_create_object)
    end interface
    !*************************************************************************************

    !*************************************************************************************
    !> 
    !  Allocate a json_value pointer and make it a null variable.
    !  The pointer should not already be allocated.
    !
    !# Example
    !
    !```fortran
    !     type(json_value),pointer :: p
    !     call json_create_null(p,'value')
    !```

    interface json_create_null
        module procedure  MAYBEWRAP(json_value_create_null)
    end interface
    !*************************************************************************************

    !*************************************************************************************
    !> 
    !  Allocate a json_value pointer and make it a string variable.
    !  The pointer should not already be allocated.
    !
    !# Example
    !
    !```fortran
    !     type(json_value),pointer :: p
    !     call json_create_string(p,'value','foobar')
    !```

    interface json_create_string
        module procedure  MAYBEWRAP(json_value_create_string)
    end interface
    !*************************************************************************************

    !*************************************************************************************
    !> 
    !  Allocate a json_value pointer and make it an integer variable.
    !  The pointer should not already be allocated.
    !
    !# Example
    !
    !```fortran
    !     type(json_value),pointer :: p
    !     call json_create_integer(p,'value',42)
    !```

    interface json_create_integer
        module procedure  MAYBEWRAP(json_value_create_integer)
    end interface
    !*************************************************************************************

    !*************************************************************************************
    !> 
    !  Allocate a json_value pointer and make it a logical variable.
    !  The pointer should not already be allocated.
    !
    !# Example
    !
    !```fortran
    !     type(json_value),pointer :: p
    !     call json_create_logical(p,'value',.true.)
    !```

    interface json_create_logical
        module procedure  MAYBEWRAP(json_value_create_logical)
    end interface
    !*************************************************************************************

    !*************************************************************************************
    !> 
    !  Parse the JSON file and populate the [[json_value]] tree.

    interface json_parse
       module procedure  json_parse_file, MAYBEWRAP(json_parse_string)
    end interface
    !*************************************************************************************

    !*************************************************************************************
    !> 
    !  Convert a 'DEFAULT' kind character input to 'ISO_10646' kind and return it

    interface to_unicode
        module procedure to_uni, to_uni_vec
    end interface
    !*************************************************************************************

    !*************************************************************************************
    !> 
    !  Throw an exception.

    interface throw_exception
       module procedure MAYBEWRAP(json_throw_exception)
    end interface throw_exception
    !*************************************************************************************

    !public routines:
    public :: json_add                   ! add data to a JSON structure
    public :: json_check_for_errors      ! check for error and get error message
    public :: json_clear_exceptions      ! clear exceptions
    public :: json_count                 ! count the number of children
    public :: json_create_array          ! allocate a json_value array
    public :: json_create_double         ! allocate a json_value double
    public :: json_create_integer        ! allocate a json_value integer
    public :: json_create_logical        ! allocate a json_value logical
    public :: json_create_null           ! allocate a json_value null
    public :: json_create_object         ! allocate a json_value object
    public :: json_create_string         ! allocate a json_value string
    public :: json_destroy               ! clear a JSON structure (destructor)
    public :: json_failed                ! check for error
    public :: json_get                   ! get data from the JSON structure
    public :: json_get_child             ! get a child of a json_value
    public :: json_info                  ! get info about a json_value
    public :: json_initialize            ! to initialize the module
    public :: json_parse                 ! read a JSON file and populate the structure
    public :: json_print                 ! print the JSON structure to a file
    public :: json_print_to_string       ! write the JSON structure to a string
    public :: json_remove                ! remove from a JSON structure
    public :: json_remove_if_present     ! remove from a JSON structure (if it is present)
    public :: json_update                ! update a value in a JSON structure
    public :: json_print_error_message   ! 
    public :: to_unicode                 ! Function to convert from 'DEFAULT' to 'ISO_10646' strings

# ifdef USE_UCS4
    public :: operator(//)
    public :: operator(==)
# endif

    character(kind=CDK,len=*),parameter,public :: json_ext = '.json'   !! JSON file extension

    !special JSON characters
    character(kind=CK,len=*),parameter :: space           = ' '
    character(kind=CK,len=*),parameter :: start_object    = '{'
    character(kind=CK,len=*),parameter :: end_object      = '}'
    character(kind=CK,len=*),parameter :: start_array     = '['
    character(kind=CK,len=*),parameter :: end_array       = ']'
    character(kind=CK,len=*),parameter :: delimiter       = ','
    character(kind=CK,len=*),parameter :: colon_char      = ':'
    character(kind=CK,len=*),parameter :: bspace          = achar(8)
    character(kind=CK,len=*),parameter :: horizontal_tab  = achar(9)
    character(kind=CK,len=*),parameter :: newline         = achar(10)
    character(kind=CK,len=*),parameter :: formfeed        = achar(12)
    character(kind=CK,len=*),parameter :: carriage_return = achar(13)
    character(kind=CK,len=*),parameter :: quotation_mark  = achar(34)
    character(kind=CK,len=*),parameter :: slash           = achar(47)
    character(kind=CK,len=*),parameter :: backslash       = achar(92)

    !These were parameters, but gfortran bug (https://gcc.gnu.org/bugzilla/show_bug.cgi?id=65141)
    !necessitates moving them here to be variables
    character(kind=CK,len=4) :: null_str  = 'null'
    character(kind=CK,len=4) :: true_str  = 'true'
    character(kind=CK,len=5) :: false_str = 'false'

    ! Control characters, possibly in unicode
    integer, private :: i_
    character(kind=CK,len=*),parameter :: control_chars(32) = [(achar(i_),i_=1,31), achar(127)]

    !for indenting (Note: jsonlint.com uses 4 spaces)
    integer(IK),parameter :: spaces_per_tab = 2

    !find out the precision of the floating point number system
    !and set safety factors
    integer(IK),parameter :: rp_safety_factor = 1
    integer(IK),parameter :: rp_addl_safety = 1
    integer(IK),parameter :: real_precision = rp_safety_factor*precision(1.0_RK) + &
                                              rp_addl_safety

    !Get the number of possible digits in the exponent when using decimal number system
    integer(IK),parameter :: real_exponent_digits = floor( 1 + log10( &
                                  real(max(maxexponent(1.0_RK),abs(minexponent(1.0_RK))),&
                                  kind=RK) ) )

    !6 = sign + leading 0 + decimal + 'E' + exponent sign + 1 extra
    integer(IK),parameter :: max_numeric_str_len = real_precision + real_exponent_digits + 6
    ! real format set by library initialization
    character(kind=CDK,len=*),parameter :: int_fmt  = '(I0)' !minimum width format for integers
    character(kind=CK, len=*),parameter :: star     = '*'    !for invalid numbers

    !real string printing:
    character(kind=CDK,len=:),allocatable :: real_fmt  !the format string to use for real numbers
                                                       ! [set in json_initialize]
    logical(LK) :: compact_real = .true.   !to use the "compact" form of real numbers for output

    !
    ! Note: the following global variables make this module non thread safe.
    !

    !exception handling [private variables]
    logical(LK) :: is_verbose = .false.                 !if true, all exceptions are immediately printed to console
    logical(LK) :: exception_thrown = .false.           !the error flag
    character(kind=CK,len=:),allocatable :: err_message !the error message

    !temp vars used when parsing lines in file [private variables]
    integer(IK) :: char_count = 0    !character position in the current line
    integer(IK) :: line_count = 1    !lines read counter
    integer(IK) :: pushed_index = 0
    character(kind=CK,len=10) :: pushed_char = ''  !JW : what is this magic number 10??

    integer(IK),parameter :: chunk_size = 100  !! for allocatable strings: allocate chunks of this size
    integer(IK) :: ipos = 1                    !! for allocatable strings: next character to read

    integer(IK),parameter :: unit2str = -1  !! unit number to cause stuff to be 
                                            !! output to strings rather than files.
                                            !! See 9.5.6.12 in the F2003/08 standard
    
    contains
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Destroy the [[json_value]] type.

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
!  date: 12/9/2013
!
!  Destroy the [[json_file]].

    subroutine json_file_destroy(me)

    implicit none

    class(json_file),intent(inout) :: me

    if (associated(me%p)) call json_value_destroy(me%p)

    end subroutine json_file_destroy
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 12/5/2014
!
!  Move the [[json_value]] pointer from one [[json_file]] to another.
!  The "from" pointer is then nullified, but not destroyed.
!
!@note If "from%p" is not associated, then an error is thrown.

    subroutine json_file_move_pointer(to,from)

    implicit none

    class(json_file),intent(inout) :: to
    class(json_file),intent(inout) :: from

    if (associated(from%p)) then
        to%p => from%p
        nullify(from%p)
    else
        call throw_exception('Error in json_file_move_pointer: '//&
                             'pointer is not associated.')
    end if

    end subroutine json_file_move_pointer

!*****************************************************************************************
!> author: Jacob Williams
!  date: 12/9/2013
!
!  Load the JSON data from a file.
!
!# Example
!
!```fortran
!     type(json_file) :: f
!     call f%load_file('my_file.json')
!```

    subroutine json_file_load(me, filename, unit)

    implicit none

    class(json_file),intent(inout)       :: me
    character(kind=CDK,len=*),intent(in) :: filename  !! the filename to open
    integer(IK),intent(in),optional      :: unit      !! the unit to use

    call json_parse(file=filename, p=me%p, unit=unit)

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

    call json_parse(str=str, p=me%p)

    end subroutine json_file_load_from_string
!*****************************************************************************************

!*****************************************************************************************
!> 
!  Alternate version of [[json_file_load_from_string]], where "str" is kind=CDK.

    subroutine wrap_json_file_load_from_string(me, str)

    implicit none

    class(json_file),intent(inout)       :: me
    character(kind=CDK,len=*),intent(in) :: str

    call json_file_load_from_string(me,to_unicode(str))

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

    character(kind=CK,len=:),allocatable :: dummy

    call json_value_print(me%p,iunit=output_unit,str=dummy,indent=1,colon=.true.)

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

    integer(IK) :: i
    character(kind=CK,len=:),allocatable :: dummy

    if (iunit/=unit2str) then
        i = iunit
        call json_value_print(me%p,iunit=i,str=dummy,indent=1,colon=.true.)
    else
        call throw_exception('Error in json_file_print_1: iunit must not be -1.')
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

    integer(IK) :: iunit,istat

    open(newunit=iunit,file=filename,status='REPLACE',iostat=istat FILE_ENCODING )
    if (istat==0) then
        call me%print_file(iunit)    !call the other routine
        close(iunit,iostat=istat)
    else
        call throw_exception('Error in json_file_print_2: could not open file: '//&
                              trim(filename))
    end if

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

    call json_value_to_string(me%p,str)

    end subroutine json_file_print_to_string
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 2/3/2014
!
!  Returns information about a variable in a [[json_file]].

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
        call json_info(p,var_type,n_children)

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

    call json_file_variable_info(me,to_unicode(path),found,var_type,n_children)

    end subroutine wrap_json_file_variable_info
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 2/13/2014
!
!  Returns information about a [[json_value]].

    subroutine json_info(p,var_type,n_children)

    implicit none

    type(json_value),pointer         :: p
    integer(IK),intent(out),optional :: var_type   !! variable type
    integer(IK),intent(out),optional :: n_children !! number of children

    if (present(var_type))    var_type = p%var_type  !variable type
    if (present(n_children))  n_children = json_count(p)  !number of children

    end subroutine json_info
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

    call json_get_by_path(me%p, path=path, p=p, found=found)

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

    call json_file_get_object(me, to_unicode(path), p, found)

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

    call json_get(me%p, path=path, value=val, found=found)

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

    call json_file_get_integer(me, to_unicode(path), val, found)

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

    call json_get(me%p, path, vec, found)

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

    call json_file_get_integer_vec(me, to_unicode(path), vec, found)

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

    call json_get(me%p, path=path, value=val, found=found)

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

    call json_file_get_double(me, to_unicode(path), val, found)

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

    call json_get(me%p, path, vec, found)

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

    call json_file_get_double_vec(me, to_unicode(path), vec, found)

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

    call json_get(me%p, path=path, value=val, found=found)

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

    call json_file_get_logical(me, to_unicode(path), val, found)

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

    call json_get(me%p, path, vec, found)

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

    call json_file_get_logical_vec(me, to_unicode(path), vec, found)

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

    call json_get(me%p, path=path, value=val, found=found)

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

    call json_file_get_string(me, to_unicode(path), val, found)

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

    call json_get(me%p, path, vec, found)

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

    call json_file_get_string_vec(me, to_unicode(path), vec, found)

    end subroutine wrap_json_file_get_string_vec
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 12/4/2013
!
!  Initialize the json-fortran module.
!  The routine must be called before any of the routines are used.
!  It can also be called after using the module and encountering exceptions.
!
!# Modified
!  * Izaak Beekman : 02/24/2015

    subroutine json_initialize(verbose,compact_reals)

    implicit none

    logical(LK),intent(in),optional :: verbose       !! mainly useful for debugging (default is false)
    logical(LK),intent(in),optional :: compact_reals !! to compact the real number strings for output

    character(kind=CDK,len=10) :: w,d,e
    integer(IK) :: istat

    !clear any errors from previous runs:
    call json_clear_exceptions()

# ifdef USE_UCS4
    ! reopen stdout and stderr with utf-8 encoding
    open(output_unit,encoding='utf-8')
    open(error_unit, encoding='utf-8')
# endif

    !Ensure gfortran bug work around "parameters" are set properly
    null_str  = 'null'
    true_str  = 'true'
    false_str = 'false'

    !optional inputs (if not present, values remains unchanged):
    if (present(verbose))       is_verbose   = verbose
    if (present(compact_reals)) compact_real = compact_reals

    ! set the default output/input format for reals:
    !  [this only needs to be done once, since it can't change]
    if (.not. allocated(real_fmt)) then
                      write(w,'(I0)',iostat=istat) max_numeric_str_len
        if (istat==0) write(d,'(I0)',iostat=istat) real_precision
        if (istat==0) write(e,'(I0)',iostat=istat) real_exponent_digits
        if (istat==0) then
            real_fmt = '(E' // trim(w) // '.' // trim(d) // 'E' // trim(e) // ')'
        else
            real_fmt = '(E30.16E3)'  !just use this one (should never happen)
        end if
    end if

    !Just in case, clear these global variables also:
    pushed_index = 0
    pushed_char  = ''
    char_count   = 0
    line_count   = 1
    ipos         = 1

    end subroutine json_initialize
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 12/4/2013
!
!  Clear exceptions in the JSON module.

    subroutine json_clear_exceptions()

    implicit none

    !clear the flag and message:
    exception_thrown = .false.
    err_message = ''

    end subroutine json_clear_exceptions
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 12/4/2013
!
!  Throw an exception in the JSON module.
!  This routine sets the error flag, and prevents any subsequent routine
!  from doing anything, until [[json_clear_exceptions]] is called.

    subroutine json_throw_exception(msg)

    implicit none

    character(kind=CK,len=*),intent(in) :: msg    !the error message

    exception_thrown = .true.
    err_message = trim(msg)

    if (is_verbose) then
        write(*,'(A)') '***********************'
        write(*,'(A)') 'JSON-FORTRAN EXCEPTION: '//trim(msg)
        !call backtrace()     ! gfortran (use -fbacktrace -fall-intrinsics flags)
        !call tracebackqq(-1) ! intel (requires "use ifcore" in this routine)
        write(*,'(A)') '***********************'
   end if

    end subroutine json_throw_exception
!*****************************************************************************************

!*****************************************************************************************
!> 
!  Alternate version of [[json_throw_exception]], where "msg" is kind=CDK.

    subroutine wrap_json_throw_exception(msg)

    implicit none

    character(kind=CDK,len=*),intent(in) :: msg    !the error message

    call json_throw_exception(to_unicode(msg))

    end subroutine wrap_json_throw_exception
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 12/4/2013
!
!  Retrieve error code from the module.
!  This should be called after [[json_parse]] to check for errors.
!  If an error is thrown, before using the module again, [[json_initialize]]
!  should be called to clean up before it is used again.
!
!# Example
!
!```fortran
!     type(json_file) :: json
!     logical :: status_ok
!     character(kind=CK,len=:),allocatable :: error_msg
!     call json%load_file(filename='myfile.json')
!     call json_check_for_errors(status_ok, error_msg)
!     if (.not. status_ok) then
!         write(*,*) 'Error: '//error_msg
!         call json_clear_exceptions()
!         call json%destroy()
!     end if
!```
!
!# See also
!  * [[json_failed]]

    subroutine json_check_for_errors(status_ok, error_msg)

    implicit none

    logical(LK),intent(out) :: status_ok !! true if there were no errors
    character(kind=CK,len=:),allocatable,intent(out) :: error_msg !! the error message (if there were errors)

    status_ok = .not. exception_thrown

    if (.not. status_ok) then
        if (allocated(err_message)) then
            error_msg = err_message
        else
            error_msg = 'Unknown Error'
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
!  Logical function to indicate if an exception has been thrown.
!
!# Example
!
!```fortran
!    type(json_file) :: json
!    logical :: status_ok
!    character(len=:),allocatable :: error_msg
!    call json%load_file(filename='myfile.json')
!    if (json_failed()) then
!        call json_check_for_errors(status_ok, error_msg)
!        write(*,*) 'Error: '//error_msg
!        call json_clear_exceptions()
!        call json%destroy()
!    end if
!```
!
!# See also
!  * [[json_check_for_errors]]
!
    function json_failed() result(failed)

    implicit none

    logical(LK) :: failed

    failed = exception_thrown

    end function json_failed
!*****************************************************************************************

!*****************************************************************************************
!> 
!  Allocate a [[json_value]] pointer variable.
!  This should be called before adding data to it.
!
!# Example
!
!```fortran
!    type(json_value),pointer :: var
!    call json_value_create(var)
!    call to_double(var,1.0d0)
!```
!
!# Notes
!  1. This routine does not check for exceptions.
!  2. The pointer should not already be allocated.

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

    recursive subroutine json_value_destroy(me,destroy_next)

    implicit none

    type(json_value),pointer :: me
    logical(LK),intent(in),optional :: destroy_next  !! if true, then me%next is also destroyed (default is true)

    logical(LK) :: des_next
    type(json_value), pointer :: p

    if (associated(me)) then

        if (present(destroy_next)) then
            des_next = destroy_next
        else
            des_next = .true.
        end if

        if (allocated(me%name)) deallocate(me%name)

        call destroy_json_data(me)

        if (associated(me%children)) then
            do while (me%n_children > 0)
                p => me%children
                me%children => me%children%next
                me%n_children = me%n_children - 1
                call json_value_destroy(p,.false.)
            end do
            nullify(me%children)
            nullify(p)
        end if

        if (associated(me%next) .and. des_next) call json_value_destroy(me%next)

        if (associated(me%previous)) nullify(me%previous)
        if (associated(me%parent))   nullify(me%parent)
        if (associated(me%tail))     nullify(me%tail)

        deallocate(me)

        nullify(me)

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
!```fortran
!     type(json_value),pointer :: json1,json2,p
!     logical :: found
!     !create and populate json1 and json2
!     call json_get(json1,'name',p,found)  ! get pointer to name element of json1
!     call json_remove(p,destroy=.false.)  ! remove it from json1 (don't destroy)
!     call json_add(json2,p)               ! add it to json2
!```
!
!  To remove an object from a JSON structure (and destroy it):
!```fortran
!     type(json_value),pointer :: json1,p
!     logical :: found
!     !create and populate json1
!     call json_get(json1,'name',p,found)  ! get pointer to name element of json1
!     call json_remove(p)                  ! remove and destroy it
!```
!
!# History
!  * Jacob Williams : 12/28/2014 : added destroy optional argument.
!

    subroutine json_value_remove(me,destroy)

    implicit none

    type(json_value),pointer        :: me
    logical(LK),intent(in),optional :: destroy  !! If destroy is not present, it is also destroyed.
                                                !! If destroy is present and true, it is destroyed.
                                                !! If destroy is present and false, it is not destroyed.

    type(json_value),pointer :: parent,previous,next
    logical(LK) :: destroy_it

    if (associated(me)) then

        !optional input argument:
        if (present(destroy)) then
            destroy_it = destroy
        else
            destroy_it = .true.
        end if

        if (associated(me%parent)) then

            parent => me%parent

            if (associated(me%next)) then

                !there are later items in the list:

                next => me%next
                nullify(me%next)

                if (associated(me%previous)) then
                    !there are earlier items in the list
                    previous => me%previous
                    previous%next => next
                    next%previous => previous
                else
                    !this is the first item in the list
                    parent%children => next
                    nullify(next%previous)
                end if

            else

                if (associated(me%previous)) then
                    !there are earlier items in the list:
                    previous => me%previous
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

        if (destroy_it) call json_value_destroy(me)

    end if

    end subroutine json_value_remove
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 12/6/2014
!
!  Given the path string, remove the variable from
!  the [[json_value]] structure, if it exists.

    subroutine json_value_remove_if_present(p,name)

    implicit none

    type(json_value),pointer            :: p
    character(kind=CK,len=*),intent(in) :: name

    type(json_value),pointer :: p_var
    logical(LK) :: found

    call json_get(p,name,p_var,found)
    if (found) call json_remove(p_var)

    end subroutine json_value_remove_if_present
!*****************************************************************************************

!*****************************************************************************************
!> 
!  Alternate version of [[json_value_remove_if_present]], where "name" is kind=CDK.

    subroutine wrap_json_value_remove_if_present(p,name)

    implicit none

    type(json_value),pointer             :: p
    character(kind=CDK,len=*),intent(in) :: name

    call json_value_remove_if_present(p,to_unicode(name))

    end subroutine wrap_json_value_remove_if_present
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

    if (.not. exception_thrown) call json_update(me%p,name,val,found)

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

    call json_file_update_integer(me,to_unicode(name),val,found)

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

    if (.not. exception_thrown) call json_update(me%p,name,val,found)

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

    call json_file_update_logical(me,to_unicode(name),val,found)

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

    if (.not. exception_thrown) call json_update(me%p,name,val,found)

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

    call json_file_update_real(me,to_unicode(name),val,found)

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

    if (.not. exception_thrown) call json_update(me%p,name,val,found)

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

    call json_file_update_string(me,to_unicode(name),to_unicode(val),found)

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

    call json_file_update_string(me,to_unicode(name),val,found)

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

    call json_file_update_string(me,name,to_unicode(val),found)

    end subroutine json_file_update_string_val_ascii
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 12/6/2014
!
!  Given the path string, if the variable is present,
!  and is a scalar, then update its value.
!  If it is not present, then create it and set its value.

    subroutine json_update_logical(p,name,val,found)

    implicit none

    type(json_value),pointer            :: p
    character(kind=CK,len=*),intent(in) :: name
    logical(LK),intent(in)              :: val
    logical(LK),intent(out)             :: found

    type(json_value),pointer :: p_var
    integer(IK) :: var_type

    call json_get(p,name,p_var,found)
    if (found) then

        call json_info(p_var,var_type)
        select case (var_type)
        case (json_null,json_logical,json_integer,json_double,json_string)
            call to_logical(p_var,val)    !update the value
        case default
            found = .false.
            call throw_exception('Error in json_update_logical: '//&
                                 'the variable is not a scalar value')
        end select

    else
        call json_add(p,name,val)   !add the new element
    end if

    end subroutine json_update_logical
!*****************************************************************************************

!*****************************************************************************************
!> 
!  Alternate version of [[json_update_logical]], where "name" is kind=CDK.

    subroutine wrap_json_update_logical(p,name,val,found)

    implicit none

    type(json_value),pointer             :: p
    character(kind=CDK,len=*),intent(in) :: name
    logical(LK),intent(in)               :: val
    logical(LK),intent(out)              :: found

    call json_update_logical(p,to_unicode(name),val,found)

    end subroutine wrap_json_update_logical
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 12/6/2014
!
!  Given the path string, if the variable is present,
!  and is a scalar, then update its value.
!  If it is not present, then create it and set its value.

    subroutine json_update_double(p,name,val,found)

    implicit none

    type(json_value),pointer            :: p
    character(kind=CK,len=*),intent(in) :: name
    real(RK),intent(in)                 :: val
    logical(LK),intent(out)             :: found

    type(json_value),pointer :: p_var
    integer(IK) :: var_type

    call json_get(p,name,p_var,found)
    if (found) then

        call json_info(p_var,var_type)
        select case (var_type)
        case (json_null,json_logical,json_integer,json_double,json_string)
            call to_double(p_var,val)    !update the value
        case default
            found = .false.
            call throw_exception('Error in json_update_double: '//&
                                 'the variable is not a scalar value')
        end select

    else
        call json_add(p,name,val)   !add the new element
    end if

    end subroutine json_update_double
!*****************************************************************************************

!*****************************************************************************************
!> 
!  Alternate version of [[json_update_double]], where "name" is kind=CDK.

    subroutine wrap_json_update_double(p,name,val,found)

    implicit none

    type(json_value),pointer             :: p
    character(kind=CDK,len=*),intent(in) :: name
    real(RK),intent(in)                  :: val
    logical(LK),intent(out)              :: found

    call json_update_double(p,to_unicode(name),val,found)

    end subroutine wrap_json_update_double
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 12/6/2014
!
!  Given the path string, if the variable is present,
!  and is a scalar, then update its value.
!  If it is not present, then create it and set its value.

    subroutine json_update_integer(p,name,val,found)

    implicit none

    type(json_value),pointer            :: p
    character(kind=CK,len=*),intent(in) :: name
    integer(IK),intent(in)              :: val
    logical(LK),intent(out)             :: found

    type(json_value),pointer :: p_var
    integer(IK) :: var_type

    call json_get(p,name,p_var,found)
    if (found) then

        call json_info(p_var,var_type)
        select case (var_type)
        case (json_null,json_logical,json_integer,json_double,json_string)
            call to_integer(p_var,val)    !update the value
        case default
            found = .false.
            call throw_exception('Error in json_update_integer: '//&
                                 'the variable is not a scalar value')
        end select

    else
        call json_add(p,name,val)   !add the new element
    end if

    end subroutine json_update_integer
!*****************************************************************************************

!*****************************************************************************************
!> 
!  Alternate version of [[json_update_integer]], where "name" is kind=CDK.

    subroutine wrap_json_update_integer(p,name,val,found)

    implicit none

    type(json_value),pointer             :: p
    character(kind=CDK,len=*),intent(in) :: name
    integer(IK),intent(in)               :: val
    logical(LK),intent(out)              :: found

    call json_update_integer(p,to_unicode(name),val,found)

    end subroutine wrap_json_update_integer
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 12/6/2014
!
!  Given the path string, if the variable is present,
!  and is a scalar, then update its value.
!  If it is not present, then create it and set its value.

    subroutine json_update_string(p,name,val,found)

    implicit none

    type(json_value),pointer            :: p
    character(kind=CK,len=*),intent(in) :: name
    character(kind=CK,len=*),intent(in) :: val
    logical(LK),intent(out)             :: found

    type(json_value),pointer :: p_var
    integer(IK) :: var_type

    call json_get(p,name,p_var,found)
    if (found) then

        call json_info(p_var,var_type)
        select case (var_type)
        case (json_null,json_logical,json_integer,json_double,json_string)
            call to_string(p_var,val)    !update the value
        case default
            found = .false.
            call throw_exception('Error in json_update_string: '//&
                                 'the variable is not a scalar value')
        end select

    else
        call json_add(p,name,val)   !add the new element
    end if

    end subroutine json_update_string
!*****************************************************************************************

!*****************************************************************************************
!> 
!  Alternate version of [[json_update_string]], where "name" and "value" are kind=CDK.

    subroutine wrap_json_update_string(p,name,val,found)

    implicit none

    type(json_value),pointer             :: p
    character(kind=CDK,len=*),intent(in) :: name
    character(kind=CDK,len=*),intent(in) :: val
    logical(LK),intent(out)              :: found

    call json_update_string(p,to_unicode(name),to_unicode(val),found)

    end subroutine wrap_json_update_string
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_update_string]], where "name" is kind=CDK.

    subroutine json_update_string_name_ascii(p,name,val,found)

    implicit none

    type(json_value),pointer             :: p
    character(kind=CDK,len=*),intent(in) :: name
    character(kind=CK, len=*),intent(in) :: val
    logical(LK),intent(out)              :: found

    call json_update_string(p,to_unicode(name),val,found)

    end subroutine json_update_string_name_ascii
!*****************************************************************************************

!*****************************************************************************************
!> 
!  Alternate version of [[json_update_string]], where "val" is kind=CDK.

    subroutine json_update_string_val_ascii(p,name,val,found)

    implicit none

    type(json_value),pointer             :: p
    character(kind=CK, len=*),intent(in) :: name
    character(kind=CDK,len=*),intent(in) :: val
    logical(LK),intent(out)              :: found

    call json_update_string(p,name,to_unicode(val),found)

    end subroutine json_update_string_val_ascii
!*****************************************************************************************

!*****************************************************************************************
!> 
!  Adds "member" as a child of "me".

    subroutine json_value_add_member(me, member)

    implicit none

    type(json_value),pointer :: me
    type(json_value),pointer :: member  !! the child member to add

    if (.not. exception_thrown) then

        ! associate the parent
        member%parent => me

        ! add to linked list
        if (associated(me%children)) then

            me%tail%next => member
            member%previous => me%tail

        else

            me%children => member
            member%previous => null()  !first in the list

        end if

        ! new member is now the last one in the list
        me%tail => member
        me%n_children = me%n_children + 1

    end if

    end subroutine json_value_add_member
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/19/2014
!
!  Add a real value child to the [[json_value]] variable
!
!@note This routine is part of the public API that can be
!      used to build a JSON structure using [[json_value]] pointers.

    subroutine json_value_add_double(me, name, val)

    implicit none

    type(json_value),pointer            :: me
    character(kind=CK,len=*),intent(in) :: name  !! variable name
    real(RK),intent(in)                 :: val   !! real value

    type(json_value),pointer :: var

    !create the variable:
    call json_value_create(var)
    call to_double(var,val,name)

    !add it:
    call json_add(me, var)

    !cleanup:
    nullify(var)

    end subroutine json_value_add_double
!*****************************************************************************************

!*****************************************************************************************
!> 
!  Alternate version of [[json_value_add_double]] where "name" is kind=CDK.

    subroutine wrap_json_value_add_double(me, name, val)

    implicit none

    type(json_value),pointer             :: me
    character(kind=CDK,len=*),intent(in) :: name  !! variable name
    real(RK),intent(in)                  :: val   !! real value

    call json_value_add_double(me, to_unicode(name), val)

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

    subroutine json_value_add_double_vec(me, name, val)

    implicit none

    type(json_value),pointer            :: me
    character(kind=CK,len=*),intent(in) :: name
    real(RK),dimension(:),intent(in)    :: val

    type(json_value),pointer :: var
    integer(IK) :: i

    !create the variable as an array:
    call json_value_create(var)
    call to_array(var,name)

    !populate the array:
    do i=1,size(val)
        call json_add(var, '', val(i))
    end do

    !add it:
    call json_add(me, var)

    !cleanup:
    nullify(var)

    end subroutine json_value_add_double_vec
!*****************************************************************************************

!*****************************************************************************************
!> 
!  Alternate version of [[json_value_add_double_vec]] where "name" is kind=CDK.

    subroutine wrap_json_value_add_double_vec(me, name, val)

    implicit none

    type(json_value),pointer             :: me
    character(kind=CDK,len=*),intent(in) :: name
    real(RK),dimension(:),intent(in)     :: val

    call json_value_add_double_vec(me, to_unicode(name), val)

    end subroutine wrap_json_value_add_double_vec
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/20/2014
!
!  Add an integer value child to the [[json_value]] variable
!
!@note This routine is part of the public API that can be
!      used to build a JSON structure using [[json_value]] pointers.

    subroutine json_value_add_integer(me, name, val)

    implicit none

    type(json_value),pointer            :: me
    character(kind=CK,len=*),intent(in) :: name
    integer(IK),intent(in)              :: val

    type(json_value),pointer :: var

    !create the variable:
    call json_value_create(var)
    call to_integer(var,val,name)

    !add it:
    call json_add(me, var)

    !cleanup:
    nullify(var)

    end subroutine json_value_add_integer
!*****************************************************************************************

!*****************************************************************************************
!> 
!  Alternate version of [[json_value_add_integer]] where "name" is kind=CDK.

    subroutine wrap_json_value_add_integer(me, name, val)

    implicit none

    type(json_value),pointer             :: me
    character(kind=CDK,len=*),intent(in) :: name
    integer(IK),intent(in)               :: val

    call json_value_add_integer(me, to_unicode(name), val)

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

    subroutine json_value_add_integer_vec(me, name, val)

    implicit none

    type(json_value),pointer            :: me
    character(kind=CK,len=*),intent(in) :: name
    integer(IK),dimension(:),intent(in) :: val

    type(json_value),pointer :: var
    integer(IK) :: i    !counter

    !create the variable as an array:
    call json_value_create(var)
    call to_array(var,name)

    !populate the array:
    do i=1,size(val)
        call json_add(var, '', val(i))
    end do

    !add it:
    call json_add(me, var)

    !cleanup:
    nullify(var)

    end subroutine json_value_add_integer_vec
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_value_add_integer_vec]] where "name" is kind=CDK.

    subroutine wrap_json_value_add_integer_vec(me, name, val)

    implicit none

    type(json_value),pointer             :: me
    character(kind=CDK,len=*),intent(in) :: name
    integer(IK),dimension(:),intent(in)  :: val

    call json_value_add_integer_vec(me, to_unicode(name), val)

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

    subroutine json_value_add_logical(me, name, val)

    implicit none

    type(json_value),pointer            :: me
    character(kind=CK,len=*),intent(in) :: name
    logical(LK),intent(in)              :: val

    type(json_value),pointer :: var

    !create the variable:
    call json_value_create(var)
    call to_logical(var,val,name)

    !add it:
    call json_add(me, var)

    !cleanup:
    nullify(var)

    end subroutine json_value_add_logical
!*****************************************************************************************

!*****************************************************************************************
!> 
!  Alternate version of [[json_value_add_logical]] where "name" is kind=CDK.

    subroutine wrap_json_value_add_logical(me, name, val)

    implicit none

    type(json_value),pointer             :: me
    character(kind=CDK,len=*),intent(in) :: name
    logical(LK),intent(in)               :: val

    call json_value_add_logical(me, to_unicode(name), val)

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

    subroutine json_value_add_logical_vec(me, name, val)

    implicit none

    type(json_value),pointer            :: me
    character(kind=CK,len=*),intent(in) :: name
    logical(LK),dimension(:),intent(in) :: val

    type(json_value),pointer :: var
    integer(IK) :: i    !counter

    !create the variable as an array:
    call json_value_create(var)
    call to_array(var,name)

    !populate the array:
    do i=1,size(val)
        call json_add(var, '', val(i))
    end do

    !add it:
    call json_add(me, var)

    !cleanup:
    nullify(var)

    end subroutine json_value_add_logical_vec
!*****************************************************************************************

!*****************************************************************************************
!> 
!  Alternate version of [[json_value_add_logical_vec]] where "name" is kind=CDK.

    subroutine wrap_json_value_add_logical_vec(me, name, val)

    implicit none

    type(json_value),pointer             :: me
    character(kind=CDK,len=*),intent(in) :: name
    logical(LK),dimension(:),intent(in)  :: val

    call json_value_add_logical_vec(me, to_unicode(name), val)

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

    subroutine json_value_add_string(me, name, val)

    implicit none

    type(json_value),pointer            :: me
    character(kind=CK,len=*),intent(in) :: name
    character(kind=CK,len=*),intent(in) :: val

    type(json_value),pointer :: var
    character(kind=CK,len=:),allocatable :: str

    !add escape characters if necessary:
    call escape_string(val, str)

    !create the variable:
    call json_value_create(var)
    call to_string(var,str,name)

    !add it:
    call json_add(me, var)

    !cleanup:
    nullify(var)

    end subroutine json_value_add_string
!*****************************************************************************************

!*****************************************************************************************
!> 
!  Alternate version of [[json_value_add_string]] where "name" and "val" are kind=CDK.

    subroutine wrap_json_value_add_string(me, name, val)

    implicit none

    type(json_value),pointer             :: me
    character(kind=CDK,len=*),intent(in) :: name
    character(kind=CDK,len=*),intent(in) :: val

    call json_value_add_string(me, to_unicode(name), to_unicode(val))

    end subroutine wrap_json_value_add_string
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_value_add_string]] where "name" is kind=CDK.

    subroutine json_value_add_string_name_ascii(me, name, val)

    implicit none

    type(json_value),pointer             :: me
    character(kind=CDK,len=*),intent(in) :: name
    character(kind=CK, len=*),intent(in) :: val

    call json_value_add_string(me, to_unicode(name), val)

    end subroutine json_value_add_string_name_ascii
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_value_add_string]] where "val" is kind=CDK.

    subroutine json_value_add_string_val_ascii(me, name, val)

    implicit none

    type(json_value),pointer             :: me
    character(kind=CK, len=*),intent(in) :: name
    character(kind=CDK,len=*),intent(in) :: val

    call json_value_add_string(me, name, to_unicode(val))

    end subroutine json_value_add_string_val_ascii
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/21/2014
!
!  Add the escape characters to a string for adding to JSON.

    subroutine escape_string(str_in, str_out)

    implicit none

    character(kind=CK,len=*),intent(in)              :: str_in
    character(kind=CK,len=:),allocatable,intent(out) :: str_out

    integer(IK) :: i,ipos
    character(kind=CK,len=1) :: c

    character(kind=CK,len=*),parameter :: specials = quotation_mark//&
                                                     backslash//&
                                                     slash//&
                                                     bspace//&
                                                     formfeed//&
                                                     newline//&
                                                     carriage_return//&
                                                     horizontal_tab

    !Do a quick scan for the special characters,
    ! if any are present, then process the string,
    ! otherwise, return the string as is.
    if (scan(str_in,specials)>0) then

        str_out = repeat(space,chunk_size)
        ipos = 1

        !go through the string and look for special characters:
        do i=1,len(str_in)

            c = str_in(i:i)    !get next character in the input string

            !if the string is not big enough, then add another chunk:
            if (ipos+3>len(str_out)) str_out = str_out // repeat(space, chunk_size)

            select case(c)
            case(quotation_mark,backslash,slash)
                str_out(ipos:ipos+1) = backslash//c
                ipos = ipos + 2
            case(bspace)
                str_out(ipos:ipos+1) = '\b'
                ipos = ipos + 2
            case(formfeed)
                str_out(ipos:ipos+1) = '\f'
                ipos = ipos + 2
            case(newline)
                str_out(ipos:ipos+1) = '\n'
                ipos = ipos + 2
            case(carriage_return)
                str_out(ipos:ipos+1) = '\r'
                ipos = ipos + 2
            case(horizontal_tab)
                str_out(ipos:ipos+1) = '\t'
                ipos = ipos + 2
            case default
                str_out(ipos:ipos) = c
                ipos = ipos + 1
            end select

        end do

        !trim the string if necessary:
        if (ipos<len(str_out)+1) then
            if (ipos==1) then
                str_out = ''
            else
                str_out = str_out(1:ipos-1)
            end if
        end if

    else

        str_out = str_in

    end if

    end subroutine escape_string
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/19/2014
!
!  Add an array of character strings to the structure.
!
!@note This routine is part of the public API that can be
!      used to build a JSON structure using [[json_value]] pointers.

    subroutine json_value_add_string_vec(me, name, val, trim_str, adjustl_str)

    implicit none

    type(json_value),pointer                         :: me
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
    call json_value_create(var)
    call to_array(var,name)

    !populate the array:
    do i=1,size(val)

        !the string to write:
        str = val(i)
        if (adjustl_string) str = adjustl(str)
        if (trim_string)    str = trim(str)

        !write it:
        call json_add(var, '', str)

        !cleanup
        deallocate(str)

    end do

    !add it:
    call json_add(me, var)

    !cleanup:
    nullify(var)

    end subroutine json_value_add_string_vec
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_value_add_string_vec]] where "name" and "val" are kind=CDK.

    subroutine wrap_json_value_add_string_vec(me, name, val, trim_str, adjustl_str)

    implicit none

    type(json_value),pointer                          :: me
    character(kind=CDK,len=*),intent(in)              :: name
    character(kind=CDK,len=*),dimension(:),intent(in) :: val
    logical(LK),intent(in),optional                   :: trim_str
    logical(LK),intent(in),optional                   :: adjustl_str

    call json_value_add_string_vec(me, to_unicode(name), to_unicode(val), trim_str, adjustl_str)

    end subroutine wrap_json_value_add_string_vec
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_value_add_string_vec]] where "name" is kind=CDK.

    subroutine json_value_add_string_vec_name_ascii(me, name, val, trim_str, adjustl_str)

    implicit none

    type(json_value),pointer                          :: me
    character(kind=CDK,len=*),intent(in)              :: name
    character(kind=CK, len=*),dimension(:),intent(in) :: val
    logical(LK),intent(in),optional                   :: trim_str
    logical(LK),intent(in),optional                   :: adjustl_str

    call json_value_add_string_vec(me, to_unicode(name), val, trim_str, adjustl_str)

    end subroutine json_value_add_string_vec_name_ascii
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_value_add_string_vec]] where "val" is kind=CDK.

    subroutine json_value_add_string_vec_val_ascii(me, name, val, trim_str, adjustl_str)

    implicit none

    type(json_value),pointer                          :: me
    character(kind=CK, len=*),intent(in)              :: name
    character(kind=CDK,len=*),dimension(:),intent(in) :: val
    logical(LK),intent(in),optional                   :: trim_str
    logical(LK),intent(in),optional                   :: adjustl_str

    call json_value_add_string_vec(me, name, to_unicode(val), trim_str, adjustl_str)

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

    pure function json_count(me) result(count)

    implicit none

    integer(IK)                         :: count  !! number of children
    type(json_value),pointer,intent(in) :: me

    count = me%n_children

    end function json_count
!*****************************************************************************************

!*****************************************************************************************
!>
!  Returns a child in the object or array given the index.

    subroutine json_value_get_by_index(me, idx, p)

    implicit none

    type(json_value),pointer,intent(in) :: me   !! object or array JSON data
    integer(IK),intent(in)              :: idx  !! index of the child
    type(json_value),pointer            :: p    !! pointer to the child

    integer(IK) :: i

    nullify(p)

    if (.not. exception_thrown) then

        if (associated(me%children)) then

            p => me%children

            do i = 1, idx - 1

                if (associated(p%next)) then
                    p => p%next
                else
                    call throw_exception('Error in json_value_get_by_index:'//&
                                         ' p%next is not associated.')
                    nullify(p)
                    return
                end if

            end do

        else

            call throw_exception('Error in json_value_get_by_index:'//&
                                 ' me%children is not associated.')

        end if

    end if

    end subroutine json_value_get_by_index
!*****************************************************************************************

!*****************************************************************************************
!>
!  Returns a child in the object or array given the name string.
!
!  It is a case-sensitive search, and the name string is not trimmed.
!  So, for example,
!```fortran
!     'a ' /= 'A ' /= 'a  '
!```
!
!@note The "name" input is not a path, and is not parsed like it is in [[json_get_by_path]].

    subroutine json_value_get_by_name_chars(me, name, p)

    implicit none

    type(json_value),pointer,intent(in) :: me
    character(kind=CK,len=*),intent(in) :: name  !! the name of a child of "me"
    type(json_value),pointer            :: p     !! pointer to the child

    integer(IK) :: i,n_children

    nullify(p)

    if (.not. exception_thrown) then

        if (associated(me)) then

            if (me%var_type==json_object) then
                n_children = json_count(me)
                p => me%children    !start with first one
                do i=1, n_children
                    if (allocated(p%name)) then
                        if (p%name == name) return
                    end if
                    p => p%next
                end do
            end if

            !did not find anything:
            call throw_exception('Error in json_value_get_by_name_chars: '//&
                                 'child variable '//trim(name)//' was not found.')
            nullify(p)

        else
            call throw_exception('Error in json_value_get_by_name_chars: '//&
                                 'pointer is not associated.')
        end if

    end if

    end subroutine json_value_get_by_name_chars
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_value_get_by_name_chars]] where "name" is kind=CDK.

    subroutine wrap_json_value_get_by_name_chars(me, name, p)

    implicit none

    type(json_value),pointer,intent(in)  :: me
    character(kind=CDK,len=*),intent(in) :: name
    type(json_value),pointer             :: p

    call json_value_get_by_name_chars(me,to_unicode(name),p)

    end subroutine wrap_json_value_get_by_name_chars
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 2/12/2014
!
!  Print the [[json_value]] structure to an allocatable string.

    subroutine json_value_to_string(me,str)

    implicit none

    type(json_value),pointer,intent(in)              :: me
    character(kind=CK,len=:),intent(out),allocatable :: str  !! prints structure to this string

    str = ''
    call json_value_print(me, iunit=unit2str, str=str, indent=1, colon=.true.)

    end subroutine json_value_to_string
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 6/20/2014
!
!  Print the [[json_value]] structure to a file.

    subroutine json_print_1(me,iunit)

    implicit none

    type(json_value),pointer,intent(in)  :: me
    integer(IK),intent(in)               :: iunit   !! the file unit (the file must already have been opened, can't be -1).
   
    character(kind=CK,len=:),allocatable :: dummy

    if (iunit/=unit2str) then
        call json_value_print(me,iunit,str=dummy, indent=1, colon=.true.)
    else
        call throw_exception('Error in json_print: iunit must not be -1.')
    end if

    end subroutine json_print_1
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 12/23/2014
!
!  Print the [[json_value]] structure to a file.

    subroutine json_print_2(me,filename)

    implicit none

    type(json_value),pointer,intent(in)  :: me
    character(kind=CDK,len=*),intent(in) :: filename  !! the filename to print to (should not already be open)
    
    integer(IK) :: iunit,istat

    open(newunit=iunit,file=filename,status='REPLACE',iostat=istat FILE_ENCODING )
    if (istat==0) then
        call json_print(me,iunit)
        close(iunit,iostat=istat)
    else
        call throw_exception('Error in json_print: could not open file: '//&
                              trim(filename))
    end if

    end subroutine json_print_2
!*****************************************************************************************

!*****************************************************************************************
!>
!  Print the JSON structure to a string or a file.
!
!# Notes
!  * This is an internal routine called by the wrapper routines
!    [[json_print]] and [[json_value_to_string]].
!  * The reason the str argument is non-optional is because of a
!    bug in v4.9 of the gfortran compiler.

    recursive subroutine json_value_print(me,iunit,str,indent,need_comma,colon,is_array_element)

    implicit none

    type(json_value),pointer,intent(in)  :: me
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

    if (.not. exception_thrown) then

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
        if (present(indent)) then
            tab = indent
        else
            tab = 0
        end if
        !convert to number of spaces:
        spaces = tab*spaces_per_tab

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

        select case (me%var_type)

        case (json_object)

            count = json_count(me)

            if (count==0) then    !special case for empty object

                call write_it( s//start_object//end_object, comma=print_comma )

            else

                call write_it( s//start_object )

                !if an object is in an array, there is an extra tab:
                if (is_array) then
                     tab = tab+1
                     spaces = tab*spaces_per_tab
                end if

                nullify(element)
                element => me%children
                do i = 1, count

                    ! print the name
                    if (allocated(element%name)) then
                        call write_it(repeat(space, spaces)//quotation_mark//&
                                      element%name//quotation_mark//colon_char//space,&
                                      advance=.false.)
                    else
                        call throw_exception('Error in json_value_print:'//&
                                             ' element%name not allocated')
                        nullify(element)
                        return
                    end if

                    ! recursive print of the element
                    call json_value_print(element, iunit=iunit, indent=tab + 1, &
                                          need_comma=i<count, colon=.true., str=str)

                    ! get the next child the list:
                    element => element%next

                end do

                ! [one fewer tab if it isn't an array element]
                if (.not. is_array) s = repeat(space, max(0,spaces-spaces_per_tab))
                call write_it( s//end_object, comma=print_comma )
                nullify(element)

            end if

        case (json_array)

            count = json_count(me)

            if (count==0) then    !special case for empty array

                call write_it( s//start_array//end_array, comma=print_comma )

            else

                call write_it( start_array )

                nullify(element)
                element => me%children
                do i = 1, count

                    ! recursive print of the element
                    call json_value_print(element, iunit=iunit, indent=tab,&
                                          need_comma=i<count, is_array_element=.true., str=str)

                    ! get the next child the list:
                    element => element%next

                end do

                !indent the closing array character:
                call write_it( repeat(space, max(0,spaces-spaces_per_tab))//end_array,&
                               comma=print_comma )
                nullify(element)

            end if

        case (json_null)

            call write_it( s//null_str, comma=print_comma )

        case (json_string)

            if (allocated(me%str_value)) then
                call write_it( s//quotation_mark// &
                               trim(me%str_value)//quotation_mark, comma=print_comma )
            else
                call throw_exception('Error in json_value_print:'//&
                                     ' me%value_string not allocated')
                return
            end if

        case (json_logical)

            if (me%log_value) then
                call write_it( s//true_str, comma=print_comma )
            else
                call write_it( s//false_str, comma=print_comma )
            end if

        case (json_integer)

            call integer_to_string(me%int_value,tmp)

            call write_it( s//trim(tmp), comma=print_comma )

        case (json_double)

            call real_to_string(me%dbl_value,tmp)

            call write_it( s//trim(tmp), comma=print_comma )

        case default

            call throw_exception('Error in json_value_print: unknown data type')

        end select

        !cleanup:
        if (allocated(s)) deallocate(s)

    end if

    contains

    !
    ! write the string to the file (or the output string)
    !
        subroutine write_it(s,advance,comma)

        implicit none

        character(kind=CK,len=*),intent(in) :: s        !string to print
        logical(LK),intent(in),optional     :: advance  !to add line break or not
        logical(LK),intent(in),optional     :: comma    !print comma after the string

        logical(LK) :: add_line_break, add_comma
        character(kind=CK,len=:),allocatable :: s2

        if (present(comma)) then
            add_comma = comma
        else
            add_comma = .false. !default is not to add comma
        end if

        if (present(advance)) then
            add_line_break = advance
        else
            add_line_break = .true. !default is to advance
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
!```fortran
!     type(json_value),pointer :: dat,p
!     logical :: found
!     !...
!     call json_get(dat,'data(2).version',p,found)
!```
!
!# Notes
!  The following special characters are used to denote paths:
!
!```
!  $         - root
!  @         - this
!  .         - child object member
!  [] or ()  - child array element
!```
!
!  Thus, if any of these characters are present in the name key,
!  this routine cannot be used to get the value.
!  In that case, the [[json_get_child]] routines would need to be used.

    subroutine json_get_by_path(me, path, p, found)

    implicit none

    type(json_value),pointer,intent(in)  :: me
    character(kind=CK,len=*),intent(in)  :: path
    type(json_value),pointer,intent(out) :: p
    logical(LK),intent(out),optional     :: found  !! true if it was found

    character(kind=CK,len=1),parameter :: start_array_alt = '('
    character(kind=CK,len=1),parameter :: end_array_alt   = ')'

    integer(IK)              :: i,length,child_i
    character(kind=CK,len=1) :: c
    logical(LK)              :: array
    type(json_value),pointer :: tmp

    if (.not. exception_thrown) then

        nullify(p)

        ! default to assuming relative to this
        p => me

        child_i = 1

        array = .false.

        length = len_trim(path)

        do i=1, length

            c = path(i:i)

            select case (c)
            case (CK_'$')

                ! root
                do while (associated (p%parent))
                    p => p%parent
                end do
                child_i = i + 1

            case (CK_'@')

                ! this
                p => me
                child_i = i + 1

            case (CK_'.')

                ! get child member from p
                if (child_i < i) then
                    nullify(tmp)
                    call json_get_child(p, path(child_i:i-1), tmp)
                    p => tmp
                    nullify(tmp)
                else
                    child_i = i + 1
                    cycle
                end if

                if (.not. associated(p)) then
                    call throw_exception('Error in json_get_by_path:'//&
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
                    call json_get_child(p, path(child_i:i-1), tmp)
                    p => tmp
                    nullify(tmp)
                else
                    child_i = i + 1
                    cycle
                end if
                if (.not. associated(p)) then
                    call throw_exception('Error in json_get_by_path:'//&
                                         ' Error getting array element')
                    exit
                end if
                child_i = i + 1

            case (end_array,end_array_alt)

                if (.not.array) then
                    call throw_exception('Error in json_get_by_path: Unexpected ]')
                    exit
                end if
                array = .false.
                child_i = string_to_integer(path(child_i:i-1))

                nullify(tmp)
                call json_get_child(p, child_i, tmp)
                p => tmp
                nullify(tmp)

                child_i= i + 1

            end select

        end do

        if (exception_thrown) then

            if (present(found)) then
                found = .false.
                call json_clear_exceptions()
            end if

        else

            ! grab the last child if present in the path
            if (child_i <= length) then
                nullify(tmp)
                call json_get_child(p, path(child_i:i-1), tmp)
                p => tmp
                nullify(tmp)
            end if
            if (associated(p)) then
                if (present(found)) found = .true.    !everything seems to be ok
            else
                call throw_exception('Error in json_get_by_path:'//&
                                     ' variable not found: '//trim(path))
                if (present(found)) then
                    found = .false.
                    call json_clear_exceptions()
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

    subroutine wrap_json_get_by_path(me, path, p, found)

    implicit none

    type(json_value),pointer,intent(in)  :: me
    character(kind=CDK,len=*),intent(in) :: path
    type(json_value),pointer,intent(out) :: p
    logical(LK),intent(out),optional     :: found

    call json_get_by_path(me, to_unicode(path), p, found)

    end subroutine wrap_json_get_by_path
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

    function string_to_integer(str) result(ival)

    implicit none

    character(kind=CK,len=*),intent(in) :: str
    integer(IK) :: ival
    
    character(kind=CDK,len=:),allocatable :: digits
    integer(IK) :: ndigits_digits,ndigits,ierr

    if (.not. exception_thrown) then

        ! Compute how many digits we need to read
        ndigits = 2*len_trim(str)
        ndigits_digits = floor(log10(real(ndigits)))+1
        allocate(character(kind=CDK,len=ndigits_digits) :: digits)
        write(digits,'(I0)') ndigits !gfortran will have a runtime error with * edit descriptor here
        ! gfortran bug: '*' edit descriptor for ISO_10646 strings does bad stuff.
        read(str,'(I'//trim(digits)//')',iostat=ierr) ival   !string to integer

        if (ierr/=0) then    !if there was an error
            ival = 0
            call throw_exception('Error in string_to_integer:'//&
                                 ' string cannot be converted to an integer: '//trim(str))
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

    function string_to_double(str) result(rval)

    implicit none

    real(RK)                            :: rval
    character(kind=CK,len=*),intent(in) :: str

    integer(IK) :: ierr

    if (.not. exception_thrown) then

        read(str,fmt=real_fmt,iostat=ierr) rval    !string to double

        if (ierr/=0) then    !if there was an error
            rval = 0.0_RK
            call throw_exception('Error in string_to_double:'//&
                                 ' string cannot be converted to a double: '//trim(str))
        end if

    end if

    end function string_to_double
!*****************************************************************************************

!*****************************************************************************************
!>
!  Get an integer value from a [[json_value]].

    subroutine json_get_integer(me, value)

    implicit none

    type(json_value),pointer,intent(in) :: me
    integer(IK),intent(out)             :: value

    value = 0
    if ( exception_thrown ) return

    select case(me%var_type)
    case (json_integer)
        value = me%int_value
    case (json_double)
        value = int(me%dbl_value)
    case (json_logical)
        if (me%log_value) then
            value = 1
        else
            value = 0
        end if
    case default
        call throw_exception('Error in get_integer:'//&
             ' Unable to resolve value to integer: '//me%name)
    end select

    end subroutine json_get_integer
!*****************************************************************************************

!*****************************************************************************************
!>
!  Get an integer value from a [[json_value]], given the path string.

    subroutine json_get_integer_with_path(me, path, value, found)

    implicit none

    type(json_value),pointer,intent(in) :: me
    character(kind=CK,len=*),intent(in) :: path
    integer(IK),intent(out)             :: value
    logical(LK),intent(out),optional    :: found

    type(json_value),pointer :: p

    value = 0
    if ( exception_thrown ) then
       if ( present(found) ) found = .false.
       return
    end if

    nullify(p)

    call json_get_by_path(me=me, path=path, p=p)

    if (.not. associated(p)) then
        call throw_exception('Error in json_get_integer:'//&
            ' Unable to resolve path: '// trim(path))
    else
        call json_get_integer(p,value)
        nullify(p)
    end if
    if ( exception_thrown ) then
        if ( present(found) ) then
            found = .false.
            call json_clear_exceptions()
        end if
    else
        if ( present(found) ) found = .true.
    end if

    end subroutine json_get_integer_with_path
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_get_integer_with_path]], where "path" is kind=CDK.

    subroutine wrap_json_get_integer_with_path(me, path, value, found)

    implicit none

    type(json_value),pointer,intent(in)  :: me
    character(kind=CDK,len=*),intent(in) :: path
    integer(IK),intent(out)              :: value
    logical(LK),intent(out),optional     :: found

    call json_get_integer_with_path(me, to_unicode(path), value, found)

    end subroutine wrap_json_get_integer_with_path
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 5/14/2014
!
!  Get an integer vector from a [[json_value]].

    subroutine json_get_integer_vec(me, vec)

    implicit none

    type(json_value),pointer                         :: me
    integer(IK),dimension(:),allocatable,intent(out) :: vec

    logical(LK) :: initialized

    initialized = .false.

    if (allocated(vec)) deallocate(vec)

    !the callback function is called for each element of the array:
    call json_get(me, array_callback=get_int_from_array)

    contains

        ! callback function for integer
        subroutine get_int_from_array(element, i, count)
        implicit none

        type(json_value),pointer,intent(in) :: element
        integer(IK),intent(in)              :: i        !index
        integer(IK),intent(in)              :: count    !size of array

        !size the output array:
        if (.not. initialized) then
            allocate(vec(count))
            initialized = .true.
        end if

        !populate the elements:
        call json_get(element, value=vec(i))

        end subroutine get_int_from_array

    end subroutine json_get_integer_vec
!*****************************************************************************************

!*****************************************************************************************
!>
!  Get an integer vector from a [[json_value]], given the path string.

    subroutine json_get_integer_vec_with_path(me, path, vec, found)

    implicit none

    type(json_value),pointer                         :: me
    character(kind=CK,len=*),intent(in)              :: path
    integer(IK),dimension(:),allocatable,intent(out) :: vec
    logical(LK),intent(out),optional                 :: found

    logical(LK) :: initialized

    initialized = .false.

    call json_get(me, path=path, array_callback=get_int_from_array, found=found)

    ! need to duplicate callback function, no other way
    contains

        ! callback function for integer
        subroutine get_int_from_array(element, i, count)
        implicit none

        type(json_value),pointer,intent(in) :: element
        integer(IK),intent(in)              :: i        !index
        integer(IK),intent(in)              :: count    !size of array

        !size the output array:
        if (.not. initialized) then
            allocate(vec(count))
            initialized = .true.
        end if

        !populate the elements:
        call json_get(element, value=vec(i))

        end subroutine get_int_from_array

    end subroutine json_get_integer_vec_with_path
!*****************************************************************************************

!*****************************************************************************************
!> 
!  Alternate version of [[json_get_integer_vec_with_path]], where "path" is kind=CDK

    subroutine wrap_json_get_integer_vec_with_path(me, path, vec, found)

    implicit none

    type(json_value),pointer                         :: me
    character(kind=CDK,len=*),intent(in)             :: path
    integer(IK),dimension(:),allocatable,intent(out) :: vec
    logical(LK),intent(out),optional                 :: found

    call json_get_integer_vec_with_path(me,path=to_unicode(path),vec=vec,found=found)

    end subroutine wrap_json_get_integer_vec_with_path
!*****************************************************************************************

!*****************************************************************************************
!>
!  Get a double value from a [[json_value]].

    subroutine json_get_double(me, value)

    implicit none

    type(json_value),pointer :: me
    real(RK),intent(out)     :: value

    value = 0.0_RK
    if ( exception_thrown ) return

    select case (me%var_type)
    case (json_integer)
        value = me%int_value
    case (json_double)
        value = me%dbl_value
    case (json_logical)
        if (me%log_value) then
            value = 1.0_RK
        else
            value = 0.0_RK
        end if
    case default

        call throw_exception('Error in json_get_double:'//&
                             ' Unable to resolve value to double: '//me%name)

    end select

    end subroutine json_get_double
!*****************************************************************************************

!*****************************************************************************************
!>
!  Get a double value from a [[json_value]], given the path.

    subroutine json_get_double_with_path(me, path, value, found)

    implicit none

    type(json_value),pointer            :: me
    character(kind=CK,len=*),intent(in) :: path
    real(RK),intent(out)                :: value
    logical(LK),intent(out),optional    :: found

    type(json_value),pointer :: p

    value = 0.0_RK
    if ( exception_thrown ) then
        if ( present(found) ) found = .false.
        return
    end if

    nullify(p)

    call json_get_by_path(me=me, path=path, p=p)

    if (.not. associated(p)) then

        call throw_exception('Error in json_get_double:'//&
                             ' Unable to resolve path: '//trim(path))

    else

        call json_get_double(p,value)
        nullify(p)

    end if

    if (exception_thrown) then
        if (present(found)) then
            found = .false.
            call json_clear_exceptions()
        end if
    else
        if (present(found)) found = .true.
    end if

    end subroutine json_get_double_with_path
!*****************************************************************************************

!*****************************************************************************************
!> 
!  Alternate version of [[json_get_double_with_path]], where "path" is kind=CDK

    subroutine wrap_json_get_double_with_path(me, path, value, found)

    implicit none

    type(json_value),pointer             :: me
    character(kind=CDK,len=*),intent(in) :: path
    real(RK),intent(out)                 :: value
    logical(LK),intent(out),optional     :: found

    call json_get_double_with_path(me,to_unicode(path),value,found)

    end subroutine wrap_json_get_double_with_path
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 5/14/2014
!
!  Get a double vector from a [[json_value]].

    subroutine json_get_double_vec(me, vec)

    implicit none

    type(json_value),pointer                      :: me
    real(RK),dimension(:),allocatable,intent(out) :: vec

    logical(LK) :: initialized

    initialized = .false.

    if (allocated(vec)) deallocate(vec)

    !the callback function is called for each element of the array:
    call json_get(me, array_callback=get_double_from_array)

    contains

        ! callback function for double
        subroutine get_double_from_array(element, i, count)
        implicit none

        type(json_value),pointer,intent(in) :: element
        integer(IK),intent(in)              :: i        !index
        integer(IK),intent(in)              :: count    !size of array

        !size the output array:
        if (.not. initialized) then
            allocate(vec(count))
            initialized = .true.
        end if

        !populate the elements:
        call json_get(element, value=vec(i))

        end subroutine get_double_from_array

    end subroutine json_get_double_vec
!*****************************************************************************************

!*****************************************************************************************
!>
!  Get a double vector from a [[json_value]], given the path.

    subroutine json_get_double_vec_with_path(me, path, vec, found)

    implicit none

    type(json_value),pointer                      :: me
    character(kind=CK,len=*),intent(in)           :: path
    real(RK),dimension(:),allocatable,intent(out) :: vec
    logical(LK),intent(out),optional              :: found

    logical(LK) :: initialized

    initialized = .false.

    if (allocated(vec)) deallocate(vec)

    !the callback function is called for each element of the array:
    call json_get(me, path=path, array_callback=get_double_from_array, found=found)

    contains

        ! callback function for double
        subroutine get_double_from_array(element, i, count)
        implicit none

        type(json_value),pointer,intent(in) :: element
        integer(IK),intent(in)              :: i        !index
        integer(IK),intent(in)              :: count    !size of array

        !size the output array:
        if (.not. initialized) then
            allocate(vec(count))
            initialized = .true.
        end if

        !populate the elements:
        call json_get(element, value=vec(i))

        end subroutine get_double_from_array

    end subroutine json_get_double_vec_with_path
!*****************************************************************************************

!*****************************************************************************************
!> 
!  Alternate version of [[json_get_double_vec_with_path]], where "path" is kind=CDK

    subroutine wrap_json_get_double_vec_with_path(me, path, vec, found)

    implicit none

    type(json_value),pointer                      :: me
    character(kind=CDK,len=*),intent(in)          :: path
    real(RK),dimension(:),allocatable,intent(out) :: vec
    logical(LK),intent(out),optional              :: found

    call json_get_double_vec_with_path(me, to_unicode(path), vec, found)

    end subroutine wrap_json_get_double_vec_with_path
!*****************************************************************************************

!*****************************************************************************************
!>
!  Get a logical value from a [[json_value]].

    subroutine json_get_logical(me, value)

    implicit none

    type(json_value),pointer,intent(in) :: me
    logical(LK)                         :: value

    value = .false.
    if ( exception_thrown ) return

    select case (me%var_type)
    case (json_integer)
        value = (me%int_value > 0)
    case (json_logical)
        value = me % log_value
    case default
        call throw_exception('Error in json_get_logical:'//&
                             ' Unable to resolve value to logical: '//me%name)
    end select

    end subroutine json_get_logical
!*****************************************************************************************

!*****************************************************************************************
!>
!  Get a logical value from a [[json_value]], given the path.

    subroutine json_get_logical_with_path(me, path, value, found)

    implicit none

    type(json_value),pointer,intent(in) :: me
    character(kind=CK,len=*),intent(in) :: path
    logical(LK)                         :: value
    logical(LK),intent(out),optional    :: found

    type(json_value),pointer :: p

    value = .false.
    if ( exception_thrown) then
        if ( present(found) ) found = .false.
        return
    end if

    nullify(p)

    call json_get_by_path(me=me, path=path, p=p)

    if (.not. associated(p)) then

        call throw_exception('Error in json_get_logical:'//&
                             ' Unable to resolve path: '//trim(path))

    else

        call json_get_logical(p,value)
        nullify(p)

    end if

    if (exception_thrown) then
        if (present(found)) then
            found = .false.
            call json_clear_exceptions()
        end if
    else
        if (present(found)) found = .true.
    end if

    end subroutine json_get_logical_with_path
!*****************************************************************************************

!*****************************************************************************************
!> 
!  Alternate version of [[json_get_logical_with_path]], where "path" is kind=CDK

    subroutine wrap_json_get_logical_with_path(me, path, value, found)

    implicit none

    type(json_value),pointer,intent(in)  :: me
    character(kind=CDK,len=*),intent(in) :: path
    logical(LK)                          :: value
    logical(LK),intent(out),optional     :: found

    call json_get_logical_with_path(me,to_unicode(path),value,found)

    end subroutine wrap_json_get_logical_with_path
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 5/14/2014
!
!  Get a logical vector from [[json_value]].

    subroutine json_get_logical_vec(me, vec)

    implicit none

    type(json_value),pointer,intent(in)              :: me
    logical(LK),dimension(:),allocatable,intent(out) :: vec

    logical(LK) :: initialized

    initialized = .false.

    if (allocated(vec)) deallocate(vec)

    !the callback function is called for each element of the array:
    call json_get(me, array_callback=get_logical_from_array)

    contains

        ! callback function for logical
        subroutine get_logical_from_array(element, i, count)
        implicit none

        type(json_value),pointer,intent(in) :: element
        integer(IK),intent(in)              :: i        !index
        integer(IK),intent(in)              :: count    !size of array

        !size the output array:
        if (.not. initialized) then
            allocate(vec(count))
            initialized = .true.
        end if

        !populate the elements:
        call json_get(element, value=vec(i))

        end subroutine get_logical_from_array

    end subroutine json_get_logical_vec
!*****************************************************************************************

!*****************************************************************************************
!>
!  Get a logical vector from a [[json_value]], given the path.

    subroutine json_get_logical_vec_with_path(me, path, vec, found)

    implicit none

    type(json_value),pointer,intent(in)              :: me
    character(kind=CK,len=*),intent(in)              :: path
    logical(LK),dimension(:),allocatable,intent(out) :: vec
    logical(LK),intent(out),optional                 :: found

    logical(LK) :: initialized

    initialized = .false.

    if (allocated(vec)) deallocate(vec)

    !the callback function is called for each element of the array:
    call json_get(me, path=path, array_callback=get_logical_from_array, found=found)

    contains

        ! callback function for logical
        subroutine get_logical_from_array(element, i, count)
        implicit none

        type(json_value),pointer,intent(in) :: element
        integer(IK),intent(in)              :: i        !index
        integer(IK),intent(in)              :: count    !size of array

        !size the output array:
        if (.not. initialized) then
            allocate(vec(count))
            initialized = .true.
        end if

        !populate the elements:
        call json_get(element, value=vec(i))

        end subroutine get_logical_from_array

    end subroutine json_get_logical_vec_with_path
!*****************************************************************************************

!*****************************************************************************************
!> 
!  Alternate version of [[json_get_logical_vec_with_path]], where "path" is kind=CDK

    subroutine wrap_json_get_logical_vec_with_path(me, path, vec, found)

    implicit none

    type(json_value),pointer,intent(in)              :: me
    character(kind=CDK,len=*),intent(in)             :: path
    logical(LK),dimension(:),allocatable,intent(out) :: vec
    logical(LK),intent(out),optional                 :: found

    call json_get_logical_vec_with_path(me,to_unicode(path),vec,found)

    end subroutine wrap_json_get_logical_vec_with_path
!*****************************************************************************************

!*****************************************************************************************
!>
!  Get a character string from a [[json_value]].

    subroutine json_get_string(me, value)

    implicit none

    type(json_value),pointer,intent(in)              :: me
    character(kind=CK,len=:),allocatable,intent(out) :: value

    character(kind=CK ,len=:),allocatable :: s,pre,post
    integer(IK) :: j,jprev,n
    character(kind=CK,len=1) :: c

    value = ''
    if ( exception_thrown) return

    select case (me%var_type)

    case (json_string)

        if (allocated(me%str_value)) then

            !get the value as is:
            s = me%str_value

            ! Now, have to remove the escape characters:
            !
            ! '\"'        quotation mark
            ! '\\'        reverse solidus
            ! '\/'        solidus
            ! '\b'        backspace
            ! '\f'        formfeed
            ! '\n'        newline (LF)
            ! '\r'        carriage return (CR)
            ! '\t'        horizontal tab
            ! '\uXXXX'    4 hexadecimal digits
            !

            !initialize:
            n = len(s)
            j = 1

            do

                jprev = j                      !initialize
                j = index(s(j:n),backslash)    !look for an escape character

                if (j>0) then            !an escape character was found

                    !index in full string of the escape character:
                    j = j + (jprev-1)

                    if (j<n) then

                        !save the bit before the escape character:
                        if (j>1) then
                            pre = s( 1 : j-1 )
                        else
                            pre = ''
                        end if

                        !character after the escape character:
                        c = s( j+1 : j+1 )

                        if (any(c == [quotation_mark,backslash,slash, &
                             to_unicode(['b','f','n','r','t'])])) then

                            !save the bit after the escape characters:
                            if (j+2<n) then
                                 post = s(j+2:n)
                            else
                                 post = ''
                            end if

                            select case(c)
                            case (quotation_mark,backslash,slash)
                                !use c as is
                            case (CK_'b')
                                 c = bspace
                            case (CK_'f')
                                 c = formfeed
                            case (CK_'n')
                                 c = newline
                            case (CK_'r')
                                 c = carriage_return
                            case (CK_'t')
                                 c = horizontal_tab
                            end select

                            s = pre//c//post

                            n = n-1    !backslash character has been
                                       ! removed from the string

                        else if (c == 'u') then !expecting 4 hexadecimal digits after
                                                !the escape character    [\uXXXX]

                            !for now, we are just printing them as is
                            ![not checking to see if it is a valid hex value]

                            if (j+5<=n) then
                                j=j+4
                            else
                                call throw_exception('Error in json_get_string:'//&
                                                     ' Invalid hexadecimal sequence'//&
                                                     ' in string: '//trim(c))
                                exit
                            end if

                        else
                            !unknown escape character
                            call throw_exception('Error in json_get_string:'//&
                                                 ' unknown escape sequence in string "'//&
                                                 trim(s)//'" ['//backslash//c//']')
                            exit
                        end if

                        j=j+1    !go to the next character

                        if (j>=n) exit    !finished

                    else
                        !an escape character is the last character in
                        ! the string [this may not be valid syntax,
                        ! but just keep it]
                        exit
                    end if

                else
                    exit    !no more escape characters in the string
                end if

            end do

            if (exception_thrown) then
               if (allocated(value)) deallocate(value)
            else
               value = s
            end if

        else
           call throw_exception('Error in json_get_string:'//&
                ' me%value not allocated')
        end if

    case default
        call throw_exception('Error in json_get_string:'//&
             ' Unable to resolve value to characters: '//me%name)

        ! Note: for the other cases, we could do val to string conversions.

    end select

    !cleanup:
    if (allocated(s)) deallocate(s)
    if (allocated(pre)) deallocate(pre)
    if (allocated(post)) deallocate(post)

    end subroutine json_get_string
!*****************************************************************************************

!*****************************************************************************************
!>
!  Get a character string from a [[json_value]], given the path.

    subroutine json_get_string_with_path(me, path, value, found)

    implicit none

    type(json_value),pointer,intent(in)              :: me
    character(kind=CK,len=*),intent(in)              :: path
    character(kind=CK,len=:),allocatable,intent(out) :: value
    logical(LK),intent(out),optional                 :: found

    type(json_value),pointer :: p

    value = ''
    if ( exception_thrown ) then
        if ( present(found) ) found = .false.
        return
    end if

    nullify(p)

    call json_get_by_path(me=me, path=path, p=p)

    if (.not. associated(p)) then
        call throw_exception('Error in json_get_string:'//&
                             ' Unable to resolve path: '//trim(path))

    else

        call json_get_string(p,value)
        nullify(p)

    end if

    if (allocated(value) .and. .not. exception_thrown) then
        if (present(found)) found = .true.
    else
        if (present(found)) then
            found = .false.
            call json_clear_exceptions()
        end if
    end if

    !cleanup:
    if (associated(p)) nullify(p)

    end subroutine json_get_string_with_path
!*****************************************************************************************

!*****************************************************************************************
!> 
!  Alternate version of [[json_get_string_with_path]], where "path" is kind=CDK

    subroutine wrap_json_get_string_with_path(me, path, value, found)

    implicit none

    type(json_value),pointer,intent(in)              :: me
    character(kind=CDK,len=*),intent(in)             :: path
    character(kind=CK,len=:),allocatable,intent(out) :: value
    logical(LK),intent(out),optional                 :: found

    call json_get_string_with_path(me,to_unicode(path),value,found)

    end subroutine wrap_json_get_string_with_path
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 5/14/2014
!
!  Get a string vector from a [[json_file]].

    subroutine json_get_string_vec(me, vec)

    implicit none

    type(json_value),pointer,intent(in)                           :: me
    character(kind=CK,len=*),dimension(:),allocatable,intent(out) :: vec

    logical(LK) :: initialized

    initialized = .false.

    if (allocated(vec)) deallocate(vec)

    !the callback function is called for each element of the array:
    call json_get(me, array_callback=get_chars_from_array)

    contains

        ! callback function for chars
        subroutine get_chars_from_array(element, i, count)

        implicit none

        type(json_value),pointer,intent(in) :: element
        integer(IK),intent(in)              :: i        !index
        integer(IK),intent(in)              :: count    !size of array

        character(kind=CK,len=:),allocatable :: cval

        !size the output array:
        if (.not. initialized) then
            allocate(vec(count))
            initialized = .true.
        end if

        !populate the elements:
        call json_get(element, value=cval)
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
!  Get a string vector from a [[json_file]], given the path.

    subroutine json_get_string_vec_with_path(me, path, vec, found)

    implicit none

    type(json_value),pointer,intent(in)                           :: me
    character(kind=CK,len=*),intent(in)                           :: path
    character(kind=CK,len=*),dimension(:),allocatable,intent(out) :: vec
    logical(LK),intent(out),optional                              :: found

    logical(LK) :: initialized

    initialized = .false.

    if (allocated(vec)) deallocate(vec)

    !the callback function is called for each element of the array:
    call json_get(me, path=path, array_callback=get_chars_from_array, found=found)

    contains

        ! callback function for chars
        subroutine get_chars_from_array(element, i, count)

        implicit none

        type(json_value),pointer,intent(in) :: element
        integer(IK),intent(in)              :: i        !index
        integer(IK),intent(in)              :: count    !size of array

        character(kind=CK,len=:),allocatable :: cval

        !size the output array:
        if (.not. initialized) then
            allocate(vec(count))
            initialized = .true.
        end if

        !populate the elements:
        call json_get(element, value=cval)
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

    subroutine wrap_json_get_string_vec_with_path(me, path, vec, found)

    implicit none

    type(json_value),pointer,intent(in)                           :: me
    character(kind=CDK,len=*),intent(in)                          :: path
    character(kind=CK,len=*),dimension(:),allocatable,intent(out) :: vec
    logical(LK),intent(out),optional                              :: found

    call json_get_string_vec_with_path(me,to_unicode(path),vec,found)

    end subroutine wrap_json_get_string_vec_with_path
!*****************************************************************************************

!*****************************************************************************************
!>
!  This routine calls the user-supplied [[array_callback_func]] subroutine
!      for each element in the array.
!
!@note For integer, double, logical, and character arrays,
!      higher-level routines are provided (see [[json_get]]), so
!      this routine does not have to be used for those cases.

    subroutine json_get_array(me, array_callback)

    implicit none

    type(json_value),pointer,intent(in) :: me
    procedure(array_callback_func)      :: array_callback

    type(json_value),pointer :: element
    integer(IK) :: i, count

    if ( exception_thrown ) return

    nullify(element)

    select case (me%var_type)
    case (json_array)
        count = json_count(me)
        element => me%children
        do i = 1, count ! callback for each child
            call array_callback(element, i, count)
            element => element%next
        end do
    case default

        call throw_exception('Error in json_get_array:'//&
             ' Resolved value is not an array ')

    end select

    !cleanup:
    if (associated(element)) nullify(element)

    end subroutine json_get_array
!*****************************************************************************************

!*****************************************************************************************
!>
!  This routine calls the user-supplied array_callback subroutine
!  for each element in the array (specified by the path).

    subroutine json_get_array_with_path(me, path, array_callback, found)

    implicit none

    type(json_value),pointer,intent(in) :: me
    character(kind=CK,len=*),intent(in) :: path
    procedure(array_callback_func)      :: array_callback
    logical(LK),intent(out),optional    :: found

    type(json_value),pointer :: p

    if ( exception_thrown ) then
        if ( present(found) ) found = .false.
        return
    end if

    nullify(p)

    ! resolve the path to the value
    call json_get_by_path(me=me, path=path, p=p)

    if (.not. associated(p)) then
        call throw_exception('Error in json_get_array:'//&
             ' Unable to resolve path: '//trim(path))
    else
       call json_get_array(me=p,array_callback=array_callback)
       nullify(p)
    end if
    if ( exception_thrown ) then
        if ( present(found) ) then
            found = .false.
            call json_clear_exceptions()
        end if
    else
        if ( present(found) ) found = .true.
    end if

    end subroutine json_get_array_with_path
!*****************************************************************************************

!*****************************************************************************************
!> 
!  Alternate version of [[json_get_array_with_path]], where "path" is kind=CDK

    subroutine wrap_json_get_array_with_path(me, path, array_callback, found)

    implicit none

    type(json_value),pointer,intent(in)  :: me
    character(kind=CDK,len=*),intent(in) :: path
    procedure(array_callback_func)       :: array_callback
    logical(LK),intent(out),optional     :: found

    call json_get_array_with_path(me, to_unicode(path), array_callback, found)

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
!  * file and unit : the specified unit is used to read JSON from file.
!                    [note if unit is already open, then the filename is ignored]
!  * file          : JSON is read from file using internal unit number
!
!# Example
!
!```fortran
!    type(json_value),pointer :: p
!    call json_parse(file='myfile.json', p=p)
!```
!
!# History
!  * Jacob Williams : 01/13/2015 : added read from string option.
!  * Izaak Beekman  : 03/08/2015 : moved read from string to separate
!    subroutine, and error annotation
!    to separate subroutine.
!
!@note When calling this routine, any exceptions thrown from previous
!      calls will automatically be cleared.

    subroutine json_parse_file(file, p, unit)

    implicit none

    character(kind=CDK,len=*),intent(in) :: file  !! JSON file name
    type(json_value),pointer             :: p     !! output structure
    integer(IK),intent(in),optional      :: unit  !! file unit number (/= 0)

    integer(IK) :: iunit, istat
    logical(LK) :: is_open

    !clear any exceptions and initialize:
    call json_initialize()

    if ( present(unit) ) then

        if (unit==0) then
            call throw_exception('Error in json_parse_file: unit number must not be 0.')
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
        call parse_value(unit=iunit, str=CK_'', value=p)
        if (exception_thrown) call annotate_invalid_json(iunit,CK_'')

        ! close the file if necessary
        close(unit=iunit, iostat=istat)

    else

        call throw_exception('Error in json_parse_file: Error opening file: '//trim(file))
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

    subroutine json_parse_string(p, str)

    implicit none

    type(json_value),pointer            :: p     !! output structure
    character(kind=CK,len=*),intent(in) :: str   !! string with JSON data

    integer(IK),parameter :: iunit = 0 !indicates that json data will be read from buffer

    if ( .not. exception_thrown ) then

        !clear any exceptions and initialize:
        call json_initialize()

        ! create the value and associate the pointer
        call json_value_create(p)

        ! Note: the name of the root json_value doesn't really matter,
        !  but we'll allocate something here just in case.
        p%name = ''

        ! parse as a value
        call parse_value(unit=iunit, str=str, value=p)

        if (exception_thrown) call annotate_invalid_json(iunit,str)

    end if

    end subroutine json_parse_string
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_parse_string]], where "str" is kind=CDK.

    subroutine wrap_json_parse_string(p, str)

    implicit none

    type(json_value),pointer             :: p     !! output structure
    character(kind=CDK,len=*),intent(in) :: str   !! string with JSON data

    call json_parse_string(p,to_unicode(str))

    end subroutine wrap_json_parse_string
!*****************************************************************************************

!*****************************************************************************************
!>
!  Generate a warning message if there was an error parsing a JSON
!  file or string.

    subroutine annotate_invalid_json(iunit,str)

    implicit none

    integer(IK),intent(in) :: iunit             !! file unit number
    character(kind=CK,len=*),intent(in) :: str  !! string with JSON data

    character(kind=CK,len=:),allocatable :: line, arrow_str
    character(kind=CK,len=10) :: line_str, char_str
    integer(IK) :: i, i_nl_prev, i_nl

    !
    !  If there was an error reading the file, then
    !   print the line where the error occurred:
    !
    if (exception_thrown) then

        !the counters for the current line and the last character read:
        call integer_to_string(line_count, line_str)
        call integer_to_string(char_count, char_str)

        !draw the arrow string that points to the current character:
        arrow_str = repeat('-',max( 0, char_count - 1) )//'^'
        
        if (line_count>0 .and. char_count>0) then

            if (iunit/=0) then

                if (use_unformatted_stream) then
                    call get_current_line_from_file_stream(iunit,line)
                else
                    call get_current_line_from_file_sequential(iunit,line)
                end if

            else

                !get the current line from the string:
                ! [this is done by counting the newline characters]
                i_nl_prev = 0  !index of previous newline character
                i_nl = 2  !just in case line_count = 0
                do i=1,line_count
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
        err_message = err_message//newline//&
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

    subroutine get_current_line_from_file_sequential(iunit,line)

    implicit none

    integer(IK),intent(in)                           :: iunit  !! file unit number
    character(kind=CK,len=:),allocatable,intent(out) :: line   !! current line

    integer(IK),parameter               :: n_chunk = 256   ! chunk size [arbitrary]
    character(kind=CDK,len=*),parameter :: nfmt = '(A256)' ! corresponding format statement

    character(kind=CK,len=n_chunk) :: chunk
    integer(IK) :: istat,isize

    !initialize:
    line = ''

    !rewind to beginning of the current record:
    backspace(iunit, iostat=istat)

    !loop to read in all the characters in the current record.
    ![the line is read in chunks until the end of the line is reached]
    if (istat==0) then
        do
            isize=0
            read(iunit,fmt=nfmt,advance='NO',size=isize,iostat=istat) chunk
            if (istat==0) then
                line = line//chunk
            else
                if (isize>0 .and. isize<=n_chunk) line = line//chunk(1:isize)
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

    subroutine get_current_line_from_file_stream(iunit,line)

    implicit none

    integer(IK),intent(in)                           :: iunit  !! file unit number
    character(kind=CK,len=:),allocatable,intent(out) :: line   !! current line

    integer(IK) :: istart,iend,ios
    character(kind=CK,len=1) :: c

    !updated for the new STREAM version:

    istart = ipos
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
    iend = ipos
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

    recursive subroutine parse_value(unit, str, value)

    implicit none

    integer(IK),intent(in)              :: unit   !! file unit number
    character(kind=CK,len=*),intent(in) :: str    !! string containing JSON data (only used if unit=0)
    type(json_value),pointer            :: value  !! JSON data that is extracted

    logical(LK) :: eof
    character(kind=CK,len=1) :: c
    character(kind=CK,len=:),allocatable :: tmp  !this is a work-around for a bug
                                                 ! in the gfortran 4.9 compiler.

    if (.not. exception_thrown) then

        !the routine is being called incorrectly.
        if (.not. associated(value)) then
            call throw_exception('Error in parse_value: value pointer not associated.')
        end if

        ! pop the next non whitespace character off the file
        c = pop_char(unit, str=str, eof = eof, skip_ws = .true.)

        if (eof) then
            return
        else
            select case (c)
            case (start_object)

                ! start object
                call to_object(value)    !allocate class
                call parse_object(unit, str, value)

            case (start_array)

                ! start array
                call to_array(value)    !allocate class
                call parse_array(unit, str, value)

            case (end_array)

                ! end an empty array
                call push_char(c)
                nullify(value)

            case (quotation_mark)

                ! string
                call to_string(value)    !allocate class

                select case (value%var_type)
                case (json_string)
                    call parse_string(unit, str, tmp)  !write to a tmp variable because of
                    value%str_value = tmp              ! a bug in 4.9 gfortran compiler.
                    deallocate(tmp)                    !
                end select

            case (CK_'t') !true_str(1:1) gfortran bug work around

                !true
                call parse_for_chars(unit, str, true_str(2:))
                !allocate class and set value:
                if (.not. exception_thrown) call to_logical(value,.true.)

            case (CK_'f') !false_str(1:1) gfortran bug work around

                !false
                call parse_for_chars(unit, str, false_str(2:))
                !allocate class and set value:
                if (.not. exception_thrown) call to_logical(value,.false.)

            case (CK_'n') !null_str(1:1) gfortran bug work around

                !null
                call parse_for_chars(unit, str, null_str(2:))
                if (.not. exception_thrown) call to_null(value)    !allocate class

            case(CK_'-', CK_'0': CK_'9')

                call push_char(c)
                call parse_number(unit, str, value)

            case default

                call throw_exception('Error in parse_value:'//&
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
!```fortran
!     type(json_value),pointer :: p
!     call json_create(p,'value',.true.)
!```

    subroutine json_value_create_logical(me,val,name)

    implicit none

    type(json_value),pointer            :: me
    character(kind=CK,len=*),intent(in) :: name  !! variable name
    logical(LK),intent(in)              :: val   !! variable value

    call json_value_create(me)
    call to_logical(me,val,name)

    end subroutine json_value_create_logical
!*****************************************************************************************

!*****************************************************************************************
!> author: Izaak Beekman
!
!  Wrapper for [[json_value_create_logical]] so [[json_create_logical]] can
!  be called with name of character kind 'DEFAULT' or 'ISO_10646'

    subroutine wrap_json_value_create_logical(me,val,name)

    implicit none

    type(json_value),pointer             :: me
    character(kind=CDK,len=*),intent(in) :: name
    logical(LK),intent(in)               :: val

    call json_value_create_logical(me,val,to_unicode(name))

    end subroutine wrap_json_value_create_logical
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Allocate a [[json_value]] pointer and make it an integer(IK) variable.
!  The pointer should not already be allocated.
!
!# Example
!```fortran
!     type(json_value),pointer :: p
!     call json_create(p,'value',1)
!```

    subroutine json_value_create_integer(me,val,name)

    implicit none

    type(json_value),pointer            :: me
    character(kind=CK,len=*),intent(in) :: name
    integer(IK),intent(in)              :: val

    call json_value_create(me)
    call to_integer(me,val,name)

    end subroutine json_value_create_integer
!*****************************************************************************************

!*****************************************************************************************
!> author: Izaak Beekman
!
!  A wrapper procedure for [[json_value_create_integer]] so that [[json_create_integer]]
!  may be called with either a 'DEFAULT' or 'ISO_10646' character kind 'name'
!  actual argument.

    subroutine wrap_json_value_create_integer(me,val,name)

    implicit none

    type(json_value),pointer             :: me
    character(kind=CDK,len=*),intent(in) :: name
    integer(IK),intent(in)               :: val

    call json_value_create_integer(me,val,to_unicode(name))

    end subroutine wrap_json_value_create_integer
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Allocate a [[json_value]] pointer and make it a real(RK) variable.
!  The pointer should not already be allocated.
!
!# Example
!```fortran
!     type(json_value),pointer :: p
!     call json_create(p,'value',1.0d0)
!```

    subroutine json_value_create_double(me,val,name)

    implicit none

    type(json_value),pointer            :: me
    character(kind=CK,len=*),intent(in) :: name
    real(RK),intent(in)                 :: val

    call json_value_create(me)
    call to_double(me,val,name)

    end subroutine json_value_create_double
!*****************************************************************************************

!*****************************************************************************************
!> author: Izaak Beekman
!
!  A wrapper for [[json_value_create_double]] so that [[json_create_double]] may be
!  called with an actual argument corresponding to the dummy argument, 'name'
!  that may be of 'DEFAULT' or 'ISO_10646' character kind.

    subroutine wrap_json_value_create_double(me,val,name)

    implicit none

    type(json_value),pointer             :: me
    character(kind=CDK,len=*),intent(in) :: name
    real(RK),intent(in)                  :: val

    call json_value_create_double(me,val,to_unicode(name))

    end subroutine wrap_json_value_create_double
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Allocate a json_value pointer and make it a string variable.
!  The pointer should not already be allocated.
!
!# Example
!```fortran
!     type(json_value),pointer :: p
!     call json_create(p,'value','hello')
!```

    subroutine json_value_create_string(me,val,name)

    implicit none

    type(json_value),pointer            :: me
    character(kind=CK,len=*),intent(in) :: name
    character(kind=CK,len=*),intent(in) :: val

    call json_value_create(me)
    call to_string(me,val,name)

    end subroutine json_value_create_string
!*****************************************************************************************

!*****************************************************************************************
!> author: Izaak Beekman
!
!  Wrap [[json_value_create_string]] so that [[json_create_string]] may be called with actual
!  character string arguments for 'name' and 'val' that are BOTH of 'DEFAULT' or
!  'ISO_10646' character kind.

    subroutine wrap_json_value_create_string(me,val,name)

    implicit none

    type(json_value),pointer             :: me
    character(kind=CDK,len=*),intent(in) :: name
    character(kind=CDK,len=*),intent(in) :: val

    call json_value_create_string(me,to_unicode(val),to_unicode(name))

    end subroutine wrap_json_value_create_string
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Allocate a json_value pointer and make it a null variable.
!  The pointer should not already be allocated.
!
!# Example
!```fortran
!     type(json_value),pointer :: p
!     call json_create(p,'value')
!```

    subroutine json_value_create_null(me,name)

    implicit none

    type(json_value),pointer            :: me
    character(kind=CK,len=*),intent(in) :: name

    call json_value_create(me)
    call to_null(me,name)

    end subroutine json_value_create_null
!*****************************************************************************************

!*****************************************************************************************
!> author: Izaak Beekman
!
!  Wrap [[json_value_create_null]] so that [[json_create_null]] may be called with an actual
!  argument corresponding to the dummy argument 'name' that is either of 'DEFAULT' or
!  'ISO_10646' character kind.

    subroutine wrap_json_value_create_null(me,name)

    implicit none

    type(json_value),pointer             :: me
    character(kind=CDK,len=*),intent(in) :: name

    call json_value_create_null(me,to_unicode(name))

    end subroutine wrap_json_value_create_null
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Allocate a [[json_value]] pointer and make it an object variable.
!  The pointer should not already be allocated.
!
!# Example
!```fortran
!     type(json_value),pointer :: p
!     call json_create(p,'objectname')
!```
!
!@note The name is not significant for the root structure or an array element.
!      In those cases, an empty string can be used.

    subroutine json_value_create_object(me,name)

    implicit none

    type(json_value),pointer            :: me
    character(kind=CK,len=*),intent(in) :: name

    call json_value_create(me)
    call to_object(me,name)

    end subroutine json_value_create_object
!*****************************************************************************************

!*****************************************************************************************
!> author: Izaak Beekman
!
!  Wrap [[json_value_create_object]] so that [[json_create_object]] may be called with an actual
!  argument corresponding to the dummy argument 'name' that is of either 'DEFAULT' or
!  'ISO_10646' character kind.

    subroutine wrap_json_value_create_object(me,name)

    implicit none

    type(json_value),pointer             :: me
    character(kind=CDK,len=*),intent(in) :: name

    call json_value_create_object(me,to_unicode(name))

    end subroutine wrap_json_value_create_object
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Allocate a [[json_value]] pointer and make it an array variable.
!  The pointer should not already be allocated.
!
!# Example
!```fortran
!     type(json_value),pointer :: p
!     call json_create(p,'arrayname')
!```

    subroutine json_value_create_array(me,name)

    implicit none

    type(json_value),pointer            :: me
    character(kind=CK,len=*),intent(in) :: name

    call json_value_create(me)
    call to_array(me,name)

    end subroutine json_value_create_array
!*****************************************************************************************

!*****************************************************************************************
!> author: Izaak Beekman
!
!  A wrapper for [[json_value_create_array]] so that [[json_create_array]] may be called with
!  an actual argument, corresponding to the dummy argument 'name', that is either of
!  'DEFAULT' or 'ISO_10646' character kind.

    subroutine wrap_json_value_create_array(me,name)

    implicit none

    type(json_value),pointer             :: me
    character(kind=CDK,len=*),intent(in) :: name

    call json_value_create_array(me,to_unicode(name))

    end subroutine wrap_json_value_create_array
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Change the [[json_value]] variable to a logical.

    subroutine to_logical(me,val,name)

    implicit none

    type(json_value),intent(inout)               :: me
    logical(LK),intent(in),optional              :: val   !! if the value is also to be set (if not present, then .false. is used).
    character(kind=CK,len=*),intent(in),optional :: name  !! if the name is also to be changed.

    !set type and value:
    call destroy_json_data(me)
    me%var_type = json_logical
    allocate(me%log_value)
    if (present(val)) then
        me%log_value = val
    else
        me%log_value = .false.    !default value
    end if

    !name:
    if (present(name)) me%name = trim(name)

    end subroutine to_logical
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Change the [[json_value]] variable to an integer.

    subroutine to_integer(me,val,name)

    implicit none

    type(json_value),intent(inout)               :: me
    integer(IK),intent(in),optional              :: val   !! if the value is also to be set (if not present, then 0 is used).
    character(kind=CK,len=*),intent(in),optional :: name  !! if the name is also to be changed.

    !set type and value:
    call destroy_json_data(me)
    me%var_type = json_integer
    allocate(me%int_value)
    if (present(val)) then
        me%int_value = val
    else
        me%int_value = 0    !default value
    end if

    !name:
    if (present(name)) me%name = trim(name)

    end subroutine to_integer
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Change the [[json_value]] variable to a double.

    subroutine to_double(me,val,name)

    implicit none

    type(json_value),intent(inout)               :: me
    real(RK),intent(in),optional                 :: val   !! if the value is also to be set (if not present, then 0.0_rk is used).
    character(kind=CK,len=*),intent(in),optional :: name  !! if the name is also to be changed.

    !set type and value:
    call destroy_json_data(me)
    me%var_type = json_double
    allocate(me%dbl_value)
    if (present(val)) then
        me%dbl_value = val
    else
        me%dbl_value = 0.0_RK    !default value
    end if

    !name:
    if (present(name)) me%name = trim(name)

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

    subroutine to_string(me,val,name)

    implicit none

    type(json_value),intent(inout)               :: me
    character(kind=CK,len=*),intent(in),optional :: val   !! if the value is also to be set (if not present, then '' is used).
    character(kind=CK,len=*),intent(in),optional :: name  !! if the name is also to be changed.

    !set type and value:
    call destroy_json_data(me)
    me%var_type = json_string
    if (present(val)) then
        me%str_value = val
    else
        me%str_value = ''    !default value
    end if

    !name:
    if (present(name)) me%name = trim(name)

    end subroutine to_string
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Change the [[json_value]] variable to a null.

    subroutine to_null(me,name)

    implicit none

    type(json_value),intent(inout)               :: me
    character(kind=CK,len=*),intent(in),optional :: name  !! if the name is also to be changed.

    !set type and value:
    call destroy_json_data(me)
    me%var_type = json_null

    !name:
    if (present(name)) me%name = trim(name)

    end subroutine to_null
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Change the [[json_value]] variable to an object.

    subroutine to_object(me,name)

    implicit none

    type(json_value),intent(inout)               :: me
    character(kind=CK,len=*),intent(in),optional :: name  !! if the name is also to be changed.

    !set type and value:
    call destroy_json_data(me)
    me%var_type = json_object

    !name:
    if (present(name)) me%name = trim(name)

    end subroutine to_object
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Change the [[json_value]] variable to an array.

    subroutine to_array(me,name)

    implicit none

    type(json_value),intent(inout)               :: me
    character(kind=CK,len=*),intent(in),optional :: name  !! if the name is also to be changed.

    !set type and value:
    call destroy_json_data(me)
    me%var_type = json_array

    !name:
    if (present(name)) me%name = trim(name)

    end subroutine to_array
!*****************************************************************************************

!*****************************************************************************************
!>
!  Core parsing routine.

    recursive subroutine parse_object(unit, str, parent)

    implicit none

    integer(IK),intent(in)              :: unit    !! file unit number (if parsing from a file)
    character(kind=CK,len=*),intent(in) :: str     !! JSON string (if parsing from a string)
    type(json_value),pointer            :: parent  !! the parsed object will be added as a child of this

    type(json_value),pointer :: pair
    logical(LK) :: eof
    character(kind=CK,len=1) :: c
    character(kind=CK,len=:),allocatable :: tmp  !! this is a work-around for a bug
                                                 !! in the gfortran 4.9 compiler.

    if (.not. exception_thrown) then

        !the routine is being called incorrectly.
        if (.not. associated(parent)) then
            call throw_exception('Error in parse_object: parent pointer not associated.')
        end if

        nullify(pair)    !probably not necessary

        ! pair name
        c = pop_char(unit, str=str, eof = eof, skip_ws = .true.)
        if (eof) then
            call throw_exception('Error in parse_object:'//&
                                 ' Unexpected end of file while parsing start of object.')
            return
        else if (end_object == c) then
            ! end of an empty object
            return
        else if (quotation_mark == c) then
            call json_value_create(pair)
            call parse_string(unit, str, tmp)   !write to a tmp variable because of
            pair % name = tmp                   ! a bug in 4.9 gfortran compiler.
            deallocate(tmp)
            if (exception_thrown) then
                call json_destroy(pair)
                return
            end if
        else
            call throw_exception('Error in parse_object: Expecting string: "'//c//'"')
            return
        end if

        ! pair value
        c = pop_char(unit, str=str, eof = eof, skip_ws = .true.)
        if (eof) then
            call throw_exception('Error in parse_object:'//&
                                 ' Unexpected end of file while parsing object member.')
            return
        else if (colon_char == c) then
            ! parse the value
            call parse_value(unit, str, pair)
            if (exception_thrown) then
                call json_destroy(pair)
                return
            else
                call json_add(parent, pair)
            end if
        else
            call throw_exception('Error in parse_object:'//&
                                 ' Expecting : and then a value: '//c)
            return
        end if

        ! another possible pair
        c = pop_char(unit, str=str, eof = eof, skip_ws = .true.)
        if (eof) then
            call throw_exception('Error in parse_object: '//&
                                 'End of file encountered when parsing an object')
            return
        else if (delimiter == c) then
            ! read the next member
            call parse_object(unit = unit, str=str, parent = parent)
        else if (end_object == c) then
            ! end of object
            return
        else
            call throw_exception('Error in parse_object: Expecting end of object: '//c)
            return
        end if

    end if

    end subroutine parse_object
!*****************************************************************************************

!*****************************************************************************************
!>
!  Core parsing routine.

    recursive subroutine parse_array(unit, str, array)

    implicit none

    integer(IK),intent(in)              :: unit   !! file unit number (if parsing from a file)
    character(kind=CK,len=*),intent(in) :: str    !! JSON string (if parsing from a string)
    type(json_value),pointer            :: array

    type(json_value),pointer :: element
    logical(LK) :: eof
    character(kind=CK,len=1) :: c

    do

        if (exception_thrown) exit

        ! try to parse an element value
        nullify(element)
        call json_value_create(element)
        call parse_value(unit, str, element)
        if (exception_thrown) then
            if (associated(element)) call json_destroy(element)
            exit
        end if

        ! parse value will disassociate an empty array value
        if (associated(element)) call json_add(array, element)

        ! popped the next character
        c = pop_char(unit, str=str, eof = eof, skip_ws = .true.)

        if (eof) then
            ! The file ended before array was finished:
            call throw_exception('Error in parse_array: '//&
                                 'End of file encountered when parsing an array.')
            exit
        else if (delimiter == c) then
            ! parse the next element
            cycle
        else if (end_array == c) then
            ! end of array
            exit
        else
            call throw_exception('Error in parse_array: '//&
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
!

    subroutine parse_string(unit, str, string)

    implicit none

    integer(IK), intent(in)                            :: unit  !! file unit number (if parsing from a file)
    character(kind=CK,len=*),intent(in)                :: str   !! JSON string (if parsing from a string)
    character(kind=CK,len=:),allocatable,intent(out)   :: string

    logical(LK) :: eof, is_hex, escape
    character(kind=CK,len=1) :: c, last
    character(kind=CK,len=4) :: hex
    integer(IK) :: i

    !to speed up by reducing the number of character string reallocations:
    integer(IK) :: ip !index to put next character

    !at least return a blank string if there is a problem:
    string = repeat(space, chunk_size)

    if (.not. exception_thrown) then

        !initialize:
        ip = 1
        last = space
        is_hex = .false.
        escape = .false.
        i = 0

        do

            !get the next character from the file:
            c = pop_char(unit, str=str, eof = eof, skip_ws = .false.)

            if (eof) then

                call throw_exception('Error in parse_string: Expecting end of string')
                return

            else if (c==quotation_mark .and. last /= backslash) then

                if (is_hex) call throw_exception('Error in parse_string:'//&
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
                            call throw_exception('Error in parse_string:'//&
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

                !update for next char:
                last = c

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

    subroutine parse_for_chars(unit, str, chars)

    implicit none

    integer(IK), intent(in)              :: unit   !! file unit number (if parsing from a file) 
    character(kind=CK,len=*),intent(in)  :: str    !! JSON string (if parsing from a string)
    character(kind=CK,len=*), intent(in) :: chars

    integer(IK) :: i, length
    logical(LK) :: eof
    character(kind=CK,len=1) :: c

    if (.not. exception_thrown) then

        length = len_trim(chars)

        do i = 1, length
            c = pop_char(unit, str=str, eof = eof, skip_ws = .true.)
            if (eof) then
                call throw_exception('Error in parse_for_chars:'//&
                                     ' Unexpected end of file while parsing array.')
                return
            else if (c /= chars(i:i)) then
                call throw_exception('Error in parse_for_chars:'//&
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

    subroutine parse_number(unit, str, value)

    implicit none

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

    !to speed up by reducing the number of character string reallocations:
    integer(IK) :: ip !index to put next character

    if (.not. exception_thrown) then

        tmp = repeat(space, chunk_size)
        ip = 1
        first = .true.
        is_integer = .true.  !assume it may be an integer, unless otherwise determined

        !read one character at a time and accumulate the string:
        do

            !get the next character:
            c = pop_char(unit, str=str, eof = eof, skip_ws = .true.)

            if (eof) then
                call throw_exception('Error in parse_number:'//&
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
                    call push_char(c)

                    !string to value:
                    if (is_integer) then
                        ival = string_to_integer(tmp)
                        call to_integer(value,ival)
                    else
                        rval = string_to_double(tmp)
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

    recursive function pop_char(unit, str, eof, skip_ws) result(popped)

    implicit none

    character(kind=CK,len=1)            :: popped  !! the popped character.
    integer(IK),intent(in)              :: unit    !! file unit number (if parsing from a file)
    character(kind=CK,len=*),intent(in) :: str     !! JSON string (if parsing from a string) -- only used if unit=0
    logical(LK),intent(out)             :: eof     !! true if the end of the file has been reached.
    logical(LK),intent(in),optional     :: skip_ws !! to ignore whitespace.

    integer(IK) :: ios,str_len
    character(kind=CK,len=1) :: c
    logical(LK) :: ignore

    if (.not. exception_thrown) then

        eof = .false.
        if (.not. present(skip_ws)) then
            ignore = .false.
        else
            ignore = skip_ws
        end if

        do

            if (pushed_index > 0) then

                ! there is a character pushed back on, most likely from the number parsing
                ! NOTE: this can only occur if reading from a file when use_unformatted_stream=.false.
                c = pushed_char(pushed_index:pushed_index)
                pushed_index = pushed_index - 1

            else

                if (unit/=0) then    !read from the file

                    !read the next character:
                    if (use_unformatted_stream) then
                        read(unit=unit,pos=ipos,iostat=ios) c
                    else
                        read(unit=unit,fmt='(A1)',advance='NO',iostat=ios) c
                    end if
                    ipos = ipos + 1

                    !....note: maybe try read the file in chunks...
                    !.... or use asynchronous read with double buffering
                    !     (see Modern Fortran: Style and Usage)

                else    !read from the string

                    str_len = len(str)   !length of the string
                    if (ipos<=str_len) then
                        c = str(ipos:ipos)
                        ios = 0
                    else
                        ios = IOSTAT_END  !end of the string
                    end if
                    ipos = ipos + 1

                end if

                char_count = char_count + 1    !character count in the current line

                if (IS_IOSTAT_END(ios)) then  !end of file

                    char_count = 0
                    eof = .true.
                    exit

                elseif (IS_IOSTAT_EOR(ios) .or. c==newline) then    !end of record

                    char_count = 0
                    line_count = line_count + 1
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
!

    subroutine push_char(c)

    implicit none

    character(kind=CK,len=1),intent(in) :: c

    character(kind=CK,len=max_numeric_str_len) :: istr

    if (.not. exception_thrown) then

        if (use_unformatted_stream) then

            !in this case, c is ignored, and we just
            !decrement the stream position counter:
            ipos = ipos - 1

        else

            pushed_index = pushed_index + 1

            if (pushed_index>0 .and. pushed_index<=len(pushed_char)) then
                pushed_char(pushed_index:pushed_index) = c
            else
                call integer_to_string(pushed_index,istr)
                call throw_exception('Error in push_char: '//&
                                     'invalid valid of pushed_index: '//trim(istr))
            end if

        end if

    end if

    end subroutine push_char
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 12/4/2013
!
!  Convert an integer to a string.

    pure subroutine integer_to_string(ival,str)

    implicit none

    integer(IK),intent(in)               :: ival  !! integer value.
    character(kind=CK,len=*),intent(out) :: str   !! ival converted to a string.

    integer(IK) :: istat

    write(str,fmt=int_fmt,iostat=istat) ival

    if (istat==0) then
        str = adjustl(str)
    else
        str = repeat(star,len(str))
    end if

    end subroutine integer_to_string
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 12/4/2013
!
!  Convert a real value to a string.
!
!# Modified
!  * Izaak Beekman : 02/24/2015 : added the compact option.

    subroutine real_to_string(rval,str)

    implicit none

    real(RK),intent(in)                  :: rval  !! real value.
    character(kind=CK,len=*),intent(out) :: str   !! rval converted to a string.

    integer(IK) :: istat

    !default format:
    write(str,fmt=real_fmt,iostat=istat) rval

    if (istat==0) then

        !in this case, the default string will be compacted,
        ! so that the same value is displayed with fewer characters.
        if (compact_real) call compact_real_string(str)

    else
        str = repeat(star,len(str))
    end if

    end subroutine real_to_string
!*****************************************************************************************

!*****************************************************************************************
!> author: Izaak Beekman
!  date: 02/24/2015
!
!  Compact a string representing a real number, so that
!  the same value is displayed with fewer characters.
!
!# See also
!  * [[real_to_string]]

    subroutine compact_real_string(str)

    implicit none

    character(kind=CK,len=*),intent(inout) :: str

    character(kind=CK,len=len(str)) :: significand, expnt
    character(kind=CK,len=2) :: separator
    integer(IK) :: exp_start,decimal_pos,sig_trim,exp_trim,i

    str = adjustl(str)
    exp_start = scan(str,CK_'eEdD')
    if (exp_start == 0) exp_start = scan(str,CK_'-+',back=.true.)
    decimal_pos = scan(str,CK_'.')
    if (exp_start /= 0) separator = str(exp_start:exp_start)

    if (exp_start > 0 .and. exp_start < decimal_pos) then !signed, exponent-less float

        significand = str
        sig_trim = len(trim(significand))
        do i = len(trim(significand)),decimal_pos+2,-1 !look from right to left at 0s
                                                       !but save one after the decimal place
            if (significand(i:i) == '0') then
                sig_trim = i-1
            else
                exit
            end if
        end do
        str = trim(significand(1:sig_trim))

    else if (exp_start > decimal_pos) then !float has exponent

        significand = str(1:exp_start-1)
        sig_trim = len(trim(significand))
        do i = len(trim(significand)),decimal_pos+2,-1 !look from right to left at 0s
            if (significand(i:i) == '0') then
                sig_trim = i-1
            else
                exit
            end if
        end do
        expnt = adjustl(str(exp_start+1:))
        if (expnt(1:1) == '+' .or. expnt(1:1) == '-') then
            separator = trim(adjustl(separator))//expnt(1:1)
            exp_start = exp_start + 1
            expnt     = adjustl(str(exp_start+1:))
        end if
        exp_trim = 1
        do i = 1,(len(trim(expnt))-1) !look at exponent leading zeros saving last
            if (expnt(i:i) == '0') then
                exp_trim = i+1
            else
                exit
            end if
        end do
        str = trim(adjustl(significand(1:sig_trim)))// &
              trim(adjustl(separator))// &
              trim(adjustl(expnt(exp_trim:)))

    !else ! mal-formed real, BUT this code should be unreachable

    end if

    end subroutine compact_real_string
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date:6/14/2014
!
!  Returns true if the string is a valid 4-digit hex string.
!
!# Examples
!```fortran
!    valid_json_hex('0000')  !returns true
!    valid_json_hex('ABC4')  !returns true
!    valid_json_hex('AB')    !returns false (< 4 characters)
!    valid_json_hex('WXYZ')  !returns false (invalid characters)
!```

    pure function valid_json_hex(str) result(valid)

    implicit none

    logical(LK)                         :: valid  !! is str a value 4-digit hex string
    character(kind=CK,len=*),intent(in) :: str    !! the string to check.

    integer(IK) :: n,i

    !an array of the valid hex characters:
    character(kind=CK,len=1),dimension(22),parameter :: valid_chars = &
        [ (achar(i),i=48,57), & ! decimal digits
          (achar(i),i=65,70), & ! capital A-F
          (achar(i),i=97,102) ] ! lowercase a-f

    !initialize
    valid = .false.

    !check all the characters in the string:
    n = len(str)
    if (n==4) then
        do i=1,n
            if (.not. any(str(i:i)==valid_chars)) return
        end do
        valid = .true.    !all are in the set, so it is OK
    end if

    end function valid_json_hex
!*****************************************************************************************

!*****************************************************************************************
!> author: Izaak Beekman
!
!  Convert string to unicode (CDK to CK).

    pure function to_uni(str)

    implicit none

    character(kind=CDK,len=*), intent(in) :: str
    character(kind=CK,len=len(str))       :: to_uni

    to_uni = str

    end function to_uni
!*****************************************************************************************

!*****************************************************************************************
!> author: Izaak Beekman
!
!  Convert array of strings to unicode (CDK to CK).
!
!@note JW: may be able to remove this by making [[to_uni]] PURE ELEMENTAL ?

    pure function to_uni_vec(str)

    implicit none

    character(kind=CDK,len=*), dimension(:), intent(in)   :: str
    character(kind=CK,len=len(str)), dimension(size(str)) :: to_uni_vec

    to_uni_vec = str

    end function to_uni_vec
!*****************************************************************************************

!*****************************************************************************************
!> author: Izaak Beekman
!
!  CK//CDK operator.

    function ucs4_join_default(ucs4_str,def_str) result(res)

    implicit none

    character(kind=CK, len=*), intent(in) :: ucs4_str
    character(kind=CDK,len=*), intent(in) :: def_str
    character(kind=CK,len=(len(ucs4_str)+len(def_str))) :: res

    res = ucs4_str//to_unicode(def_str)

    end function ucs4_join_default
!*****************************************************************************************

!*****************************************************************************************
!> author: Izaak Beekman
!
!  CDK//CK operator.

    function default_join_ucs4(def_str,ucs4_str) result(res)

    implicit none

    character(kind=CDK,len=*), intent(in) :: def_str
    character(kind=CK, len=*), intent(in) :: ucs4_str
    character(kind=CK,len=(len(def_str)+len(ucs4_str))) :: res

    res = to_unicode(def_str)//ucs4_str

    end function default_join_ucs4
!*****************************************************************************************

!*****************************************************************************************
!> author: Izaak Beekman
!
!  CK==CDK operator.

    function ucs4_comp_default(ucs4_str,def_str) result(res)

    implicit none

    character(kind=CK, len=*), intent(in) :: ucs4_str
    character(kind=CDK,len=*), intent(in) :: def_str
    logical(LK) :: res

    res = ( ucs4_str == to_unicode(def_str) )

    end function ucs4_comp_default
!*****************************************************************************************

!*****************************************************************************************
!> author: Izaak Beekman
!
!  CDK==CK operator.

    function default_comp_ucs4(def_str,ucs4_str) result(res)

    implicit none

    character(kind=CDK,len=*), intent(in) :: def_str
    character(kind=CK, len=*), intent(in) :: ucs4_str
    logical(LK) :: res

    res = ( to_unicode(def_str) == ucs4_str)

    end function default_comp_ucs4
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Print any error message, and then clear the exceptions.
!
!@note This routine is used by the unit tests.
!      It was originally in json_example.f90, and was
!      moved here 2/26/2015 by Izaak Beekman.

    subroutine json_print_error_message(io_unit)

    implicit none

    integer, intent(in), optional :: io_unit

    character(kind=CK,len=:),allocatable :: error_msg
    logical :: status_ok

    !get error message:
    call json_check_for_errors(status_ok, error_msg)

    !print it if there is one:
    if (.not. status_ok) then
        if (present(io_unit)) then
            write(io_unit,'(A)') error_msg
        else
            write(*,'(A)') error_msg
        end if
        deallocate(error_msg)
        call json_clear_exceptions()
    end if

    end subroutine json_print_error_message
!*****************************************************************************************

!*****************************************************************************************
    end module json_module
!*****************************************************************************************
