!*****************************************************************************************
    module json_module
!*****************************************************************************************
!****h* JSON/json_module
!
!  NAME
!    json_module
!
!  DESCRIPTION
!    JSON-FORTRAN: A Fortran 2008 JSON (JavaScript Object Notation) API.
!
!  NOTES
!    -Based on fson by Joseph A. Levin (see License below)
!        -The original F95 code was split into four files:
!            fson_path_m.f95, fson_string_m.f95, fson_value_m.f95, fson.f95
!        -The code has been extensively modified and combined into this
!            one module (json_module.f90).
!        -Some Fortran 2003/2008 features are now used
!            (e.g., allocatable strings, newunit, generic, class, abstract interface)
!    -The headers in this file follow the ROBODoc conventions.
!            Compile with: robodoc --src ./ --doc ./doc --multidoc --html
!                                  --tabsize 4 --ignore_case_when_linking
!                                  --syntaxcolors --source_line_numbers --index
!
!  HISTORY
!    Joseph A. Levin : March 2012
!    Jacob Williams : 2/8/2013 : Extensive modifications to the original code.
!
!  SEE ALSO
!    [1] http://github.com/jacobwilliams/json-fortran [json-fortran development site]
!    [2] http://jacobwilliams.github.io/json-fortran [json-fortran online documentation]
!    [3] http://www.json.org/ [JSON website]
!    [4] http://jsonlint.com/ [JSON validator]
!
!  COPYRIGHT
!
!    -------------------------------------------------------------------------------------
!    json-fortran License:
!
!    JSON-FORTRAN: A Fortran 2008 JSON API
!    http://github.com/jacobwilliams/json-fortran
!
!    Copyright (c) 2014, Jacob Williams
!    All rights reserved.
!
!    Redistribution and use in source and binary forms, with or without modification,
!    are permitted provided that the following conditions are met:
!
!    * Redistributions of source code must retain the above copyright notice, this
!      list of conditions and the following disclaimer.
!
!    * Redistributions in binary form must reproduce the above copyright notice, this
!      list of conditions and the following disclaimer in the documentation and/or
!      other materials provided with the distribution.
!
!    * The names of its contributors may not be used to endorse or promote products
!      derived from this software without specific prior written permission.
!
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
!    -------------------------------------------------------------------------------------
!    Original FSON License:
!
!    http://github.com/josephalevin/fson [FSON code retrieved on 12/2/2013]
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
!*****************************************************************************************
    use,intrinsic :: iso_fortran_env

    implicit none

    private

    !*********************************************************
    !****d* json_module/kinds
    !
    !  NAME
    !    kinds
    !
    !  DESCRIPTION
    !   Kind definitions for real, integer, character, and logical variables.
    !   The sizes given here are valid for the Intel and Gfortran compilers
    !    (and perhaps others)
    !
    !  SOURCE

    !default real kind [8 bytes]
    integer,parameter :: RK = real64

    !default integer kind [4 bytes]
    integer,parameter :: IK = int32

    !default character kind [1 byte]    
    integer,parameter :: CK = character_kinds(1)

    !default logical kind [4 bytes]  
    !The statement here is to ensure a valid kind
    ! if the compiler doesn't have a logical_kinds(3)
    integer,parameter :: LK = logical_kinds(min(3,size(logical_kinds)))
    !*********************************************************

    !parameters:
    character(kind=CK,len=*),parameter,public :: json_ext = '.json'   !JSON file extension

    character(kind=CK,len=*),parameter :: space           = ' '   !special json characters
    character(kind=CK,len=*),parameter :: start_object    = '{'
    character(kind=CK,len=*),parameter :: end_object      = '}'
    character(kind=CK,len=*),parameter :: start_array     = '['
    character(kind=CK,len=*),parameter :: end_array       = ']'
    character(kind=CK,len=*),parameter :: delimiter       = ','
    character(kind=CK,len=*),parameter :: colon_char      = ':'
    character(kind=CK,len=*),parameter :: null_str        = 'null'
    character(kind=CK,len=*),parameter :: true_str        = 'true'
    character(kind=CK,len=*),parameter :: false_str       = 'false'
    character(kind=CK,len=*),parameter :: bspace          = achar(8)
    character(kind=CK,len=*),parameter :: horizontal_tab  = achar(9)
    character(kind=CK,len=*),parameter :: newline         = achar(10)
    character(kind=CK,len=*),parameter :: formfeed        = achar(12)
    character(kind=CK,len=*),parameter :: carriage_return = achar(13)
    character(kind=CK,len=*),parameter :: quotation_mark  = achar(34)
    character(kind=CK,len=*),parameter :: slash           = achar(47)
    character(kind=CK,len=*),parameter :: backslash       = achar(92)

    integer(IK),parameter :: spaces_per_tab = 2    !for indenting (Note: jsonlint.com uses 4 spaces)

    integer(IK),parameter :: max_numeric_str_len = 32
    character(kind=CK,len=*),parameter :: real_fmt = '(E30.16E3)' !format for real numbers
    character(kind=CK,len=*),parameter :: int_fmt  = '(I10)'      !format for integers
    character(kind=CK,len=*),parameter :: star     = '*'          !for invalid numbers

    !*********************************************************
    !****d* json_module/var_type
    !
    !  NAME
    !    var_type
    !
    !  DESCRIPTION
    !   The types of JSON data.
    !   These are the values returned by the var_type arguments
    !    of the routines json_file_variable_info and json_info.
    !
    !  SOURCE
    integer(IK),parameter,public :: json_unknown   = 0
    integer(IK),parameter,public :: json_null      = 1
    integer(IK),parameter,public :: json_object    = 2
    integer(IK),parameter,public :: json_array     = 3
    integer(IK),parameter,public :: json_logical   = 4
    integer(IK),parameter,public :: json_integer   = 5
    integer(IK),parameter,public :: json_double    = 6
    integer(IK),parameter,public :: json_string    = 7
    !*********************************************************

    !*********************************************************
    !****c* json_module/json_value
    !
    !  NAME
    !    json_value
    !
    !  DESCRIPTION
    !    Type used to construct the linked-list json structure
    !
    !  EXAMPLE
    !    type(json_value),pointer :: p
    !    call json_create_object(p)
    !    call json_add(p,'year',1805)
    !    call json_add(p,'value',1.0d0)
    !    call json_print(p,'test.json')
    !    call json_destroy(p)
    !
    !  SOURCE

        type,public :: json_value

        !force the constituents to be stored contiguously
        ![note: on Intel, the order of the variables below
        ! is significant to avoid the misaligned field warnings]
        sequence

        !for the linked list:
        type(json_value),pointer :: previous => null()
        type(json_value),pointer :: next     => null()
        type(json_value),pointer :: parent   => null()
        type(json_value),pointer :: children => null()
        type(json_value),pointer :: tail     => null()

        !variable name:
        character(kind=CK,len=:),allocatable :: name

        !the data for this variable:
        real(RK),allocatable                 :: dbl_value
        logical(LK),allocatable              :: log_value
        character(kind=CK,len=:),allocatable :: str_value
        integer(IK),allocatable              :: int_value

        integer(IK) :: var_type = json_unknown  !variable type

        integer(IK),private :: n_children = 0   !number of children

        end type json_value
    !*********************************************************

    !*********************************************************
        type,public :: json_file
    !*********************************************************
    !****c* json_module/json_file
    !
    !  NAME
    !    json_file
    !
    !  DESCRIPTION
    !    The json_file is the main public class that is
    !    used to open a file and get data from it.
    !
    !  EXAMPLE
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
    !
    !  AUTHOR
    !    Jacob Williams : 12/9/2013
    !
    !  SOURCE

        private

        !the JSON structure read from the file:
        type(json_value),pointer :: p => null()

        contains

        procedure,public :: load_file        => json_file_load
        procedure,public :: load_from_string => json_file_load_from_string

        procedure,public :: destroy     => json_file_destroy
        procedure,public :: move        => json_file_move_pointer
        procedure,public :: info        => json_file_variable_info

        procedure,public :: print_to_string => json_file_print_to_string

        generic,public :: print_file => json_file_print_to_console, &
                                        json_file_print_1, &
                                        json_file_print_2

        generic,public :: get => json_file_get_object,      &
                                 json_file_get_integer,     &
                                 json_file_get_double,      &
                                 json_file_get_logical,     &
                                 json_file_get_string,      &
                                 json_file_get_integer_vec, &
                                 json_file_get_double_vec,  &
                                 json_file_get_logical_vec, &
                                 json_file_get_string_vec

        generic,public :: update =>  json_file_update_integer,  &
                                     json_file_update_logical,  &
                                     json_file_update_real,     &
                                     json_file_update_string

        !get:
        procedure :: json_file_get_object
        procedure :: json_file_get_integer
        procedure :: json_file_get_double
        procedure :: json_file_get_logical
        procedure :: json_file_get_string
        procedure :: json_file_get_integer_vec
        procedure :: json_file_get_double_vec
        procedure :: json_file_get_logical_vec
        procedure :: json_file_get_string_vec

        !update:
        procedure :: json_file_update_integer
        procedure :: json_file_update_logical
        procedure :: json_file_update_real
        procedure :: json_file_update_string

        !print_file:
        procedure :: json_file_print_to_console
        procedure :: json_file_print_1
        procedure :: json_file_print_2

    !*********************************************************
        end type json_file
    !*********************************************************

    !array element callback function
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
    !****f* json_module/json_get_child
    !
    !  NAME
    !    json_get_child
    !
    !  DESCRIPTION
    !    Get a child, either by index or name string.
    !    Both of these return a json_value pointer.
    !
    !  NOTES
    !    Formerly, this was called json_value_get_child
    !
    !  SOURCE
    interface json_get_child
        module procedure json_value_get_by_index
        module procedure json_value_get_by_name_chars
    end interface json_get_child
    !*************************************************************************************

    !*************************************************************************************
    !****f* json_module/json_add
    !
    !  NAME
    !    json_add
    !
    !  DESCRIPTION
    !    Add objects to a linked list of json_values.
    !
    !  NOTES
    !    Formerly, this was called json_value_add
    !
    !  SOURCE
    interface json_add
        module procedure :: json_value_add_member
        module procedure :: json_value_add_integer, json_value_add_integer_vec
        module procedure :: json_value_add_double,  json_value_add_double_vec
        module procedure :: json_value_add_logical, json_value_add_logical_vec
        module procedure :: json_value_add_string,  json_value_add_string_vec
    end interface json_add
    !*************************************************************************************

    !*************************************************************************************
    !****f* json_module/json_update
    !
    !  NAME
    !    json_update
    !
    !  DESCRIPTION
    !    These are like json_add, except if a child with the same name is
    !     already present, then its value is simply updated.
    !    Note that currently, these only work for scalar variables.
    !    These routines can also change the variable's type (but an error will be
    !     thrown if the existing variable is not a scalar).
    !
    !  NOTES
    !    It should not be used to change the type of a variable in an array,
    !     or it will produce an invalid JSON file.
    !
    !  SOURCE
    interface json_update
        module procedure :: json_update_logical,&
                            json_update_double,&
                            json_update_integer,&
                            json_update_string
    end interface json_update
    !*************************************************************************************

    !*************************************************************************************
    !****f* json_module/json_get
    !
    !  NAME
    !    json_get
    !
    !  DESCRIPTION
    !    Get data from a json_value linked list.
    !
    !  SOURCE
    interface json_get
        module procedure :: json_get_by_path
        module procedure :: json_get_integer, json_get_integer_vec
        module procedure :: json_get_double,  json_get_double_vec
        module procedure :: json_get_logical, json_get_logical_vec
        module procedure :: json_get_string,  json_get_string_vec
        module procedure :: json_get_array
    end interface json_get
    !*************************************************************************************

    !*************************************************************************************
    !****f* json_module/json_print_to_string
    !
    !  NAME
    !    json_print_to_string
    !
    !  DESCRIPTION
    !    Print the json_value structure to an allocatable string.
    !
    !  SOURCE
    interface json_print_to_string
        module procedure :: json_value_to_string
    end interface
    !*************************************************************************************

    !*************************************************************************************
    !****f* json_module/json_print
    !
    !  NAME
    !    json_print
    !
    !  DESCRIPTION
    !    Print the json_value to a file.
    !
    !  EXAMPLE
    !    type(json_value) :: p
    !    ...
    !    call json_print(p,'test.json')  !this is json_print_2
    !
    !  SOURCE
    interface json_print
        module procedure :: json_print_1    !input is unit number
        module procedure :: json_print_2    !input is file name
    end interface
    !*************************************************************************************

    !*************************************************************************************
    !****f* json_module/json_destroy
    !
    !  NAME
    !    json_destroy
    !
    !  DESCRIPTION
    !    Destructor routine for a json_value pointer.
    !    This must be called explicitly if it is no longer needed,
    !    before it goes out of scope.  Otherwise, a memory leak will result.
    !
    !  USAGE
    !    type(json_value) :: p
    !    ...
    !    call json_destroy(p)
    !
    !  SOURCE
    interface json_destroy
        module procedure :: json_value_destroy
    end interface
    !*************************************************************************************

    !*************************************************************************************
    !****f* json_module/json_remove
    !
    !  NAME
    !    json_remove
    !
    !  DESCRIPTION
    !    Remove a json_value from a linked-list structure.
    !
    !  SOURCE
    interface json_remove
        module procedure :: json_value_remove
    end interface
    !*************************************************************************************

    !*************************************************************************************
    !****f* json_module/json_remove_if_present
    !
    !  NAME
    !    json_remove_if_present
    !
    !  DESCRIPTION
    !    If the child variable is present, then remove it.
    !
    !  SOURCE
    interface json_remove_if_present
        module procedure :: json_value_remove_if_present
    end interface
    !*************************************************************************************

    !public routines:
    public :: json_add                   !add data to a JSON structure
    public :: json_check_for_errors      !check for error and get error message
    public :: json_clear_exceptions      !clear exceptions
    public :: json_count                 !count the number of children
    public :: json_create_array          !allocate a json_value array
    public :: json_create_double         !allocate a json_value double
    public :: json_create_integer        !allocate a json_value integer
    public :: json_create_logical        !allocate a json_value logical
    public :: json_create_null           !allocate a json_value null
    public :: json_create_object         !allocate a json_value object
    public :: json_create_string         !allocate a json_value string
    public :: json_destroy               !clear a JSON structure (destructor)
    public :: json_failed                !check for error
    public :: json_get                   !get data from the JSON structure
    public :: json_get_child             !get a child of a json_value
    public :: json_info                  !get info about a json_value
    public :: json_initialize            !to initialize the module
    public :: json_parse                 !read a JSON file and populate the structure
    public :: json_print                 !print the JSON structure to a file
    public :: json_print_to_string       !write the JSON structure to a string
    public :: json_remove                !remove from a JSON structure
    public :: json_remove_if_present     !remove from a JSON structure (if it is present)
    public :: json_update                !update a value in a JSON structure

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

    contains
!*****************************************************************************************

!*****************************************************************************************
!****if* json_module/destroy_json_data
!
!  NAME
!    destroy_json_data
!
!  USAGE
!    call destroy_json_data(d)
!
!  DESCRIPTION
!    Destroy the json_data type.
!
!  AUTHOR
!    Jacob Williams
!
!  SOURCE

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
!****f* json_module/json_file_destroy
!
!  NAME
!    json_file_destroy
!
!  USAGE
!    call me%destroy()
!
!  DESCRIPTION
!    Destroy the JSON file.
!
!  AUTHOR
!    Jacob Williams : 12/9/2013
!
!  SOURCE

    subroutine json_file_destroy(me)

    implicit none

    class(json_file),intent(inout) :: me

    if (associated(me%p)) call json_value_destroy(me%p)

    end subroutine json_file_destroy
!*****************************************************************************************

!*****************************************************************************************
!****f* json_module/json_file_move_pointer
!
!  NAME
!    json_file_move_pointer
!
!  USAGE
!    call to%move(from)
!
!  DESCRIPTION
!    Move the json_value pointer from one json_file to another.
!    "from" is then nullified, but not destroyed.
!    Note: if "from%p" is not associated, then an error is thrown.
!
!  AUTHOR
!    Jacob Williams : 12/5/2014
!
!  SOURCE

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
!****f* json_module/json_file_load
!
!  NAME
!    json_file_load
!
!  DESCRIPTION
!    Load a JSON file.
!
!  EXAMPLE
!    type(json_file) :: f
!    call f%load_file('my_file.json')
!
!  AUTHOR
!    Jacob Williams : 12/9/2013
!
!  SOURCE

    subroutine json_file_load(me, filename, unit)

    implicit none

    class(json_file),intent(inout)      :: me
    character(kind=CK,len=*),intent(in) :: filename
    integer(IK),intent(in),optional     :: unit

    call json_parse(file=filename, p=me%p, unit=unit)

    end subroutine json_file_load
!*****************************************************************************************

!*****************************************************************************************
!****f* json_module/json_file_load_from_string
!
!  NAME
!    json_file_load_from_string
!
!  DESCRIPTION
!    Load the JSON data from a string.
!
!  EXAMPLE
!    type(json_file) :: f
!    call f%load_from_string('{ "name": "Leonidas" }')
!
!  AUTHOR
!    Jacob Williams : 1/13/2015
!
!  SOURCE

    subroutine json_file_load_from_string(me, str)

    implicit none

    class(json_file),intent(inout)      :: me
    character(kind=CK,len=*),intent(in) :: str

    call json_parse(str=str, p=me%p)

    end subroutine json_file_load_from_string
!*****************************************************************************************

!*****************************************************************************************
!****f* json_module/json_file_print_to_console
!
!  NAME
!    json_file_print_to_console
!
!  USAGE
!    call me%print_file()
!
!  DESCRIPTION
!    Print the JSON file to the console.
!
!  AUTHOR
!    Jacob Williams : 1/11/2015
!
!  SOURCE

    subroutine json_file_print_to_console(me)

    implicit none

    class(json_file),intent(inout)  :: me

    character(kind=CK,len=:),allocatable :: dummy

    call json_value_print(me%p,iunit=output_unit,str=dummy,indent=1,colon=.true.)

    end subroutine json_file_print_to_console
!*****************************************************************************************

!*****************************************************************************************
!****f* json_module/json_file_print_1
!
!  NAME
!    json_file_print_1
!
!  USAGE
!    call me%print_file(iunit)
!
!  DESCRIPTION
!    Print the JSON file.
!    Prints to the specified file unit (if not present, then prints to the console)
!
!  AUTHOR
!    Jacob Williams : 12/9/2013
!
!  SOURCE

    subroutine json_file_print_1(me, iunit)

    implicit none

    class(json_file),intent(inout)  :: me
    integer(IK),intent(in)          :: iunit  !must be non-zero

    integer(IK) :: i
    character(kind=CK,len=:),allocatable :: dummy

    if (iunit/=0) then
        i = iunit
    else
        call throw_exception('Error in json_file_print_1: iunit must be nonzero.')
        return
    end if

    call json_value_print(me%p,iunit=i,str=dummy,indent=1,colon=.true.)

    end subroutine json_file_print_1
!*****************************************************************************************

!*****************************************************************************************
!****f* json_module/json_file_print_2
!
!  NAME
!    json_file_print_2
!
!  USAGE
!    call me%print_file(filename)
!
!  DESCRIPTION
!    Print the JSON file.
!    Input is the filename.  The file is opened, printed, and then closed.
!
!  EXAMPLE
!    type(json_file) :: f
!    logical :: found
!    call f%load_file('my_file.json')    !open the original file
!    call f%update('version',4,found)    !change the value of a variable
!    call f%print_file('my_file_2.json') !save file as new name
!
!  AUTHOR
!    Jacob Williams : 1/11/2015
!
!  SOURCE

    subroutine json_file_print_2(me,filename)

    implicit none

    class(json_file),intent(inout)      :: me
    character(kind=CK,len=*),intent(in) :: filename

    integer(IK) :: iunit,istat

    open(newunit=iunit,file=filename,status='REPLACE',iostat=istat)
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
!****f* json_module/json_file_print_to_string
!
!  NAME
!    json_file_print_to_string
!
!  USAGE
!    call me%print_to_string(str)
!
!  DESCRIPTION
!    Print the JSON file to a string.
!
!  EXAMPLE
!    type(json_file) :: f
!    character(kind=CK,len=:),allocatable :: str
!    call f%load_file('my_file.json')
!    call f%print_file(str)
!
!  AUTHOR
!    Jacob Williams : 1/11/2015
!
!  SOURCE

    subroutine json_file_print_to_string(me,str)

    implicit none

    class(json_file),intent(inout)                   :: me
    character(kind=CK,len=:),allocatable,intent(out) :: str

    call json_value_to_string(me%p,str)

    end subroutine json_file_print_to_string
!*****************************************************************************************

!*****************************************************************************************
!****f* json_module/json_file_variable_info
!
!  NAME
!    json_file_variable_info
!
!  USAGE
!    call me%info(path,found,var_type,n_children)
!
!  DESCRIPTION
!    Returns information about a variable in a file.
!
!  AUTHOR
!    Jacob Williams : 2/3/2014
!
!  SOURCE

    subroutine json_file_variable_info(me,path,found,var_type,n_children)

    implicit none

    class(json_file),intent(inout)      :: me
    character(kind=CK,len=*),intent(in) :: path
    logical(LK),intent(out)             :: found
    integer(IK),intent(out)             :: var_type
    integer(IK),intent(out)             :: n_children

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
!****f* json_module/json_info
!
!  NAME
!    json_info
!
!  USAGE
!    call me%info(path,found,var_type,n_children)
!
!  DESCRIPTION
!    Returns information about a json_value
!
!  AUTHOR
!    Jacob Williams : 2/13/2014
!
!  SOURCE

    subroutine json_info(p,var_type,n_children)

    implicit none

    type(json_value),pointer         :: p
    integer(IK),intent(out),optional :: var_type
    integer(IK),intent(out),optional :: n_children

    if (present(var_type))    var_type = p%var_type  !variable type
    if (present(n_children))  n_children = json_count(p)  !number of children

    end subroutine json_info
!*****************************************************************************************

!*****************************************************************************************
!****f* json_module/json_file_get_object
!
!  NAME
!    json_file_get_object
!
!  USAGE
!    call me%get(path,p,found)
!
!  DESCRIPTION
!    Get a pointer to an object from a JSON file.
!
!  AUTHOR
!    Jacob Williams : 2/3/2014
!
!  SOURCE

    subroutine json_file_get_object(me, path, p, found)

    implicit none

    class(json_file),intent(inout)       :: me
    character(kind=CK,len=*),intent(in)  :: path
    type(json_value),pointer,intent(out) :: p
    logical(LK),intent(out),optional     :: found

    call json_get_by_path(me%p, path=path, p=p, found=found)

    end subroutine json_file_get_object
!*****************************************************************************************

!*****************************************************************************************
!****f* json_module/json_file_get_integer
!
!  NAME
!    json_file_get_integer
!
!  USAGE
!    call me%get(path,val)
!
!  DESCRIPTION
!    Get an integer from a JSON file.
!
!  AUTHOR
!    Jacob Williams : 12/9/2013
!
!  SOURCE

    subroutine json_file_get_integer(me, path, val, found)

    implicit none

    class(json_file),intent(inout)      :: me
    character(kind=CK,len=*),intent(in) :: path
    integer(IK),intent(out)             :: val
    logical(LK),intent(out),optional    :: found

    call json_get(me%p, path=path, value=val, found=found)

    end subroutine json_file_get_integer
!*****************************************************************************************

!*****************************************************************************************
!****f* json_module/json_file_get_integer_vec
!
!  NAME
!    json_file_get_integer_vec
!
!  USAGE
!    call me%get(path,vec)
!
!  DESCRIPTION
!    Get an integer vector from a JSON file.
!
!  AUTHOR
!    Jacob Williams : 1/20/2014
!
!  SOURCE

    subroutine json_file_get_integer_vec(me, path, vec, found)

    implicit none

    class(json_file),intent(inout)                   :: me
    character(kind=CK,len=*),intent(in)              :: path
    integer(IK),dimension(:),allocatable,intent(out) :: vec
    logical(LK),intent(out),optional                 :: found

    call json_get(me%p, path, vec, found)

    end subroutine json_file_get_integer_vec
!*****************************************************************************************

!*****************************************************************************************
!****f* json_module/json_file_get_double
!
!  NAME
!    json_file_get_double
!
!  USAGE
!    call me%get(path,val,found)
!
!  DESCRIPTION
!    Get a double from a JSON file.
!
!  AUTHOR
!    Jacob Williams : 12/9/2013
!
!  SOURCE

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
!****f* json_module/json_file_get_double_vec
!
!  NAME
!    json_file_get_double_vec
!
!  USAGE
!    call me%get(path,vec,found)
!
!  DESCRIPTION
!    Get a double vector from a JSON file.
!
!  AUTHOR
!    Jacob Williams : 1/19/2014
!
!  SOURCE

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
!****f* json_module/json_file_get_logical
!
!  NAME
!    json_file_get_logical
!
!  USAGE
!    call me%get(path,val,found)
!
!  DESCRIPTION
!    Get a logical from a JSON file.
!
!  AUTHOR
!    Jacob Williams : 12/9/2013
!
!  SOURCE

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
!****f* json_module/json_file_get_logical_vec
!
!  NAME
!    json_file_get_logical_vec
!
!  USAGE
!    call me%get(path,vec)
!
!  DESCRIPTION
!    Get a logical vector from a JSON file.
!
!  AUTHOR
!    Jacob Williams : 1/20/2014
!
!  SOURCE

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
!****f* json_module/json_file_get_string
!
!  NAME
!    json_file_get_string
!
!  USAGE
!    call me%get(path,val)
!
!  DESCRIPTION
!    Get a character string from a json file.
!    Note: val is an allocatable character string.
!
!  AUTHOR
!    Jacob Williams : 12/9/2013
!
!  SOURCE

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
!****f* json_module/json_file_get_string_vec
!
!  NAME
!    json_file_get_string_vec
!
!  USAGE
!    call me%get(path,vec)
!
!  DESCRIPTION
!    Get a string vector from a JSON file.
!
!  AUTHOR
!    Jacob Williams : 1/19/2014
!
!  SOURCE

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
!****f* json_module/json_initialize
!
!  NAME
!    json_initialize
!
!  DESCRIPTION
!    Initialize the module.
!    Should be called before the routines are used.
!    It can also be called after using the module and encountering exceptions.
!
!  AUTHOR
!    Jacob Williams : 12/4/2013
!
!  SOURCE

    subroutine json_initialize(verbose)

    implicit none

    logical(LK),intent(in),optional :: verbose  !mainly useful for debugging (default is false)

    !optional input (if not present, value remains unchanged):
    if (present(verbose)) is_verbose = verbose

    !clear any errors from previous runs:
    call json_clear_exceptions()

    !Just in case, clear these global variables also:
    pushed_index = 0
    pushed_char  = ''
    char_count   = 0
    line_count   = 1

    end subroutine json_initialize
!*****************************************************************************************

!*****************************************************************************************
!****f* json_module/json_clear_exceptions
!
!  NAME
!    json_clear_exceptions
!
!  DESCRIPTION
!    Clear exceptions in the JSON module.
!
!  AUTHOR
!    Jacob Williams : 12/4/2013
!
!  SOURCE

    subroutine json_clear_exceptions()

    implicit none

    !clear the flag and message:
    exception_thrown = .false.
    err_message = ''

    end subroutine json_clear_exceptions
!*****************************************************************************************

!*****************************************************************************************
!****if* json_module/throw_exception
!
!  NAME
!    throw_exception
!
!  DESCRIPTION
!    Throw an exception in the JSON module.
!    This routine sets the error flag, and prevents any subsequent routine
!    from doing anything, until json_clear_exceptions is called.
!
!  AUTHOR
!    Jacob Williams : 12/4/2013
!
!  SOURCE

    subroutine throw_exception(msg)

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

    end subroutine throw_exception
!*****************************************************************************************

!*****************************************************************************************
!****f* json_module/json_check_for_errors
!
!  NAME
!    json_check_for_errors
!
!  DESCRIPTION
!    Retrieve error code from the module.
!    This should be called after json_parse to check for errors.
!    If an error is thrown, before using the module again, json_initialize
!    should be called to clean up before it is used again.
!
!  EXAMPLE
!    type(json_file) :: json
!    logical :: status_ok
!    character(len=:),allocatable :: error_msg
!    call json%load_file(filename='myfile.json')
!    call json_check_for_errors(status_ok, error_msg)
!    if (.not. status_ok) then
!        write(*,*) 'Error: '//error_msg
!        call json_clear_exceptions()
!        call json%destroy()
!    end if
!
!  SEE ALSO
!    json_failed
!
!  AUTHOR
!    Jacob Williams : 12/4/2013
!
!  SOURCE

    subroutine json_check_for_errors(status_ok, error_msg)

    implicit none

    logical(LK),intent(out) :: status_ok
    character(kind=CK,len=:),allocatable,intent(out) :: error_msg

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
!****f* json_module/json_failed
!
!  NAME
!    json_failed
!
!  DESCRIPTION
!    Logical function to indicate if an exception has been thrown.
!
!  EXAMPLE
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
!
!  SEE ALSO
!    json_check_for_errors
!
!  AUTHOR
!    Jacob Williams : 12/5/2013
!
!  SOURCE

    function json_failed() result(failed)

    implicit none

    logical(LK) :: failed

    failed = exception_thrown

    end function json_failed
!*****************************************************************************************

!*****************************************************************************************
!****if* json_module/json_value_create
!
!  NAME
!    json_value_create
!
!  DESCRIPTION
!    Allocate a json_value pointer variable.
!    This should be called before adding data to it.
!
!  EXAMPLE
!    type(json_value),pointer :: var
!    call json_value_create(var)
!    call to_double(var,1.0d0)
!
!  NOTES
!    This routine does not check for exceptions.
!    The pointer should not already be allocated.
!
!  SOURCE

    subroutine json_value_create(p)

    implicit none

    type(json_value),pointer :: p

    nullify(p)
    allocate(p)

    end subroutine json_value_create
!*****************************************************************************************

!*****************************************************************************************
!****f* json_module/json_value_destroy
!
!  NAME
!    json_value_destroy
!
!  DESCRIPTION
!    Destroy a json_value linked-list structure.
!
!  NOTES
!    This routine does not check for exceptions.
!    I believe the original version of this routine was not
!        properly freeing the memory.  It has been rewritten.
!
!  AUTHOR
!    Jacob Williams : 1/22/2014
!
!  SOURCE

    recursive subroutine json_value_destroy(this)

    implicit none

    type(json_value),pointer :: this

    if (associated(this)) then

        if (allocated(this%name)) deallocate(this%name)

        call destroy_json_data(this)

        if (associated(this%children)) call json_value_destroy(this%children)
        this%n_children = 0

        if (associated(this%next)) call json_value_destroy(this%next)

        if (associated(this%previous)) nullify(this%previous)
        if (associated(this%parent))   nullify(this%parent)
        if (associated(this%tail))     nullify(this%tail)

        deallocate(this)

        nullify(this)

    end if

    end subroutine json_value_destroy
!*****************************************************************************************

!*****************************************************************************************
!****f* json_module/json_value_remove
!
!  NAME
!    json_value_remove
!
!  DESCRIPTION
!    Remove a json_value (and all its children)
!      from a linked-list structure, preserving the rest of the structure.
!    If destroy is not present, it is also destroyed.
!    If destroy is present and true, it is destroyed.
!    If destroy is present and false, it is not destroyed.
!
!  EXAMPLE
!
!    !to extract an object from one json structure, and add it to another:
!    type(json_value),pointer :: json1,json2,p
!    logical :: found
!    [create and populate json1 and json2]
!    call json_get(json1,'name',p,found)  ! get pointer to name element of json1
!    call json_remove(p,destroy=.false.)  ! remove it from json1 (don't destroy)
!    call json_add(json2,p)         ! add it to json2
!
!    !to remove an object from a json structure (and destroy it)
!    type(json_value),pointer :: json1,p
!    logical :: found
!    [create and populate json1]
!    call json_get(json1,'name',p,found)  ! get pointer to name element of json1
!    call json_remove(p)                  ! remove and destroy it
!
!  AUTHOR
!    Jacob Williams : 9/9/2014
!
!  HISTORY
!    JW : 12/28/2014 : added destroy optional argument.
!
!  SOURCE

    subroutine json_value_remove(me,destroy)

    implicit none

    type(json_value),pointer        :: me
    logical(LK),intent(in),optional :: destroy

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
!****f* json_module/json_value_remove_if_present
!
!  NAME
!    json_value_remove_if_present
!
!  DESCRIPTION
!    Given the path string, remove the variable from
!    the json_value structure, if it exists.
!
!  AUTHOR
!    Jacob Williams : 12/6/2014
!
!  SOURCE

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
!****f* json_module/json_file_update_integer
!
!  NAME
!    json_file_update_integer
!
!  DESCRIPTION
!    Given the path string, if the variable is present in the file,
!    and is a scalar, then update its value.
!    If it is not present, then create it and set its value.
!
!  SEE ALSO
!    json_update_integer
!
!  AUTHOR
!    Jacob Williams : 1/10/2015
!
!  SOURCE

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
!****f* json_module/json_file_update_logical
!
!  NAME
!    json_file_update_logical
!
!  DESCRIPTION
!    Given the path string, if the variable is present in the file,
!    and is a scalar, then update its value.
!    If it is not present, then create it and set its value.
!
!  SEE ALSO
!    json_update_logical
!
!  AUTHOR
!    Jacob Williams : 1/10/2015
!
!  SOURCE

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
!****f* json_module/json_file_update_real
!
!  NAME
!    json_file_update_real
!
!  DESCRIPTION
!    Given the path string, if the variable is present in the file,
!    and is a scalar, then update its value.
!    If it is not present, then create it and set its value.
!
!  SEE ALSO
!    json_update_real
!
!  AUTHOR
!    Jacob Williams : 1/10/2015
!
!  SOURCE

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
!****f* json_module/json_file_update_string
!
!  NAME
!    json_file_update_string
!
!  DESCRIPTION
!    Given the path string, if the variable is present in the file,
!    and is a scalar, then update its value.
!    If it is not present, then create it and set its value.
!
!  SEE ALSO
!    json_update_string
!
!  AUTHOR
!    Jacob Williams : 1/10/2015
!
!  SOURCE

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
!****f* json_module/json_update_logical
!
!  NAME
!    json_update_logical
!
!  DESCRIPTION
!    Given the path string, if the variable is present,
!    and is a scalar, then update its value.
!    If it is not present, then create it and set its value.
!
!  AUTHOR
!    Jacob Williams : 12/6/2014
!
!  SOURCE

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
!****f* json_module/json_update_double
!
!  NAME
!    json_update_double
!
!  DESCRIPTION
!    Given the path string, if the variable is present,
!    and is a scalar, then update its value.
!    If it is not present, then create it and set its value.
!
!  AUTHOR
!    Jacob Williams : 12/6/2014
!
!  SOURCE

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
!****f* json_module/json_update_integer
!
!  NAME
!    json_update_integer
!
!  DESCRIPTION
!    Given the path string, if the variable is present,
!    and is a scalar, then update its value.
!    If it is not present, then create it and set its value.
!
!  AUTHOR
!    Jacob Williams : 12/6/2014
!
!  SOURCE

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
!****f* json_module/json_update_string
!
!  NAME
!    json_update_string
!
!  DESCRIPTION
!    Given the path string, if the variable is present,
!    and is a scalar, then update its value.
!    If it is not present, then create it and set its value.
!
!  AUTHOR
!    Jacob Williams : 12/6/2014
!
!  SOURCE

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
!****f* json_module/json_value_add_member
!
!  NAME
!    json_value_add_member
!
!  DESCRIPTION
!    Adds the member as a child of this.
!
!  SOURCE

    subroutine json_value_add_member(this, member)

    implicit none

    type(json_value),pointer :: this, member

    if (.not. exception_thrown) then

        ! associate the parent
        member%parent => this

        ! add to linked list
        if (associated(this%children)) then

            this%tail%next => member
            member%previous => this%tail

        else

            this%children => member
            member%previous => null()  !first in the list

        end if

        ! new member is now the last one in the list
        this%tail => member
        this%n_children = this%n_children + 1

    end if

    end subroutine json_value_add_member
!*****************************************************************************************

!*****************************************************************************************
!****f* json_module/json_value_add_double
!
!  NAME
!    json_value_add_double
!
!  DESCRIPTION
!    Add a real value to the structure.
!
!    These routines are part of the public API that can be
!        used to build a json structure using data.
!
!  AUTHOR
!    Jacob Williams : 1/19/2014
!
!  SOURCE

    subroutine json_value_add_double(me, name, val)

    implicit none

    type(json_value),pointer            :: me
    character(kind=CK,len=*),intent(in) :: name
    real(RK),intent(in)                 :: val

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
!****f* json_module/json_value_add_double_vec
!
!  NAME
!    json_value_add_double_vec
!
!  DESCRIPTION
!    Add a real vector to the structure.
!
!    These routines are part of the public API that can be
!        used to build a json structure using data.
!
!  AUTHOR
!    Jacob Williams : 1/20/2014
!
!  SOURCE

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
!****f* json_module/json_value_add_integer
!
!  NAME
!    json_value_add_integer
!
!  DESCRIPTION
!    Add an integer value to the structure.
!
!    These routines are part of the public API that can be
!        used to build a json structure using data.
!
!  AUTHOR
!    Jacob Williams : 1/20/2014
!
!  SOURCE

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
!****f* json_module/json_value_add_integer_vec
!
!  NAME
!    json_value_add_integer_vec
!
!  DESCRIPTION
!    Add an integer vector to the structure.
!
!    These routines are part of the public API that can be
!        used to build a json structure using data.
!
!  AUTHOR
!    Jacob Williams : 1/20/2014
!
!  SOURCE

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
!****f* json_module/json_value_add_logical
!
!  NAME
!    json_value_add_logical
!
!  DESCRIPTION
!    Add a logical value to the structure.
!
!    These routines are part of the public API that can be
!        used to build a json structure using data.
!
!  AUTHOR
!    Jacob Williams : 1/20/2014
!
!  SOURCE

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
!****f* json_module/json_value_add_logical_vec
!
!  NAME
!    json_value_add_logical_vec
!
!  DESCRIPTION
!    Add a logical vector to the structure.
!
!    These routines are part of the public API that can be
!        used to build a json structure using data.
!
!  AUTHOR
!    Jacob Williams : 1/20/2014
!
!  SOURCE

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
!****f* json_module/json_value_add_string
!
!  NAME
!    json_value_add_string
!
!  DESCRIPTION
!    Add a character string the structure.
!
!    These routines are part of the public API that can be
!        used to build a json structure using data.
!
!  AUTHOR
!    Jacob Williams : 1/19/2014
!
!  SOURCE

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
!****if* json_module/escape_string
!
!  NAME
!    escape_string
!
!  DESCRIPTION
!    Add the escape characters to a string for adding to JSON.
!
!  AUTHOR
!    Jacob Williams : 1/21/2014
!
!  SOURCE

    subroutine escape_string(str_in, str_out)

    implicit none

    character(kind=CK,len=*),intent(in)              :: str_in
    character(kind=CK,len=:),allocatable,intent(out) :: str_out

    integer(IK) :: i
    character(kind=CK,len=1) :: c

    str_out = ''

    !go through the string and look for special characters:
    do i=1,len(str_in)

        c = str_in(i:i)    !get next character in the input string

        select case(c)
        case(quotation_mark,backslash,slash)
            str_out = str_out//backslash//c
        case(bspace)
            str_out = str_out//'\b'
        case(formfeed)
            str_out = str_out//'\f'
        case(newline)
            str_out = str_out//'\n'
        case(carriage_return)
            str_out = str_out//'\r'
        case(horizontal_tab)
            str_out = str_out//'\t'
        case default
            str_out = str_out//c
        end select

    end do

    end subroutine escape_string
!*****************************************************************************************

!*****************************************************************************************
!****f* json_module/json_value_add_string_vec
!
!  NAME
!    json_value_add_string_vec
!
!  DESCRIPTION
!    Add an array of character strings to the structure.
!
!    These routines are part of the public API that can be
!        used to build a json structure using data.
!
!  AUTHOR
!    Jacob Williams : 1/19/2014
!
!  SOURCE

    subroutine json_value_add_string_vec(me, name, val, trim_str, adjustl_str)

    implicit none

    type(json_value),pointer                         :: me
    character(kind=CK,len=*),intent(in)              :: name
    character(kind=CK,len=*),dimension(:),intent(in) :: val
    logical(LK),intent(in),optional                  :: trim_str
    logical(LK),intent(in),optional                  :: adjustl_str

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
!****f* json_module/json_count
!
!  NAME
!    json_count
!
!  DESCRIPTION
!    Count the number of children.
!
!  HISTORY
!    JW : 1/4/2014 : Original routine removed.
!                    Now using n_children variable.
!                    Renamed from json_value_count.
!
!  SOURCE

    function json_count(me) result(count)

    implicit none

    integer(IK)                         :: count
    type(json_value),pointer,intent(in) :: me

    count = me%n_children

    end function json_count
!*****************************************************************************************

!*****************************************************************************************
!****f* json_module/json_value_get_by_index
!
!  NAME
!    json_value_get_by_index
!
!  DESCRIPTION
!    Returns a child in the object or array given the index.
!
!  SOURCE

    subroutine json_value_get_by_index(this, idx, p)

    implicit none

    type(json_value),pointer,intent(in) :: this
    integer(IK),intent(in)              :: idx
    type(json_value),pointer            :: p

    integer(IK) :: i

    nullify(p)

    if (.not. exception_thrown) then

        if (associated(this%children)) then

            p => this%children

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
                                 ' this%children is not associated.')

        end if

    end if

    end subroutine json_value_get_by_index
!*****************************************************************************************

!*****************************************************************************************
!****f* json_module/json_value_get_by_name_chars
!
!  NAME
!    json_value_get_by_name_chars
!
!  DESCRIPTION
!    Returns a child in the object or array given the name string.
!
!  NOTES
!    It is a case-sensitive search, and the name string is not trimmed,
!        So, for example, 'a ' /= 'A ' /= 'a  '
!    Note that the name is not parsed like it is in json_get_by_path.
!
!  SOURCE

    subroutine json_value_get_by_name_chars(this, name, p)

    implicit none

    type(json_value),pointer,intent(in) :: this
    character(kind=CK,len=*),intent(in) :: name
    type(json_value),pointer            :: p

    integer(IK) :: i,n_children

    nullify(p)

    if (.not. exception_thrown) then

        if (associated(this)) then

            if (this%var_type==json_object) then
                n_children = json_count(this)
                p => this%children    !start with first one
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
!****f* json_module/json_value_to_string
!
!  NAME
!    json_value_to_string
!
!  DESCRIPTION
!    Print the JSON structure to an allocatable string.
!
!  AUTHOR
!    Jacob Williams : 2/12/2014
!
!  SOURCE

    subroutine json_value_to_string(me,str)

    implicit none

    type(json_value),pointer,intent(in)              :: me
    character(kind=CK,len=:),intent(out),allocatable :: str

    str = ''
    call json_value_print(me, iunit=0, str=str, indent=1, colon=.true.)

    end subroutine json_value_to_string
!*****************************************************************************************

!*****************************************************************************************
!****f* json_module/json_print_1
!
!  NAME
!    json_print_1
!
!  DESCRIPTION
!    Print the JSON structure to a file
!    Input is the nonzero file unit (the file must already have been opened).
!
!  AUTHOR
!    Jacob Williams, 6/20/2014
!
!  SOURCE

    subroutine json_print_1(me,iunit)

    implicit none

    type(json_value),pointer,intent(in) :: me
    integer(IK),intent(in)              :: iunit    !must be non-zero

    character(kind=CK,len=:),allocatable :: dummy

    if (iunit/=0) then
        call json_value_print(me,iunit,str=dummy, indent=1, colon=.true.)
    else
        call throw_exception('Error in json_print: iunit must be nonzero.')
    end if

    end subroutine json_print_1

!*****************************************************************************************
!****f* json_module/json_print_2
!
!  NAME
!    json_print_2
!
!  DESCRIPTION
!    Print the JSON structure to a file.
!    Input is the filename.
!
!  AUTHOR
!    Jacob Williams, 12/23/2014
!
!  SOURCE

    subroutine json_print_2(me,filename)

    implicit none

    type(json_value),pointer,intent(in) :: me
    character(kind=CK,len=*),intent(in) :: filename

    integer(IK) :: iunit,istat

    open(newunit=iunit,file=filename,status='REPLACE',iostat=istat)
    if (istat==0) then
        call json_print(me,iunit)
        close(iunit,iostat=istat)
    else
        call throw_exception('Error in json_print: could not open file: '//&
                              trim(filename))
    end if

    end subroutine json_print_2

!*****************************************************************************************
!****if* json_module/json_value_print
!
!  NAME
!    json_value_print
!
!  DESCRIPTION
!    Print the JSON structure to a string or a file.
!
!  NOTES
!    This is an internal routine called by the wrapper routines
!        json_print and json_value_to_string
!    The reason the str argument is non-optional is because of a
!        bug in v4.9 of the gfortran compiler.
!
!  SOURCE

    recursive subroutine json_value_print(this,iunit,str,indent,need_comma,colon,is_array_element)

    implicit none

    type(json_value),pointer,intent(in)  :: this
    integer(IK),intent(in)               :: iunit             !file unit to write to (6=console)
    integer(IK),intent(in),optional      :: indent            !indention level
    logical(LK),intent(in),optional      :: is_array_element  !if this is an array element
    logical(LK),intent(in),optional      :: need_comma        !if it needs a comma after it
    logical(LK),intent(in),optional      :: colon             !if the colon was just written
    character(kind=CK,len=:),intent(inout),allocatable :: str
                                                      !if iunit==0, then the structure is
                                                      ! printed to this string rather than
                                                      ! a file. This mode is used by
                                                      ! json_value_to_string.

    character(kind=CK,len=max_numeric_str_len) :: tmp !for val to string conversions
    character(kind=CK,len=:),allocatable :: s
    type(json_value),pointer :: element
    integer(IK) :: tab, i, count, spaces
    logical(LK) :: print_comma
    logical(LK) :: write_file, write_string
    logical(LK) :: is_array

    if (.not. exception_thrown) then

        !whether to write a string or a file (one or the other):
        write_string = (iunit==0)
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

        select case (this%var_type)

        case (json_object)

            count = json_count(this)

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
                element => this%children
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

            count = json_count(this)

            if (count==0) then    !special case for empty array

                call write_it( s//start_array//end_array, comma=print_comma )

            else

                call write_it( start_array )

                nullify(element)
                element => this%children
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

            if (allocated(this%str_value)) then
                call write_it( s//quotation_mark// &
                               trim(this%str_value)//quotation_mark, comma=print_comma )
            else
                call throw_exception('Error in json_value_print:'//&
                                     ' this%value_string not allocated')
                return
            end if

        case (json_logical)

            if (this%log_value) then
                call write_it( s//true_str, comma=print_comma )
            else
                call write_it( s//false_str, comma=print_comma )
            end if

        case (json_integer)

            call integer_to_string(this%int_value,tmp)

            call write_it( s//trim(tmp), comma=print_comma )

        case (json_double)

            call real_to_string(this%dbl_value,tmp)

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
!****f* json_module/json_get_by_path
!
!  NAME
!    json_get_by_path
!
!  USAGE
!    call json_get(me,path,p,found)
!
!  DESCRIPTION
!    Returns the json_value pointer given the path string.
!
!  EXAMPLE
!    type(json_value),pointer :: dat,p
!    logical :: found
!    ...
!    call json_get(dat,'data(2).version',p,found)
!
!  NOTES
!    The following special characters are used to denote paths:
!     $         - root
!     @         - this
!     .         - child object member
!     [] or ()  - child array element
!
!    Thus, if any of these characters are present in the name key,
!        this routine cannot be used to get the value.
!    In that case, the json_get_child routines would need to be used.
!
!  SOURCE

    subroutine json_get_by_path(this, path, p, found)

    implicit none

    type(json_value),pointer,intent(in)  :: this
    character(kind=CK,len=*),intent(in)  :: path
    type(json_value),pointer,intent(out) :: p
    logical(LK),intent(out),optional     :: found

    character(kind=CK,len=1),parameter :: start_array_alt = '('
    character(kind=CK,len=1),parameter :: end_array_alt   = ')'

    integer(IK) :: i, length, child_i
    character(kind=CK,len=1) :: c
    logical(LK) :: array
    type(json_value),pointer :: tmp

    if (.not. exception_thrown) then

        nullify(p)

        ! default to assuming relative to this
        p => this

        child_i = 1

        array = .false.

        length = len_trim(path)

        do i=1, length

            c = path(i:i)

            select case (c)
            case ('$')

                ! root
                do while (associated (p%parent))
                    p => p%parent
                end do
                child_i = i + 1

            case ('@')

                ! this
                p => this
                child_i = i + 1

            case ('.')

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
!****if* json_module/string_to_integer
!
!  NAME
!    string_to_integer
!
!  DESCRIPTION
!    Convert a string into an integer.
!
!  NOTES
!    Replacement for the parse_integer function in the original code.
!
!  AUTHOR
!    Jacob Williams : 12/10/2013 : Rewrote routine.  Added error checking.
!
!  SOURCE

    function string_to_integer(str) result(ival)

    implicit none

    integer                             :: ival
    character(kind=CK,len=*),intent(in) :: str

    integer(IK) :: ierr

    if (.not. exception_thrown) then

        read(str,*,iostat=ierr) ival   !string to integer

        if (ierr/=0) then           !if there was an error
            ival = 0
            call throw_exception('Error in string_to_integer:'//&
                                 ' string cannot be converted to an integer: '//trim(str))
        end if

    end if

    end function string_to_integer
!*****************************************************************************************

!*****************************************************************************************
!****if* json_module/string_to_double
!
!  NAME
!    string_to_double
!
!  DESCRIPTION
!    Convert a string into a double.
!
!  AUTHOR
!    Jacob Williams : 1/19/2014
!
!  SOURCE

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
!****f* json_module/json_get_integer
!
!  NAME
!    json_get_integer
!
!  DESCRIPTION
!    Get an integer value from a json_value.
!
!  SOURCE

    subroutine json_get_integer(this, path, value, found)

    implicit none

    type(json_value),pointer,intent(in) :: this
    character(kind=CK,len=*),optional   :: path
    integer(IK),intent(out)             :: value
    logical(LK),intent(out),optional    :: found

    type(json_value),pointer :: p

    if (.not. exception_thrown) then

        nullify(p)
        if (present(path)) then
            call json_get_by_path(this=this, path=path, p=p)
        else
            p => this
        end if

        if (.not. associated(p)) then

            call throw_exception('Error in json_get_integer:'//&
                                 ' Unable to resolve path: '// trim(path))

        else

            select case(p%var_type)
            case (json_integer)
                value = p%int_value
            case (json_double)
                value = int(p%dbl_value)
            case (json_logical)
                if (p%log_value) then
                    value = 1
                else
                    value = 0
                end if
            case default
                call throw_exception('Error in get_integer:'//&
                                     ' Unable to resolve value to integer: '//&
                                     trim(path))
            end select

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

    else

        value = 0
        if (present(found)) found = .false.

    end if

    end subroutine json_get_integer
!*****************************************************************************************

!*****************************************************************************************
!****f* json_module/json_get_integer_vec
!
!  NAME
!    json_get_integer_vec
!
!  DESCRIPTION
!    Get an integer vector from a JSON value.
!
!  AUTHOR
!    Jacob Williams : 5/14/2014
!
!  SOURCE

    subroutine json_get_integer_vec(me, path, vec, found)

    implicit none

    type(json_value),pointer                         :: me
    character(kind=CK,len=*),intent(in)              :: path
    integer(IK),dimension(:),allocatable,intent(out) :: vec
    logical(LK),intent(out),optional                 :: found

    logical(LK) :: initialized

    initialized = .false.

    if (allocated(vec)) deallocate(vec)

    !the callback function is called for each element of the array:
    call json_get(me, path=path, array_callback=get_int_from_array, found=found)

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
!****f* json_module/json_get_double
!
!  NAME
!    json_get_double
!
!  DESCRIPTION
!    Get a double value from a json_value.
!
!  SOURCE

    subroutine json_get_double(this, path, value, found)

    implicit none

    type(json_value),pointer           :: this
    character(kind=CK,len=*), optional :: path
    real(RK),intent(out)               :: value
    logical(LK),intent(out),optional   :: found

    type(json_value),pointer :: p

    if (.not. exception_thrown) then

        nullify(p)

        if (present(path)) then
            call json_get_by_path(this=this, path=path, p=p)
        else
            p => this
        end if

        if (.not. associated(p)) then

            call throw_exception('Error in json_get_double:'//&
                                 ' Unable to resolve path: '//trim(path))

        else

            select case (p%var_type)
            case (json_integer)
                value = p%int_value
            case (json_double)
                value = p%dbl_value
            case (json_logical)
                if (p%log_value) then
                    value = 1.0_RK
                else
                    value = 0.0_RK
                end if
            case default
                call throw_exception('Error in json_get_double:'//&
                                     ' Unable to resolve value to double: '//&
                                     trim(path))
            end select

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

    else

        value = 0.0_RK
        if (present(found)) found = .false.

    end if

    end subroutine json_get_double
!*****************************************************************************************

!*****************************************************************************************
!****f* json_module/json_get_double_vec
!
!  NAME
!    json_get_double_vec
!
!  DESCRIPTION
!    Get a double vector from a JSON value.
!
!  AUTHOR
!    Jacob Williams : 5/14/2014
!
!  SOURCE

    subroutine json_get_double_vec(me, path, vec, found)

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

    end subroutine json_get_double_vec
!*****************************************************************************************

!*****************************************************************************************
!****f* json_module/json_get_logical
!
!  NAME
!    json_get_logical
!
!  DESCRIPTION
!    Get a logical value from a json_value.
!
!  SOURCE

    subroutine json_get_logical(this, path, value, found)

    implicit none

    type(json_value),pointer,intent(in) :: this
    character(kind=CK,len=*),optional   :: path
    logical(LK)                         :: value
    logical(LK),intent(out),optional    :: found

    type(json_value),pointer :: p

    if (.not. exception_thrown) then

        nullify(p)

        if (present(path)) then
            call json_get_by_path(this=this, path=path, p=p)
        else
            p => this
        end if

        if (.not. associated(p)) then

            call throw_exception('Error in json_get_logical:'//&
                                 ' Unable to resolve path: '//trim(path))

        else

            select case (p%var_type)
            case (json_integer)
                value = (p%int_value > 0)
            case (json_logical)
                value = p % log_value
            case default
                call throw_exception('Error in json_get_logical:'//&
                                     ' Unable to resolve value to logical: '//&
                                     trim(path))
            end select

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

    else

        value = .false.
        if (present(found)) found = .false.

    end if

    end subroutine json_get_logical
!*****************************************************************************************

!*****************************************************************************************
!****f* json_module/json_get_logical_vec
!
!  NAME
!    json_get_logical_vec
!
!  DESCRIPTION
!    Get a logical vector from a JSON value.
!
!  AUTHOR
!    Jacob Williams : 5/14/2014
!
!  SOURCE

    subroutine json_get_logical_vec(me, path, vec, found)

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

    end subroutine json_get_logical_vec
!*****************************************************************************************

!*****************************************************************************************
!****f* json_module/json_get_string
!
!  NAME
!    json_get_string
!
!  DESCRIPTION
!    Get a character string from a json_value.
!
!  SOURCE

    subroutine json_get_string(this, path, value, found)

    implicit none

    type(json_value),pointer,intent(in)              :: this
    character(kind=CK,len=*),intent(in),optional     :: path
    character(kind=CK,len=:),allocatable,intent(out) :: value
    logical(LK),intent(out),optional                 :: found

    type(json_value),pointer :: p
    character(kind=CK,len=:),allocatable :: s,pre,post
    integer(IK) :: j,jprev,n
    character(kind=CK,len=1) :: c

    if (.not. exception_thrown) then

        nullify(p)

        if (present(path)) then
            call json_get_by_path(this=this, path=path, p=p)
        else
            p => this
        end if

        if (.not. associated(p)) then

            call throw_exception('Error in json_get_string:'//&
                                 ' Unable to resolve path: '//trim(path))

        else

            select case (p%var_type)

            case (json_string)

                if (allocated(p%str_value)) then

                    !get the value as is:
                    s = p%str_value

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

                                select case (c)
                                case(quotation_mark,backslash,slash,&
                                     'b','f','n','r','t')

                                    !save the bit after the escape characters:
                                    if (j+2<n) then
                                        post = s(j+2:n)
                                    else
                                        post = ''
                                    end if

                                    select case(c)
                                    case(quotation_mark,backslash,slash)
                                        !use c as is
                                    case('b')
                                        c = bspace
                                    case('f')
                                        c = formfeed
                                    case('n')
                                        c = newline
                                    case('r')
                                        c = carriage_return
                                    case('t')
                                        c = horizontal_tab
                                    end select

                                    s = pre//c//post

                                    n = n-1    !backslash character has been 
                                               ! removed from the string

                                case('u')    !expecting 4 hexadecimal digits after 
                                             ! the escape character    [\uXXXX]

                                    !for now, we are just printing them as is
                                    ![not checking to see if it is a valid hex value]

                                    if (j+5<=n) then
                                        j=j+4
                                    else
                                        call throw_exception(&
                                            'Error in json_get_string:'//&
                                            ' Invalid hexadecimal sequence'//&
                                            ' in string: '//trim(c))
                                        exit
                                    end if

                                case default
                                    !unknown escape character
                                    call throw_exception('Error in json_get_string:'//&
                                            ' unknown escape sequence in string "'//&
                                            trim(s)//'" ['//backslash//c//']')
                                    exit
                                end select

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
                                         ' p%value not allocated')
                end if

            case default

                call throw_exception('Error in json_get_string:'//&
                                     ' Unable to resolve value to characters: '//&
                                     trim(path))

                ! Note: for the other cases, we could do val to string conversions.

            end select

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
        if (allocated(s)) deallocate(s)
        if (allocated(pre)) deallocate(pre)
        if (allocated(post)) deallocate(post)

    else

        value = ''
        found = .false.

    end if

    end subroutine json_get_string
!*****************************************************************************************

!*****************************************************************************************
!****f* json_module/json_get_string_vec
!
!  NAME
!    json_get_string_vec
!
!  DESCRIPTION
!    Get a string vector from a JSON file.
!
!  AUTHOR
!    Jacob Williams : 5/14/2014
!
!  SOURCE

    subroutine json_get_string_vec(me, path, vec, found)

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

    end subroutine json_get_string_vec
!*****************************************************************************************

!*****************************************************************************************
!****f* json_module/json_get_array
!
!  NAME
!    json_get_array
!
!  DESCRIPTION
!    This routine calls the user-supplied array_callback subroutine
!        for each element in the array.
!    Note: for integer, double, logical, and character arrays,
!        a higher-level routine is provided (see json_get), so
!        this routine does not have to be used for those cases.
!
!  SOURCE

    subroutine json_get_array(this, path, array_callback, found)

    implicit none

    type(json_value),pointer,intent(in)          :: this
    character(kind=CK,len=*),intent(in),optional :: path
    procedure(array_callback_func)               :: array_callback
    logical(LK),intent(out),optional             :: found

    type(json_value),pointer :: element,p
    integer(IK) :: i, count

    if (.not. exception_thrown) then

        nullify(p)

        ! resolve the path to the value
        if (present(path)) then
            call json_get_by_path(this=this, path=path, p=p)
        else
            p => this
        end if

        if (.not. associated(p)) then

            call throw_exception('Error in json_get_array:'//&
                                 ' Unable to resolve path: '//trim(path))

        else

            select case (p%var_type)
            case (json_array)
                count = json_count(p)
                element => p%children
                do i = 1, count ! callback for each child
                    call array_callback(element, i, count)
                    element => element%next
                end do
            case default
                call throw_exception('Error in json_get_array:'//&
                                     ' Resolved value is not an array. '//&
                                     trim(path))
            end select

            !cleanup:
            if (associated(p))       nullify(p)
            if (associated(element)) nullify(element)

        end if

        if (exception_thrown) then
            if (present(found)) then
                found = .false.
                call json_clear_exceptions()
            end if
        else
            if (present(found)) found = .true.
        end if

    else
        if (present(found)) found = .false.
    end if

    end subroutine json_get_array
!*****************************************************************************************

!*****************************************************************************************
!****f* json_module/json_parse
!
!  NAME
!    json_parse
!
!  DESCRIPTION
!    Parse the JSON file and populate the json_value tree.
!
!    Inputs can be:
!      file and unit : the specified unit is used to read JSON from file
!                      [note if unit is already open, then the filename is ignored]
!      file          : JSON is read from file using internal unit number
!      str           : JSON data is read from the string instead
!
!  EXAMPLE
!    type(json_value),pointer :: p
!    call json_parse(file='myfile.json', p=p)
!
!  NOTES
!    When calling this routine, any exceptions thrown from previous
!        calls will automatically be cleared.
!
!  HISTORY
!    Jacob Williams : 1/13/2015 : added read from string option.
!
!  SOURCE

    subroutine json_parse(file, p, unit, str)

    implicit none

    character(kind=CK,len=*),intent(in),optional :: file  !JSON file name
    type(json_value),pointer                     :: p     !output structure
    integer(IK),intent(in),optional              :: unit  !file unit number (/= 0)
    character(kind=CK,len=*),intent(in),optional :: str   !string with JSON data

    integer(IK) :: iunit, istat, i, i_nl_prev, i_nl
    character(kind=CK,len=:),allocatable :: line, arrow_str
    character(kind=CK,len=10) :: line_str, char_str
    logical(LK) :: is_open
    character(len=:),allocatable :: buffer

    !clear any exceptions and initialize:
    call json_initialize()

    if (present(unit) .and. present(file) .and. .not. present(str)) then

        if (unit==0) then
            call throw_exception('Error in json_parse: unit number must not be 0.')
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
                    form        = 'FORMATTED', &
                    position    = 'REWIND', &
                    iostat      = istat)
        end if

    elseif (.not. present(unit) .and. present(file) .and. .not. present(str)) then

        ! open the file with a new unit number:
        open (  newunit     = iunit, &
                file        = file, &
                status      = 'OLD', &
                action      = 'READ', &
                form        = 'FORMATTED', &
                position    = 'REWIND', &
                iostat      = istat)

    elseif (.not. present(unit) .and. .not. present(file) .and. present(str)) then

        buffer = str
        iunit = 0    !indicates that json data will be read from buffer
        istat = 0

    else
        call throw_exception('Error in json_parse: Invalid inputs')
        return
    end if

    if (istat==0) then

        ! create the value and associate the pointer
        call json_value_create(p)

        ! Note: the name of the root json_value doesn't really matter,
        !  but we'll allocate something here just in case.
        if (present(file)) then
            p%name = trim(file)  !use the file name
        else
            p%name = ''          !if reading it from the string
        end if

        ! parse as a value
        call parse_value(unit=iunit, str=buffer, value=p)

        ! cleanup:
        if (allocated(buffer)) deallocate(buffer)

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

            if (iunit/=0) then

                call get_current_line_from_file(iunit,line)

            else

                !get the current line from the string:
                ! [this is done by counting the newline characters]
                i_nl_prev = 0  !index of previous newline character
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

            !create the error message:
            err_message = err_message//newline//&
                           'line: '//trim(adjustl(line_str))//', '//&
                           'character: '//trim(adjustl(char_str))//newline//&
                           trim(line)//newline//arrow_str

            if (allocated(line)) deallocate(line)

        end if

        ! close the file if necessary
        if (iunit/=0) close(unit=iunit, iostat=istat)

    else

        call throw_exception('Error in json_parse: Error opening file: '//trim(file))
        nullify(p)

    end if

    end subroutine json_parse
!*****************************************************************************************

!*****************************************************************************************
!****if* json_module/get_current_line_from_file
!
!  NAME
!    get_current_line_from_file
!
!  DESCRIPTION
!    Rewind the file to the beginning of the current line, and return this line.
!    The file is assumed to be opened.
!
!  AUTHOR
!    Jacob Williams
!
!  SOURCE

    subroutine get_current_line_from_file(iunit,line)

    implicit none

    integer(IK),intent(in)                           :: iunit
    character(kind=CK,len=:),allocatable,intent(out) :: line

    integer(IK),parameter              :: n_chunk = 256   ! chunk size [arbitrary]
    character(kind=CK,len=*),parameter :: nfmt = '(A256)' ! corresponding format statement

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
            read(iunit,fmt=nfmt,advance='NO',size=isize,iostat=istat) chunk
            if (istat==0) then
                line = line//chunk
            else
                if (isize>0) line = line//chunk(1:isize)
                exit
            end if
        end do
    end if

    end subroutine get_current_line_from_file
!*****************************************************************************************

!*****************************************************************************************
!****if* json_module/parse_value
!
!  NAME
!    parse_value
!
!  DESCRIPTION
!    Core parsing routine.
!
!  SOURCE

    recursive subroutine parse_value(unit, str, value)

    implicit none

    integer(IK),intent(in)                             :: unit
    character(kind=CK,len=:),allocatable,intent(inout) :: str  !only used if unit=0
    type(json_value),pointer                           :: value

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

            case (true_str(1:1))

                !true
                call parse_for_chars(unit, str, true_str(2:))
                !allocate class and set value:
                if (.not. exception_thrown) call to_logical(value,.true.)

            case (false_str(1:1))

                !false
                call parse_for_chars(unit, str, false_str(2:))
                !allocate class and set value:
                if (.not. exception_thrown) call to_logical(value,.false.)

            case (null_str(1:1))

                !null
                call parse_for_chars(unit, str, null_str(2:))
                if (.not. exception_thrown) call to_null(value)    !allocate class

            case('-', '0': '9')

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
!****f* json_module/json_create_logical
!
!  NAME
!    json_create_logical
!
!  DESCRIPTION
!    Allocate a json_value pointer and make it a logical variable.
!    The pointer should not already be allocated.
!
!  EXAMPLE
!    type(json_value),pointer :: p
!    call json_create(p,'value',.true.)
!
!  AUTHOR
!    Jacob Williams
!
!  SOURCE

    subroutine json_create_logical(me,val,name)

    implicit none

    type(json_value),pointer            :: me
    character(kind=CK,len=*),intent(in) :: name
    logical(LK),intent(in)              :: val

    call json_value_create(me)
    call to_logical(me,val,name)

    end subroutine json_create_logical
!*****************************************************************************************

!*****************************************************************************************
!****f* json_module/json_create_integer
!
!  NAME
!    json_create_integer
!
!  DESCRIPTION
!    Allocate a json_value pointer and make it an integer variable.
!    The pointer should not already be allocated.
!
!  EXAMPLE
!    type(json_value),pointer :: p
!    call json_create(p,'value',1)
!
!  AUTHOR
!    Jacob Williams
!
!  SOURCE

    subroutine json_create_integer(me,val,name)

    implicit none

    type(json_value),pointer            :: me
    character(kind=CK,len=*),intent(in) :: name
    integer(IK),intent(in)              :: val

    call json_value_create(me)
    call to_integer(me,val,name)

    end subroutine json_create_integer
!*****************************************************************************************

!*****************************************************************************************
!****f* json_module/json_create_double
!
!  NAME
!    json_create_double
!
!  DESCRIPTION
!    Allocate a json_value pointer and make it a double variable.
!    The pointer should not already be allocated.
!
!  EXAMPLE
!    type(json_value),pointer :: p
!    call json_create(p,'value',1.0d0)
!
!  AUTHOR
!    Jacob Williams
!
!  SOURCE

    subroutine json_create_double(me,val,name)

    implicit none

    type(json_value),pointer            :: me
    character(kind=CK,len=*),intent(in) :: name
    real(RK),intent(in)                 :: val

    call json_value_create(me)
    call to_double(me,val,name)

    end subroutine json_create_double
!*****************************************************************************************

!*****************************************************************************************
!****f* json_module/json_create_string
!
!  NAME
!    json_create_string
!
!  DESCRIPTION
!    Allocate a json_value pointer and make it a string variable.
!    The pointer should not already be allocated.
!
!  EXAMPLE
!    type(json_value),pointer :: p
!    call json_create(p,'value','hello')
!
!  AUTHOR
!    Jacob Williams
!
!  SOURCE

    subroutine json_create_string(me,val,name)

    implicit none

    type(json_value),pointer            :: me
    character(kind=CK,len=*),intent(in) :: name
    character(kind=CK,len=*),intent(in) :: val

    call json_value_create(me)
    call to_string(me,val,name)

    end subroutine json_create_string
!*****************************************************************************************

!*****************************************************************************************
!****f* json_module/json_create_null
!
!  NAME
!    json_create_null
!
!  DESCRIPTION
!    Allocate a json_value pointer and make it a null variable.
!    The pointer should not already be allocated.
!
!  EXAMPLE
!    type(json_value),pointer :: p
!    call json_create(p,'value')
!
!  AUTHOR
!    Jacob Williams
!
!  SOURCE

    subroutine json_create_null(me,name)

    implicit none

    type(json_value),pointer            :: me
    character(kind=CK,len=*),intent(in) :: name

    call json_value_create(me)
    call to_null(me,name)

    end subroutine json_create_null
!*****************************************************************************************

!*****************************************************************************************
!****f* json_module/json_create_object
!
!  NAME
!    json_create_object
!
!  DESCRIPTION
!    Allocate a json_value pointer and make it an object variable.
!    The pointer should not already be allocated.
!
!  EXAMPLE
!    type(json_value),pointer :: p
!    call json_create(p,'objectname')
!
!  AUTHOR
!    Jacob Williams
!
!  SOURCE

    subroutine json_create_object(me,name)

    implicit none

    type(json_value),pointer            :: me
    character(kind=CK,len=*),intent(in) :: name

    call json_value_create(me)
    call to_object(me,name)

    end subroutine json_create_object
!*****************************************************************************************

!*****************************************************************************************
!****f* json_module/json_create_array
!
!  NAME
!    json_create_array
!
!  DESCRIPTION
!    Allocate a json_value pointer and make it an array variable.
!    The pointer should not already be allocated.
!
!  EXAMPLE
!    type(json_value),pointer :: p
!    call json_create(p,'arrayname')
!
!  AUTHOR
!    Jacob Williams
!
!  SOURCE

    subroutine json_create_array(me,name)

    implicit none

    type(json_value),pointer            :: me
    character(kind=CK,len=*),intent(in) :: name

    call json_value_create(me)
    call to_array(me,name)

    end subroutine json_create_array
!*****************************************************************************************

!*****************************************************************************************
!****if* json_module/to_logical
!
!  NAME
!    to_logical
!
!  DESCRIPTION
!    Change the variable to a logical.
!
!  AUTHOR
!    Jacob Williams
!
!  SOURCE

    subroutine to_logical(me,val,name)

    implicit none

    type(json_value),intent(inout)               :: me
    logical(LK),intent(in),optional              :: val
    character(kind=CK,len=*),intent(in),optional :: name

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
!****if* json_module/to_integer
!
!  NAME
!    to_integer
!
!  DESCRIPTION
!    Change the variable to an integer.
!
!  AUTHOR
!    Jacob Williams
!
!  SOURCE

    subroutine to_integer(me,val,name)

    implicit none

    type(json_value),intent(inout)               :: me
    integer(IK),intent(in),optional              :: val
    character(kind=CK,len=*),intent(in),optional :: name

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
!****if* json_module/to_double
!
!  NAME
!    to_double
!
!  DESCRIPTION
!    Change the variable to a double.
!
!  AUTHOR
!    Jacob Williams
!
!  SOURCE

    subroutine to_double(me,val,name)

    implicit none

    type(json_value),intent(inout)               :: me
    real(RK),intent(in),optional                 :: val
    character(kind=CK,len=*),intent(in),optional :: name

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
!****if* json_module/to_string
!
!  NAME
!    to_string
!
!  DESCRIPTION
!    Change the variable to a string.
!
!  AUTHOR
!    Jacob Williams
!
!  SOURCE

    subroutine to_string(me,val,name)

    implicit none

    type(json_value),intent(inout)               :: me
    character(kind=CK,len=*),intent(in),optional :: val
    character(kind=CK,len=*),intent(in),optional :: name

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
!****if* json_module/to_null
!
!  NAME
!    to_null
!
!  DESCRIPTION
!    Change the variable to a null.
!
!  AUTHOR
!    Jacob Williams
!
!  SOURCE

    subroutine to_null(me,name)

    implicit none

    type(json_value),intent(inout)               :: me
    character(kind=CK,len=*),intent(in),optional :: name

    !set type and value:
    call destroy_json_data(me)
    me%var_type = json_null

    !name:
    if (present(name)) me%name = trim(name)

    end subroutine to_null
!*****************************************************************************************

!*****************************************************************************************
!****if* json_module/to_object
!
!  NAME
!    to_object
!
!  DESCRIPTION
!    Change the variable to an object.
!
!  AUTHOR
!    Jacob Williams
!
!  SOURCE

    subroutine to_object(me,name)

    implicit none

    type(json_value),intent(inout)               :: me
    character(kind=CK,len=*),intent(in),optional :: name

    !set type and value:
    call destroy_json_data(me)
    me%var_type = json_object

    !name:
    if (present(name)) me%name = trim(name)

    end subroutine to_object
!*****************************************************************************************

!*****************************************************************************************
!****if* json_module/to_array
!
!  NAME
!    to_array
!
!  DESCRIPTION
!    Change the variable to an array.
!
!  AUTHOR
!    Jacob Williams
!
!  SOURCE

    subroutine to_array(me,name)

    implicit none

    type(json_value),intent(inout)               :: me
    character(kind=CK,len=*),intent(in),optional :: name

    !set type and value:
    call destroy_json_data(me)
    me%var_type = json_array

    !name:
    if (present(name)) me%name = trim(name)

    end subroutine to_array
!*****************************************************************************************

!*****************************************************************************************
!****if* json_module/parse_object
!
!  NAME
!    parse_object
!
!  DESCRIPTION
!    Core parsing routine.
!
!  SOURCE

    recursive subroutine parse_object(unit, str, parent)

    implicit none

    integer(IK), intent(in)  :: unit
    character(kind=CK,len=:),allocatable,intent(inout) :: str
    type(json_value),pointer :: parent

    type(json_value),pointer :: pair
    logical(LK) :: eof
    character(kind=CK,len=1) :: c
    character(kind=CK,len=:),allocatable :: tmp  !this is a work-around for a bug
                                                 !  in the gfortran 4.9 compiler.

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
!****if* json_module/parse_array
!
!  NAME
!    parse_array
!
!  DESCRIPTION
!   Core parsing routine.
!
!  SOURCE

    recursive subroutine parse_array(unit, str, array)

    implicit none

    integer(IK), intent(in)  :: unit
    character(kind=CK,len=:),allocatable,intent(inout) :: str
    type(json_value),pointer :: array

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
!****if* json_module/parse_string
!
!  NAME
!    parse_string
!
!  DESCRIPTION
!    Parses a string while reading a json file
!
!  HISTORY
!    JW : 6/16/2014 : added hex validation.
!
!  SOURCE

    subroutine parse_string(unit, str, string)

    implicit none

    integer(IK), intent(in)                            :: unit
    character(kind=CK,len=:),allocatable,intent(inout) :: str
    character(kind=CK,len=:),allocatable,intent(out)   :: string

    logical(LK) :: eof, is_hex, escape
    character(kind=CK,len=1) :: c, last
    character(kind=CK,len=4) :: hex
    integer(IK) :: i

    !at least return a blank string if there is a problem:
    string = ''

    if (.not. exception_thrown) then

        !initialize:
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

            else if (quotation_mark == c .and. last /= backslash) then

                if (is_hex) call throw_exception('Error in parse_string:'//&
                                                 ' incomplete hex string: \u'//trim(hex))
                exit

            else

                !append to string:
                string = string//c

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

    end if

    end subroutine parse_string
!*****************************************************************************************

!*****************************************************************************************
!****if* json_module/parse_for_chars
!
!  NAME
!    parse_for_chars
!
!  DESCRIPTION
!    Core parsing routine.
!
!  SOURCE

    subroutine parse_for_chars(unit, str, chars)

    implicit none

    integer(IK), intent(in)                            :: unit
    character(kind=CK,len=:),allocatable,intent(inout) :: str
    character(kind=CK,len = *), intent(in)             :: chars

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
!****if* json_module/parse_number
!
!  NAME
!
!  DESCRIPTION
!    Read a numerical value from the file.
!    The routine will determine if it is an integer or a double, and
!    allocate the type accordingly.
!
!  NOTES
!    Complete rewrite of the original FSON routine, which had some problems.
!
!  AUTHOR
!    Jacob Williams : 1/20/2014
!
!  SOURCE

    subroutine parse_number(unit, str, value)

    implicit none

    integer(IK),intent(in)                             :: unit
    character(kind=CK,len=:),allocatable,intent(inout) :: str
    type(json_value),pointer                           :: value

    character(kind=CK,len=:),allocatable :: tmp
    character(kind=CK,len=1) :: c
    logical(LK) :: eof
    real(RK) :: rval
    integer(IK) :: ival
    logical(LK) :: first
    logical(LK) :: is_integer

    if (.not. exception_thrown) then

        tmp = ''
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
                case('-','+')    !note: allowing a '+' as the first character here.

                    if (is_integer .and. (.not. first)) is_integer = .false.

                    !add it to the string:
                    tmp = tmp // c

                case('.','E','e')    !can be present in real numbers

                    if (is_integer) is_integer = .false.

                    !add it to the string:
                    tmp = tmp // c

                case('0':'9')    !valid characters for numbers

                    !add it to the string:
                    tmp = tmp // c

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
!****if* json_module/pop_char
!
!  NAME
!    pop_char
!
!  DESCRIPTION
!    Get the next character from the file (or string).
!
!  NOTES
!    This routine ignores non-printing ascii characters (iachar<=31) that
!    are in strings.
!
!  SOURCE

    recursive function pop_char(unit, str, eof, skip_ws) result(popped)

    implicit none

    character(kind=CK,len=1)                           :: popped
    integer(IK),intent(in)                             :: unit
    character(kind=CK,len=:),allocatable,intent(inout) :: str  !only used if unit=0
    logical(LK),intent(out)                            :: eof
    logical(LK),intent(in),optional                    :: skip_ws

    integer(IK) :: ios
    character(kind=CK,len=1) :: c
    logical(LK) :: ignore
    integer(IK) :: str_len
    character(kind=CK,len=:),allocatable :: tmp  !workaround for bug in gfortran 4.9.2 compiler

    if (.not. exception_thrown) then

        eof = .false.
        if (.not.present(skip_ws)) then
            ignore = .false.
        else
            ignore = skip_ws
        end if

        do

            if (pushed_index > 0) then

                ! there is a character pushed back on, most likely from the number parsing
                c = pushed_char(pushed_index:pushed_index)
                pushed_index = pushed_index - 1

            else

                if (unit/=0) then    !read from the file
                    read (unit = unit, fmt = '(A1)', advance = 'NO', iostat = ios) c
                else    !read from the string
                    tmp = str   !!! copy to a temp variable to workaround a bug in gfortran 4.9.2
                    str_len = len(tmp)   !length of the string
                    if (str_len>0) then
                        c = tmp(1:1)
                        if (str_len>1) then
                            tmp = tmp(2:str_len)  !remove the character that was read
                        else
                            tmp = ''    !that was the last one
                        end if
                        str = tmp
                        deallocate(tmp)   !!!
                        ios = 0
                    else
                        ios = IOSTAT_END  !end of the string
                    end if
                end if

                char_count = char_count + 1    !character count in the current line

                if (IS_IOSTAT_EOR(ios) .or. c==newline) then    !end of record

                    char_count = 0
                    line_count = line_count + 1
                    cycle

                else if (IS_IOSTAT_END(ios)) then  !end of file

                    char_count = 0
                    eof = .true.
                    exit

                end if

            end if

            if (iachar(c) <= 31) then         !JW : fixed so it will read spaces 
                                              !      in the string (was 32)

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
!****if* json_module/push_char
!
!  NAME
!    push_char
!
!  DESCRIPTION
!
!
!  SOURCE

    subroutine push_char(c)

    implicit none

    character(kind=CK,len=1), intent(in) :: c

    character(kind=CK,len=max_numeric_str_len) :: istr

    if (.not. exception_thrown) then

        pushed_index = pushed_index + 1

        if (pushed_index>0 .and. pushed_index<=len(pushed_char)) then
            pushed_char(pushed_index:pushed_index) = c
        else
            call integer_to_string(pushed_index,istr)
            call throw_exception('Error in push_char:'//&
                                 ' invalid valid of pushed_index: '//trim(istr))
        end if

    end if

    end subroutine push_char
!*****************************************************************************************

!*****************************************************************************************
!****if* json_module/integer_to_string
!
!  NAME
!    integer_to_string
!
!  DESCRIPTION
!    Convert an integer to a string.
!
!  AUTHOR
!    Jacob Williams : 12/4/2013
!
!  SOURCE

    subroutine integer_to_string(ival,str)

    implicit none

    integer(IK),intent(in)               :: ival
    character(kind=CK,len=*),intent(out) :: str

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
!****if* json_module/real_to_string
!
!  NAME
!    real_to_string
!
!  DESCRIPTION
!    Convert a real value to a string.
!
!  AUTHOR
!    Jacob Williams : 12/4/2013
!
!  SOURCE

    subroutine real_to_string(rval,str)

    implicit none

    real(RK),intent(in)                  :: rval
    character(kind=CK,len=*),intent(out) :: str

    integer(IK) :: istat

    write(str,fmt=real_fmt,iostat=istat) rval

    if (istat==0) then
        str = adjustl(str)
    else
        str = repeat(star,len(str))
    end if

    end subroutine real_to_string
!*****************************************************************************************

!*****************************************************************************************
!****if* json_module/valid_json_hex
!
!  NAME
!    valid_json_hex
!
!  DESCRIPTION
!    Returns true if the string is a valid 4-digit hex string.
!
!  EXAMPLE
!    valid_json_hex('0000')  !returns true
!    valid_json_hex('ABC4')  !returns true
!    valid_json_hex('AB')    !returns false (< 4 characters)
!    valid_json_hex('WXYZ')  !returns false (invalid characters)
!
!  AUTHOR
!    Jacob Williams : 6/14/2014
!
!  SOURCE

    function valid_json_hex(str) result(valid)

    implicit none

    logical(LK)                         :: valid
    character(kind=CK,len=*),intent(in) :: str

    integer(IK) :: n,i

    !an array of the valid hex characters:
    character(kind=CK,len=1),dimension(16),parameter :: valid_chars = &
        ['0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F']

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
    end module json_module
!*****************************************************************************************
