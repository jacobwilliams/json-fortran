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
!            (e.g., allocatable strings, associate, newunit, generic, class, 
!            and abstract interface)
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
    use,intrinsic :: iso_fortran_env, only: wp => real64    !double precision reals  

    implicit none

    private

    !parameters:
    character(len=*),parameter,public :: json_ext = '.json'       !JSON file extension
    
    character(len=1),parameter :: space             = ' '         !special characters
    character(len=1),parameter :: bspace            = achar(8)
    character(len=1),parameter :: horizontal_tab    = achar(9)
    character(len=1),parameter :: newline           = achar(10)  
    character(len=1),parameter :: formfeed          = achar(12)
    character(len=1),parameter :: carriage_return   = achar(13)
    character(len=1),parameter :: quotation_mark    = achar(34)
    character(len=1),parameter :: slash             = achar(47)
    character(len=1),parameter :: backslash         = achar(92)  
       
    character(len=*),parameter :: real_fmt  = '(E30.16E3)'      !format for real numbers
    character(len=*),parameter :: int_fmt   = '(I10)'           !format for integers
    character(len=*),parameter :: null_str  = 'null'
    character(len=*),parameter :: true_str  = 'true'
    character(len=*),parameter :: false_str = 'false'

    ! The types of data:
    integer,parameter,public :: json_unknown   = 0
    integer,parameter,public :: json_null      = 1
    integer,parameter,public :: json_object    = 2
    integer,parameter,public :: json_array     = 3
    integer,parameter,public :: json_logical   = 4
    integer,parameter,public :: json_integer   = 5
    integer,parameter,public :: json_double    = 6
    integer,parameter,public :: json_string    = 7

    !*********************************************************
    !****ic* json_module/json_data_non_polymorphic
    !
    !  NAME
    !    json_data_non_polymorphic
    !
    !  DESCRIPTION
    !    The data in a json_value class
    !
    !  SOURCE
    
        type :: json_data_non_polymorphic

            integer :: var_type = json_unknown

            logical,allocatable             :: log_value
            integer,allocatable             :: int_value
            real(wp),allocatable            :: dbl_value
            character(len=:),allocatable    :: str_value

        contains

            procedure :: destroy => destroy_json_data_non_polymorphic

        end type json_data_non_polymorphic
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
    !    call json_value_create(p) 
    !    call to_object(p) 
    !    call json_value_add(p,'year',1805)
    !    call json_value_add(p,'value',1.0_wp)
    !    call json_print(p,'test.json')
    !    call json_destroy(p)
    !
    !  SOURCE
    
        type,public :: json_value

        !variable name:
        character(len=:),allocatable :: name

        !the data for this variable:
        type(json_data_non_polymorphic) :: data

        !for the linked list:
        type(json_value), pointer :: previous => null()
        type(json_value), pointer :: next => null()
        type(json_value), pointer :: parent => null()
        type(json_value), pointer :: children => null()

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
    !    real(wp) :: rval
    !    character(len=:),allocatable :: cval
    !    logical :: found
    !    call json%load_file(filename='myfile.json')
    !    call json%print_file()
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

        type(json_value), pointer :: p => null()    !the JSON structure read from the file

        contains

        procedure,public :: load_file   => load_json_file
        procedure,public :: print_file  => print_json_file
        procedure,public :: destroy     => destroy_json_file
        procedure,public :: move        => move_pointer_in_json_file
        procedure,public :: info        => variable_info_in_file

        generic,public :: get => get_pointer,&
                                 get_integer,&
                                 get_double,&
                                 get_logical,&
                                 get_chars,&
                                 get_double_vec,&
                                 get_char_vec,&
                                 get_integer_vec,&
                                 get_logical_vec

        !scalars:
        procedure :: get_pointer        => get_object_from_json_file
        procedure :: get_integer        => get_integer_from_json_file
        procedure :: get_double         => get_double_from_json_file
        procedure :: get_logical        => get_logical_from_json_file
        procedure :: get_chars          => get_chars_from_json_file

        !vectors:
        procedure :: get_integer_vec    => get_integer_vec_from_json_file
        procedure :: get_double_vec     => get_double_vec_from_json_file
        procedure :: get_logical_vec    => get_logical_vec_from_json_file
        procedure :: get_char_vec       => get_char_vec_from_json_file
                
    !*********************************************************
        end type json_file
    !*********************************************************

    !array element callback function
    abstract interface
        subroutine array_callback_func(element, i, count)
            import :: json_value
            implicit none
            type(json_value), pointer,intent(in) :: element
            integer,intent(in) :: i        !index
            integer,intent(in) :: count    !size of array
        end subroutine array_callback_func
    end interface

    !*************************************************************************************
    !****f* json_module/json_value_get
    !
    !  NAME
    !    json_value_get
    !
    !  DESCRIPTION
    !    Get a child, either by index or name string.
    !
    !  SOURCE
    interface json_value_get           !consider renaming this json_value_get_child
        module procedure get_by_index
        module procedure get_by_name_chars
    end interface json_value_get
    !*************************************************************************************

    !*************************************************************************************
    !****f* json_module/json_value_add
    !
    !  NAME
    !    json_value_add
    !
    !  DESCRIPTION
    !    Add objects to a linked list of json_values.
    !
    !  SOURCE
    interface json_value_add
        module procedure :: json_value_add_member
        module procedure :: json_value_add_integer, json_value_add_integer_vec
        module procedure :: json_value_add_double,  json_value_add_double_vec
        module procedure :: json_value_add_logical, json_value_add_logical_vec
        module procedure :: json_value_add_string,  json_value_add_string_vec
    end interface json_value_add
    !*************************************************************************************
    
    !*************************************************************************************
    !****f* json_module/json_update
    !
    !  NAME
    !    json_update
    !
    !  DESCRIPTION
    !    These are like json_value_add, except if a child with the same name is
    !     already present, then its value is simply updated.
    !    Note that currently, these only work for scalar variables.
    !    These routines can also change the variable's type (but an error will be 
    !     thrown if the existing variable is not a scalar).
    !
    !  SOURCE
    interface json_update
        module procedure :: json_update_logical,&
                            json_update_double,&
                            json_update_integer,&
                            json_update_chars
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
        module procedure :: json_get_chars,   json_get_char_vec
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
    !    Remove and destroy a json_value (and all its children) 
    !        from a linked-list structure.
    !    The rest of the structure is preserved.
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
    public :: json_initialize            !to initialize the module
    public :: json_destroy               !clear a JSON structure (destructor)
    public :: json_remove                !remove from a JSON structure
    public :: json_remove_if_present     !remove from a JSON structure (if it is present)
    public :: json_parse                 !read a JSON file and populate the structure
    public :: json_clear_exceptions      !clear exceptions
    public :: json_check_for_errors      !check for error and get error message
    public :: json_failed                !check for error
    public :: json_value_get             !use either a 1 based index or member name to get a json_value.
    public :: json_value_add             !add data to a JSON structure
    public :: json_update                !update a value in a JSON structure
    public :: json_get                   !get data from the JSON structure  
    public :: json_print                 !print the JSON structure to a file
    public :: json_print_to_string       !write the JSON structure to a string
    public :: json_value_create          !initialize a json_value pointer
    public :: json_value_count           !count the number of children
    public :: json_info                  !get info about a json_value
    public :: to_logical                 !set the data type of a json_value
    public :: to_integer                 !
    public :: to_string                  !
    public :: to_double                  !
    public :: to_null                    !
    public :: to_object                  !
    public :: to_array                   !
    
    !exception handling [private variables]
    logical :: exception_thrown = .false.            !the error flag
    character(len=:),allocatable :: err_message      !the error message

    ! POP/PUSH CHARACTER [private variables]
    integer :: char_count = 0
    integer :: line_count = 1
    integer :: pushed_index = 0
    character(len=10) :: pushed_char    !JW : what is this magic number 10??

    contains
!*****************************************************************************************

!*****************************************************************************************
!****if* json_module/destroy_json_data_non_polymorphic
!
!  NAME
!    destroy_json_data_non_polymorphic
!
!  USAGE
!    call me%destroy()
!
!  DESCRIPTION
!    Destroy the nonpolymorphic data type.
!
!  AUTHOR
!    Jacob Williams
!
!  SOURCE

    subroutine destroy_json_data_non_polymorphic(me)

    implicit none

    class(json_data_non_polymorphic),intent(inout) :: me

    me%var_type = 0

    if (allocated(me%log_value)) deallocate(me%log_value)
    if (allocated(me%int_value)) deallocate(me%int_value)
    if (allocated(me%dbl_value)) deallocate(me%dbl_value)
    if (allocated(me%str_value)) deallocate(me%str_value)

    end subroutine destroy_json_data_non_polymorphic
!*****************************************************************************************

!*****************************************************************************************
!****f* json_module/destroy_json_file
!
!  NAME
!    destroy_json_file
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

    subroutine destroy_json_file(me)
    
    implicit none

    class(json_file),intent(inout) :: me

    if (associated(me%p)) call json_value_destroy(me%p)

    end subroutine destroy_json_file
!*****************************************************************************************

!*****************************************************************************************
!****f* json_module/move_pointer_in_json_file
!
!  NAME
!    move_pointer_in_json_file
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

    subroutine move_pointer_in_json_file(to,from)
    
    implicit none

    class(json_file),intent(inout) :: to
    class(json_file),intent(inout) :: from

    if (associated(from%p)) then
        to%p => from%p
        nullify(from%p)
    else
        call throw_exception('Error in move_pointer_in_json_file: '//&
                             'pointer is not associated.')
    end if

    end subroutine move_pointer_in_json_file
    
!*****************************************************************************************
!****f* json_module/load_json_file
!
!  NAME
!    load_json_file
!
!  USAGE
!    call me%load_file(filename)
!
!  DESCRIPTION
!    Load the JSON file.
!
!  AUTHOR
!    Jacob Williams : 12/9/2013
!
!  SOURCE

    subroutine load_json_file(me, filename)

    implicit none

    class(json_file),intent(inout) :: me
    character(len=*),intent(in) :: filename

    call json_parse(filename, me%p)

    end subroutine load_json_file
!*****************************************************************************************

!*****************************************************************************************
!****f* json_module/print_json_file
!
!  NAME
!    print_json_file
!
!  USAGE
!    call me%print_file()
!
!  DESCRIPTION
!    Print the JSON file.
!    Prints to the specified file unit (if not present, then prints to the console)
!
!  AUTHOR
!    Jacob Williams : 12/9/2013
!
!  SOURCE

    subroutine print_json_file(me, iunit)

    use, intrinsic :: iso_fortran_env,    only: output_unit

    implicit none

    class(json_file),intent(inout) :: me
    integer,intent(in),optional :: iunit  !must be non-zero

    integer :: i
    character(len=:),allocatable :: dummy

    if (present(iunit)) then
        if (iunit/=0) then
            i = iunit
        else
            call throw_exception('Error in print_json_file: iunit must be nonzero.')
            return
        end if
    else
        i = output_unit
    end if

    call json_value_print(me%p,iunit=i,str=dummy)

    end subroutine print_json_file
!*****************************************************************************************

!*****************************************************************************************
!****f* json_module/variable_info_in_file
!
!  NAME
!    variable_info_in_file
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

    subroutine variable_info_in_file(me,path,found,var_type,n_children)

    implicit none

    class(json_file),intent(inout) :: me
    character(len=*),intent(in)    :: path
    logical,intent(out)            :: found
    integer,intent(out)            :: var_type
    integer,intent(out)            :: n_children

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

    end subroutine variable_info_in_file
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

    type(json_value),pointer        :: p
    integer,intent(out),optional    :: var_type
    integer,intent(out),optional    :: n_children
        
    if (present(var_type))    var_type = p%data%var_type        !variable type
    if (present(n_children))  n_children = json_value_count(p)  !number of children
    
    end subroutine json_info
!*****************************************************************************************
    
!*****************************************************************************************
!****f* json_module/get_object_from_json_file
!
!  NAME
!    get_object_from_json_file
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

    subroutine get_object_from_json_file(me, path, p, found)
    
    implicit none

    class(json_file),intent(inout)          :: me
    character(len=*),intent(in)             :: path
    type(json_value),pointer,intent(out)    :: p
    logical,intent(out),optional            :: found

    call json_get_by_path(me%p, path=path, p=p, found=found)

    end subroutine get_object_from_json_file
!*****************************************************************************************

!*****************************************************************************************
!****f* json_module/get_integer_from_json_file
!
!  NAME
!    get_integer_from_json_file
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

    subroutine get_integer_from_json_file(me, path, val, found)

    implicit none

    class(json_file),intent(inout)    :: me
    character(len=*),intent(in)       :: path
    integer,intent(out)               :: val
    logical,intent(out),optional      :: found

    call json_get(me%p, path=path, value=val, found=found)

    end subroutine get_integer_from_json_file
!*****************************************************************************************

!*****************************************************************************************
!****f* json_module/get_integer_vec_from_json_file
!
!  NAME
!    get_integer_vec_from_json_file
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

    subroutine get_integer_vec_from_json_file(me, path, vec, found)

    implicit none

    class(json_file),intent(inout)                  :: me
    character(len=*),intent(in)                     :: path
    integer,dimension(:),allocatable,intent(out)    :: vec
    logical,intent(out),optional                    :: found

    call json_get(me%p, path, vec, found)

    end subroutine get_integer_vec_from_json_file
!*****************************************************************************************

!*****************************************************************************************
!****f* json_module/get_double_from_json_file
!
!  NAME
!    get_double_from_json_file
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

    subroutine get_double_from_json_file (me, path, val, found)

    implicit none

    class(json_file),intent(inout)  :: me
    character(len=*),intent(in)     :: path
    real(wp),intent(out)            :: val
    logical,intent(out),optional    :: found

    call json_get(me%p, path=path, value=val, found=found)

    end subroutine get_double_from_json_file
!*****************************************************************************************

!*****************************************************************************************
!****f* json_module/get_double_vec_from_json_file
!
!  NAME
!    get_double_vec_from_json_file
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

    subroutine get_double_vec_from_json_file(me, path, vec, found)
    
    implicit none

    class(json_file),intent(inout)                  :: me
    character(len=*),intent(in)                     :: path
    real(wp),dimension(:),allocatable,intent(out)   :: vec
    logical,intent(out),optional                    :: found

    call json_get(me%p, path, vec, found)

    end subroutine get_double_vec_from_json_file
!*****************************************************************************************

!*****************************************************************************************
!****f* json_module/get_logical_from_json_file
!
!  NAME
!    get_logical_from_json_file
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

    subroutine get_logical_from_json_file(me,path,val,found)

    implicit none

    class(json_file),intent(inout)  :: me
    character(len=*),intent(in)     :: path
    logical,intent(out)             :: val
    logical,intent(out),optional    :: found

    call json_get(me%p, path=path, value=val, found=found)

    end subroutine get_logical_from_json_file
!*****************************************************************************************

!*****************************************************************************************
!****f* json_module/get_logical_vec_from_json_file
!
!  NAME
!    get_logical_vec_from_json_file
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

    subroutine get_logical_vec_from_json_file(me, path, vec, found)

    implicit none

    class(json_file),intent(inout)                 :: me
    character(len=*),intent(in)                    :: path
    logical,dimension(:),allocatable,intent(out)   :: vec
    logical,intent(out),optional                   :: found
    
    call json_get(me%p, path, vec, found)

    end subroutine get_logical_vec_from_json_file
!*****************************************************************************************

!*****************************************************************************************
!****f* json_module/get_chars_from_json_file
!
!  NAME
!    get_chars_from_json_file
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

    subroutine get_chars_from_json_file(me, path, val, found)

    implicit none

    class(json_file),intent(inout)              :: me
    character(len=*),intent(in)                 :: path
    character(len=:),allocatable,intent(out)    :: val
    logical,intent(out),optional                :: found

    call json_get(me%p, path=path, value=val, found=found)

    end subroutine get_chars_from_json_file
!*****************************************************************************************

!*****************************************************************************************
!****f* json_module/get_char_vec_from_json_file
!
!  NAME
!    get_char_vec_from_json_file
!
!  USAGE
!    call me%get(path,vec)
!
!  DESCRIPTION
!    Get a char vector from a JSON file.
!
!  AUTHOR
!    Jacob Williams : 1/19/2014
!
!  SOURCE

    subroutine get_char_vec_from_json_file(me, path, vec, found)

    implicit none

    class(json_file),intent(inout)                         :: me
    character(len=*),intent(in)                            :: path
    character(len=*),dimension(:),allocatable,intent(out)  :: vec
    logical,intent(out),optional                           :: found

    call json_get(me%p, path, vec, found)

    end subroutine get_char_vec_from_json_file
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

    subroutine json_initialize()

    implicit none

    !clear any errors from previous runs:
    call json_clear_exceptions()

    !Just in case, clear these global variables also:
    pushed_index = 0
    pushed_char = ''
    char_count = 0
    line_count = 1

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
!****f* json_module/throw_exception
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

    character(len=*),intent(in) :: msg    !the error message

    exception_thrown = .true.
    err_message = trim(msg)

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

    logical,intent(out) :: status_ok
    character(len=:),allocatable,intent(out) :: error_msg

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

    logical :: failed

    failed = exception_thrown

    end function json_failed
!*****************************************************************************************

!*****************************************************************************************
!****f* json_module/json_value_create
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

    type(json_value), pointer :: p

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

        call this%data%destroy()

        if (associated(this%children))    call json_value_destroy(this%children)

        if (associated(this%next))        call json_value_destroy(this%next)

        deallocate(this)

        nullify(this)

    end if

    end subroutine json_value_destroy
!*****************************************************************************************

!*****************************************************************************************
!****f* json_module/json_value_remove
!
!  NAME
!    json_value_destroy
!
!  DESCRIPTION
!    Remove and destroy a json_value (and all its children) 
!        from a linked-list structure.
!    The rest of the structure is preserved.
!
!  AUTHOR
!    Jacob Williams : 9/9/2014
!
!  SOURCE

    subroutine json_value_remove(me)

    implicit none

    type(json_value),pointer :: me
    
    type(json_value),pointer :: parent,previous,next
    
    if (associated(me)) then
        if (associated(me%parent)) then
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
                    parent => me%parent                    
                    parent%children => next
                    next%previous => null()
                end if
                
            else
            
                if (associated(me%previous)) then
                    !there are earlier items in the list:
                    previous => me%previous 
                    previous%next => null()  
                else
                    !this is the only item in the list:
                    parent => me%parent 
                    parent%children => null()                   
                end if
                
            end if     
                   
        end if
            
        call json_value_destroy(me)

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
!    If the child variable is present, then remove it.
!
!  AUTHOR
!    Jacob Williams : 12/6/2014
!
!  SOURCE

    subroutine json_value_remove_if_present(p,name)
     
    implicit none
    
    type(json_value),pointer    :: p
    character(len=*),intent(in) :: name
    
    type(json_value),pointer :: p_var
    logical :: found
    
    call json_get(p,name,p_var,found)
    if (found) call json_remove(p_var)
     
    end subroutine json_value_remove_if_present
!*****************************************************************************************

!*****************************************************************************************
!****f* json_module/json_update_logical
!
!  NAME
!    json_update_logical
!
!  DESCRIPTION
!    If the child variable is present, and is a scalar, then update its value.
!    If it is not present, then create it and set its value.
!
!  AUTHOR
!    Jacob Williams : 12/6/2014
!
!  SOURCE

    subroutine json_update_logical(p,name,val,found)
     
    implicit none
    
    type(json_value), pointer   :: p
    character(len=*),intent(in) :: name
    logical,intent(in)          :: val
    logical,intent(out)         :: found
    
    type(json_value),pointer :: p_var
    integer :: var_type
 
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
        call json_value_add(p,name,val)   !add the new element
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
!    If the child variable is present, and is a scalar, then update its value.
!    If it is not present, then create it and set its value.
!
!  AUTHOR
!    Jacob Williams : 12/6/2014
!
!  SOURCE

    subroutine json_update_double(p,name,val,found)
     
    implicit none
    
    type(json_value), pointer   :: p
    character(len=*),intent(in) :: name
    real(wp),intent(in)         :: val
    logical,intent(out)         :: found
    
    type(json_value),pointer :: p_var
    integer :: var_type
 
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
        call json_value_add(p,name,val)   !add the new element
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
!    If the child variable is present, and is a scalar, then update its value.
!    If it is not present, then create it and set its value.
!
!  AUTHOR
!    Jacob Williams : 12/6/2014
!
!  SOURCE

    subroutine json_update_integer(p,name,val,found)

    implicit none
    
    type(json_value), pointer   :: p
    character(len=*),intent(in) :: name
    integer,intent(in)          :: val
    logical,intent(out)         :: found
    
    type(json_value),pointer :: p_var
    integer :: var_type
 
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
        call json_value_add(p,name,val)   !add the new element
    end if
                     
    end subroutine json_update_integer
!*****************************************************************************************

!*****************************************************************************************
!****f* json_module/json_update_chars
!
!  NAME
!    json_update_chars
!
!  DESCRIPTION
!    If the child variable is present, and is a scalar, then update its value.
!    If it is not present, then create it and set its value.
!
!  AUTHOR
!    Jacob Williams : 12/6/2014
!
!  SOURCE

    subroutine json_update_chars(p,name,val,found)
     
    implicit none
    
    type(json_value), pointer   :: p
    character(len=*),intent(in) :: name
    character(len=*),intent(in) :: val
    logical,intent(out)         :: found
    
    type(json_value),pointer :: p_var
    integer :: var_type
 
    call json_get(p,name,p_var,found)
    if (found) then

        call json_info(p_var,var_type)
        select case (var_type)
        case (json_null,json_logical,json_integer,json_double,json_string)
            call to_string(p_var,val)    !update the value
        case default
            found = .false.
            call throw_exception('Error in json_update_chars: '//&
                                 'the variable is not a scalar value')
        end select          

    else
        call json_value_add(p,name,val)   !add the new element
    end if
                     
    end subroutine json_update_chars
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

    type(json_value), pointer :: this, member

    type(json_value), pointer :: p

    if (.not. exception_thrown) then

        nullify(p)

        ! associate the parent
        member % parent => this

        ! add to linked list
        if (associated(this % children)) then

            ! get to the tail of the linked list
            p => this % children
            do while (associated(p % next))
                p => p % next
            end do

            p%next => member
            member%previous => p

            nullify(p)    !cleanup

        else

            this%children => member
            member%previous => null()

        end if

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

    type(json_value), pointer   :: me
    character(len=*),intent(in) :: name
    real(wp),intent(in)         :: val

    type(json_value),pointer :: var

    !create the variable:
    call json_value_create(var)
    call to_double(var,val,name)

    !add it:
    call json_value_add(me, var)

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

    type(json_value), pointer         :: me
    character(len=*),intent(in)       :: name
    real(wp),dimension(:),intent(in)  :: val

    type(json_value),pointer :: var
    integer :: i

    !create the variable as an array:
    call json_value_create(var)
    call to_array(var,name)

    !populate the array:
    do i=1,size(val)
        call json_value_add(var, '', val(i))
    end do

    !add it:
    call json_value_add(me, var)

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

    type(json_value), pointer     :: me
    character(len=*),intent(in)   :: name
    integer,intent(in)            :: val

    type(json_value),pointer :: var

    !create the variable:
    call json_value_create(var)
    call to_integer(var,val,name)

    !add it:
    call json_value_add(me, var)

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

    type(json_value), pointer       :: me
    character(len=*),intent(in)     :: name
    integer,dimension(:),intent(in) :: val

    type(json_value),pointer :: var
    integer :: i    !counter

    !create the variable as an array:
    call json_value_create(var)
    call to_array(var,name)

    !populate the array:
    do i=1,size(val)
        call json_value_add(var, '', val(i))
    end do

    !add it:
    call json_value_add(me, var)

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

    type(json_value), pointer   :: me
    character(len=*),intent(in) :: name
    logical,intent(in)          :: val

    type(json_value),pointer :: var

    !create the variable:
    call json_value_create(var)
    call to_logical(var,val,name)

    !add it:
    call json_value_add(me, var)

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

    type(json_value), pointer       :: me
    character(len=*),intent(in)     :: name
    logical,dimension(:),intent(in) :: val

    type(json_value),pointer :: var
    integer :: i    !counter

    !create the variable as an array:
    call json_value_create(var)
    call to_array(var,name)

    !populate the array:
    do i=1,size(val)
        call json_value_add(var, '', val(i))
    end do

    !add it:
    call json_value_add(me, var)

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

    type(json_value), pointer   :: me
    character(len=*),intent(in) :: name
    character(len=*),intent(in) :: val

    type(json_value),pointer        :: var
    character(len=:),allocatable    :: str

    !add escape characters if necessary:
    call escape_string(val, str)

    !create the variable:
    call json_value_create(var)
    call to_string(var,str,name)

    !add it:
    call json_value_add(me, var)

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

    character(len=*),intent(in)              :: str_in
    character(len=:),allocatable,intent(out) :: str_out

    integer :: i
    character(len=1) :: c

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
!    Add an array of character string to the structure.
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

    type(json_value), pointer                :: me
    character(len=*),intent(in)              :: name
    character(len=*),dimension(:),intent(in) :: val
    logical,intent(in),optional              :: trim_str
    logical,intent(in),optional              :: adjustl_str

    type(json_value),pointer :: var
    integer :: i
    logical :: trim_string, adjustl_string
    character(len=:),allocatable :: str

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
        call json_value_add(var, '', str)

        !cleanup
        deallocate(str)

    end do

    !add it:
    call json_value_add(me, var)

    !cleanup:
    nullify(var)

    end subroutine json_value_add_string_vec
!*****************************************************************************************

!*****************************************************************************************
!****f* json_module/json_value_count
!
!  NAME
!    json_value_count
!
!  DESCRIPTION
!    Count the number of children.
!
!  SOURCE

    function json_value_count(this) result(count)

    implicit none

    integer :: count
    type(json_value),pointer,intent(in) :: this

    type(json_value), pointer :: p

    if (.not. exception_thrown) then

        count = 0
        
        if (associated(this)) then

            if (associated(this%children)) then

                p => this%children

                do while (associated(p))
                    count = count + 1
                    p => p%next
                end do

                nullify(p)

            end if
            
        end if
        
    end if

    end function json_value_count
!*****************************************************************************************

!*****************************************************************************************
!****f* json_module/get_by_index
!
!  NAME
!    get_by_index
!
!  DESCRIPTION
!    Returns a child in the object given the index.
!
!  SOURCE

    subroutine get_by_index(this, idx, p)

    implicit none

    type(json_value),pointer,intent(in) :: this
    integer,intent(in)                  :: idx
    type(json_value), pointer           :: p

    integer :: i

    if (.not. exception_thrown) then

        nullify(p)

        if (associated(this%children)) then

            p => this%children

            do i = 1, idx - 1

                if (associated(p%next)) then
                    p => p%next
                else
                    call throw_exception('Error in get_by_index:'//&
                                         ' p%next is not associated.')
                    return
                end if

            end do

        else

            call throw_exception('Error in get_by_index:'//&
                                 ' this%children is not associated.')

        end if

    end if

    end subroutine get_by_index
!*****************************************************************************************

!*****************************************************************************************
!****f* json_module/get_by_name_chars
!
!  NAME
!    get_by_name_chars
!
!  DESCRIPTION
!    Returns a child in the object given the name string.
!
!  SOURCE

    subroutine get_by_name_chars(this, name, p)

    implicit none

    type(json_value),pointer,intent(in) :: this
    character(len=*),intent(in)         :: name
    type(json_value),pointer            :: p

    integer :: i

    if (.not. exception_thrown) then

        if (associated(this)) then

            nullify(p)

            if (this%data%var_type==json_object) then
                do i=1, json_value_count(this)
                    call json_value_get(this, i, p)
                    if (allocated(p%name)) then
                        if (p%name == name) return
                    end if
                end do
            end if

            !did not find anything:
            nullify(p)

        else
            call throw_exception('Error in get_by_name_chars: pointer is not associated.')
        end if

    end if

    end subroutine get_by_name_chars
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
    
    type(json_value),pointer,intent(in)        :: me
    character(len=:),intent(out),allocatable   :: str
    
    str = ''
    call json_value_print(me, iunit=0, str=str)
        
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
    
    type(json_value),pointer,intent(in)  :: me
    integer,intent(in) :: iunit                   !must be non-zero
    
    character(len=:),allocatable :: dummy
    
    if (iunit/=0) then
        call json_value_print(me,iunit,str=dummy)
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
    
    type(json_value),pointer,intent(in)  :: me
    character(len=*),intent(in) :: filename
    
    integer :: iunit,istat
    
    open(newunit=iunit,file=filename,status='REPLACE',iostat=istat)
    if (istat==0) then
        call json_print(me,iunit)
        close(iunit,iostat=istat)
    else
        call throw_exception('Error in json_print: could not open file: '//trim(filename))
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

    recursive subroutine json_value_print(this,iunit,indent,need_comma,colon,str)

    implicit none

    type(json_value),pointer,intent(in)  :: this
    integer,intent(in)                   :: iunit     !file unit to write to (6=console)
    integer,intent(in),optional          :: indent
    logical,intent(in),optional          :: need_comma
    logical,intent(in),optional          :: colon
    character(len=:),intent(inout),allocatable :: str !if iunit==0, then the structure is 
                                                      ! printed to this string rather than 
                                                      ! a file. This mode is used by 
                                                      ! json_value_to_string.

    type(json_value), pointer :: element
    integer :: tab, i, count, spaces
    character(len=32) :: tmp    !for val to string conversions
    logical :: print_comma
    logical :: print_spaces
    logical :: write_file, write_string

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
        !if the colon was the last thing written
        if (present(colon)) then
            print_spaces = .not. colon
        else
            print_spaces = .true.
        end if

        if (present(indent)) then
            tab = indent
        else
            tab = 0
        end if

        if (print_spaces) then
            spaces = tab * 2
        else
            spaces = 0
        end if
        
        nullify(element)

        !associate (d => this%data)

            select case (this%data%var_type)

            case (json_object)
                
                call write_it( repeat(space, spaces)//'{' )
                 
                count = json_value_count(this)
                do i = 1, count

                    ! get the element
                    call json_value_get(this, i, element)

                    ! print the name
                    if (allocated(element%name)) then
                        call write_it(repeat(space, spaces)//'"'//&
                                      trim(element % name)//'": ',advance=.false.)
                    else
                        call throw_exception('Error in json_value_print:'//&
                                             ' element%name not allocated')
                        call cleanup()
                        return
                    end if

                    ! recursive print of the element
                    call json_value_print(element, iunit=iunit, indent=tab + 1, &
                                          need_comma=i<count, colon=.true., str=str)

                end do

                call write_it( repeat(space, spaces)//'}', comma=print_comma )

            case (json_array)

                call write_it( '[' )
                count = json_value_count(this)

                do i = 1, count

                    ! get the element
                    call json_value_get(this, i, element)

                    ! recursive print of the element
                    call json_value_print(element, iunit=iunit, indent=tab + 1, &
                                          need_comma=i<count, str=str)

                end do
                
                !indent the closing array character:
                call write_it( repeat(space, tab * 2)//']', comma=print_comma ) 

            case (json_null)

                call write_it( repeat(space, spaces)//null_str, comma=print_comma )

            case (json_string)

                if (allocated(this%data%str_value)) then
                    call write_it( repeat(space, spaces)//'"'// &
                                   trim(this%data%str_value)//'"', comma=print_comma )
                else
                    call throw_exception('Error in json_value_print:'//&
                                         ' this%value_string not allocated')
                    call cleanup()
                    return
                end if

            case (json_logical)

                if (this%data%log_value) then
                    call write_it( repeat(space, spaces)//true_str, comma=print_comma )
                else
                    call write_it( repeat(space, spaces)//false_str, comma=print_comma )
                end if

            case (json_integer)

                call integer_to_string(this%data%int_value,tmp)

                call write_it( repeat(space, spaces)//trim(tmp), comma=print_comma )

            case (json_double)

                call real_to_string(this%data%dbl_value,tmp)

                call write_it( repeat(space, spaces)//trim(tmp), comma=print_comma )

            case default

                call throw_exception('Error in json_value_print: unknown data type')

            end select

        !end associate

        call cleanup()

    end if

    contains

        !-------------------------------
        ! cleanup routine
        !-------------------------------
        subroutine cleanup()
        implicit none

        if (associated(element)) nullify(element)

        end subroutine cleanup
         !-------------------------------
   
        !-------------------------------
        ! write the string to the file (or the output string)
        !-------------------------------
        subroutine write_it(s,advance,comma)

        implicit none

        character(len=*),intent(in) :: s
        logical,intent(in),optional :: advance
        logical,intent(in),optional :: comma
       
        logical :: add_line_break, add_comma
        character(len=:),allocatable :: s2
        
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
        if (add_comma) s2 = s2 // ','
        
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
        !-------------------------------

    end subroutine json_value_print
!*****************************************************************************************

!*****************************************************************************************
!****f* json_module/json_get_by_path
!
!  NAME
!    json_get_by_path
!
!  DESCRIPTION
!    Returns the json_value pointer given the path string.
!
!  NOTES
!     $         root
!     @         this
!     .         child object member
!     [] or ()  child array element
!
!  SOURCE

    subroutine json_get_by_path(this, path, p, found)

    implicit none

    type(json_value),pointer,intent(in)     :: this
    character(len=*),intent(in)             :: path
    type(json_value),pointer,intent(out)    :: p
    logical,intent(out),optional            :: found

    integer :: i, length, child_i
    character(len=1) :: c
    logical :: array
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
                do while (associated (p % parent))
                    p => p % parent
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
                    call json_value_get(p, path(child_i:i-1), tmp)
                    p => tmp
                    nullify(tmp)
                else
                    child_i = i + 1
                    cycle
                end if

                if (.not.associated(p)) then
                    call throw_exception('Error in json_get_by_path:'//&
                                         ' Error getting child member.')
                    exit
                end if

                child_i = i+1

            case ('[','(')

                !....Modified to allow for 'var[3]' style syntax
                !Note: jmozmoz/fson has a slightly different version of this...

                ! start looking for the array element index
                array = .true.

                ! get child member from p
                if (child_i < i) then
                    nullify(tmp)
                    call json_value_get(p, path(child_i:i-1), tmp)
                    p => tmp
                    nullify(tmp)
                else
                    child_i = i + 1
                    cycle
                end if
                if (.not.associated(p)) then
                    call throw_exception('Error in json_get_by_path:'//&
                                         ' Error getting array element')
                    exit
                end if
                child_i = i + 1

            case (']',')')

                if (.not.array) then
                    call throw_exception('Error in json_get_by_path: Unexpected ]')
                    exit
                end if
                array = .false.
                child_i = string_to_integer(path(child_i:i-1))

                nullify(tmp)
                call json_value_get(p, child_i, tmp)
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
                call json_value_get(p, path(child_i:i-1), tmp)
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

    integer                     :: ival
    character(len=*),intent(in) :: str

    integer :: ierr

    if (.not. exception_thrown) then

        read(str,*,iostat=ierr) ival        !string to integer

        if (ierr/=0) then                   !if there was an error
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

    real(wp)                    :: rval
    character(len=*),intent(in) :: str

    integer :: ierr

    if (.not. exception_thrown) then

        read(str,fmt=real_fmt,iostat=ierr) rval    !string to double

        if (ierr/=0) then    !if there was an error
            rval = 0.0_wp
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

    type(json_value),pointer,intent(in)  :: this
    character(len=*),optional            :: path
    integer,intent(out)                  :: value
    logical,intent(out),optional         :: found

    type(json_value), pointer :: p

    if (.not. exception_thrown) then

        nullify(p)
        if (present(path)) then
            call json_get_by_path(this=this, path=path, p=p)
        else
            p => this
        end if

        if (.not.associated(p)) then

            call throw_exception('Error in json_get_integer:'//&
                                 ' Unable to resolve path: '// trim(path))

        else

            !associate (d => p%data)
                select case(p%data%var_type)
                case (json_integer)
                    value = p%data%int_value
                case (json_double)
                    value = int(p%data%dbl_value)
                case (json_logical)
                    if (p%data%log_value) then
                        value = 1
                    else
                        value = 0
                    end if
                case default
                    call throw_exception('Error in get_integer:'//&
                                         ' Unable to resolve value to integer: '//&
                                         trim(path))
                end select
            !end associate

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

    type(json_value), pointer                       :: me
    character(len=*),intent(in)                     :: path
    integer,dimension(:),allocatable,intent(out)    :: vec
    logical,intent(out),optional                    :: found

    logical :: initialized

    initialized = .false.

    if (allocated(vec)) deallocate(vec)

    !the callback function is called for each element of the array:
    call json_get(me, path=path, array_callback=get_int_from_array, found=found)

    contains

        ! callback function for integer
        subroutine get_int_from_array(element, i, count)
        implicit none

        type(json_value),pointer,intent(in)     :: element
        integer,intent(in)                      :: i        !index
        integer,intent(in)                      :: count    !size of array

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

    type(json_value), pointer       :: this
    character(len=*), optional      :: path
    real(wp),intent(out)            :: value
    logical,intent(out),optional    :: found

    type(json_value), pointer :: p

    if (.not. exception_thrown) then

        nullify(p)

        if (present(path)) then
            call json_get_by_path(this=this, path=path, p=p)
        else
            p => this
        end if

        if (.not.associated(p)) then

            call throw_exception('Error in json_get_double:'//&
                                 ' Unable to resolve path: '//trim(path))

        else

            !associate (d => p%data)
                select case (p%data%var_type)
                case (json_integer)
                    value = p%data%int_value
                case (json_double)
                    value = p%data%dbl_value
                case (json_logical)
                    if (p%data%log_value) then
                        value = 1.0_wp
                    else
                        value = 0.0_wp
                    end if
                case default
                    call throw_exception('Error in json_get_double:'//&
                                         ' Unable to resolve value to double: '//&
                                         trim(path))
                end select
            !end associate

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

        value = 0.0_wp
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

    type(json_value), pointer                       :: me
    character(len=*),intent(in)                     :: path
    real(wp),dimension(:),allocatable,intent(out)    :: vec
    logical,intent(out),optional                    :: found

    logical :: initialized

    initialized = .false.

    if (allocated(vec)) deallocate(vec)

    !the callback function is called for each element of the array:
    call json_get(me, path=path, array_callback=get_double_from_array, found=found)

    contains

        ! callback function for double
        subroutine get_double_from_array(element, i, count)
        implicit none

        type(json_value),pointer,intent(in)     :: element
        integer,intent(in)                      :: i        !index
        integer,intent(in)                      :: count    !size of array

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
    character(len=*),optional           :: path
    logical                             :: value
    logical,intent(out),optional        :: found

    type(json_value), pointer :: p

    if (.not. exception_thrown) then

        nullify(p)

        if (present(path)) then
            call json_get_by_path(this=this, path=path, p=p)
        else
            p => this
        end if

        if (.not.associated(p)) then

            call throw_exception('Error in json_get_logical:'//&
                                 ' Unable to resolve path: '//trim(path))

        else

            !associate (d => p%data)
                select case (p%data%var_type)
                case (json_integer)
                    value = (p%data%int_value > 0)
                case (json_logical)
                    value = p%data % log_value
                case default
                    call throw_exception('Error in json_get_logical:'//&
                                         ' Unable to resolve value to logical: '//&
                                         trim(path))
                end select
            !end associate

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

    type(json_value),pointer,intent(in)            :: me
    character(len=*),intent(in)                    :: path
    logical,dimension(:),allocatable,intent(out)   :: vec
    logical,intent(out),optional                   :: found

    logical :: initialized

    initialized = .false.

    if (allocated(vec)) deallocate(vec)

    !the callback function is called for each element of the array:
    call json_get(me, path=path, array_callback=get_logical_from_array, found=found)

    contains

        ! callback function for logical
        subroutine get_logical_from_array(element, i, count)
        implicit none

        type(json_value),pointer,intent(in)  :: element
        integer,intent(in)                   :: i        !index
        integer,intent(in)                   :: count    !size of array

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
!****f* json_module/json_get_chars
!
!  NAME
!    json_get_chars
!
!  DESCRIPTION
!    Get a character string from a json_value.
!
!  SOURCE

    subroutine json_get_chars(this, path, value, found)

    implicit none

    type(json_value),pointer,intent(in)         :: this
    character(len=*),intent(in),optional        :: path
    character(len=:),allocatable,intent(out)    :: value
    logical,intent(out),optional                :: found

    type(json_value), pointer :: p
    character(len=:),allocatable :: s,pre,post
    integer :: j,jprev,n
    character(len=1) :: c

    if (.not. exception_thrown) then

        nullify(p)

        if (present(path)) then
            call json_get_by_path(this=this, path=path, p=p)
        else
            p => this
        end if

        if (.not.associated(p)) then

            call throw_exception('Error in json_get_chars:'//&
                                 ' Unable to resolve path: '//trim(path))

        else

            !associate (d => p%data)
                select case (p%data%var_type)
                case (json_string)
                    if (allocated(p%data%str_value)) then

                        !get the value as is:
                        s = p%data%str_value

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
                                                'Error in json_get_chars:'//&
                                                ' Invalid hexadecimal sequence'//&
                                                ' in string: '//trim(c))
                                            exit
                                        end if

                                    case default
                                        !unknown escape character
                                        call throw_exception('Error in json_get_chars:'//&
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
                        call throw_exception('Error in json_get_chars:'//&
                                             ' p%data%value not allocated')
                    end if

                !class default
                case default

                    call throw_exception('Error in json_get_chars:'//&
                                         ' Unable to resolve value to characters: '//&
                                         trim(path))

                    ! Note: for the other cases, we could do val to string conversions.

                end select
            !end associate

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

    end subroutine json_get_chars
!*****************************************************************************************

!*****************************************************************************************
!****f* json_module/json_get_char_vec
!
!  NAME
!    json_get_char_vec
!
!  DESCRIPTION
!    Get a char vector from a JSON file.
!
!  AUTHOR
!    Jacob Williams : 5/14/2014
!
!  SOURCE

    subroutine json_get_char_vec(me, path, vec, found)

    implicit none

    type(json_value),pointer,intent(in)                    :: me
    character(len=*),intent(in)                            :: path
    character(len=*),dimension(:),allocatable,intent(out)  :: vec
    logical,intent(out),optional                           :: found

    logical :: initialized

    initialized = .false.

    if (allocated(vec)) deallocate(vec)

    !the callback function is called for each element of the array:
    call json_get(me, path=path, array_callback=get_chars_from_array, found=found)

    contains

        ! callback function for chars
        subroutine get_chars_from_array(element, i, count)
        
        implicit none

        type(json_value),pointer,intent(in)  :: element
        integer,intent(in)                   :: i        !index
        integer,intent(in)                   :: count    !size of array

        character(len=:),allocatable :: cval

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

    end subroutine json_get_char_vec
!*****************************************************************************************

!*****************************************************************************************
!****f* json_module/json_get_array
!
!  NAME
!    json_get_array
!
!  DESCRIPTION
!    Get an array from a json_value.
!    This routine calls the user-supplied array_callback subroutine
!        for each element in the array.
!
!  SOURCE

    subroutine json_get_array(this, path, array_callback, found)

    implicit none

    type(json_value),pointer,intent(in)  :: this
    character(len=*),intent(in),optional :: path
    procedure(array_callback_func)       :: array_callback
    logical,intent(out),optional         :: found

    type(json_value), pointer :: element,p
    integer :: i, count

    if (.not. exception_thrown) then

        nullify(p)

        ! resolve the path to the value
        if (present(path)) then
            call json_get_by_path(this=this, path=path, p=p)
        else
            p => this
        end if

        if (.not.associated(p)) then

            call throw_exception('Error in json_get_array:'//&
                                 ' Unable to resolve path: '//trim(path))

        else

            !associate (d => p%data)
                select case (p%data%var_type)
                case (json_array)
                    count = json_value_count(p)
                    do i = 1, count
                        call json_value_get(p, i, element)
                        call array_callback(element, i, count)
                    end do
                case default
                    call throw_exception('Error in json_get_array:'//&
                                         ' Resolved value is not an array. '//trim(path))
                end select
            !end associate

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
!  NOTES
!    When calling this routine, any exceptions thrown from previous
!        calls will automatically be cleared.
!
!  SOURCE
 
    subroutine json_parse(file, p, unit)

    implicit none

    character(len=*),intent(in) :: file
    type(json_value),pointer    :: p
    integer,intent(in),optional :: unit

    integer :: iunit, istat
    character(len=:),allocatable :: line, arrow_str
    character(len=10) :: line_str, char_str
    logical :: is_open

    !clean any exceptions and initialize:
    call json_initialize()

    if (present(unit)) then
        
        iunit = unit   
        
        !check to see if the file is already open
        ! if it is, then use it, otherwise open the file.
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
        
    else
    
        ! open the file with a new unit number:
        open (  newunit     = iunit, &
                file        = file, &
                status      = 'OLD', &
                action      = 'READ', &
                form        = 'FORMATTED', &
                position    = 'REWIND', &
                iostat      = istat)
    
    end if

    if (istat==0) then

        ! create the value and associate the pointer
        call json_value_create(p)

        !add the file name as the name of the overall structure:
        p%name = trim(file)

        ! parse as a value
        call parse_value(unit = iunit, value = p)

        !
        !  If there was an error reading the file, then
        !   can print the line where the error occurred:
        !
        if (exception_thrown) then
        
            call get_current_line_from_file(iunit,line)
            
            !the counters for the current line and the last character read:
            call integer_to_string(line_count, line_str)
            call integer_to_string(char_count, char_str)
            
            !draw the arrow string that points to the current character:
            arrow_str = repeat('-',max( 0, char_count - 1) )//'^'

            !create the error message:
            err_message = err_message//newline//&
                           'line: '//trim(adjustl(line_str))//', '//&
                           'character: '//trim(adjustl(char_str))//newline//&
                           trim(line)//newline//arrow_str
                                        
            if (allocated(line)) deallocate(line)
                        
        end if

        ! close the file
        close (iunit, iostat=istat)

    else

        call throw_exception('Error in json_parse: Error opening file: '//trim(file))

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
    
    integer,intent(in)    :: iunit
    character(len=:),allocatable,intent(out)    :: line
    
    integer,parameter           :: n_chunk = 256      !chunk size [arbitrary]
    character(len=*),parameter  :: nfmt = '(A256)'    !corresponding format statement
    
    character(len=n_chunk) :: chunk
    integer :: istat,isize
    
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

    recursive subroutine parse_value(unit, value)

    implicit none

    integer, intent(in)       :: unit
    type(json_value), pointer :: value

    logical :: eof
    character(len=1) :: c
    character(len=:),allocatable :: tmp  !this is a work-around for a bug 
                                         !  in the gfortran 4.9 compiler.

    if (.not. exception_thrown) then

        !the routine is being called incorrectly.
        if (.not. associated(value)) then
            call throw_exception('Error in parse_value: value pointer not associated.')
        end if

        ! pop the next non whitespace character off the file
        c = pop_char(unit, eof = eof, skip_ws = .true.)

        if (eof) then
            return
        else
            select case (c)
            case ('{')

                ! start object
                call to_object(value)    !allocate class
                call parse_object(unit, value)

            case ('[')

                ! start array
                call to_array(value)    !allocate class
                call parse_array(unit, value)

            case (']')

                ! end an empty array
                call push_char(c)
                nullify(value)

            case ('"')

                ! string
                call to_string(value)    !allocate class

                 !associate (d => value%data)
                    !select type (value%data)
                    select case (value%data%var_type)
                    !type is (json_string)
                    case (json_string)
                        call parse_string(unit, tmp)  !write to a tmp variable because of
                        value%data%str_value = tmp    ! a bug in 4.9 gfortran compiler.
                        deallocate(tmp)
                    end select
                !end associate

            case (true_str(1:1))

                !true
                call parse_for_chars(unit, true_str(2:))
                !allocate class and set value:
                if (.not. exception_thrown) call to_logical(value,.true.)    

            case (false_str(1:1))

                !false
                call parse_for_chars(unit, false_str(2:))
                !allocate class and set value:
                if (.not. exception_thrown) call to_logical(value,.false.)

            case (null_str(1:1))

                !null
                call parse_for_chars(unit, null_str(2:))
                if (.not. exception_thrown) call to_null(value)    !allocate class

            case('-', '0': '9')

                call push_char(c)
                call parse_number(unit, value)

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
!****f* json_module/to_logical
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

    type(json_value),intent(inout)         :: me
    character(len=*),intent(in),optional   :: name
    logical,intent(in),optional            :: val

    !set type and value:
    !associate (d => me%data)
        call me%data%destroy()
        me%data%var_type = json_logical
        allocate(me%data%log_value)
        if (present(val)) then
            me%data%log_value = val
        else
            me%data%log_value = .false.    !default value
        end if
    !end associate

    !name:
    if (present(name)) me%name = trim(name)

    end subroutine to_logical
!*****************************************************************************************

!*****************************************************************************************
!****f* json_module/to_integer
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

    type(json_value),intent(inout)         :: me
    character(len=*),intent(in),optional   :: name
    integer,intent(in),optional            :: val

    !set type and value:
    !associate (d => me%data)
        call me%data%destroy()
        me%data%var_type = json_integer
        allocate(me%data%int_value)
        if (present(val)) then
            me%data%int_value = val
        else
            me%data%int_value = 0    !default value
        end if
    !end associate

    !name:
    if (present(name)) me%name = trim(name)

    end subroutine to_integer
!*****************************************************************************************

!*****************************************************************************************
!****f* json_module/to_double
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

    type(json_value),intent(inout)         :: me
    character(len=*),intent(in),optional   :: name
    real(wp),intent(in),optional           :: val

    !set type and value:
    !associate (d => me%data)
        call me%data%destroy()
        me%data%var_type = json_double
        allocate(me%data%dbl_value)
        if (present(val)) then
            me%data%dbl_value = val
        else
            me%data%dbl_value = 0.0_wp    !default value
        end if
    !end associate

    !name:
    if (present(name)) me%name = trim(name)

    end subroutine to_double
!*****************************************************************************************

!*****************************************************************************************
!****f* json_module/to_string
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

    type(json_value),intent(inout)         :: me
    character(len=*),intent(in),optional   :: name
    character(len=*),intent(in),optional   :: val

    !set type and value:
    !associate (d => me%data)
        call me%data%destroy()
        me%data%var_type = json_string
        if (present(val)) then
            me%data%str_value = val
        else
            me%data%str_value = ''    !default value
        end if
    !end associate

    !name:
    if (present(name)) me%name = trim(name)

    end subroutine to_string
!*****************************************************************************************

!*****************************************************************************************
!****f* json_module/to_null
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

    type(json_value),intent(inout)         :: me
    character(len=*),intent(in),optional   :: name

    !set type and value:
    !associate (d => me%data)
        call me%data%destroy()
        me%data%var_type = json_null
    !end associate

    !name:
    if (present(name)) me%name = trim(name)

    end subroutine to_null
!*****************************************************************************************

!*****************************************************************************************
!****f* json_module/to_object
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

    type(json_value),intent(inout)         :: me
    !type(json_value),pointer,intent(inout) :: me  !this causes crash in gfortran (compiler bug?)
    character(len=*),intent(in),optional   :: name
	
    !set type and value:
    !associate (d => me%data)
        call me%data%destroy()
        me%data%var_type = json_object
    !end associate

    !name:
    if (present(name)) me%name = trim(name)

    end subroutine to_object
!*****************************************************************************************

!*****************************************************************************************
!****f* json_module/to_array
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

    type(json_value),intent(inout)         :: me
    character(len=*),intent(in),optional   :: name

    !set type and value:
    !associate (d => me%data)
        call me%data%destroy()
        me%data%var_type = json_array
    !end associate

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

    recursive subroutine parse_object(unit, parent)

    implicit none

    integer, intent(in)       :: unit
    type(json_value), pointer :: parent

    type(json_value), pointer :: pair
    logical :: eof
    character(len=1) :: c
    character(len=:),allocatable :: tmp  !this is a work-around for a bug 
                                         !  in the gfortran 4.9 compiler.

    if (.not. exception_thrown) then

        !the routine is being called incorrectly.
        if (.not. associated(parent)) then
            call throw_exception('Error in parse_object: parent pointer not associated.')
        end if

        nullify(pair)    !probably not necessary

        ! pair name
        c = pop_char(unit, eof = eof, skip_ws = .true.)
        if (eof) then
            call throw_exception('Error in parse_object:'//&
                                 ' Unexpected end of file while parsing start of object.')
            call cleanup()
            return
        else if ('}' == c) then
            ! end of an empty object
            call cleanup()
            return
        else if ('"' == c) then
            call json_value_create(pair)
            call parse_string(unit, tmp)   !write to a tmp variable because of
            pair % name = tmp              ! a bug in 4.9 gfortran compiler.
            deallocate(tmp)
            if (exception_thrown) return
        else
            call throw_exception('Error in parse_object: Expecting string: "'//c//'"')
            call cleanup()
            return
        end if

        ! pair value
        c = pop_char(unit, eof = eof, skip_ws = .true.)
        if (eof) then
            call throw_exception('Error in parse_object:'//&
                                 ' Unexpected end of file while parsing object member.')
            call cleanup()
            return
        else if (':' == c) then
            ! parse the value
            call parse_value(unit, pair)
            if (exception_thrown) return
            call json_value_add(parent, pair)
        else
            call throw_exception('Error in parse_object:'//&
                                 ' Expecting : and then a value: '//c)
            call cleanup()
            return
        end if

        ! another possible pair
        c = pop_char(unit, eof = eof, skip_ws = .true.)
        if (eof) then
            call cleanup()
            return
        else if (',' == c) then
            ! read the next member
            call parse_object(unit = unit, parent = parent)
        else if ('}' == c) then
            call cleanup()
            return
        else
            call throw_exception('Error in parse_object: Expecting end of object: '//c)
            call cleanup()
            return
        end if

        call cleanup()

    end if

    contains

        !cleanup routine:
        subroutine cleanup()
        
        implicit none

        if (associated(pair)) nullify(pair)

        end subroutine cleanup

    end subroutine parse_object
!*****************************************************************************************

!*****************************************************************************************
!****if* json_module/parse_array
!
!  NAME
!    parse_array
!
!  DESCRIPTION
!
!
!  SOURCE

    recursive subroutine parse_array(unit, array)

    implicit none

    integer, intent(in)       :: unit
    type(json_value), pointer :: array

    type(json_value), pointer :: element
    logical :: eof
    character(len=1) :: c

    if (.not. exception_thrown) then
        nullify(element)

        ! try to parse an element value
        call json_value_create(element)
        call parse_value(unit, element)
        if (exception_thrown) return

        ! parse value will disassociate an empty array value
        if (associated(element)) then
            call json_value_add(array, element)
            nullify(element)    !cleanup
        end if

        ! popped the next character
        c = pop_char(unit, eof = eof, skip_ws = .true.)

        if (eof) then
            return
        else if (',' == c) then
            ! parse the next element
            call parse_array(unit, array)
            if (exception_thrown) return
        else if (']' == c) then
            ! end of array
            return
        end if

    end if

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

    subroutine parse_string(unit, string)

    implicit none

    integer, intent(in)                      :: unit
    character(len=:),allocatable,intent(out) :: string

    logical :: eof, is_hex, escape
    character(len=1) :: c, last
    character(len=4) :: hex
    integer :: i
    
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
            c = pop_char(unit, eof = eof, skip_ws = .false.)
        
            if (eof) then
            
                call throw_exception('Error in parse_string: Expecting end of string')
                return
                
            else if ('"' == c .and. last /= backslash) then
            
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
!
!
!  SOURCE

    subroutine parse_for_chars(unit, chars)

    implicit none

    integer, intent(in)            :: unit
    character(len = *), intent(in) :: chars

    integer :: i, length
    logical :: eof
    character(len=1) :: c

    if (.not. exception_thrown) then

        length = len_trim(chars)

        do i = 1, length
            c = pop_char(unit, eof = eof, skip_ws = .true.)
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

    subroutine parse_number(unit, value)

    implicit none

    integer, intent(in)       :: unit
    type(json_value), pointer :: value

    character(len=:),allocatable :: str
    character(len=1) :: c
    logical :: eof
    real(wp) :: rval
    integer :: ival
    logical :: first
    logical :: is_integer

    if (.not. exception_thrown) then

        str = ''
        first = .true.
        is_integer = .true.    !assume it may be an integer,unless otherwise determined

        !read one character at a time and accumulate the string:
        do

            !get the next character:
            c = pop_char(unit, eof = eof, skip_ws = .true.)

            if (eof) then
                call throw_exception('Error in parse_number:'//&
                                     ' Unexpected end of file while parsing number.')
                return
            else

                select case (c)
                case('-','+')    !note: allowing a '+' as the first character here.

                    if (is_integer .and. (.not. first)) is_integer = .false.

                    !add it to the string:
                    str = str // c

                case('.','E','e')    !can be present in real numbers

                    if (is_integer) is_integer = .false.

                    !add it to the string:
                    str = str // c

                case('0':'9')    !valid characters for numbers

                    !add it to the string:
                    str = str // c

                case default

                    !push back the last character read:
                    call push_char(c)

                    !string to value:
                    if (is_integer) then
                        ival = string_to_integer(str)
                        call to_integer(value,ival)
                    else
                        rval = string_to_double(str)
                        call to_double(value,rval)
                    end if

                    exit    !finished

                end select

            end if
            if (first) first = .false.

        end do

        !cleanup:
        if (allocated(str)) deallocate(str)

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
!    Get the next character from the file.
!
!  NOTES
!    This routine ignores non-printing ascii characters (iachar<=31) that
!    are in strings.
!
!  SOURCE

    recursive function pop_char(unit, eof, skip_ws) result(popped)
    
    implicit none

    character(len=1)              :: popped
    integer, intent(in)           :: unit
    logical, intent(out)          :: eof
    logical, intent(in), optional :: skip_ws

    integer :: ios
    character(len=1) :: c
    logical :: ignore

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

                read (unit = unit, fmt = '(A)', advance = 'NO', iostat = ios) c
                char_count = char_count + 1    !character count in the current line

                if (IS_IOSTAT_EOR(ios)) then            !JW : use intrinsic

                    char_count = 0
                    line_count = line_count + 1
                    cycle

                else if (IS_IOSTAT_END(ios)) then        !JW : use intrinsic

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

    character(len=1), intent(in) :: c

    character(len=32) :: istr

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

    integer,intent(in)           :: ival
    character(len=*),intent(out) :: str

    integer :: istat

    write(str,fmt=int_fmt,iostat=istat) ival

    if (istat==0) then
        str = adjustl(str)
    else
        str = repeat('*',len(str))
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

    real(wp),intent(in)          :: rval
    character(len=*),intent(out) :: str

    integer :: istat

    write(str,fmt=real_fmt,iostat=istat) rval

    if (istat==0) then
        str = adjustl(str)
    else
        str = repeat('*',len(str))
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

    character(len=*),intent(in) :: str
    logical :: valid
    
    integer :: n,i
    
    !an array of the valid hex characters:
    character(len=1),dimension(16),parameter :: valid_chars = &
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
