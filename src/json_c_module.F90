!*****************************************************************************************
!> author: Jacob Williams
!  license: BSD
!
!  A C interface to JSON-Fortran.
!
!  This module provides `bind(C)` routines for creating, navigating,
!  modifying, and destroying [[json_value]] linked lists from C.
!  The C declarations are in `jsonfortran.h`.
!
!  It is only compiled for the default kinds (`IK=c_int`, `RK=c_double`, and
!  `CK=c_char`). Builds using other kinds or `USE_UCS4` will omit this module.
!
!### Notes
!  * A handle to a [[json_value]] is just its C address.
!  * A root is owned by the caller, everything else is borrowed from the root.
!  * Errors are never sticky. Each call clears the previous error.
!
!### License
!  * JSON-Fortran is released under a BSD-style license.
!    See the [LICENSE](https://github.com/jacobwilliams/json-fortran/blob/master/LICENSE)
!    file for details.

#if !defined(INT8) && !defined(INT16) && !defined(INT64) && !defined(REAL32) && \
    !defined(REAL128) && !defined(USE_UCS4)

    module json_c_module

    use,intrinsic :: iso_c_binding
    use json_kinds
    use json_parameters, only: json_null, json_object, json_array, json_logical, &
                               json_integer, json_real, json_string
    use json_value_module, only: json_core, json_value

    implicit none

    private

    integer(c_int),parameter,public :: JSONF_API_VERSION_NUMBER = 1 !! version of the C API

    ! values of `jsonf_status` in `jsonfortran.h`:
    integer(c_int),parameter,public :: JSONF_OK               = 0
    integer(c_int),parameter,public :: JSONF_NOT_FOUND        = 1
    integer(c_int),parameter,public :: JSONF_TYPE_ERROR       = 2
    integer(c_int),parameter,public :: JSONF_INVALID_ARGUMENT = 3
    integer(c_int),parameter,public :: JSONF_ERROR            = 4

    ! values of `jsonf_type` in `jsonfortran.h`:
    integer(c_int),parameter :: type_null    = 0
    integer(c_int),parameter :: type_object  = 1
    integer(c_int),parameter :: type_array   = 2
    integer(c_int),parameter :: type_logical = 3
    integer(c_int),parameter :: type_integer = 4
    integer(c_int),parameter :: type_real    = 5
    integer(c_int),parameter :: type_string  = 6

    type,bind(C),public :: jsonf_options
        !! Settings for [[jsonf_core_new]].
        integer(c_int) :: case_sensitive_keys  = 1 !! if name and path comparisons are case sensitive
        integer(c_int) :: strict_type_checking = 0 !! if true, no type conversions are done in `get`
        integer(c_int) :: path_mode            = 1 !! 1 : default, 2 : RFC 6901, 3 : JSONPath bracket
    end type jsonf_options

    type :: jsonf_core_t
        !! The object behind a `jsonf_core` handle.
        type(json_core) :: json  !! the [[json_core(type)]] used for all the operations
        character(kind=c_char,len=1),dimension(:),allocatable :: err !! null-terminated message
                                                                     !! (not allocated if no error)
    end type jsonf_core_t

    interface
        function c_malloc(n) result(p) bind(C,name='malloc')
            import :: c_ptr, c_size_t
            implicit none
            integer(c_size_t),value :: n
            type(c_ptr) :: p
        end function c_malloc
        subroutine c_free(p) bind(C,name='free')
            import :: c_ptr
            implicit none
            type(c_ptr),value :: p
        end subroutine c_free
        function c_strlen(s) result(n) bind(C,name='strlen')
            import :: c_ptr, c_size_t
            implicit none
            type(c_ptr),value :: s
            integer(c_size_t) :: n
        end function c_strlen
    end interface

    ! lifecycle:
    public :: jsonf_options_default, jsonf_core_new, jsonf_core_free
    public :: jsonf_last_error, jsonf_free, jsonf_api_version
    ! create:
    public :: jsonf_new_object, jsonf_new_array, jsonf_new_null, jsonf_new_logical
    public :: jsonf_new_integer, jsonf_new_real, jsonf_new_string
    ! tree structure:
    public :: jsonf_add, jsonf_detach, jsonf_destroy, jsonf_clone, jsonf_rename
    ! inspect and navigate:
    public :: jsonf_info, jsonf_name, jsonf_first_child, jsonf_next, jsonf_parent
    public :: jsonf_child_by_name, jsonf_child_by_index, jsonf_get_by_path
    ! get:
    public :: jsonf_get_logical, jsonf_get_integer, jsonf_get_real, jsonf_get_string
    ! set:
    public :: jsonf_set_logical, jsonf_set_integer, jsonf_set_real, jsonf_set_string
    public :: jsonf_set_null
    ! vectors:
    public :: jsonf_get_real_vec, jsonf_get_integer_vec

    contains
!*****************************************************************************************

!*****************************************************************************************
!>
!  Clear the error for this core.

    subroutine reset_error(c)

    implicit none

    type(jsonf_core_t),intent(inout) :: c

    call c%json%clear_exceptions()
    if (allocated(c%err)) deallocate(c%err)

    end subroutine reset_error
!*****************************************************************************************

!*****************************************************************************************
!>
!  Store the error message for [[jsonf_last_error]].

    subroutine set_error(c,msg)

    implicit none

    type(jsonf_core_t),intent(inout)    :: c
    character(kind=CK,len=*),intent(in) :: msg

    integer :: i  !! counter

    if (allocated(c%err)) deallocate(c%err)
    allocate(c%err(len(msg)+1))
    do i = 1, len(msg)
        c%err(i) = msg(i:i)
    end do
    c%err(len(msg)+1) = c_null_char

    end subroutine set_error
!*****************************************************************************************

!*****************************************************************************************
!>
!  Record an error from the C interface itself, and return `status`.

    function fail(c,status,msg) result(r)

    implicit none

    type(jsonf_core_t),intent(inout)    :: c
    integer(c_int),intent(in)           :: status
    character(kind=CK,len=*),intent(in) :: msg
    integer(c_int)                      :: r

    call set_error(c,msg)
    r = status

    end function fail
!*****************************************************************************************

!*****************************************************************************************
!>
!  Move any [[json_core(type)]] exception to the core's error message.
!  Returns `JSONF_OK`, or `status` if there was an exception.

    function status_from_core(c,status) result(r)

    implicit none

    type(jsonf_core_t),intent(inout) :: c
    integer(c_int),intent(in)        :: status
    integer(c_int)                   :: r

    logical(LK) :: status_ok
    character(kind=CK,len=:),allocatable :: msg

    call c%json%check_for_errors(status_ok,msg)
    if (status_ok) then
        r = JSONF_OK
    else
        call set_error(c,msg)
        call c%json%clear_exceptions()
        r = status
    end if

    end function status_from_core
!*****************************************************************************************

!*****************************************************************************************
!>
!  Start a call: get the core from its handle and clear the previous error.

    function enter(core,c) result(status)

    implicit none

    type(c_ptr),intent(in)                 :: core
    type(jsonf_core_t),pointer,intent(out) :: c
    integer(c_int)                         :: status

    nullify(c)
    if (c_associated(core)) then
        call c_f_pointer(core,c)
        call reset_error(c)
        status = JSONF_OK
    else
        status = JSONF_INVALID_ARGUMENT
    end if

    end function enter
!*****************************************************************************************

!*****************************************************************************************
!>
!  Start a call that uses a [[json_value]] handle.

    function enter_node(core,node,c,p) result(status)

    implicit none

    type(c_ptr),intent(in)                 :: core
    type(c_ptr),intent(in)                 :: node
    type(jsonf_core_t),pointer,intent(out) :: c
    type(json_value),pointer,intent(out)   :: p
    integer(c_int)                         :: status

    nullify(p)
    status = enter(core,c)
    if (status==JSONF_OK) then
        if (c_associated(node)) then
            call c_f_pointer(node,p)
        else
            status = fail(c,JSONF_INVALID_ARGUMENT,'Error: the jsonf_value is NULL.')
        end if
    end if

    end function enter_node
!*****************************************************************************************

!*****************************************************************************************
!>
!  Start a call to create a new value: clears `out` and gets the optional name.

    function enter_new(core,name,c,s,out) result(status)

    implicit none

    type(c_ptr),intent(in)                                :: core
    type(c_ptr),intent(in)                                :: name
    type(jsonf_core_t),pointer,intent(out)                :: c
    character(kind=CK,len=:),allocatable,intent(out)      :: s
    type(c_ptr),intent(out),optional                      :: out
    integer(c_int)                                        :: status

    if (present(out)) out = c_null_ptr
    status = enter(core,c)
    if (status==JSONF_OK) then
        if (c_associated(name)) then
            call cstring_to_fortran(name,s)
        else
            s = CK_''
        end if
    end if

    end function enter_new
!*****************************************************************************************

!*****************************************************************************************
!>
!  Start a call that reads from a [[json_value]]. If the `path` is
!  not NULL, then `v` is the variable it refers to, otherwise it is the node.

    function enter_path(core,node,path,c,v) result(status)

    implicit none

    type(c_ptr),intent(in)                 :: core
    type(c_ptr),intent(in)                 :: node
    type(c_ptr),intent(in)                 :: path
    type(jsonf_core_t),pointer,intent(out) :: c
    type(json_value),pointer,intent(out)   :: v
    integer(c_int)                         :: status

    type(json_value),pointer :: p
    character(kind=CK,len=:),allocatable :: s
    logical(LK) :: found

    nullify(v)
    status = enter_node(core,node,c,p)
    if (status/=JSONF_OK) return

    if (c_associated(path)) then
        call cstring_to_fortran(path,s)
        call c%json%get(p,s,v,found)
        if (.not. found) then
            nullify(v)
            status = JSONF_NOT_FOUND
        end if
    else
        v => p
    end if

    end function enter_path
!*****************************************************************************************

!*****************************************************************************************
!>
!  Start a call that writes to a path. The `path` is required.

    function enter_set(core,node,path,c,p,s) result(status)

    implicit none

    type(c_ptr),intent(in)                           :: core
    type(c_ptr),intent(in)                           :: node
    type(c_ptr),intent(in)                           :: path
    type(jsonf_core_t),pointer,intent(out)           :: c
    type(json_value),pointer,intent(out)             :: p
    character(kind=CK,len=:),allocatable,intent(out) :: s
    integer(c_int)                                   :: status

    status = enter_node(core,node,c,p)
    if (status==JSONF_OK) then
        if (c_associated(path)) then
            call cstring_to_fortran(path,s)
        else
            status = fail(c,JSONF_INVALID_ARGUMENT,'Error: the path is NULL.')
        end if
    end if

    end function enter_set
!*****************************************************************************************

!*****************************************************************************************
!>
!  Convert a null-terminated C string to a Fortran string.

    subroutine cstring_to_fortran(str,s)

    implicit none

    type(c_ptr),intent(in)                           :: str
    character(kind=CK,len=:),allocatable,intent(out) :: s

    call copy_from_c(str,c_strlen(str),s)

    end subroutine cstring_to_fortran
!*****************************************************************************************

!*****************************************************************************************
!>
!  Copy `n` characters from C to a Fortran string.

    subroutine copy_from_c(str,n,s)

    implicit none

    type(c_ptr),intent(in)                           :: str
    integer(c_size_t),intent(in)                     :: n
    character(kind=CK,len=:),allocatable,intent(out) :: s

    character(kind=c_char,len=1),dimension(:),pointer :: chars
    integer(c_size_t) :: i  !! counter

    allocate(character(kind=CK,len=n) :: s)
    if (n>0) then
        call c_f_pointer(str,chars,[n])
        do i = 1, n
            s(i:i) = chars(i)
        end do
    end if

    end subroutine copy_from_c
!*****************************************************************************************

!*****************************************************************************************
!>
!  Copy a `(ptr, len)` string value from C.
!  The pointer can only be NULL if the length is zero.

    function value_from_c(c,str,n,s) result(status)

    implicit none

    type(jsonf_core_t),intent(inout)                 :: c
    type(c_ptr),intent(in)                           :: str
    integer(c_size_t),intent(in)                     :: n
    character(kind=CK,len=:),allocatable,intent(out) :: s
    integer(c_int)                                   :: status

    if (n>0 .and. .not. c_associated(str)) then
        status = fail(c,JSONF_INVALID_ARGUMENT,'Error: the string is NULL.')
    else
        call copy_from_c(str,n,s)
        status = JSONF_OK
    end if

    end function value_from_c
!*****************************************************************************************

!*****************************************************************************************
!>
!  Return a Fortran string to C. It is null-terminated, and
!  is freed with [[jsonf_free]]. The length `n` is optional.

    function string_result(c,s,out,n) result(status)

    implicit none

    type(jsonf_core_t),intent(inout)      :: c
    character(kind=CK,len=*),intent(in)   :: s
    type(c_ptr),intent(out),optional      :: out
    integer(c_size_t),intent(out),optional :: n
    integer(c_int)                        :: status

    character(kind=c_char,len=1),dimension(:),pointer :: chars
    integer(c_size_t) :: i     !! counter
    integer(c_size_t) :: ilen  !! length of `s`

    if (present(n)) n = 0_c_size_t

    if (.not. present(out)) then
        status = fail(c,JSONF_INVALID_ARGUMENT,'Error: the output argument is NULL.')
        return
    end if

    ilen = len(s,kind=c_size_t)
    out = c_malloc(ilen+1_c_size_t)
    if (.not. c_associated(out)) then
        status = fail(c,JSONF_ERROR,'Error: unable to allocate the string.')
        return
    end if

    call c_f_pointer(out,chars,[ilen+1_c_size_t])
    do i = 1, ilen
        chars(i) = s(i:i)
    end do
    chars(ilen+1_c_size_t) = c_null_char

    if (present(n)) n = ilen
    status = JSONF_OK

    end function string_result
!*****************************************************************************************

!*****************************************************************************************
!>
!  Return a borrowed node. If it is not associated, the status is `JSONF_NOT_FOUND`.

    function node_result(c,p,out) result(status)

    implicit none

    type(jsonf_core_t),intent(inout)    :: c
    type(json_value),pointer,intent(in) :: p
    type(c_ptr),intent(out),optional    :: out
    integer(c_int)                      :: status

    if (.not. present(out)) then
        status = fail(c,JSONF_INVALID_ARGUMENT,'Error: the output argument is NULL.')
    else if (associated(p)) then
        out = c_loc(p)
        status = JSONF_OK
    else
        out = c_null_ptr
        status = JSONF_NOT_FOUND
    end if

    end function node_result
!*****************************************************************************************

!*****************************************************************************************
!>
!  Return a new root to the caller. It is destroyed if it can't be returned.

    function owned_result(c,p,out) result(status)

    implicit none

    type(jsonf_core_t),intent(inout)    :: c
    type(json_value),pointer            :: p
    type(c_ptr),intent(out),optional    :: out
    integer(c_int)                      :: status

    status = status_from_core(c,JSONF_ERROR)
    if (status==JSONF_OK) then
        status = node_result(c,p,out)
    else
        if (present(out)) out = c_null_ptr
    end if
    if (status/=JSONF_OK) call c%json%destroy(p)

    end function owned_result
!*****************************************************************************************

!*****************************************************************************************
!>
!  Set the default values for the options.

    subroutine jsonf_options_default(opts) bind(C,name='jsonf_options_default')
    !DEC$ ATTRIBUTES DLLEXPORT :: jsonf_options_default

    implicit none

    type(jsonf_options),intent(out),optional :: opts

    if (present(opts)) opts = jsonf_options()

    end subroutine jsonf_options_default
!*****************************************************************************************

!*****************************************************************************************
!>
!  Create a core. If `opts` is NULL, the defaults are used.
!  Returns NULL if the options are not valid.

    function jsonf_core_new(opts) result(core) bind(C,name='jsonf_core_new')
    !DEC$ ATTRIBUTES DLLEXPORT :: jsonf_core_new

    implicit none

    type(jsonf_options),intent(in),optional :: opts
    type(c_ptr)                             :: core

    type(jsonf_core_t),pointer :: c
    type(jsonf_options) :: o  !! local copy of `opts`
    integer :: istat

    core = c_null_ptr

    if (present(opts)) o = opts

    allocate(c,stat=istat)
    if (istat/=0) return

    call c%json%initialize(verbose              = .false._LK, &
                           stop_on_error        = .false._LK, &
                           case_sensitive_keys  = o%case_sensitive_keys/=0, &
                           strict_type_checking = o%strict_type_checking/=0, &
                           path_mode            = o%path_mode)

    if (c%json%failed()) then
        deallocate(c)
    else
        core = c_loc(c)
    end if

    end function jsonf_core_new
!*****************************************************************************************

!*****************************************************************************************
!>
!  Free a core. This has no effect on the [[json_value]] structures.

    subroutine jsonf_core_free(core) bind(C,name='jsonf_core_free')
    !DEC$ ATTRIBUTES DLLEXPORT :: jsonf_core_free

    implicit none

    type(c_ptr),value :: core

    type(jsonf_core_t),pointer :: c

    if (c_associated(core)) then
        call c_f_pointer(core,c)
        deallocate(c)
    end if

    end subroutine jsonf_core_free
!*****************************************************************************************

!*****************************************************************************************
!>
!  Returns the last error message, or NULL if there is no error.
!  The string belongs to the core, and is valid until the next call using it.

    function jsonf_last_error(core) result(msg) bind(C,name='jsonf_last_error')
    !DEC$ ATTRIBUTES DLLEXPORT :: jsonf_last_error

    implicit none

    type(c_ptr),value :: core
    type(c_ptr)       :: msg

    type(jsonf_core_t),pointer :: c

    msg = c_null_ptr
    if (c_associated(core)) then
        call c_f_pointer(core,c)
        if (allocated(c%err)) msg = c_loc(c%err(1))
    end if

    end function jsonf_last_error
!*****************************************************************************************

!*****************************************************************************************
!>
!  Free a string returned by the library.

    subroutine jsonf_free(p) bind(C,name='jsonf_free')
    !DEC$ ATTRIBUTES DLLEXPORT :: jsonf_free

    implicit none

    type(c_ptr),value :: p

    call c_free(p)

    end subroutine jsonf_free
!*****************************************************************************************

!*****************************************************************************************
!>
!  Returns the version of the C API.

    function jsonf_api_version() result(version) bind(C,name='jsonf_api_version')
    !DEC$ ATTRIBUTES DLLEXPORT :: jsonf_api_version

    implicit none

    integer(c_int) :: version

    version = JSONF_API_VERSION_NUMBER

    end function jsonf_api_version
!*****************************************************************************************

!*****************************************************************************************
!>
!  Create a new object.

    function jsonf_new_object(core,name,out) result(status) bind(C,name='jsonf_new_object')
    !DEC$ ATTRIBUTES DLLEXPORT :: jsonf_new_object

    implicit none

    type(c_ptr),value                :: core
    type(c_ptr),value                :: name
    type(c_ptr),intent(out),optional :: out
    integer(c_int)                   :: status

    type(jsonf_core_t),pointer :: c
    type(json_value),pointer :: p
    character(kind=CK,len=:),allocatable :: s

    status = enter_new(core,name,c,s,out)
    if (status/=JSONF_OK) return

    nullify(p)
    call c%json%create_object(p,s)
    status = owned_result(c,p,out)

    end function jsonf_new_object
!*****************************************************************************************

!*****************************************************************************************
!>
!  Create a new array.

    function jsonf_new_array(core,name,out) result(status) bind(C,name='jsonf_new_array')
    !DEC$ ATTRIBUTES DLLEXPORT :: jsonf_new_array

    implicit none

    type(c_ptr),value                :: core
    type(c_ptr),value                :: name
    type(c_ptr),intent(out),optional :: out
    integer(c_int)                   :: status

    type(jsonf_core_t),pointer :: c
    type(json_value),pointer :: p
    character(kind=CK,len=:),allocatable :: s

    status = enter_new(core,name,c,s,out)
    if (status/=JSONF_OK) return

    nullify(p)
    call c%json%create_array(p,s)
    status = owned_result(c,p,out)

    end function jsonf_new_array
!*****************************************************************************************

!*****************************************************************************************
!>
!  Create a new null.

    function jsonf_new_null(core,name,out) result(status) bind(C,name='jsonf_new_null')
    !DEC$ ATTRIBUTES DLLEXPORT :: jsonf_new_null

    implicit none

    type(c_ptr),value                :: core
    type(c_ptr),value                :: name
    type(c_ptr),intent(out),optional :: out
    integer(c_int)                   :: status

    type(jsonf_core_t),pointer :: c
    type(json_value),pointer :: p
    character(kind=CK,len=:),allocatable :: s

    status = enter_new(core,name,c,s,out)
    if (status/=JSONF_OK) return

    nullify(p)
    call c%json%create_null(p,s)
    status = owned_result(c,p,out)

    end function jsonf_new_null
!*****************************************************************************************

!*****************************************************************************************
!>
!  Create a new logical.

    function jsonf_new_logical(core,name,v,out) result(status) bind(C,name='jsonf_new_logical')
    !DEC$ ATTRIBUTES DLLEXPORT :: jsonf_new_logical

    implicit none

    type(c_ptr),value                :: core
    type(c_ptr),value                :: name
    integer(c_int),value             :: v
    type(c_ptr),intent(out),optional :: out
    integer(c_int)                   :: status

    type(jsonf_core_t),pointer :: c
    type(json_value),pointer :: p
    character(kind=CK,len=:),allocatable :: s

    status = enter_new(core,name,c,s,out)
    if (status/=JSONF_OK) return

    nullify(p)
    call c%json%create_logical(p,v/=0,s)
    status = owned_result(c,p,out)

    end function jsonf_new_logical
!*****************************************************************************************

!*****************************************************************************************
!>
!  Create a new integer.

    function jsonf_new_integer(core,name,v,out) result(status) bind(C,name='jsonf_new_integer')
    !DEC$ ATTRIBUTES DLLEXPORT :: jsonf_new_integer

    implicit none

    type(c_ptr),value                :: core
    type(c_ptr),value                :: name
    integer(c_int),value             :: v
    type(c_ptr),intent(out),optional :: out
    integer(c_int)                   :: status

    type(jsonf_core_t),pointer :: c
    type(json_value),pointer :: p
    character(kind=CK,len=:),allocatable :: s

    status = enter_new(core,name,c,s,out)
    if (status/=JSONF_OK) return

    nullify(p)
    call c%json%create_integer(p,v,s)
    status = owned_result(c,p,out)

    end function jsonf_new_integer
!*****************************************************************************************

!*****************************************************************************************
!>
!  Create a new real.

    function jsonf_new_real(core,name,v,out) result(status) bind(C,name='jsonf_new_real')
    !DEC$ ATTRIBUTES DLLEXPORT :: jsonf_new_real

    implicit none

    type(c_ptr),value                :: core
    type(c_ptr),value                :: name
    real(c_double),value             :: v
    type(c_ptr),intent(out),optional :: out
    integer(c_int)                   :: status

    type(jsonf_core_t),pointer :: c
    type(json_value),pointer :: p
    character(kind=CK,len=:),allocatable :: s

    status = enter_new(core,name,c,s,out)
    if (status/=JSONF_OK) return

    nullify(p)
    call c%json%create_real(p,v,s)
    status = owned_result(c,p,out)

    end function jsonf_new_real
!*****************************************************************************************

!*****************************************************************************************
!>
!  Create a new string.

    function jsonf_new_string(core,name,str,n,out) result(status) bind(C,name='jsonf_new_string')
    !DEC$ ATTRIBUTES DLLEXPORT :: jsonf_new_string

    implicit none

    type(c_ptr),value                :: core
    type(c_ptr),value                :: name
    type(c_ptr),value                :: str
    integer(c_size_t),value          :: n
    type(c_ptr),intent(out),optional :: out
    integer(c_int)                   :: status

    type(jsonf_core_t),pointer :: c
    type(json_value),pointer :: p
    character(kind=CK,len=:),allocatable :: s, val

    status = enter_new(core,name,c,s,out)
    if (status/=JSONF_OK) return
    status = value_from_c(c,str,n,val)
    if (status/=JSONF_OK) return

    nullify(p)
    call c%json%create_string(p,val,s)
    status = owned_result(c,p,out)

    end function jsonf_new_string
!*****************************************************************************************

!*****************************************************************************************
!>
!  Add a child to the end of an object or array. The parent now owns the child.
!
!  The child must be a root, and the parent can't be inside of the child.
!  This is checked here since [[json_value_add_member]] does not check for this,
!  and it would corrupt the structure.

    function jsonf_add(core,parent,child) result(status) bind(C,name='jsonf_add')
    !DEC$ ATTRIBUTES DLLEXPORT :: jsonf_add

    implicit none

    type(c_ptr),value :: core
    type(c_ptr),value :: parent
    type(c_ptr),value :: child
    integer(c_int)    :: status

    type(jsonf_core_t),pointer :: c
    type(json_value),pointer :: p, m, tmp, up
    integer(IK) :: var_type

    status = enter_node(core,parent,c,p)
    if (status/=JSONF_OK) return

    if (.not. c_associated(child)) then
        status = fail(c,JSONF_INVALID_ARGUMENT,'Error: the child is NULL.')
        return
    end if
    call c_f_pointer(child,m)

    call c%json%info(p,var_type=var_type)
    if (var_type/=json_object .and. var_type/=json_array) then
        status = fail(c,JSONF_TYPE_ERROR,'Error: can only add a child to an object or array.')
        return
    end if

    call c%json%get_parent(m,tmp)
    if (associated(tmp)) then
        status = fail(c,JSONF_INVALID_ARGUMENT,'Error: the child already has a parent.')
        return
    end if

    ! the parent, or anything above it, can't be the child:
    tmp => p
    do while (associated(tmp))
        if (associated(tmp,m)) then
            status = fail(c,JSONF_INVALID_ARGUMENT,'Error: a value cannot be added to itself.')
            return
        end if
        call c%json%get_parent(tmp,up)
        tmp => up
    end do

    call c%json%add(p,m)
    status = status_from_core(c,JSONF_ERROR)

    end function jsonf_add
!*****************************************************************************************

!*****************************************************************************************
!>
!  Remove a node from its parent, and return ownership of it to the caller.

    function jsonf_detach(core,node) result(status) bind(C,name='jsonf_detach')
    !DEC$ ATTRIBUTES DLLEXPORT :: jsonf_detach

    implicit none

    type(c_ptr),value :: core
    type(c_ptr),value :: node
    integer(c_int)    :: status

    type(jsonf_core_t),pointer :: c
    type(json_value),pointer :: p

    status = enter_node(core,node,c,p)
    if (status/=JSONF_OK) return

    call c%json%remove(p,destroy=.false._LK)
    status = status_from_core(c,JSONF_ERROR)

    end function jsonf_detach
!*****************************************************************************************

!*****************************************************************************************
!>
!  Destroy a node and its children. The node can be anywhere in a structure.
!
!@note This uses `remove`, since `json%destroy` on a node
!      that has a parent will also destroy the later siblings.

    function jsonf_destroy(core,node) result(status) bind(C,name='jsonf_destroy')
    !DEC$ ATTRIBUTES DLLEXPORT :: jsonf_destroy

    implicit none

    type(c_ptr),value :: core
    type(c_ptr),value :: node
    integer(c_int)    :: status

    type(jsonf_core_t),pointer :: c
    type(json_value),pointer :: p

    status = enter_node(core,node,c,p)
    if (status/=JSONF_OK) return

    call c%json%remove(p,destroy=.true._LK)
    status = status_from_core(c,JSONF_ERROR)

    end function jsonf_destroy
!*****************************************************************************************

!*****************************************************************************************
!>
!  Deep copy a node. The copy is a new root.

    function jsonf_clone(core,node,out) result(status) bind(C,name='jsonf_clone')
    !DEC$ ATTRIBUTES DLLEXPORT :: jsonf_clone

    implicit none

    type(c_ptr),value                :: core
    type(c_ptr),value                :: node
    type(c_ptr),intent(out),optional :: out
    integer(c_int)                   :: status

    type(jsonf_core_t),pointer :: c
    type(json_value),pointer :: p, copy

    if (present(out)) out = c_null_ptr
    status = enter_node(core,node,c,p)
    if (status/=JSONF_OK) return

    nullify(copy)
    call c%json%clone(p,copy)
    status = owned_result(c,copy,out)

    end function jsonf_clone
!*****************************************************************************************

!*****************************************************************************************
!>
!  Rename a node.

    function jsonf_rename(core,node,name) result(status) bind(C,name='jsonf_rename')
    !DEC$ ATTRIBUTES DLLEXPORT :: jsonf_rename

    implicit none

    type(c_ptr),value :: core
    type(c_ptr),value :: node
    type(c_ptr),value :: name
    integer(c_int)    :: status

    type(jsonf_core_t),pointer :: c
    type(json_value),pointer :: p
    character(kind=CK,len=:),allocatable :: s

    status = enter_set(core,node,name,c,p,s)
    if (status/=JSONF_OK) return

    call c%json%rename(p,s)
    status = status_from_core(c,JSONF_ERROR)

    end function jsonf_rename
!*****************************************************************************************

!*****************************************************************************************
!>
!  Get the type and number of children of a node. Either output can be NULL.

    function jsonf_info(core,node,var_type,n_children) result(status) bind(C,name='jsonf_info')
    !DEC$ ATTRIBUTES DLLEXPORT :: jsonf_info

    implicit none

    type(c_ptr),value                   :: core
    type(c_ptr),value                   :: node
    integer(c_int),intent(out),optional :: var_type
    integer(c_int),intent(out),optional :: n_children
    integer(c_int)                      :: status

    type(jsonf_core_t),pointer :: c
    type(json_value),pointer :: p
    integer(IK) :: itype, n

    status = enter_node(core,node,c,p)
    if (status/=JSONF_OK) return

    call c%json%info(p,var_type=itype,n_children=n)
    if (present(n_children)) n_children = n
    if (present(var_type)) then
        select case (itype)
        case (json_null);    var_type = type_null
        case (json_object);  var_type = type_object
        case (json_array);   var_type = type_array
        case (json_logical); var_type = type_logical
        case (json_integer); var_type = type_integer
        case (json_real);    var_type = type_real
        case (json_string);  var_type = type_string
        case default
            status = fail(c,JSONF_ERROR,'Error: the variable has an unknown type.')
        end select
    end if

    end function jsonf_info
!*****************************************************************************************

!*****************************************************************************************
!>
!  Get the name of a node.

    function jsonf_name(core,node,str,n) result(status) bind(C,name='jsonf_name')
    !DEC$ ATTRIBUTES DLLEXPORT :: jsonf_name

    implicit none

    type(c_ptr),value                      :: core
    type(c_ptr),value                      :: node
    type(c_ptr),intent(out),optional       :: str
    integer(c_size_t),intent(out),optional :: n
    integer(c_int)                         :: status

    type(jsonf_core_t),pointer :: c
    type(json_value),pointer :: p
    character(kind=CK,len=:),allocatable :: s

    if (present(str)) str = c_null_ptr
    status = enter_node(core,node,c,p)
    if (status/=JSONF_OK) return

    call c%json%info(p,name=s)
    status = string_result(c,s,str,n)

    end function jsonf_name
!*****************************************************************************************

!*****************************************************************************************
!>
!  Get the first child of a node. Returns `JSONF_NOT_FOUND` if there isn't one.

    function jsonf_first_child(core,node,out) result(status) bind(C,name='jsonf_first_child')
    !DEC$ ATTRIBUTES DLLEXPORT :: jsonf_first_child

    implicit none

    type(c_ptr),value                :: core
    type(c_ptr),value                :: node
    type(c_ptr),intent(out),optional :: out
    integer(c_int)                   :: status

    type(jsonf_core_t),pointer :: c
    type(json_value),pointer :: p, child

    if (present(out)) out = c_null_ptr
    status = enter_node(core,node,c,p)
    if (status/=JSONF_OK) return

    call c%json%get_child(p,child)
    status = node_result(c,child,out)

    end function jsonf_first_child
!*****************************************************************************************

!*****************************************************************************************
!>
!  Get the next sibling of a node. Returns `JSONF_NOT_FOUND` if there isn't one.

    function jsonf_next(core,node,out) result(status) bind(C,name='jsonf_next')
    !DEC$ ATTRIBUTES DLLEXPORT :: jsonf_next

    implicit none

    type(c_ptr),value                :: core
    type(c_ptr),value                :: node
    type(c_ptr),intent(out),optional :: out
    integer(c_int)                   :: status

    type(jsonf_core_t),pointer :: c
    type(json_value),pointer :: p, next

    if (present(out)) out = c_null_ptr
    status = enter_node(core,node,c,p)
    if (status/=JSONF_OK) return

    call c%json%get_next(p,next)
    status = node_result(c,next,out)

    end function jsonf_next
!*****************************************************************************************

!*****************************************************************************************
!>
!  Get the parent of a node. Returns `JSONF_NOT_FOUND` if there isn't one.

    function jsonf_parent(core,node,out) result(status) bind(C,name='jsonf_parent')
    !DEC$ ATTRIBUTES DLLEXPORT :: jsonf_parent

    implicit none

    type(c_ptr),value                :: core
    type(c_ptr),value                :: node
    type(c_ptr),intent(out),optional :: out
    integer(c_int)                   :: status

    type(jsonf_core_t),pointer :: c
    type(json_value),pointer :: p, parent

    if (present(out)) out = c_null_ptr
    status = enter_node(core,node,c,p)
    if (status/=JSONF_OK) return

    call c%json%get_parent(p,parent)
    status = node_result(c,parent,out)

    end function jsonf_parent
!*****************************************************************************************

!*****************************************************************************************
!>
!  Get a child of an object by its name.

    function jsonf_child_by_name(core,node,name,out) result(status) &
        bind(C,name='jsonf_child_by_name')
    !DEC$ ATTRIBUTES DLLEXPORT :: jsonf_child_by_name

    implicit none

    type(c_ptr),value                :: core
    type(c_ptr),value                :: node
    type(c_ptr),value                :: name
    type(c_ptr),intent(out),optional :: out
    integer(c_int)                   :: status

    type(jsonf_core_t),pointer :: c
    type(json_value),pointer :: p, child
    character(kind=CK,len=:),allocatable :: s
    logical(LK) :: found

    if (present(out)) out = c_null_ptr
    status = enter_set(core,node,name,c,p,s)
    if (status/=JSONF_OK) return

    call c%json%get_child(p,s,child,found)
    if (.not. found) nullify(child)
    status = node_result(c,child,out)

    end function jsonf_child_by_name
!*****************************************************************************************

!*****************************************************************************************
!>
!  Get a child of an object or array by its 0-based index.

    function jsonf_child_by_index(core,node,i0,out) result(status) &
        bind(C,name='jsonf_child_by_index')
    !DEC$ ATTRIBUTES DLLEXPORT :: jsonf_child_by_index

    implicit none

    type(c_ptr),value                :: core
    type(c_ptr),value                :: node
    integer(c_int),value             :: i0
    type(c_ptr),intent(out),optional :: out
    integer(c_int)                   :: status

    type(jsonf_core_t),pointer :: c
    type(json_value),pointer :: p, child
    integer(IK) :: n
    logical(LK) :: found

    if (present(out)) out = c_null_ptr
    status = enter_node(core,node,c,p)
    if (status/=JSONF_OK) return

    nullify(child)
    call c%json%info(p,n_children=n)
    if (i0>=0 .and. i0<n) then
        call c%json%get_child(p,i0+1_IK,child,found)
        if (.not. found) nullify(child)
    end if
    status = node_result(c,child,out)

    end function jsonf_child_by_index
!*****************************************************************************************

!*****************************************************************************************
!>
!  Get a node from a path. The path is required.

    function jsonf_get_by_path(core,node,path,out) result(status) &
        bind(C,name='jsonf_get_by_path')
    !DEC$ ATTRIBUTES DLLEXPORT :: jsonf_get_by_path

    implicit none

    type(c_ptr),value                :: core
    type(c_ptr),value                :: node
    type(c_ptr),value                :: path
    type(c_ptr),intent(out),optional :: out
    integer(c_int)                   :: status

    type(jsonf_core_t),pointer :: c
    type(json_value),pointer :: p, v
    character(kind=CK,len=:),allocatable :: s
    logical(LK) :: found

    if (present(out)) out = c_null_ptr
    status = enter_set(core,node,path,c,p,s)
    if (status/=JSONF_OK) return

    call c%json%get(p,s,v,found)
    if (.not. found) nullify(v)
    status = node_result(c,v,out)

    end function jsonf_get_by_path
!*****************************************************************************************

!*****************************************************************************************
!>
!  Get a logical. If `path` is NULL, the node itself is read.

    function jsonf_get_logical(core,node,path,val) result(status) &
        bind(C,name='jsonf_get_logical')
    !DEC$ ATTRIBUTES DLLEXPORT :: jsonf_get_logical

    implicit none

    type(c_ptr),value                   :: core
    type(c_ptr),value                   :: node
    type(c_ptr),value                   :: path
    integer(c_int),intent(out),optional :: val
    integer(c_int)                      :: status

    type(jsonf_core_t),pointer :: c
    type(json_value),pointer :: v
    logical(LK) :: tmp

    if (present(val)) val = 0
    status = enter_path(core,node,path,c,v)
    if (status/=JSONF_OK) return

    if (.not. present(val)) then
        status = fail(c,JSONF_INVALID_ARGUMENT,'Error: the output argument is NULL.')
        return
    end if

    call c%json%get(v,tmp)
    status = status_from_core(c,JSONF_TYPE_ERROR)
    if (status==JSONF_OK .and. tmp) val = 1

    end function jsonf_get_logical
!*****************************************************************************************

!*****************************************************************************************
!>
!  Get an integer. If `path` is NULL, the node itself is read.

    function jsonf_get_integer(core,node,path,val) result(status) &
        bind(C,name='jsonf_get_integer')
    !DEC$ ATTRIBUTES DLLEXPORT :: jsonf_get_integer

    implicit none

    type(c_ptr),value                   :: core
    type(c_ptr),value                   :: node
    type(c_ptr),value                   :: path
    integer(c_int),intent(out),optional :: val
    integer(c_int)                      :: status

    type(jsonf_core_t),pointer :: c
    type(json_value),pointer :: v

    if (present(val)) val = 0
    status = enter_path(core,node,path,c,v)
    if (status/=JSONF_OK) return

    if (.not. present(val)) then
        status = fail(c,JSONF_INVALID_ARGUMENT,'Error: the output argument is NULL.')
        return
    end if

    call c%json%get(v,val)
    status = status_from_core(c,JSONF_TYPE_ERROR)

    end function jsonf_get_integer
!*****************************************************************************************

!*****************************************************************************************
!>
!  Get a real. If `path` is NULL, the node itself is read.

    function jsonf_get_real(core,node,path,val) result(status) bind(C,name='jsonf_get_real')
    !DEC$ ATTRIBUTES DLLEXPORT :: jsonf_get_real

    implicit none

    type(c_ptr),value                   :: core
    type(c_ptr),value                   :: node
    type(c_ptr),value                   :: path
    real(c_double),intent(out),optional :: val
    integer(c_int)                      :: status

    type(jsonf_core_t),pointer :: c
    type(json_value),pointer :: v

    if (present(val)) val = 0.0_c_double
    status = enter_path(core,node,path,c,v)
    if (status/=JSONF_OK) return

    if (.not. present(val)) then
        status = fail(c,JSONF_INVALID_ARGUMENT,'Error: the output argument is NULL.')
        return
    end if

    call c%json%get(v,val)
    status = status_from_core(c,JSONF_TYPE_ERROR)

    end function jsonf_get_real
!*****************************************************************************************

!*****************************************************************************************
!>
!  Get a string. If `path` is NULL, the node itself is read.
!  The result is freed with [[jsonf_free]].

    function jsonf_get_string(core,node,path,str,n) result(status) &
        bind(C,name='jsonf_get_string')
    !DEC$ ATTRIBUTES DLLEXPORT :: jsonf_get_string

    implicit none

    type(c_ptr),value                      :: core
    type(c_ptr),value                      :: node
    type(c_ptr),value                      :: path
    type(c_ptr),intent(out),optional       :: str
    integer(c_size_t),intent(out),optional :: n
    integer(c_int)                         :: status

    type(jsonf_core_t),pointer :: c
    type(json_value),pointer :: v
    character(kind=CK,len=:),allocatable :: s

    if (present(str)) str = c_null_ptr
    if (present(n)) n = 0_c_size_t
    status = enter_path(core,node,path,c,v)
    if (status/=JSONF_OK) return

    call c%json%get(v,s)
    status = status_from_core(c,JSONF_TYPE_ERROR)
    if (status==JSONF_OK) status = string_result(c,s,str,n)

    end function jsonf_get_string
!*****************************************************************************************

!*****************************************************************************************
!>
!  Set a logical at a path. Any missing variables are created.

    function jsonf_set_logical(core,node,path,val) result(status) &
        bind(C,name='jsonf_set_logical')
    !DEC$ ATTRIBUTES DLLEXPORT :: jsonf_set_logical

    implicit none

    type(c_ptr),value    :: core
    type(c_ptr),value    :: node
    type(c_ptr),value    :: path
    integer(c_int),value :: val
    integer(c_int)       :: status

    type(jsonf_core_t),pointer :: c
    type(json_value),pointer :: p
    character(kind=CK,len=:),allocatable :: s

    status = enter_set(core,node,path,c,p,s)
    if (status/=JSONF_OK) return

    call c%json%add_by_path(p,s,val/=0)
    status = status_from_core(c,JSONF_ERROR)

    end function jsonf_set_logical
!*****************************************************************************************

!*****************************************************************************************
!>
!  Set an integer at a path. Any missing variables are created.

    function jsonf_set_integer(core,node,path,val) result(status) &
        bind(C,name='jsonf_set_integer')
    !DEC$ ATTRIBUTES DLLEXPORT :: jsonf_set_integer

    implicit none

    type(c_ptr),value    :: core
    type(c_ptr),value    :: node
    type(c_ptr),value    :: path
    integer(c_int),value :: val
    integer(c_int)       :: status

    type(jsonf_core_t),pointer :: c
    type(json_value),pointer :: p
    character(kind=CK,len=:),allocatable :: s

    status = enter_set(core,node,path,c,p,s)
    if (status/=JSONF_OK) return

    call c%json%add_by_path(p,s,val)
    status = status_from_core(c,JSONF_ERROR)

    end function jsonf_set_integer
!*****************************************************************************************

!*****************************************************************************************
!>
!  Set a real at a path. Any missing variables are created.

    function jsonf_set_real(core,node,path,val) result(status) bind(C,name='jsonf_set_real')
    !DEC$ ATTRIBUTES DLLEXPORT :: jsonf_set_real

    implicit none

    type(c_ptr),value    :: core
    type(c_ptr),value    :: node
    type(c_ptr),value    :: path
    real(c_double),value :: val
    integer(c_int)       :: status

    type(jsonf_core_t),pointer :: c
    type(json_value),pointer :: p
    character(kind=CK,len=:),allocatable :: s

    status = enter_set(core,node,path,c,p,s)
    if (status/=JSONF_OK) return

    call c%json%add_by_path(p,s,val)
    status = status_from_core(c,JSONF_ERROR)

    end function jsonf_set_real
!*****************************************************************************************

!*****************************************************************************************
!>
!  Set a string at a path. Any missing variables are created.

    function jsonf_set_string(core,node,path,str,n) result(status) &
        bind(C,name='jsonf_set_string')
    !DEC$ ATTRIBUTES DLLEXPORT :: jsonf_set_string

    implicit none

    type(c_ptr),value       :: core
    type(c_ptr),value       :: node
    type(c_ptr),value       :: path
    type(c_ptr),value       :: str
    integer(c_size_t),value :: n
    integer(c_int)          :: status

    type(jsonf_core_t),pointer :: c
    type(json_value),pointer :: p
    character(kind=CK,len=:),allocatable :: s, val

    status = enter_set(core,node,path,c,p,s)
    if (status/=JSONF_OK) return
    status = value_from_c(c,str,n,val)
    if (status/=JSONF_OK) return

    call c%json%add_by_path(p,s,val)
    status = status_from_core(c,JSONF_ERROR)

    end function jsonf_set_string
!*****************************************************************************************

!*****************************************************************************************
!>
!  Set a null at a path. Any missing variables are created.

    function jsonf_set_null(core,node,path) result(status) bind(C,name='jsonf_set_null')
    !DEC$ ATTRIBUTES DLLEXPORT :: jsonf_set_null

    implicit none

    type(c_ptr),value :: core
    type(c_ptr),value :: node
    type(c_ptr),value :: path
    integer(c_int)    :: status

    type(jsonf_core_t),pointer :: c
    type(json_value),pointer :: p
    character(kind=CK,len=:),allocatable :: s

    status = enter_set(core,node,path,c,p,s)
    if (status/=JSONF_OK) return

    call c%json%add_null_by_path(p,s)
    status = status_from_core(c,JSONF_ERROR)

    end function jsonf_set_null
!*****************************************************************************************

!*****************************************************************************************
!>
!  Get a vector of reals. If `path` is NULL, the node itself is read.
!
!  The length is always returned in `n`. If `buf` is NULL, nothing is copied.
!  If `buf` is too small, nothing is copied and `JSONF_INVALID_ARGUMENT` is returned.

    function jsonf_get_real_vec(core,node,path,buf,cap,n) result(status) &
        bind(C,name='jsonf_get_real_vec')
    !DEC$ ATTRIBUTES DLLEXPORT :: jsonf_get_real_vec

    implicit none

    type(c_ptr),value                      :: core
    type(c_ptr),value                      :: node
    type(c_ptr),value                      :: path
    real(c_double),intent(inout),optional  :: buf(*)
    integer(c_size_t),value                :: cap
    integer(c_size_t),intent(out),optional :: n
    integer(c_int)                         :: status

    type(jsonf_core_t),pointer :: c
    type(json_value),pointer :: v
    real(RK),dimension(:),allocatable :: vec

    if (present(n)) n = 0_c_size_t
    status = enter_path(core,node,path,c,v)
    if (status/=JSONF_OK) return

    if (.not. present(n)) then
        status = fail(c,JSONF_INVALID_ARGUMENT,'Error: the output argument is NULL.')
        return
    end if

    call c%json%get(v,vec)
    status = status_from_core(c,JSONF_TYPE_ERROR)
    if (status/=JSONF_OK) return

    n = size(vec,kind=c_size_t)
    if (present(buf)) then
        if (n>cap) then
            status = fail(c,JSONF_INVALID_ARGUMENT,'Error: the buffer is too small.')
        else
            buf(1:n) = vec
        end if
    end if

    end function jsonf_get_real_vec
!*****************************************************************************************

!*****************************************************************************************
!>
!  Get a vector of integers. If `path` is NULL, the node itself is read.
!
!  The length is always returned in `n`. If `buf` is NULL, nothing is copied.
!  If `buf` is too small, nothing is copied and `JSONF_INVALID_ARGUMENT` is returned.

    function jsonf_get_integer_vec(core,node,path,buf,cap,n) result(status) &
        bind(C,name='jsonf_get_integer_vec')
    !DEC$ ATTRIBUTES DLLEXPORT :: jsonf_get_integer_vec

    implicit none

    type(c_ptr),value                      :: core
    type(c_ptr),value                      :: node
    type(c_ptr),value                      :: path
    integer(c_int),intent(inout),optional  :: buf(*)
    integer(c_size_t),value                :: cap
    integer(c_size_t),intent(out),optional :: n
    integer(c_int)                         :: status

    type(jsonf_core_t),pointer :: c
    type(json_value),pointer :: v
    integer(IK),dimension(:),allocatable :: vec

    if (present(n)) n = 0_c_size_t
    status = enter_path(core,node,path,c,v)
    if (status/=JSONF_OK) return

    if (.not. present(n)) then
        status = fail(c,JSONF_INVALID_ARGUMENT,'Error: the output argument is NULL.')
        return
    end if

    call c%json%get(v,vec)
    status = status_from_core(c,JSONF_TYPE_ERROR)
    if (status/=JSONF_OK) return

    n = size(vec,kind=c_size_t)
    if (present(buf)) then
        if (n>cap) then
            status = fail(c,JSONF_INVALID_ARGUMENT,'Error: the buffer is too small.')
        else
            buf(1:n) = vec
        end if
    end if

    end function jsonf_get_integer_vec
!*****************************************************************************************

    end module json_c_module
!*****************************************************************************************

#endif
