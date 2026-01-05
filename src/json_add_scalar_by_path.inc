! to use this include file, the following variables must be defined:
!   * value        : the scalar value to add
!   * json_type    : the JSON type of the value to add
!
! also, the following subroutines must be defined:
!   * assign() : assigns 'value' to p
!   * create() : creates a new json_value of type 'json_type' with
!                'value' and name 'name', and returns it in 'tmp'

    class(json_core),intent(inout)      :: json
    type(json_value),pointer            :: me           !! the JSON structure
    character(kind=CK,len=*),intent(in) :: path         !! the path to the variable
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

            call json%throw_exception('Error in json_add_scalar_by_path:'//&
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

            if (p%var_type==json_type) then
                call assign()
            else
                call json%info(p,name=name)
                call create()
                call json%replace(p,tmp,destroy=.true.)
            end if
        end if

    else
        if ( present(found) )       found = .false.
        if ( present(was_created) ) was_created = .false.
    end if