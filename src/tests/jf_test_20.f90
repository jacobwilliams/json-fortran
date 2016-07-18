!*****************************************************************************************
!> author: Jacob Williams
!  date: 7/17/2016
!
!  Test the `insert` routine.

module jf_test_20_mod

    use json_module, lk => json_lk, rk => json_rk, ik => json_ik,&
                     ck => json_ck, cdk => json_cdk
    use, intrinsic :: iso_fortran_env , only: error_unit,output_unit

    implicit none

contains

    subroutine test_20(error_cnt)

    implicit none

    integer,intent(out) :: error_cnt !! report number of errors to caller

    type(json_core) :: json
    type(json_value),pointer :: p,new,element
    logical(lk) :: found,is_valid
    integer(IK),dimension(:),allocatable :: iarray
    character(kind=CK,len=:),allocatable :: error_msg

    character(kind=CK,len=*),parameter :: json_example = '{"x":[1,2,3,4]}'

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') '   TEST 20'
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') ''

    error_cnt = 0

    call json%parse(p,json_example)
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    else

        !insert one in the middle:
        nullify(element)
        call json%get(p,'x(3)',element) ! get pointer to an array element in the file
        if (json%failed()) then
            call json%print_error_message(error_unit)
            error_cnt = error_cnt + 1
        else
            call json%create_integer(new,33,'')   ! create a new element
            call json%insert_after(element,new)   ! insert new element after x(3)
            if (json%failed()) then
                call json%print_error_message(error_unit)
                error_cnt = error_cnt + 1
            else
                call json%get(p,'x',iarray)
                if (.not. all(iarray==[1,2,3,33,4])) then
                    write(error_unit,'(A,1x,*(I2,1X))') 'Error: unexpected output:',iarray
                    error_cnt = error_cnt + 1
                else
                    write(error_unit,'(A,1x,*(I2,1X))') 'Success:',iarray
                end if
            end if
        end if

        !insert one at the end:
        nullify(element)
        call json%get(p,'x(5)',element) ! get pointer to an array element in the file
        if (json%failed()) then
            call json%print_error_message(error_unit)
            error_cnt = error_cnt + 1
        else
            call json%create_integer(new,44,'')   ! create a new element
            call json%insert_after(element,new)   ! insert new element after x(5)
            if (json%failed()) then
                call json%print_error_message(error_unit)
                error_cnt = error_cnt + 1
            else
                call json%get(p,'x',iarray)
                if (.not. all(iarray==[1,2,3,33,4,44])) then
                    write(error_unit,'(A,1x,*(I2,1X))') 'Error: unexpected output:',iarray
                    error_cnt = error_cnt + 1
                else
                    write(error_unit,'(A,1x,*(I2,1X))') 'Success:',iarray
                end if
            end if
        end if

        !now, insert by index:
        nullify(element)
        call json%get(p,'x',element) ! get pointer to the array itself
        if (json%failed()) then
            call json%print_error_message(error_unit)
            error_cnt = error_cnt + 1
        else
            call json%create_integer(new,22,'')   ! create a new element
            call json%insert_after(element,2,new) ! insert new element after x(2)
            if (json%failed()) then
                call json%print_error_message(error_unit)
                error_cnt = error_cnt + 1
            else
                call json%get(p,'x',iarray)
                if (.not. all(iarray==[1,2,22,3,33,4,44])) then
                    write(error_unit,'(A,1x,*(I2,1X))') 'Error: unexpected output:',iarray
                    error_cnt = error_cnt + 1
                else
                    write(error_unit,'(A,1x,*(I2,1X))') 'Success:',iarray
                end if
            end if
        end if

        call json%validate(p,is_valid,error_msg)
        if (.not. is_valid) then
            write(error_unit,'(A)') trim(error_msg)
            error_cnt = error_cnt + 1
        end if

        !just in case:
        if (json%failed()) then
            call json%print_error_message(error_unit)
            error_cnt = error_cnt + 1
        end if

    end if

    ! cleanup:
    call json%destroy(p)

    ! now, just a test of the edge case:
    ! (where p doesn't have a parent)
    call json%create_object(p,'root')
    call json%create_object(new,'next')
    call json%insert_after(p,new)
    call json%destroy(p)

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') ''

    end subroutine test_20

end module jf_test_20_mod
!*****************************************************************************************

!*****************************************************************************************
program jf_test_20

    !! 20th unit test.

    use jf_test_20_mod, only: test_20

    implicit none

    integer :: n_errors
    call test_20(n_errors)
    if ( n_errors /= 0) stop 1

end program jf_test_20
!*****************************************************************************************
