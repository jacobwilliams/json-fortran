!*****************************************************************************************
!>
! Module for the 29th unit test.

module jf_test_29_mod

    use json_module, rk => json_rk, lk => json_lk, ik => json_ik, ck => json_ck, cdk => json_cdk
    use, intrinsic :: iso_fortran_env , only: error_unit, output_unit

    implicit none

contains

    subroutine test_29(error_cnt)

    !! Test the checking for duplicate keys in a JSON structure

    implicit none

    integer,intent(out) :: error_cnt

    type(json_value),pointer :: p
    type(json_core) :: json
    logical(LK) :: has_duplicate
    character(kind=CK,len=:),allocatable :: name
    character(kind=CK,len=:),allocatable :: path

    character(kind=CK,len=*),parameter :: json_str = &
            '{"vars": {"a":1, "b":2, "a":3, "a":4, "c":5} }'

    error_cnt = 0
    call json%initialize()

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') '   TEST 29'
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') ''

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'JSON string: '//json_str

    call json%parse(p,json_str)
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    else

        write(error_unit,'(A)') ''
        call json%check_for_duplicate_keys(p,has_duplicate,name,path)
        if (json%failed()) then
            call json%print_error_message(error_unit)
            error_cnt = error_cnt + 1
        else
            if (has_duplicate) then
                write(output_unit,'(A)') 'Duplicate key found:'
                write(output_unit,'(A)') '  name: '//trim(name)
                write(output_unit,'(A)') '  path: '//trim(path)
                if (name /= CK_'a' .or. path /= CK_'vars.a') then
                    write(error_unit,'(A)') 'Error: incorrect duplicate key name or path'
                    error_cnt = error_cnt + 1
                else
                    write(output_unit,'(A)') 'Test passed'
                end if
            else
                write(error_unit,'(A)') 'Test failed. Duplicate keys not found'
                error_cnt = error_cnt + 1
            end if
        end if

    end if

    call json%destroy(p)

    end subroutine test_29

end module jf_test_29_mod
!*****************************************************************************************

!*****************************************************************************************
program jf_test_29

    !! 29th unit test.

    use jf_test_29_mod , only: test_29
    implicit none
    integer :: n_errors
    n_errors = 0
    call test_29(n_errors)
    if (n_errors /= 0) stop 1

end program jf_test_29
!*****************************************************************************************
