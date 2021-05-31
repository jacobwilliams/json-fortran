!*****************************************************************************************
!>
! Module for the 29th unit test.

module jf_test_29_mod

    use json_module, rk => json_rk, lk => json_lk, ik => json_ik, ck => json_ck, cdk => json_cdk
    use, intrinsic :: iso_fortran_env , only: error_unit, output_unit

    implicit none

    private
    public :: test_29

contains

    subroutine test_29(error_cnt)

    !! Test the checking for duplicate keys in a JSON structure

    implicit none

    integer,intent(out) :: error_cnt

    error_cnt = 0

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') '   TEST 29'
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') ''

    call test(CK_'{"vars":{"a":1,"b":2,"a":3,"a":4,"c":5}}',.true.,CK_'a',CK_'vars.a')
    call test(CK_'{"vars":{"a":1,"a":3}}',.true.,CK_'a',CK_'vars.a')
    call test(CK_'{"vars":{"aaa":1,"b":2,"aaa":3,"a":4,"c":5}}',.true.,CK_'aaa',CK_'vars.aaa')
    call test(CK_'{"vars":{"aaaa":1,"aaaa":3}}',.true.,CK_'aaaa',CK_'vars.aaaa')
    call test(CK_'{"a":1,"b":2,"a":3,"a":4,"c":5}',.true.,CK_'a',CK_'a')
    call test(CK_'{"c":5}',.false.,CK_'',CK_'')
    call test(CK_'{"vars":{"c":5},"array":[1,2]}',.false.,CK_'',CK_'')
    call test(CK_'{}',.false.,CK_'',CK_'')

    contains

        subroutine test(json_str,correct_has_duplicate,correct_name,correct_path)

        implicit none

        character(kind=CK,len=*),intent(in) :: json_str !! JSON string to check
        logical(LK),intent(in)              :: correct_has_duplicate !! expected result
        character(kind=CK,len=*),intent(in) :: correct_name !! expected result
        character(kind=CK,len=*),intent(in) :: correct_path !! expected result

        type(json_value),pointer :: p
        type(json_core) :: json
        logical(LK) :: has_duplicate
        character(kind=CK,len=:),allocatable :: name
        character(kind=CK,len=:),allocatable :: path
        logical(LK) :: is_valid
        character(kind=CK,len=:),allocatable :: error_msg

        call json%initialize(no_whitespace=.true.)

        write(error_unit,'(A)') ''
        write(error_unit,'(A)') 'JSON string: '//json_str

        call json%deserialize(p,json_str)
        if (json%failed()) then
            call json%print_error_message(error_unit)
            error_cnt = error_cnt + 1
        else

            write(error_unit,'(A)') ''

            ! just test all options:
            call json%check_for_duplicate_keys(p,has_duplicate,name=name)
            call json%check_for_duplicate_keys(p,has_duplicate,path=path)
            call json%check_for_duplicate_keys(p,has_duplicate)
            call json%check_for_duplicate_keys(p,has_duplicate,name=name,path=path)
            if (json%failed()) then
                call json%print_error_message(error_unit)
                error_cnt = error_cnt + 1
            else
                if (correct_has_duplicate .neqv. has_duplicate) then
                    write(error_unit,'(A)') '   Test failed.'
                    error_cnt = error_cnt + 1
                else
                    if (has_duplicate) then
                        write(output_unit,'(A)') '   Duplicate key found:'
                        write(output_unit,'(A)') '    name: '//trim(name)
                        write(output_unit,'(A)') '    path: '//trim(path)
                        if (name/=correct_name .or. path/=correct_path) then
                            write(error_unit,'(A)') '   Error: incorrect duplicate key name or path'
                            error_cnt = error_cnt + 1
                        else
                            write(output_unit,'(A)') '   Test passed: correct duplicate found'
                        end if
                    else
                        write(output_unit,'(A)') '   Test passed: no duplicates present'
                    end if
                end if
            end if

        end if
        call json%destroy(p)
        call json%destroy()

        ! also check using two other methods:
        if (error_cnt==0) then

            ! check when the string is parsed:
            call json%initialize(allow_duplicate_keys=.false.)
            call json%deserialize(p,json_str)
            if (json%failed() .eqv. correct_has_duplicate) then
                write(output_unit,'(A)') '   Test passed: parse'
            else
                write(output_unit,'(A)') '   Test failed: parse'
                error_cnt = error_cnt + 1
            end if
            call json%destroy(p)
            call json%destroy()

            ! check by explicit call to validate:
            call json%initialize()  ! don't throw an exception when parsing
            call json%deserialize(p,json_str)
            call json%initialize(allow_duplicate_keys=.false.)
            call json%validate(p,is_valid,error_msg)
            if (is_valid .eqv. (.not. correct_has_duplicate)) then
                write(output_unit,'(A)') '   Test passed: validate'
            else
                write(output_unit,'(A)') '   Test failed: validate'
                error_cnt = error_cnt + 1
            end if
            call json%destroy(p)
            call json%destroy()

        end if

        end subroutine test

    end subroutine test_29

end module jf_test_29_mod
!*****************************************************************************************

#ifndef INTEGRATED_TESTS
!*****************************************************************************************
program jf_test_29

    !! 29th unit test.

    use jf_test_29_mod , only: test_29
    implicit none
    integer :: n_errors
    call test_29(n_errors)
    if (n_errors /= 0) stop 1

end program jf_test_29
!*****************************************************************************************
#endif
